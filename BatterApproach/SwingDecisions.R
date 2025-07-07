library(tidyverse)
library(purrr)
library(gridExtra)
library(gt)

# Data Collection
kcl_path = "/Users/noahlippman/Documents/Github/Kernelytics-Projects/kclData"
kcl_files <- list.files(path = kcl_path)
game_data <- read.csv(paste(kcl_path, "/", kcl_files[1], sep = ""))

for(i in kcl_files[2:length(kcl_files)]){
  game_data = bind_rows(game_data, read.csv(paste(kcl_path,"/",i, sep = "")))
}


## Fix Names in DataFrame ##
corrections <- c(
  "Justin Trusner" = "Jacob Trusner",
  "Brooks Neuhof" = "Brooks Neuhoff",
  "Teagan Disharoom" = "Teagan Disharoon",
  "Kam Ross" = "Kam Ross",
  "Brayden Windy" = "Brayden Windy",
  "Sammy Descarpentrie" = "Sam DesCarpentrie"
)

fixNames <- function(df, column, lookup){
  old_values <- df[[column]]
  needs_fix <- old_values %in% names(lookup)
  df[[column]] <- ifelse(
    needs_fix,
    lookup[old_values],
    old_values
  )
  return(df)
}
game_data <- fixNames(game_data, "Catcher", corrections) %>%
  filter(PitchUUID != "")

## xWOBACON for each player by zone ##
pitchData <- read.csv("/Users/noahlippman/Documents/GitHub/Kernelytics-Projects/BaseballSavant/Pitch_By_Pitch_xWOBA.csv") %>% 
  filter(PitchUUID != "")

xWOBA_Data <- game_data %>%
  right_join(pitchData, join_by(PitchUUID)) %>%
  mutate(isSwing = if_else(PitchCall %in% c("Foul", "StrikeSwinging", "InPlay"),1,0))

xWOBACON_Data <- xWOBA_Data %>%
  mutate(xZone = case_when(
    PlateLocSide <= 1.3333 & PlateLocSide >= 1 ~ 0,
    PlateLocSide < 1 & PlateLocSide >= .3333 ~ 1,
    PlateLocSide < .3333 & PlateLocSide >= -.3333 ~ 2,
    PlateLocSide < -.3333 & PlateLocSide >= -1 ~ 3,
    PlateLocSide < -1 & PlateLocSide >= -1.3333 ~ 4
  )) %>%
  mutate(yZone = case_when(
    PlateLocHeight >= .84 & PlateLocHeight <= 1.5 ~ 0,
    PlateLocHeight > 1.5 & PlateLocHeight <= 2.17 ~ 1,
    PlateLocHeight > 2.17 & PlateLocHeight <= 2.83 ~ 2,
    PlateLocHeight > 2.83 & PlateLocHeight <= 3.5 ~ 3,
    PlateLocHeight > 3.5 & PlateLocHeight <= 4.16 ~ 4
  )) %>%
  mutate(SwingingStrike = if_else(PitchCall %in% c("Foul","StrikeSwinging"), 1,0))

xWOBACON_Player_Data <- xWOBACON_Data %>%
  filter(!is.na(xZone)) %>%
  filter(!is.na(yZone)) %>%
  group_by(Batter, xZone, yZone) %>%
  filter(PitchCall == "InPlay") %>%
  summarise(xWOBA = mean(xWOBA))

strikeProduced_Player_Data <- xWOBACON_Data %>%
  filter(!is.na(xZone)) %>%
  filter(!is.na(yZone)) %>%
  filter(PitchCall %in% c("Foul", "StrikeSwinging", "InPlay")) %>%
  mutate(isFoul = if_else(PitchCall == "Foul", 1, 0)) %>%
  group_by(Batter, xZone, yZone) %>%
  summarise(SwingingStrikeAverage = mean(SwingingStrike), foulBallPercent = mean(isFoul))

mergedData <- xWOBACON_Player_Data %>%
  full_join(strikeProduced_Player_Data, join_by(Batter, xZone, yZone))

## Average WOBA for each count for each player##
player_Count_xWOBA <- function(player_name){
  xWOBA_Data_P <- xWOBA_Data %>%
    mutate(atBatID = paste(Date,PAofInning,PitcherTeam,BatterTeam,Inning)) %>%
    filter(Batter == player_name)
  
  abResults <- xWOBA_Data_P %>% 
    filter(!(is.na(xWOBA))) %>%
    rename(xWOBA_Result = xWOBA) %>%
    select(atBatID, xWOBA_Result)
  
  xWOBA_Data_P <- xWOBA_Data_P %>%
    inner_join(abResults, join_by(atBatID))
  
  countResults_P <- xWOBA_Data_P %>% 
    group_by(Balls, Strikes) %>% 
    summarise(averageXWOBA = mean(xWOBA_Result), atBats = length(xWOBA_Result))
  
  return(countResults_P %>% arrange(Strikes, Balls))
}

## Function to calculate the grade of a swing ##
swingScore <- function(player_name, row){
  
  walkStrikeoutValues <- tibble(
    Balls = c(0,1,2,3,4,4,4),
    Strikes = c(3,3,3,3,2,1,0),
    averageXWOBA = c(0,0,0,0,.695,.695,.695)
  )
  
  zoneXWOBA_Data <- mergedData %>%
    filter(Batter == player_name)
  countXOBA_Data <- bind_rows(player_Count_xWOBA(player_name), walkStrikeoutValues)
  
  isStrike <- if_else(row$xZone < 4 & row$xZone > 0 & row$yZone < 4 & row$yZone > 0, 1, 0)
  strikeNewXWOBA <- (countXOBA_Data %>% filter(Balls == row$Balls & Strikes == row$Strikes + 1))$averageXWOBA
  ballNewXWOBA <- (countXOBA_Data %>% filter(Balls == row$Balls + 1 & Strikes == row$Strikes))$averageXWOBA
  # If chase
  if(is.na(row$xZone) || is.na(row$yZone)) {
    return(strikeNewXWOBA - ballNewXWOBA)
  }
  # If there is no xWOBA data for specific zone
  else if(nrow(zoneXWOBA_Data %>% filter(xZone == row$xZone & yZone == row$yZone)) == 0 || is.na((zoneXWOBA_Data %>% filter(xZone == row$xZone & yZone == row$yZone))$xWOBA)){
    meanXWOBA <- mean((xWOBACON_Data %>% 
                         filter(xZone == row$xZone & yZone == row$yZone) %>%
                         filter(PitchCall == "InPlay"))$xWOBA)
    meanStrikeProb <- mean((xWOBACON_Data %>% 
                              filter(xZone == row$xZone & yZone == row$yZone) %>%
                              filter(PitchCall == "InPlay"))$SwingingStrike)
    return((1 - meanStrikeProb) * meanXWOBA + (meanStrikeProb * strikeNewXWOBA) - if_else(isStrike == 1, strikeNewXWOBA, ballNewXWOBA))
  } 
  else{
    zoneXWOBA <- (zoneXWOBA_Data %>% filter(xZone == row$xZone & yZone == row$yZone))$xWOBA
    swingingStrikeProb <- (zoneXWOBA_Data %>% filter(xZone == row$xZone & yZone == row$yZone))$SwingingStrikeAverage
    score <- (1 - swingingStrikeProb) * zoneXWOBA + (swingingStrikeProb * strikeNewXWOBA) - if_else(isStrike == 1, strikeNewXWOBA, ballNewXWOBA)
    return(score)
  } 
}

## Function to calculate the grade of a Take ##
takeScore <- function(player_name, row){
  
  walkStrikeoutValues <- tibble(
    Balls = c(0,1,2,3,4,4,4),
    Strikes = c(3,3,3,3,2,1,0),
    averageXWOBA = c(0,0,0,0,.695,.695,.695)
  )
  
  zoneXWOBA_Data <- mergedData %>%
    filter(Batter == player_name)
  countXOBA_Data <- bind_rows(player_Count_xWOBA(player_name), walkStrikeoutValues)
  
  isStrike <- if_else(row$xZone < 4 & row$xZone > 0 & row$yZone < 4 & row$yZone > 0, 1, 0)
  strikeNewXWOBA <- (countXOBA_Data %>% filter(Balls == row$Balls & Strikes == row$Strikes + 1))$averageXWOBA
  ballNewXWOBA <- (countXOBA_Data %>% filter(Balls == row$Balls + 1 & Strikes == row$Strikes))$averageXWOBA
  
  if(is.na(row$xZone) || is.na(row$yZone)) {
    return(ballNewXWOBA - strikeNewXWOBA)
  }
  else if(nrow(zoneXWOBA_Data %>% filter(xZone == row$xZone & yZone == row$yZone)) == 0 || is.na((zoneXWOBA_Data %>% filter(xZone == row$xZone & yZone == row$yZone))$xWOBA)){
    meanXWOBA <- mean((xWOBACON_Data %>% 
                         filter(xZone == row$xZone & yZone == row$yZone) %>%
                         filter(PitchCall == "InPlay"))$xWOBA)
    meanStrikeProb <- mean((xWOBACON_Data %>% 
                              filter(xZone == row$xZone & yZone == row$yZone) %>%
                              filter(PitchCall == "InPlay"))$SwingingStrike)
    return(if_else(isStrike == 1, strikeNewXWOBA, ballNewXWOBA) - ((1 - meanStrikeProb) * meanXWOBA + (meanStrikeProb * strikeNewXWOBA)))
  }
  else{
    zoneXWOBA <- (zoneXWOBA_Data %>% filter(xZone == row$xZone & yZone == row$yZone))$xWOBA
    swingingStrikeProb <- (zoneXWOBA_Data %>% filter(xZone == row$xZone & yZone == row$yZone))$SwingingStrikeAverage
    score <- if_else(isStrike == 1, strikeNewXWOBA, ballNewXWOBA) - ((1 - swingingStrikeProb) * zoneXWOBA + (swingingStrikeProb * strikeNewXWOBA))
    return(score)
  }
}

decisionScoreData <- function(player_name, data){
  playerPitches <- data %>%
    filter(Batter == player_name)
  playerPitches <- playerPitches %>%
    mutate(decisionScore = pmap_dbl(
      list(xZone = xZone, yZone = yZone, Balls = Balls, Strikes = Strikes, PitchCall = PitchCall),
      function(xZone, yZone, Balls, Strikes, PitchCall) {
        rowData <- tibble(xZone = xZone, yZone = yZone, Balls = Balls, Strikes = Strikes, PitchCall = PitchCall)
        if (PitchCall %in% c("Foul", "StrikeSwinging", "InPlay")) {
          swingScore(player_name, rowData)
        } else {
          takeScore(player_name, rowData)
        }
      }
    ))
  
  return(playerPitches %>% select(PitchCall, xZone, yZone, Balls, Strikes, decisionScore))
}

playerDecisionScores <- function(){
  players <- tibble(Batter = unique(xWOBACON_Data$Batter), 
                    totalDecisionScore = 0,
                    pitches = 0,
                    possibleScore = 0)
  errorPlayers = c()
  for(player in players$Batter){
    tryCatch({
      decisionData <- decisionScoreData(player, xWOBACON_Data)
      total <- sum(decisionData$decisionScore, na.rm = TRUE)
      pitches <- length(decisionData$decisionScore)
      possibleScore <- sum(abs(decisionData$decisionScore), na.rm = TRUE)
      
      players$totalDecisionScore[players$Batter == player] <- total
      players$pitches[players$Batter == player] <- pitches
      players$possibleScore[players$Batter == player] <- possibleScore
      },error = function(e){
        errorPlayers <- append(errorPlayers, player)
      })
  }
  print(errorPlayers)
  return(players)
}

playerScores <- playerDecisionScores()

league_Count_xWOBA <- function(){
  xWOBA_Data_P <- xWOBA_Data %>%
    mutate(atBatID = paste(Date,PAofInning,PitcherTeam,BatterTeam,Inning))
  
  abResults <- xWOBA_Data_P %>% 
    filter(!(is.na(xWOBA))) %>%
    rename(xWOBA_Result = xWOBA) %>%
    select(atBatID, xWOBA_Result)
  
  xWOBA_Data_P <- xWOBA_Data_P %>%
    inner_join(abResults, join_by(atBatID))
  
  countResults_P <- xWOBA_Data_P %>% 
    group_by(Balls, Strikes) %>% 
    summarise(averageXWOBA = mean(xWOBA_Result), atBats = length(xWOBA_Result))
  
  print(paste("Mean League xWOBA = ", mean(xWOBACON_Data$xWOBA, na.rm = TRUE)))
  return(countResults_P %>% arrange(desc(averageXWOBA)))
}

# Process Data into heatmap data
data_processor <- function(data){
  in_zone_Matrix <- data %>%
    filter(xZone > 0 & xZone < 4 & yZone > 0 & yZone < 4) %>%
    group_by(yZone, xZone) %>%
    mutate(height = 1) %>%
    mutate(width  = 1)
  
  xOut_Matrix <- data %>%
    filter(xZone == 0 | xZone == 4 & yZone != 0 & yZone != 4) %>%
    group_by(xZone) %>%
    summarise(xWOBA = mean(xWOBA, na.rm = TRUE), SwingingStrikeAverage = mean(SwingingStrikeAverage, na.rm = TRUE)) %>%
    mutate(yZone = 2) %>%
    mutate(height = 3) %>%
    mutate(width  = 1)
  
  yOut_Matrix <- data %>%
    filter(yZone == 0 | yZone == 4 & xZone != 0 & xZone != 4) %>%
    group_by(yZone) %>%
    summarise(xWOBA = mean(xWOBA, na.rm = TRUE), SwingingStrikeAverage = mean(SwingingStrikeAverage, na.rm = TRUE)) %>%
    mutate(xZone = 2) %>%
    mutate(height = 1) %>%
    mutate(width  = 3)
  
  corner_Matrix <- data %>%
    filter((xZone == 0 & yZone == 0)|
             (xZone == 0 & yZone == 4)|
             (xZone == 4 & yZone == 0)|
             (xZone == 4 & yZone == 4)) %>%
    mutate(height = 1) %>%
    mutate(width  = 1)
  
  full <- rbind(in_zone_Matrix, xOut_Matrix, yOut_Matrix, corner_Matrix) 
  return(full)
}

xWOBA_HeatMap <- function(player_name) {
  old_data <- mergedData  %>% filter(Batter == player_name)
  data <- data_processor(old_data)
  p <- ggplot(data, aes(xZone, yZone, fill = xWOBA)) +
    geom_tile(aes(height = height, width = width)) +
    ylim(-2,6) +
    xlim(-6, 10) + 
    theme_void() +
    scale_fill_gradientn(
      colours = c("#1E90FF", "#87CEEB", "#ADD8E6", "white","#FFC1CC", "#FFC1CC", "#FF6666", "#FF6666", "#FF6666"),
      values = scales::rescale(c(0,.15,.3,.35,.4,.6,.8,1)),
      na.value = "grey90"
    ) +
    labs(fill = "xWOBA") + 
    theme(legend.position = c(.75,.5)) + 
    geom_text(aes(label = round(xWOBA,2)), color = "black") +
    geom_segment(aes(x = .5, y = .5, xend = 3.5, yend = .5), linewidth = 1) + 
    geom_segment(aes(x = .5, y = 3.5, xend = 3.5, yend = 3.5), linewidth = 1) + 
    geom_segment(aes(x = .5, y = .5, xend = .5, yend = 3.5), linewidth = 1) +  
    geom_segment(aes(x = 3.5, y = .5, xend = 3.5, yend = 3.5), linewidth = 1) +
    geom_segment(aes(x = .5, y = -1, xend = 3.5, yend = -1), linewidth = 1) +
    geom_segment(aes(x = .5, y = -1, xend = .5, yend = -1.5), linewidth = 1) +
    geom_segment(aes(x = 3.5, y = -1, xend = 3.5, yend = -1.5), linewidth = 1) +
    geom_segment(aes(x = .5, y = -1.5, xend = 2, yend = -2), linewidth = 1) +
    geom_segment(aes(x = 3.5, y = -1.5, xend = 2, yend = -2), linewidth = 1) + 
    ggtitle(paste("xWOBA by Zone For", player_name)) +
    theme(plot.title = element_text(hjust = .5)) + 
    theme(plot.title = element_text(vjust = -10)) +
    theme(plot.subtitle = element_text(hjust =.5, vjust = -20))
  return(p)
}

inPlayRate_Heat_Map <- function(player_name){
  old_data <- mergedData %>% filter(Batter == player_name)
  data <- data_processor(old_data) %>% 
    mutate(InPlayRate = 1 - SwingingStrikeAverage)
  p <- ggplot(data, aes(xZone, yZone, fill = InPlayRate)) +
    geom_tile(aes(height = height, width = width)) +
    ylim(-2,6) +
    xlim(-6, 10) + 
    theme_void() +
    scale_fill_gradientn(
      colours = c("#1E90FF", "#87CEEB", "#ADD8E6", "white", "#FFC1CC", "#FF6666", "#FF6666"),
      values = scales::rescale(c(0,.1,.2,.3,.4,.5,.6)),
      na.value = "grey90"
    ) +
    labs(fill = "InPlayRate") + 
    theme(legend.position = c(.75,.5)) + 
    geom_text(aes(label = round(InPlayRate,2)), color = "black") +
    geom_segment(aes(x = .5, y = .5, xend = 3.5, yend = .5), linewidth = 1) + 
    geom_segment(aes(x = .5, y = 3.5, xend = 3.5, yend = 3.5), linewidth = 1) + 
    geom_segment(aes(x = .5, y = .5, xend = .5, yend = 3.5), linewidth = 1) +  
    geom_segment(aes(x = 3.5, y = .5, xend = 3.5, yend = 3.5), linewidth = 1) +
    geom_segment(aes(x = .5, y = -1, xend = 3.5, yend = -1), linewidth = 1) +
    geom_segment(aes(x = .5, y = -1, xend = .5, yend = -1.5), linewidth = 1) +
    geom_segment(aes(x = 3.5, y = -1, xend = 3.5, yend = -1.5), linewidth = 1) +
    geom_segment(aes(x = .5, y = -1.5, xend = 2, yend = -2), linewidth = 1) +
    geom_segment(aes(x = 3.5, y = -1.5, xend = 2, yend = -2), linewidth = 1) + 
    ggtitle(paste("InPlayRate by Zone For", player_name)) +
    theme(plot.title = element_text(hjust = .5)) + 
    theme(plot.title = element_text(vjust = -10)) +
    theme(plot.subtitle = element_text(hjust =.5, vjust = -20))
  return(p)
}

swingDecisionMatrix <- function(player_name){
  pitchCall <- c("InPlay")
  xZone <- c(0,1,2,3,4)
  yZone <- c(0,1,2,3,4)
  balls <- c(0,1,2,3)
  strikes <- c(0,1,2)
  situations <- expand.grid(PitchCall = pitchCall, xZone = xZone, yZone = yZone, Balls = balls, Strikes = strikes) %>%
    mutate(Batter = player_name)
  
  situation_Data <- decisionScoreData(player_name, situations) %>%
    mutate(swingScore = decisionScore)
  
  situation_Data <- situation_Data %>%
    mutate(takeScore = -1 * swingScore) %>%
    select(-c(PitchCall, decisionScore))
           
  return(situation_Data)
}

swingRate_Heat_Map <- function(player_name, swingData){
  p <- ggplot(swingData, aes(xZone, yZone, fill = swingRate)) +
    geom_tile() +
    ylim(-2,6) +
    xlim(-6, 10) + 
    theme_void() +
    scale_fill_gradientn(
      colours = c("#1E90FF", "#87CEEB", "#ADD8E6", "white", "#FFC1CC", "#FF6666", "#FF6666"),
      na.value = "grey90"
    ) +
    labs(fill = "Swing Rate") + 
    theme(legend.position = c(.75,.5)) + 
    geom_text(aes(label = round(swingRate,2)), color = "black") +
    geom_segment(aes(x = .5, y = .5, xend = 3.5, yend = .5), linewidth = 1) + 
    geom_segment(aes(x = .5, y = 3.5, xend = 3.5, yend = 3.5), linewidth = 1) + 
    geom_segment(aes(x = .5, y = .5, xend = .5, yend = 3.5), linewidth = 1) +  
    geom_segment(aes(x = 3.5, y = .5, xend = 3.5, yend = 3.5), linewidth = 1) +
    geom_segment(aes(x = .5, y = -1, xend = 3.5, yend = -1), linewidth = 1) +
    geom_segment(aes(x = .5, y = -1, xend = .5, yend = -1.5), linewidth = 1) +
    geom_segment(aes(x = 3.5, y = -1, xend = 3.5, yend = -1.5), linewidth = 1) +
    geom_segment(aes(x = .5, y = -1.5, xend = 2, yend = -2), linewidth = 1) +
    geom_segment(aes(x = 3.5, y = -1.5, xend = 2, yend = -2), linewidth = 1) + 
    ggtitle(paste("Swing Rate for",player_name,"while",swingData$countType, "in the Count")) +
    theme(plot.title = element_text(hjust = .5)) + 
    theme(plot.title = element_text(vjust = -10)) +
    theme(plot.subtitle = element_text(hjust =.5, vjust = -20))
  return(p)
}

swingGrade_Heat_Map <- function(player_name, swingDecisionData){
  p <- ggplot(swingDecisionData, aes(xZone, yZone, fill = swingScore)) +
    geom_tile() +
    ylim(-2,6) +
    xlim(-6, 10) + 
    theme_void() +
    scale_fill_gradientn(
      colours = c("#1E90FF", "#87CEEB", "#ADD8E6", "white", "#FFC1CC", "#FF6666", "#FF6666"),
      values = scales::rescale(c(-.6, -.35, -.1, 0, .1, .35, .6), from = c(-0.6, 0.6)),
      na.value = "grey90",
      limits = c(-1,1)
    ) +
    labs(fill = "Swing Decision Score") + 
    theme(legend.position = c(.75,.5)) + 
    geom_text(aes(label = round(swingScore,2)), color = "black") +
    geom_segment(aes(x = .5, y = .5, xend = 3.5, yend = .5), linewidth = 1) + 
    geom_segment(aes(x = .5, y = 3.5, xend = 3.5, yend = 3.5), linewidth = 1) + 
    geom_segment(aes(x = .5, y = .5, xend = .5, yend = 3.5), linewidth = 1) +  
    geom_segment(aes(x = 3.5, y = .5, xend = 3.5, yend = 3.5), linewidth = 1) +
    geom_segment(aes(x = .5, y = -1, xend = 3.5, yend = -1), linewidth = 1) +
    geom_segment(aes(x = .5, y = -1, xend = .5, yend = -1.5), linewidth = 1) +
    geom_segment(aes(x = 3.5, y = -1, xend = 3.5, yend = -1.5), linewidth = 1) +
    geom_segment(aes(x = .5, y = -1.5, xend = 2, yend = -2), linewidth = 1) +
    geom_segment(aes(x = 3.5, y = -1.5, xend = 2, yend = -2), linewidth = 1) + 
    ggtitle(paste("Swing Decision Score by Zone For", player_name,"while", swingDecisionData$countType, "in the Count")) +
    theme(plot.title = element_text(hjust = .5)) + 
    theme(plot.title = element_text(vjust = -10)) +
    theme(plot.subtitle = element_text(hjust =.5, vjust = -20))
  return(p)
}

swingGrade_HeatDashBoard <- function(player_name){
  swingDecisionData <- swingDecisionMatrix(player_name = player_name)
  
  swingDecisionData <- swingDecisionData %>%
    mutate(countType = case_when(
      (Balls*10 + Strikes) %in% c(30,31,20,10) ~ "Ahead",
      (Balls*10 + Strikes) %in% c(21,0,11) ~ "Even",
      (Balls*10 + Strikes) == 1 ~ "Behind"
    ))
  groupedDecisionData <- swingDecisionData %>%
    filter(xZone != 0 & xZone != 4 & yZone != 0 & yZone != 4) %>%
    group_by(countType, xZone, yZone) %>%
    summarise(swingScore = mean(swingScore, na.rm = TRUE))
  
  swingRateData <- xWOBACON_Data %>%
    filter(Batter == player_name) %>%
    mutate(countType = case_when(
      (Balls*10 + Strikes) %in% c(30,31,20,10) ~ "Ahead",
      (Balls*10 + Strikes) %in% c(21,0,11) ~ "Even",
      (Balls*10 + Strikes) == 1 ~ "Behind"
    ))
  
  groupedSwingRateData <- swingRateData %>%
    filter(xZone != 0 & xZone != 4 & yZone != 0 & yZone != 4) %>%
    group_by(countType, xZone, yZone) %>%
    summarise(swingRate = mean(isSwing, na.rm = TRUE))
  
  aheadPlot <- swingGrade_Heat_Map(player_name = player_name, groupedDecisionData %>% filter(countType == "Ahead"))
  aheadReal <- swingRate_Heat_Map(player_name = player_name, groupedSwingRateData %>% filter(countType == "Ahead"))
  evenPlot <- swingGrade_Heat_Map(player_name = player_name, groupedDecisionData %>% filter(countType == "Even"))
  evenReal <- swingRate_Heat_Map(player_name = player_name, groupedSwingRateData %>% filter(countType == "Even"))
  behindPlot <- swingGrade_Heat_Map(player_name = player_name, groupedDecisionData %>% filter(countType == "Behind"))
  behindReal <- swingRate_Heat_Map(player_name = player_name, groupedSwingRateData %>% filter(countType == "Behind"))
  
  aheadMatrix <- grid.arrange(aheadPlot, aheadReal, ncol = 1)
  #evenMatrix <- grid.arrange(evenPlot, evenReal, ncol = 1)
  #behindMatrix <- grid.arrange(behindPlot, behindReal, ncol = 1)
  
  #$return(c(aheadMatrix, evenMatrix, behindMatrix))
}


#swingGrade_HeatDashBoard("Gavin Bailey")
data <- player_Count_xWOBA("Cole Yates") %>%
  arrange(Strikes)



data %>%
  ungroup() %>%
  gt() %>%
  data_color(
    columns = c(averageXWOBA),
    colors = scales::col_numeric(
    palette = c("#1E90FF", "#87CEEB", "#ADD8E6", "white", "#FFC1CC", "#FF6666", "#FF6666"),
    domain = c(0,.1,.2,.3,.4,.5,.8)
    )
 )


player_list <- c("")

for (x in player_list) {
    png(file = paste("/Users/noahlippman/Documents/approachGraphs/", x,"aheadMatrix", ".png"),
        width = 1500, height = 1200, res = 175)
    print(swingGrade_HeatDashBoard(x))
    dev.off()
}

for (x in player_list) {
  png(file = paste("/Users/noahlippman/Documents/approachGraphs/", x,"xWOBA", ".png"),
      width = 1500, height = 1200, res = 175)
  print(xWOBA_HeatMap(x))
  dev.off()
  png(file = paste("/Users/noahlippman/Documents/approachGraphs/", x,"inPlay", ".png"),
      width = 1500, height = 1200, res = 175)
  print(inPlayRate_Heat_Map(x))
  dev.off()
}



