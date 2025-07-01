library(tidyverse)
library(purrr)

## Data Collection ##
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
  right_join(pitchData, join_by(PitchUUID))

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
  group_by(Batter, xZone, yZone) %>%
  summarise(SwingingStrikeAverage = mean(SwingingStrike))

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
  
  return(countResults_P)
}

## Function to calculate the grade of a swing ##
swingScore <- function(player_name, row){
  #browser()
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
    return(strikeNewXWOBA - ballNewXWOBA)
  }
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

decisionScoreData <- function(player_name){
  playerPitches <- xWOBACON_Data %>%
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

leage_Count_xWOBA <- function(){
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
  return(countResults_P)
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
  old_data <- mergedData %>% filter(Batter == player_name)
  data <- data_processor(old_data)
  p <- ggplot(data, aes(xZone, yZone, fill = xWOBA)) +
    geom_tile(aes(height = height, width = width)) +
    ylim(-2,6) +
    xlim(-6, 10) + 
    theme_void() +
    scale_fill_gradientn(
      colours = c("#1E90FF", "#87CEEB", "#ADD8E6", "white", "#FFC1CC", "#FF6666", "#FF6666"),
      values = scales::rescale(c(0,.1,.2,.3,.4,.5,.6)),
      na.value = "grey90"
    ) +
    labs(fill = "xWOBA") + 
    theme(legend.position.inside = c(.85,.5)) + 
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
    theme(legend.position.inside = c(.85,.5)) + 
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
  xZone <- c(0,1,2,3,4)
  yZone <- c(0,1,2,3,4)
  balls <- c(0,1,2,3)
  strikes <- c(0,1,2)

  situations <- expand.grid(xZone,yZone, balls, strikes)
  return(situations)
}
