library(tidyverse)

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
    rowwise() %>%
    mutate(decisionScore = if_else(PitchCall %in% c("Foul","StrikeSwinging", "InPlay"), swingScore(player_name, pick(xZone, yZone, Balls, Strikes, PitchCall)), takeScore(player_name, pick(xZone, yZone, Balls, Strikes, PitchCall)))) %>%
    ungroup()
  
  return(playerPitches %>% select(PitchCall, xZone, yZone, Balls, Strikes, decisionScore))
}