library(tidyverse)
library(plotly)
 
## Load Data ##
startingPositions <- read.csv("/Users/noahlippman/Documents/GitHub/Kernelytics-Projects/KCL_Defense/startingPositions.csv") %>%
  mutate(startingX = xCord) %>%
  mutate(startingY = yCord) %>%
  select(Position, startingX, startingY)

playsDataOAA <- read.csv("/Users/noahlippman/Documents/GitHub/Kernelytics-Projects/KCL_Defense/playerScores.csv") %>%
  left_join(startingPositions, by = c("playerPosition" = "Position")) %>%
  mutate(distanceFromAverageStart = sqrt((startingX - X_Cord)^2 + (startingY - Y_Cord)^2)) %>%
  mutate(outOrHit = if_else(PlayResult %in% c("Out", "Sacrifice"), "Out","Hit")) %>%
  mutate(inLeftVal = if_else(X_Cord < startingX & Y_Cord < startingY, playScore, NA)) %>%
  mutate(inRightVal = if_else(X_Cord > startingX & Y_Cord < startingY, playScore, NA)) %>%
  mutate(backLeftVal = if_else(X_Cord < startingX & Y_Cord > startingY, playScore, NA)) %>%
  mutate(backRightVal = if_else(X_Cord > startingX & Y_Cord > startingY, playScore, NA)) %>%
  mutate(inningID = paste(Inning, "-", Date))
  
## Num to Position Map ##
numToPosition <- c('3' = "1B", '4' = "2B", '5' = "3B", '6' = 'SS', 
                   '7' = 'LF', '8' = 'CF', '9' = 'RF')

## Create a DataFrame of all players and their OAA in each Direction ##
directional_leadearboard_OAA <- playsDataOAA %>%
  select(Player, inRightVal, inLeftVal, backRightVal, backLeftVal, inningID) %>%
  group_by(Player) %>%
  summarise(innings =  length(unique(inningID)),
            inRightOAA = round(sum(inRightVal, na.rm = TRUE),2),
            inLeftOAA = round(sum(inLeftVal, na.rm = TRUE),2),
            backRightOAA = round(sum(backRightVal, na.rm = TRUE),2),
            backLeftOAA = round(sum(backLeftVal, na.rm = TRUE),2)) %>%
  mutate(inTotal = inRightOAA + inLeftOAA) %>%
  mutate(backTotal = backRightOAA + backLeftOAA) %>%
  mutate(leftTotal = inLeftOAA + backLeftOAA) %>%
  mutate(rightTotal = inRightOAA + backRightOAA)

## Create a DataFrame of Each player's total OAA ##
totals_only_leaderboard <- playsDataOAA %>%
  filter(playScore != 0 & !(is.na(playScore))) %>%
  select(Player, playerPosition, playScore, inningID) %>%
  group_by(Player) %>%
  summarise(OAA = round(sum(playScore),2),
            playerPosition = "Total") %>%
  full_join(., directional_leadearboard_OAA, by = join_by(Player))

## Create a DataFrame of Each player's positional OAA ##
position_leaderboard <- playsDataOAA %>%
  select(Player, playScore, playerPosition, inRightVal, inLeftVal, backRightVal, backLeftVal, inningID) %>%
  mutate(playerPosition = unname(numToPosition[as.character(playerPosition)])) %>%
  group_by(Player, playerPosition) %>% 
  summarise(innings =  length(unique(inningID)),
            OAA = round(sum(playScore, na.rm = TRUE),2), 
            inRightOAA = round(sum(inRightVal, na.rm = TRUE),2),
            inLeftOAA = round(sum(inLeftVal, na.rm = TRUE),2),
            backRightOAA = round(sum(backRightVal, na.rm = TRUE),2),
            backLeftOAA = round(sum(backLeftVal, na.rm = TRUE),2)) %>%
  mutate(inTotal = inRightOAA + inLeftOAA) %>%
  mutate(backTotal = backRightOAA + backLeftOAA) %>%
  mutate(leftTotal = inLeftOAA + backLeftOAA) %>%
  mutate(rightTotal = inRightOAA + backRightOAA)

full_leaderboard <- rbind(totals_only_leaderboard, position_leaderboard) %>%
  mutate("OAA/175 innings" = (OAA/innings) * 175)

## Return a DataFrame of Total and Directional OAA ##
oAATable <- function(player_name){
  return(full_leaderboard %>% 
           filter(Player == player_name) %>%
           select(Player, "Position" = playerPosition, innings, OAA, 'OAA/175 innings',  inTotal, backTotal, leftTotal, rightTotal, inRightOAA,	inLeftOAA,	backRightOAA,	backLeftOAA) %>%
           arrange(desc(innings)))
}


# -----------------------------
# Shiny Module UI/Server
# -----------------------------
oAATableUI <- function(id) {
  ns <- NS(id)
  div(
    style = "
      display: flex;
      flex-direction: row;
      gap: 10px;
      background: #f9f9f9;
      padding: 1px;
      border: 1px solid #ccc;
      border-radius: 8px;
      overflow-y: auto;
      overflow-x: scroll;
      background-color: white;
      height: 300px;
    ",
    # the plot itself
    div(
      style = "flex: 1 1 auto; min-width: 0;",
      tableOutput(ns("oAATable"))
    )
  )
}
oAATableServer <- function(id, player_name) {
  moduleServer(id, function(input, output, session) {
    output$oAATable <- renderTable({
      
      req(player_name())
      
      oAATable(player_name())
    })
  })
}