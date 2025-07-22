library(tidyverse)
library(plotly)

startingPositions <- read.csv("/Users/noahlippman/Documents/GitHub/Kernelytics-Projects/KCL_Defense/startingPositions.csv") %>%
  mutate(startingX = xCord) %>%
  mutate(startingY = yCord) %>%
  select(Position, startingX, startingY)

playsData <- read.csv("/Users/noahlippman/Documents/GitHub/Kernelytics-Projects/KCL_Defense/playerScores.csv") %>%
  left_join(startingPositions, by = c("playerPosition" = "Position")) %>%
  mutate(distanceFromAverageStart = sqrt((startingX - X_Cord)^2 + (startingY - Y_Cord)^2)) %>%
  mutate(outOrHit = if_else(PlayResult %in% c("Out", "Sacrifice"), "Out","Hit")) %>%
  mutate(inLeftVal = if_else(X_Cord < startingX & Y_Cord < startingY, playScore, NA)) %>%
  mutate(inRightVal = if_else(X_Cord > startingX & Y_Cord < startingY, playScore, NA)) %>%
  mutate(backLeftVal = if_else(X_Cord < startingX & Y_Cord > startingY, playScore, NA)) %>%
  mutate(backRightVal = if_else(X_Cord > startingX & Y_Cord > startingY, playScore, NA))

directional_leaderboard <- playsData %>%
  filter(playScore != 0 & !(is.na(playScore))) %>%
  select(Player, inRightVal, inLeftVal, backRightVal, backLeftVal) %>%
  group_by(Player) %>%
  summarise(inRightOAA = round(sum(inRightVal, na.rm = TRUE),2),
            inLeftOAA = round(sum(inLeftVal, na.rm = TRUE),2),
            backRightOAA = round(sum(backRightVal, na.rm = TRUE),2),
            backLeftOAA = round(sum(backLeftVal, na.rm = TRUE),2)) %>%
  mutate(inTotal = inRightOAA + inLeftOAA) %>%
  mutate(backTotal = backRightOAA + backLeftOAA) %>%
  mutate(leftTotal = inLeftOAA + backLeftOAA) %>%
  mutate(rightTotal = inRightOAA + backRightOAA)

totals_only_leaderboard <- playsData %>%
  filter(playScore != 0 & !(is.na(playScore))) %>%
  select(Player, playScore) %>%
  group_by(Player) %>%
  summarise(OAA = round(sum(playScore),2)) %>%
  full_join(., directional_leaderboard, by = join_by(Player))



oAATable <- function(player_name){
  return(totals_only_leaderboard %>% 
           filter(Player == player_name) %>%
           select(Player, OAA, inTotal, backTotal, leftTotal, rightTotal, inRightOAA,	inLeftOAA,	backRightOAA,	backLeftOAA))
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
      height: 100px;
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