library(tidyverse)
library(plotly)

## Load Data ##
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

## Create a directional DataFrame ##
directional_leaderboard_range <- playsData %>%
  filter(playScore != 0 & !(is.na(playScore))) %>%
  select(Player, inRightVal, inLeftVal, backRightVal, backLeftVal) %>%
  group_by(Player) %>%
  summarise(inRightOAA = sum(inRightVal, na.rm = TRUE),
            inLeftOAA = sum(inLeftVal, na.rm = TRUE),
            backRightOAA = sum(backRightVal, na.rm = TRUE),
            backLeftOAA = sum(backLeftVal, na.rm = TRUE)) %>%
  mutate(inTotal = inRightOAA + inLeftOAA) %>%
  mutate(backTotal = backRightOAA + backLeftOAA) %>%
  mutate(leftTotal = inLeftOAA + backLeftOAA) %>%
  mutate(rightTotal = inRightOAA + backRightOAA)

## Add Percentiles to Directional DataFrame
directional_leaderboard_range <- directional_leaderboard_range %>%
  rowwise() %>%
  mutate(inRightPercentile = round(sum(directional_leaderboard_range$inRightOAA < inRightOAA) / length(directional_leaderboard_range$inRightOAA),2)) %>%
  mutate(inLeftPercentile = round(sum(directional_leaderboard_range$inLeftOAA < inLeftOAA) / length(directional_leaderboard_range$inLeftOAA),2)) %>%
  mutate(backLeftPercentile = round(sum(directional_leaderboard_range$backLeftOAA < backLeftOAA) / length(directional_leaderboard_range$backLeftOAA),2)) %>%
  mutate(backRightPercentile = round(sum(directional_leaderboard_range$backRightOAA < backRightOAA) / length(directional_leaderboard_range$backRightOAA),2)) %>%
  mutate(inPercentile = round(sum(directional_leaderboard_range$inTotal < inTotal) / length(directional_leaderboard_range$inTotal),2)) %>%
  mutate(backPercentile = round(sum(directional_leaderboard_range$backTotal < backTotal) / length(directional_leaderboard_range$backTotal),2)) %>%
  mutate(leftPercentile = round(sum(directional_leaderboard_range$leftTotal < leftTotal) / length(directional_leaderboard_range$leftTotal),2)) %>%
  mutate(rightPercentile = round(sum(directional_leaderboard_range$rightTotal < rightTotal) / length(directional_leaderboard_range$rightTotal),2)) %>%
  ungroup()

## Function to Create a range Percentile Chart ##
rangePercentileChart <- function(player_name){
  individualData <- directional_leaderboard_range %>%
    filter(Player == player_name)
  
  p <- plot_ly(
            type = 'scatterpolar',
            r = c(individualData$backRightPercentile, individualData$rightPercentile, individualData$inRightPercentile, 
                  individualData$inPercentile, individualData$inLeftPercentile, 
                  individualData$leftPercentile, individualData$backLeftPercentile, individualData$backPercentile, individualData$backRightPercentile),
            theta = c('Back Right', 'Right Total', 'In Right ', 'In Total', 'In Left', 'Left Total', 'Back Left','Back Total', 'Back Right'),
            fill = 'toself'
         ) %>%
        layout(
          autosize = TRUE,
          margin = list(
            l = 10,
            r = 10, 
            b = 10, 
            top = 100
          ), 
            title = list(
              text = "",
              x = .5
            ),
            polar = list(
              radialaxis = list(
                range = c(0, 1),              
                tickvals = round(seq(0,1,.2),1),  
                ticktext = round(seq(0,1,.2),1)   
              ),
              angularaxis = list(
                rotation = 45,
                direction = 'clockwise',
                tickvals = seq(0,100),
                tickText = c('Back Right', 'Right Total', 'In Right ', 'In Total', 'In Left', 'Left Total', 'Back Left','Back Total', 'Back Right')
              )
          ),
          showlegend = F
          )
  
  return(p)
}

# -----------------------------
# Shiny Module UI/Server
# -----------------------------
rangePercentileChartUI <- function(id) {
  ns <- NS(id)
  div(
    style = "
      display: flex;
      flex-direction: row;
      gap: 10px;
      background: #f9f9f9;
      padding: 1px;
      border: 2px solid #ccc;
      border-radius: 8px;
    ",
    # the plot itself
    div(
      style = "flex: 1 1 auto; min-width: 0;",
      plotlyOutput(ns("rangePercentile_Chart"), height = "450px")
    )
  )
}
rangePercentileChartServer <- function(id, player_name) {
  moduleServer(id, function(input, output, session) {
    output$rangePercentile_Chart <- renderPlotly({
      
      req(player_name())
      
      rangePercentileChart(player_name())
    })
  })
}