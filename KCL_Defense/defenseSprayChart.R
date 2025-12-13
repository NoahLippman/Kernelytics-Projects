library(tidyverse)

## Load Data ##
startingPositions <- read.csv("/Users/noahlippman/Documents/GitHub/Kernelytics-Projects/KCL_Defense/startingPositions.csv") %>%
  mutate(startingX = xCord) %>%
  mutate(startingY = yCord) %>%
  select(Position, startingX, startingY)

playsData <- read.csv("/Users/noahlippman/Documents/GitHub/Kernelytics-Projects/KCL_Defense/playerScores.csv") %>%
  left_join(startingPositions, by = c("playerPosition" = "Position")) %>%
  mutate(distanceFromAverageStart = sqrt((startingX - X_Cord)^2 + (startingY - Y_Cord)^2)) %>%
  mutate(outOrHit = if_else(PlayResult %in% c("Out", "Sacrifice"), "Out","Hit"))

## Set Colors for Outcomes ##
customColors = c("Hit" = "darkgray", "Out" = "darkorange")

## Defense Spray Chart Function ##
defenseSprayChart <- function(player_name, position, hang_time_lower, hang_time_upper){
  ## Filter Data ##
  individualData <- playsData %>%
    filter(Player == player_name) %>%
    filter(playScore > 0 | playScore < -.01) %>%
    filter(playerPosition %in% position) %>%
    filter(between(HangTime, hang_time_lower, hang_time_upper))
  
  ## Semicircle for infield on spray chart ##
  r <- 150
  theta <- seq(0,pi, .001)
  x <- r * cos(theta)
  y <- r * sin(theta)
  semicircle <- data.frame(x = x, y = y) %>%
    filter(x >= -106 & x <= 106)
  
  ## Create Plot ##
  p <- ggplot(data = individualData, aes(x = X_Cord, y = Y_Cord, color = outOrHit)) + 
    geom_point(size = 3) + 
    geom_point(data = startingPositions %>% filter(Position == position), aes(x = startingX, y = startingY, color = "League Average Starting Position"),
               shape = 17, size = 3, inherit.aes = FALSE) +
    scale_color_manual(
      name = "Color", 
      values = c(customColors,
                 "League Average Starting Position" = "hotpink")
    )+ 
    xlim(-300, 350) + 
    ylim(0,400) + 
    ## Draw Fences ##
    geom_segment(aes(x = 0, y = 0, xend = 232, yend = 232), color = "black") + 
    geom_segment(aes(x = 0, y = 0, xend = -232, yend = 232), color = "black") + 
    geom_segment(aes(x = -232, y = 232, xend = -55, yend = 380), color = "black", linetype = "dotted") + 
    geom_segment(aes(x = -55, y = 380, xend = 55, yend = 380), color = "black", linetype = "dotted") + 
    geom_segment(aes(x = 55, y = 380, xend = 232, yend = 234), color = "black", linetype = "dotted") + 
    geom_path(data = semicircle, aes(x = x, y = y), color = "black", size = .75) + 
    theme_void() +
    xlab("") + 
    ylab("") + 
    labs(color = "") +
    ## Create Legend ##
    theme(legend.position = c(.97,.05), 
          legend.justification = c(1,0),
          legend.text = element_text(size = 13, face = "bold"),
          legend.title = element_blank())

  return(p)
    
} 

# -----------------------------
# Shiny Module UI/Server
# -----------------------------
defenseSprayChartUI <- function(id) {
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
      plotOutput(ns("defenseSpray_Chart"), height = "450px")
    )
  )
}

defenseSprayChartServer <- function(id, player_name, position, hang_time_lower, hang_time_upper) {
  moduleServer(id, function(input, output, session) {
    output$defenseSpray_Chart <- renderPlot({
      positionToNum = c("First Base" = 3, "Second Base" = 4, "Third Base" = 5, "Short Stop" = 6,
                        "Leftfield" = 7, "Centerfield" = 8, "Rightfield" = 9)
      position = positionToNum[position()]
      req(player_name())
      req(position())
      req(hang_time_lower())
      req(hang_time_upper())

      defenseSprayChart(player_name(), position, hang_time_lower(), hang_time_upper())
    })
  })
}