library(tidyverse)

## Load Data ##
startingPositions <- read.csv("/Users/noahlippman/Documents/GitHub/Kernelytics-Projects/KCL_Defense/startingPositions.csv") %>%
  mutate(startingX = xCord) %>%
  mutate(startingY = yCord) %>%
  select(Position, startingX, startingY)

playsData_playsResponsible <- read.csv("/Users/noahlippman/Documents/GitHub/Kernelytics-Projects/KCL_Defense/playerScores.csv") %>%
  left_join(startingPositions, by = c("playerPosition" = "Position")) %>%
  mutate(distanceFromAverageStart = sqrt((startingX - X_Cord)^2 + (startingY - Y_Cord)^2)) %>%
  mutate(outOrHit = if_else(PlayResult %in% c("Out", "Sacrifice"), "Out","Hit"))
  
customColors = c("Hit" = "#6a6a6a", "Out" = "darkorange")

## Train Distance / HangTime Catch Probability Model ##
modelData <- playsData_playsResponsible %>% 
  filter(playScore != 0) %>%
  mutate(isCatch = if_else(outOrHit == "Out", 1, 0))

catchProb_Model<- glm(isCatch ~ distanceFromAverageStart + HangTime, data = modelData, family = binomial)

## Create Simulated Data With distance, hangtime and Catch Prob ##
hangTimes <- c(seq(1,7,.1))
distance <- c(0:200)
simdata <- expand.grid('HangTime' = hangTimes, 'distanceFromAverageStart' = distance)
simdata <- simdata %>%
  mutate(catchProb = predict(catchProb_Model, ., type = 'response')) %>%
  filter(between(catchProb, .05,.8))

## Catch Prob Chart Function ##
playsResponsibleMap <- function(player_name){
  
  individualData <- playsData_playsResponsible %>%
    filter(Player == player_name) %>%
    filter(playScore >= .05 | playScore < -.05)
  
  p <- ggplot(data = individualData, aes(x = distanceFromAverageStart, y = HangTime)) + 
    geom_raster(data = simdata, aes(x = distanceFromAverageStart, y = HangTime, fill = catchProb), interpolate = FALSE, alpha = .5, inherit.aes = FALSE) +
    scale_fill_stepsn(
      colors = c("#118565", "#5acdad", "#a0ffe4", "#c6ffef", "white"),
      values = scales::rescale(c(.05, 0.3, 0.5, 0.7, 0.8)),
      limits = c(.05,.8),
      name = "Catch Probability"
    ) +
    guides(fill = guide_legend(reverse = TRUE)) +
    geom_point(size = 2.5, aes(color = outOrHit)) +
    scale_color_manual(values = customColors, 
                       name = "Result") +
    ylim(0, 7) + 
    xlim(0, 200) +
    xlab("Distance From Average Start (Feet)") + 
    ylab("Hang Time (Sec)") + 
    theme_classic()

  return(p)
} 

playsResponsibleMap("Adan Nieves")
# -----------------------------
# Shiny Module UI/Server
# -----------------------------
playsResponsibleUI <- function(id) {
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
      plotOutput(ns("playsResponsible_Chart"), click = "plot_click", height = "450px")
    )
  )
}
playsResponsibleServer <- function(id, player_name) {
  moduleServer(id, function(input, output, session) {
    output$playsResponsible_Chart <- renderPlot({

      req(player_name())

      playsResponsibleMap(player_name())
    })
  })
}