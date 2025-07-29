library(tidyverse)
library(hexbin)
library(rlang)

## Load Data ##
startingPositions <- read.csv("/Users/noahlippman/Documents/GitHub/Kernelytics-Projects/KCL_Defense/startingPositions.csv") %>%
  mutate(startingX = xCord) %>%
  mutate(startingY = yCord) %>%
  select(Position, startingX, startingY)
 
simData <- read.csv("/Users/noahlippman/Documents/GitHub/Kernelytics-Projects/KCL_Defense/SimData.csv") %>%
  mutate(outProb = X3 + X4 + X5 + X6 + X7 + X8 + X9) %>%
      filter(
      (X_Cord < -55 & (Y_Cord < (X_Cord + 232) * (.8362) + 222)) |
      (between(X_Cord, -55, 55) & Y_Cord < 370) |
      (X_Cord > 55 & (Y_Cord < (X_Cord - 55) * (-.8362) + 370)) 
    ) %>%
    filter(
      ((X_Cord < 0) & Y_Cord >= (-1 * X_Cord)) |
      (X_Cord >= 0 & X_Cord > 0 & X_Cord <= Y_Cord)
    )

numToPosition <- c('3' = "1B", '4' = "2B", '5' = "3B", '6' = 'SS', 
                   '7' = 'LF', '8' = 'CF', '9' = 'RF')

r <- 150
theta <- seq(0,pi, .001)
x <- r * cos(theta)
y <- r * sin(theta)
semicircle <- data.frame(x = x, y = y) %>%
  filter(x >= -106 & x <= 106)

customColorsCatchProb = c("Hit" = "#6a6a6a", "Out" = "darkorange")

catchProbChart <- function(position, X_Cord, Y_Cord, hangTime, outOrHit){
  positionName <- unname(numToPosition[as.character(position)])
  colName <- paste0("X", position)
  
  relevantData <- simData %>%
    filter(HangTime == hangTime) %>%
    filter(outProb != 0) %>%
    filter((.data[[colName]] >= .data[['X3']]& 
           .data[[colName]] >= .data[['X4']]&
           .data[[colName]] >= .data[['X5']]&
           .data[[colName]] >= .data[['X6']]&
           .data[[colName]] >= .data[['X7']]&
           .data[[colName]] >= .data[['X8']]&
           .data[[colName]] >= .data[['X9']])) %>%
    mutate(catchProb = .data[[colName]]) %>%
    select(X_Cord, Y_Cord, outProb)
  
  pointData <- data.frame('Position' = position, 'X_Cord' = X_Cord, 'Y_Cord' = Y_Cord, 'outOrHit' = outOrHit)
  
  p <- ggplot(data = relevantData, aes(x = X_Cord, y = Y_Cord)) + 
      ggtitle(paste("Catch Probability for balls with a ~ ", hangTime, "Hang Time")) + 
      stat_summary_hex(aes(z = outProb), fun = "mean", bins = 35, alpha = .45) + 
      scale_fill_stepsn(
        colors = c("#f5f8ff", "#d6e0ff", "#a1b9ff", "#5580ff", "#0040ff"),
        values = scales::rescale(c(.05, 0.3, 0.5, 0.7, 0.9)),
        limits = c(.05,.8),
        name = "Catch Probability\n"
      ) +
      geom_point(data = pointData, aes(x = X_Cord, y = Y_Cord, color = outOrHit), size = 4, alpha = 1, inherit.aes = FALSE) + 
      geom_point(data = startingPositions %>% filter(Position == position), aes(x = startingX, y = startingY, color = paste(positionName, "Average Starting Position")),
               shape = 17, size = 3, inherit.aes = FALSE) +
    scale_color_manual(
      name = "\nLegend", 
      values = c(
        customColorsCatchProb, 
        setNames("black", paste(positionName, "Average Starting Position"))
      )
     ) + 
      xlim(-300, 350) + 
      ylim(0,400) + 
      geom_segment(aes(x = 0, y = 0, xend = 232, yend = 232), color = "black") + 
      geom_segment(aes(x = 0, y = 0, xend = -232, yend = 232), color = "black") + 
      geom_segment(aes(x = -232, y = 232, xend = -55, yend = 380), color = "black", linetype = "dotted") + 
      geom_segment(aes(x = -55, y = 380, xend = 55, yend = 380), color = "black", linetype = "dotted") + 
      geom_segment(aes(x = 55, y = 380, xend = 232, yend = 234), color = "black", linetype = "dotted") + 
      geom_path(data = semicircle, aes(x = x, y = y), color = "black", size = .75, inherit.aes = FALSE) + 
      theme_void() +
      xlab("") + 
      ylab("") + 
      theme(legend.position = c(.99,.02), 
          legend.justification = c(1,0),
          legend.text = element_text(size = 11),
          legend.title = element_text(size = 14, face = "bold")) + 
      theme(plot.title = element_text(size = 16, face = "bold", hjust = .5, vjust = .15))
  
  return(p)
}

# -----------------------------
# Shiny Module UI/Server
# -----------------------------
catchProbUI <- function(id) {
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
      plotOutput(ns("CatchProb_Chart"), height = "450px")
    )
  )
}
catchProbServer <- function(id, Position, X_Cord, Y_Cord, hangTime, outOrHit) {
  moduleServer(id, function(input, output, session) {
    output$CatchProb_Chart <- renderPlot({
      
      req(Position)
      req(X_Cord)
      req(Y_Cord)
      req(hangTime)
      req(outOrHit)
      
      catchProbChart(Position, X_Cord, Y_Cord, hangTime, outOrHit)
    })
  })
}


