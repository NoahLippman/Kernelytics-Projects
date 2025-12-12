library(tidyverse)

# Make Spray Chart
make_spray_chart <- function(df, player_name){
  spray_data <- df %>%
    mutate(Bearing = Bearing * pi / 180)
  spray_data <- spray_data %>%
    mutate("X_Cord" = sin(Bearing) * Distance) %>%
    mutate("Y_Cord" = cos(Bearing) * Distance) %>%
    filter(PitchCall == "InPlay") %>%
    filter(!is.na(Distance)) %>%
    filter(!is.na(Bearing)) %>%
    filter(Batter == player_name)
  max_val <- max(abs(spray_data$X_Cord))
  
  custom_colors = c("Single" = "#FF5733", "Double" = "#6666CC", "Triple" = "#FFC107", "HomeRun" = "#CC527A",
                    "Out" = "gray", "Error" = "gray")
  
  r <- 150
  theta <- seq(0,pi, .001)
  x <- r * cos(theta)
  y <- r * sin(theta)
  semicircle <- data.frame(x = x, y = y) %>%
    filter(x >= -106 & x <= 106)
  
  y_max <- max(c(spray_data$Y_Cord, 400))
  
  p <- ggplot(data = spray_data, aes(x = X_Cord, y = Y_Cord, color = PlayResult)) + 
    geom_point() + 
    scale_color_manual(values = custom_colors) + 
    xlim(-300, 350) + 
    ylim(0,y_max) + 
    geom_segment(aes(x = 0, y = 0, xend = 232, yend = 232), color = "black") + 
    geom_segment(aes(x = 0, y = 0, xend = -232, yend = 232), color = "black") + 
    geom_segment(aes(x = -232, y = 232, xend = -55, yend = 380), color = "black", linetype = "dotted") + 
    geom_segment(aes(x = -55, y = 380, xend = 55, yend = 380), color = "black", linetype = "dotted") + 
    geom_segment(aes(x = 55, y = 380, xend = 232, yend = 234), color = "black", linetype = "dotted") + 
    geom_path(data = semicircle, aes(x = x, y = y), color = "black", size = .75) + 
    theme_void() +
    xlab("") + 
    #ylab("") + 
    labs(color = "") + 
    theme(legend.position = c(0.9,0.5))
  
  return(p)
}

make_spray_chart(game_data, "Kaileb Hackman")

# -----------------------------
# Shiny Module UI/Server
# -----------------------------
sprayChartUI <- function(id) {
  ns <- NS(id)
  div(
    style = "
      display: flex;
      flex-direction: row;
      gap: 10px;
      background: #f9f9f9;
      padding: 10px;
      border: 1px solid #ccc;
      border-radius: 8px;
    ",
    # the plot itself
    div(
      style = "flex: 1 1 auto; min-width: 0;",
      plotOutput(ns("sprayPlot"), height = "300px")
    )
  )
}

sprayChartServer <- function(id, data_source, player_name) {
  moduleServer(id, function(input, output, session) {
    output$sprayPlot <- renderPlot({
      # make sure we have a batter
      req(player_name())
      df <- data_source()
      # call your function
      make_spray_chart(df, player_name())
    })
  })
}
