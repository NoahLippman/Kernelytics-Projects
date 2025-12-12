library(tidyverse)
library(scales)

plotRollingXWOBA <- function(data, batterName){
  leagueXWOBA <- data |>
    filter(!is.na(PlayResult)) |>
    filter(
      !(PlayResult %in% c("Single","Double","Triple","HomeRun","Out","Error")
        & is.na(Angle) & is.na(ExitSpeed))
    ) |>
    summarize(mean = mean(predicted_xwoba)) |>
    pull(mean)
  
  x_max <- data |>
    filter(Batter == batterName, !is.na(rolling_xwOBA), PlayerPA > 3) |>
    summarize(max_x = max(PlayerPA, na.rm = TRUE)) |>
    pull(max_x)
  
  grid_lines <- c(0.0, 0.1, 0.2, 0.4, 0.5, 0.6)
  
  plot_segs <- data |>
    filter(Batter == batterName, !is.na(rolling_xwOBA), PlayerPA > 3) |>
    arrange(PlayerPA) |>
    mutate(
      xend = lead(PlayerPA),
      yend = lead(rolling_xwOBA),
      ymid = (rolling_xwOBA + yend) / 2
    ) |>
    filter(!is.na(xend))
  
  ggplot() +
    #main line
    geom_segment(
      data = plot_segs,
      aes(x = PlayerPA, y = rolling_xwOBA,
          xend = xend, yend = yend,
          colour = ymid),
      size = 1.2
    ) +
    scale_colour_gradientn(
      limits = c(0.1, 0.5),
      colours = c("blue", "lightblue", "gray", "lightcoral", "red"),
      values  = rescale(c(0.1, 0.2, 0.32, 0.4, 0.5)),
      oob     = squish,
      guide   = "none"
    ) +
    guides(colour = "none") +
    
    # league average dashed line
    geom_segment(
      data = data.frame(x = 3.5, xend = x_max,
                        y = leagueXWOBA, yend = leagueXWOBA),
      aes(x = x, xend = xend, y = y, yend = yend),
      inherit.aes = FALSE,
      linetype     = "dashed",
      color        = "black",
      size         = 0.7
    ) +
    
    # grid lines
    geom_segment(
      data = tibble(y = grid_lines),
      aes(x = 3.5, xend = x_max, y = y, yend = y),
      inherit.aes = FALSE,
      linetype = "dashed", color = "gray70", linewidth = 0.5
    ) +
    
    annotate(
      "text",
      x = x_max, y = leagueXWOBA,
      label = "AVG", hjust = -0.1, vjust = 0.35, size = 3
    ) +
    
    scale_x_continuous(
      limits = c(3.5, x_max + 2), expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = c(0.000, 0.600), breaks = seq(0, 0.6, 0.1), labels = function(x) sub("^0", "", format(x, nsmall = 3))
    ) +
    
    theme_classic() +
    labs(title = "100 PAs Rolling xWOBA") +
    theme(
      axis.title.x       = element_blank(),
      axis.title.y       = element_blank(),
      axis.text.x        = element_blank(),
      axis.ticks.x       = element_blank(),
      axis.line.x        = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    )
}

# UI ---------------------------------------------------------
rollingXWOBAUI <- function(id) {
  ns <- NS(id)
  div(
    style = "
      height: 100%;
      background: #f9f9f9;
      padding: 2px;
      border: 1px solid #ccc;
      border-radius: 8px;
      box-sizing: border-box;
    ",
    plotOutput(ns("rollingPlot"), height = "100%")
  )
}

# SERVER -----------------------------------------------------
rollingXWOBAServer <- function(id, data_source, player_name) {
  moduleServer(id, function(input, output, session) {
    
    output$rollingPlot <- renderPlot({
      req(player_name())
      
      plotRollingXWOBA(
        data = data_source(),
        batterName = player_name()
      )
    }, res = 96)
  })
}
