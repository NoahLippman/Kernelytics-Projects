library(tidyverse)
library(shiny)
library(ggplot2)

# Plot Advanced Stat Visuals for Pitchers
plot_pitcher_percentile_bars <- function(player_data, stat_cols, display_names) {
  pct_cols <- paste0(stat_cols, "_pct")
  
  df <- tibble(
    Stat = factor(display_names, levels = rev(display_names)),
    Value = as.numeric(player_data[stat_cols]),
    Percentile = round(as.numeric(player_data[pct_cols]), 0)
  ) %>%
    mutate(
      Value = case_when(
        as.character(Stat) %in% c("Chase%", "Whiff%", "K%", "BB%", "Hard-Hit%", "GB%", "Barrel%") ~ Value * 100,
        TRUE ~ Value
      ),
      Value = round(Value, 3),
      Color = case_when(
        Percentile >= 85 ~ "#d82129",
        Percentile >= 70 ~ "#d77768",
        Percentile >= 55 ~ "#c6b3ad",
        Percentile >= 45 ~ "#aec9cf",
        Percentile >= 20 ~ "#6886ba",
        TRUE ~ "#325aa1"
      ),
      ScaledPct = round(0.9 * Percentile, 0)
    ) %>%
    filter(!is.na(Percentile) & !is.na(Value) & is.finite(Percentile) & is.finite(Value))
  
  if (nrow(df) == 0) {
    message("No valid metrics for plotting after NA filtering.")
    return(NULL)
  }
  
  ggplot(df, aes(x = ScaledPct, y = Stat)) +
    geom_bar(stat = "identity", aes(fill = Color), width = 0.9) +
    geom_point(aes(fill = Color), shape = 21, color = "white", size = 6, stroke = 1.3) +
    geom_text(aes(label = Percentile), color = "white", size = 3.5, fontface = "bold") +
    geom_text(
      aes(label = case_when(
        as.character(Stat) %in% c("Chase%", "Whiff%", "K%", "BB%", "Hard-Hit%", "GB%", "Barrel%") ~ sprintf("%.1f%%", Value),
        TRUE ~ sprintf("%.1f", Value)
      ), x = 95),
      hjust = 0, size = 4
    ) +
    scale_fill_identity() +
    scale_x_continuous(limits = c(-4, 110), expand = c(0, 0)) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
}

# UI
pitcherPercentilePlotUI <- function(id) {
  ns <- NS(id)
  div(
    style = "
      height: 100%;
      background: #f9f9f9;
      padding: 10px;
      border: 1px solid #ccc;
      border-radius: 8px;
      box-sizing: border-box;
    ",
    plotOutput(ns("barPlot"), height = "100%"),
    textOutput(ns("error_message"))
  )
}

# SERVER
pitcherPercentilePlotServer <- function(id, player_data) {
  moduleServer(id, function(input, output, session) {
    output$barPlot <- renderPlot({
      req(player_data())
      if (nrow(player_data()) == 0 || all(is.na(player_data()[,-1]))) {
        output$error_message <- renderText("No valid data found for the selected pitcher.")
        return(NULL)
      }
      
      stat_cols <- c("xBA", "FastballVelo", "avgExitVelo", "chasePct", "whiffPct", 
                     "kPct", "bbPct", "hardHitPct", "groundBallPct")
      display_names <- c("exp BA", "Fastball Velo", "Avg Exit Velo", "Chase%", "Whiff%", 
                         "K%", "BB%", "Hard-Hit%", "GB%")
      
      # Include BarrelPct if available
      if (!is.na(player_data()$BarrelPct)) {
        stat_cols <- c(stat_cols, "BarrelPct")
        display_names <- c(display_names, "Barrel%")
      }
      
      plot <- plot_pitcher_percentile_bars(player_data(), stat_cols, display_names)
      if (is.null(plot)) {
        output$error_message <- renderText("No valid metrics available for plotting.")
        return(NULL)
      }
      output$error_message <- renderText("")
      plot
    }, res = 96)
    
    output$error_message <- renderText({
      if (is.null(player_data()) || nrow(player_data()) == 0 || all(is.na(player_data()[,-1]))) {
        "No valid data found for the selected pitcher."
      } else {
        ""
      }
    })
  })
}