library(tidyverse)


#Plot Advanced Stat Visuals
plot_percentile_bars <- function(player_data, stat_cols) {
  pct_cols <- paste0(stat_cols, "_pct")
  
  df <- tibble(
    Stat = factor(stat_cols, levels = rev(stat_cols)),
    Value = round(as.numeric(player_data[stat_cols]), 2),
    Percentile = round(as.numeric(player_data[pct_cols]), 0)
  )
  
  df <- df |> 
    mutate(
      Color = case_when(
        Percentile >= 85 ~ "#d82129",
        Percentile >= 70 ~ "#d77768",
        Percentile >= 55 ~ "#c6b3ad",
        Percentile >= 45 ~ "#aec9cf",
        Percentile >= 20 ~ "#6886ba",
        TRUE            ~ "#325aa1"
      ),
      ScaledPct = round(.9*Percentile,0)
    )
  
  ggplot(df, aes(x = ScaledPct, y = Stat)) +
    geom_bar(stat = "identity", aes(fill = Color), width = .9) +
    geom_point(aes(fill = Color), shape = 21, color = "white", size = 8, stroke = 1.3) +
    geom_text(aes(label = paste0(Percentile)), color = "white", size = 3.5, fontface = "bold") +
    geom_text(aes(label = round(Value, 3), x = 95),
              hjust = 0, size = 4) +
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

# UI ---------------------------------------------------------
advancedStatsUI <- function(id) {
  ns <- NS(id)
  div(
    style = "
      height: 100%;                /* fill its parent panel */
      background: #f9f9f9;
      padding: 10px;
      border: 1px solid #ccc;
      border-radius: 8px;
      box-sizing: border-box;
    ",
    plotOutput(ns("barPlot"), height = "100%")
  )
}

# SERVER -----------------------------------------------------
advancedStatsServer <- function(id, data_source, player_name, stat_cols) {
  moduleServer(id, function(input, output, session) {
    output$barPlot <- renderPlot({
      req(player_name())                        # wait for a selection
      df <- data_source() %>% 
        filter(Batter == player_name())     # pick out that player
      
      # now call your plotting fn
      plot_percentile_bars(df, stat_cols)
    }, res = 96)
  })
}