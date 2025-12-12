library(tidyverse)
library(shiny)


# -----------------------------
# Metric Calculation Function
# -----------------------------
getZoneMetric <- function(df, metric, collapse_zones = FALSE) {
  df2 <- df |> 
    mutate(
      is_AB  = !PlayResult %in% c("Walk", "Sacrifice", NA_character_),
      is_hit = PlayResult %in% c("Single","Double","Triple","HomeRun")
    )
  
  #If no zones and just the whole value
  if (collapse_zones) {
    if (metric == "BA") {
      total_H  <- sum(df2$is_hit, na.rm = TRUE)
      total_AB <- sum(df2$is_AB,  na.rm = TRUE)
      value <- if (total_AB > 0) total_H / total_AB else NA_real_
      return(tibble(zone = "All", value = value))
    }
    if (metric == "SLG") {
      total_TB <- sum(df2$bases, na.rm = TRUE)
      total_AB <- sum(df2$is_AB,  na.rm = TRUE)
      value <- if (total_AB > 0) total_TB / total_AB else NA_real_
      return(tibble(zone = "All", value = value))
    }
    if (metric == "xBA") {
      df3   <- df2 %>% filter(!is.na(PlayResult))
      value <- if (nrow(df3) > 0) mean(df3$predicted_xba, na.rm = TRUE) else NA_real_
      return(tibble(zone = "All", value = value))
    }
    if (metric == "xSLG") {
      df3   <- df2 %>% filter(!is.na(PlayResult))
      value <- if (nrow(df3) > 0) mean(df3$predicted_xslg, na.rm = TRUE) else NA_real_
      return(tibble(zone = "All", value = value))
    }
    if (metric == "xWOBA") {
      df3   <- df2 %>% filter(!is.na(PlayResult))
      value <- if (nrow(df3) > 0) mean(df3$predicted_xwoba, na.rm = TRUE) else NA_real_
      return(tibble(zone = "All", value = value))
    }
    stop("Unsupported metric: ", metric)
  }
  
  if (metric == "BA") {
    return(
      df2 |> 
        group_by(zone) |> 
        summarize(
          H  = sum(is_hit, na.rm = TRUE),
          AB = sum(is_AB,  na.rm = TRUE),
          value = if_else(AB > 0, H / AB, NA_real_),
          .groups = "drop"
        ) |> 
        mutate(zone = as.character(zone)) |> 
        select(zone, value)
    )
  }
  if (metric == "SLG") {
    return(
      df2 %>%
        group_by(zone) %>%
        summarize(
          TB = sum(bases, na.rm = TRUE),
          AB = sum(is_AB,  na.rm = TRUE),
          value = if_else(AB > 0, TB / AB, NA_real_),
          .groups = "drop"
        ) %>%
        mutate(zone = as.character(zone)) %>%
        select(zone, value)
    )
  }
  if (metric == "xBA") {
    return(
      df2 %>%
        filter(!is.na(PlayResult)) %>%
        group_by(zone) %>%
        summarize(
          value = mean(predicted_xba, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(zone = as.character(zone))
    )
  }
  if (metric == "xSLG") {
    return(
      df2 %>%
        filter(!is.na(PlayResult)) %>%
        group_by(zone) %>%
        summarize(
          value = mean(predicted_xslg, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(zone = as.character(zone))
    )
  }
  if (metric == "xWOBA") {
    return(
      df2 %>%
        filter(!is.na(PlayResult)) %>%
        group_by(zone) %>%
        summarize(
          value = mean(predicted_xwoba, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(zone = as.character(zone))
    )
  }
  
  stop("Unsupported metric: ", metric)
}


# -----------------------------
# Chart Rendering Logic
# -----------------------------

get_zone_chart_plot <- function(df, granularity, metric, hand, pitches) {
  collapse <- (granularity == "whole")
  vals <- getZoneMetric(df, metric, collapse)
  
  # Determine fill scale range
  zd_vals <- vals$value[!is.na(vals$value)]
  qs <- if (length(zd_vals) > 0) quantile(zd_vals, c(0.10, 0.50, 0.90)) else c(0, 0.5, 1)
  buffer <- (qs[3] - qs[1]) * 0.05
  lower_lim <- qs[1] - buffer
  upper_lim <- qs[3] + buffer
  
  #1x1 grid
  if (granularity == "whole") {
    return(
      ggplot(tibble(x = 1, y = 1, value = vals$value[1]), aes(x, y, fill = value)) +
        geom_tile(color = "black", width = 1, height = 1) +
        geom_text(aes(label = sprintf("%.3f", value)), size = 10, color = "black") +
        scale_fill_gradientn(
          colours = c("#08519C", "#3182BD", "snow2", "#FC9272", "#CB181D"),
          values = c(0, 0.25, 0.5, 0.75, 1),
          limits = c(lower_lim, upper_lim),
          oob = scales::squish,
          na.value = "snow3"
        ) +
        coord_fixed(xlim = c(0.5, 1.5), ylim = c(0.5, 1.5), expand = FALSE) +
        theme_void() +
        theme(legend.position = "none")
    )
  }
  
  #3x3 grid
  else {
    vals <- vals %>%
      filter(!is.na(zone), zone %in% 1:9) %>%
      mutate(zone = as.integer(zone))
    
    zone_grid <- tibble(zone = 1:9) %>%
      mutate(
        x = ((zone - 1) %% 3) + 1,
        y = ((zone - 1) %/% 3) + 1
      )
    
    plot_data <- zone_grid %>%
      left_join(vals, by = "zone")
    
    return(
      ggplot(plot_data, aes(x = x, y = y, fill = value)) +
        geom_tile(color = "black") +
        geom_text(aes(label = ifelse(is.na(value), "", sprintf("%.3f", value))),
                  size = 5, color = "black") +
        scale_fill_gradientn(
          colours = c("#08519C", "#3182BD", "snow2", "#FC9272", "#CB181D"),
          values = c(0, 0.25, 0.5, 0.75, 1),
          limits = c(lower_lim, upper_lim),
          oob = scales::squish,
          na.value = "snow3"
        ) +
        coord_fixed(xlim = c(0.5, 3.5), ylim = c(0.5, 3.5), expand = FALSE) +
        theme_void() +
        theme(legend.position = "none")
    )
  }
}


# -----------------------------
# Shiny Module UI/Server
# -----------------------------
zoneChartUI <- function(id) {
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
      width: 100%;
      box-sizing: border-box;
    ",
    
    # Sidebar controls
    div(
      style = "
        /* don’t grow or shrink, always 80px wide */
        flex: 0 0 100px;
        max-width: 100px;
        font-size: 12px;
        max-height: 360px;
        padding-right: 4px;  /* a little inner breathing room */
      ",
      selectInput(ns("metric"), "Statistic:",
                  choices = c("BA", "SLG", "xBA", "xSLG", "xWOBA")),
      radioButtons(ns("hand"), "Pitcher Hand:",
                   choices = c("All", "Left", "Right"), selected = "All"),
      checkboxGroupInput(ns("pitches"), "Pitch Category:",
                         choices = c("Fastball", "Offspeed", "Breaking"),
                         selected = c("Fastball", "Offspeed", "Breaking")),
      radioButtons(ns("granularity"), "Zone:",
                   choices = c("3×3 Zones" = "zones", "Whole Zone" = "whole"), selected = "zones")
    ),
    
    # Plot
    div(
      style = "
        flex: 1 1 auto;      /* grow to fill leftover width */
        min-width: 0;        /* allow plot to shrink below content size */
      ",
      plotOutput(ns("zonePlot"), height = "350px")
    )
  )
}


zoneChartServer <- function(id, data_source, player_name) {
  moduleServer(id, function(input, output, session) {
    
    # 1. Filter data by the chosen player, plus hand & pitch filters
    filtered_data <- reactive({
      # wait until a player is selected
      req(player_name())
      
      data_source() %>%
        filter(Batter == player_name()) %>%
        { if (input$hand != "All")
          filter(., PitcherThrows == input$hand)
          else
            .
        } %>%
        filter(PitchCategory %in% input$pitches)
    })
    
    # 2. Render the zone plot off that filtered data
    output$zonePlot <- renderPlot({
      req(filtered_data())
      get_zone_chart_plot(
        df          = filtered_data(),
        granularity = input$granularity,
        metric      = input$metric,
        hand        = input$hand,
        pitches     = input$pitches
      )
    }, res = 96)
    
  })
}