library(shiny)
library(tidyverse)
library(ggplot2)
library(scales)

# UI function
percentileChartUI <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns("percentile_plot"), height = "600px"),
    textOutput(ns("error_message")),
    downloadButton(ns("download_plot"), "Download Percentile Plot")
  )
}

# Server function
percentileChartServer <- function(id, data, pitcher_data, pitcher_name) {
  moduleServer(id, function(input, output, session) {
    # Function: Calculate advanced pitching stats
    getAdvancedPitching <- function(df, name) {
      df <- df %>% filter(Pitcher == name)
      
      FastballVelo <- df %>%
        filter(TaggedPitchType == "Fastball") %>%
        summarize(velo = mean(RelSpeed, na.rm = TRUE)) %>%
        pull(velo)
      
      avgExitVelo <- df %>%
        filter(!is.na(ExitSpeed)) %>%
        summarize(avgExitVelo = mean(ExitSpeed, na.rm = TRUE)) %>%
        pull(avgExitVelo)
      
      hardHitPct <- df %>%
        filter(!is.na(ExitSpeed)) %>%
        summarize(pct = mean(ExitSpeed >= 95, na.rm = TRUE)) %>%
        pull(pct)
      
      kPct <- df %>%
        filter(!is.na(PlayResult) | !is.na(KorBB)) %>%
        summarize(
          pct = {
            total_PA <- n()
            strikeouts <- sum(PlayResult %in% c("StrikeoutLooking", "StrikeoutSwinging"), na.rm = TRUE)
            if (total_PA == 0) 0 else strikeouts / total_PA
          }
        ) %>%
        pull(pct)
      
      bbPct <- df %>%
        filter(!is.na(PlayResult) | !is.na(KorBB)) %>%
        summarize(pct = sum(KorBB == "Walk", na.rm = TRUE) / n()) %>%
        pull(pct)
      
      whiffPct <- df %>%
        filter(!is.na(PitchCall)) %>%
        filter(!(PitchCall %in% c("BallCalled", "HitByPitch", "StrikeCalled"))) %>%
        summarize(pct = mean(PitchCall == "StrikeSwinging", na.rm = TRUE)) %>%
        pull(pct)
      
      chasePct <- df %>%
        filter(IsStrike == FALSE) %>%
        summarize(pct = mean(IsSwing == TRUE, na.rm = TRUE)) %>%
        pull(pct)
      
      groundBallPct <- df %>%
        filter(!is.na(HitType) & HitType != "Throwdown") %>%
        summarize(pct = mean(HitType == "GroundBall", na.rm = TRUE)) %>%
        pull(pct)
      
      # BarrelPct (if Angle available)
      BarrelPct <- if ("Angle" %in% names(df)) {
        df %>%
          filter(!is.na(ExitSpeed), !is.na(Angle)) %>%
          summarize(pct = mean(ExitSpeed > 90 & Angle >= 26 & Angle <= 30, na.rm = TRUE)) %>%
          pull(pct)
      } else {
        NA_real_
      }
      
      list(
        FastballVelo = FastballVelo,
        avgExitVelo = avgExitVelo,
        hardHitPct = hardHitPct,
        kPct = kPct,
        bbPct = bbPct,
        whiffPct = whiffPct,
        chasePct = chasePct,
        groundBallPct = groundBallPct,
        BarrelPct = BarrelPct
      )
    }
    
    # Function: Plot pitcher percentiles
    plot_pitcher_percentiles <- function(df, pitcher_name, pitcher_df) {
      # Metrics info
      metric_info <- tribble(
        ~metric, ~higher_is_better,
        "Fastball Velo", TRUE,
        "Avg Exit Velocity", TRUE,
        "Chase%", TRUE,
        "Whiff%", TRUE,
        "K%", TRUE,
        "BB%", FALSE,
        "Hard-Hit%", FALSE,
        "GB%", FALSE
      )
      
      # Include Barrel% if Angle is available
      if ("Angle" %in% names(pitcher_df)) {
        metric_info <- metric_info %>%
          add_row(metric = "Barrel%", higher_is_better = FALSE)
      }
      
      # Calculate pitcher metrics
      pitcher_metrics <- getAdvancedPitching(pitcher_df, pitcher_name)
      pitcher_metrics <- list(
        "Fastball Velo" = pitcher_metrics$FastballVelo,
        "Avg Exit Velocity" = pitcher_metrics$avgExitVelo,
        "Chase%" = pitcher_metrics$chasePct * 100,
        "Whiff%" = pitcher_metrics$whiffPct * 100,
        "K%" = pitcher_metrics$kPct * 100,
        "BB%" = pitcher_metrics$bbPct * 100,
        "Hard-Hit%" = pitcher_metrics$hardHitPct * 100,
        "GB%" = pitcher_metrics$groundBallPct * 100,
        "Barrel%" = pitcher_metrics$BarrelPct * 100
      )
      
      # Calculate league metrics
      league_pitchers <- unique(df$Pitcher)
      advanced_by_pitcher <- map_df(
        league_pitchers,
        ~ as_tibble(getAdvancedPitching(df, .x)),
        .id = "Pitcher"
      ) %>%
        mutate(
          chasePct = chasePct * 100,
          whiffPct = whiffPct * 100,
          kPct = kPct * 100,
          bbPct = bbPct * 100,
          hardHitPct = hardHitPct * 100,
          groundBallPct = groundBallPct * 100,
          BarrelPct = BarrelPct * 100
        ) %>%
        rename(
          "Fastball Velo" = FastballVelo,
          "Avg Exit Velocity" = avgExitVelo,
          "Chase%" = chasePct,
          "Whiff%" = whiffPct,
          "K%" = kPct,
          "BB%" = bbPct,
          "Hard-Hit%" = hardHitPct,
          "GB%" = groundBallPct,
          "Barrel%" = BarrelPct
        )
      
      # Calculate percentiles
      invert_cols <- c("BB%", "Hard-Hit%", "GB%", "Barrel%")
      advanced_by_pitcher <- advanced_by_pitcher %>%
        mutate(
          across(
            .cols = where(is.numeric) & !all_of(invert_cols),
            .fns = ~ percent_rank(.) * 100,
            .names = "{.col}_pct"
          ),
          across(
            .cols = all_of(invert_cols),
            .fns = ~ percent_rank(-.) * 100,
            .names = "{.col}_pct"
          )
        )
      
      metric_map <- list(
        "Fastball Velo" = "Fastball Velo_pct",
        "Avg Exit Velocity" = "Avg Exit Velocity_pct",
        "Chase%" = "Chase%_pct",
        "Whiff%" = "Whiff%_pct",
        "K%" = "K%_pct",
        "BB%" = "BB%_pct",
        "Hard-Hit%" = "Hard-Hit%_pct",
        "GB%" = "GB%_pct",
        "Barrel%" = "Barrel%_pct"
      )
      
      percentiles <- advanced_by_pitcher %>%
        filter(Pitcher == pitcher_name) %>%
        select(all_of(unlist(metric_map[metric_info$metric]))) %>%
        pivot_longer(everything(), names_to = "metric", values_to = "percentile") %>%
        mutate(metric = str_remove(metric, "_pct$")) %>%
        pull(percentile, name = metric)
      
      # Prepare data for plotting
      plot_data <- tibble(
        metric = metric_info$metric,
        percentile = percentiles[metric_info$metric],
        value = map_dbl(metric_info$metric, ~ pitcher_metrics[[.x]] %||% NA_real_)
      ) %>%
        mutate(
          metric = factor(metric, levels = rev(metric_info$metric)),
          color = case_when(
            is.na(percentile) ~ "grey",
            TRUE ~ scales::col_numeric("RdBu", domain = c(0, 100))(percentile)
          )
        )
      
      # Plot
      ggplot(plot_data, aes(y = metric)) +
        geom_bar(aes(x = percentile, fill = I(color)), stat = "identity", color = "black") +
        geom_point(aes(x = percentile), size = 10, shape = 21, fill = I(plot_data$color), color = "black") +
        geom_text(aes(x = percentile, label = round(percentile)), size = 3, fontface = "bold") +
        geom_vline(xintercept = 102, linetype = "dashed", color = "grey") +
        geom_text(
          aes(x = 104, label = case_when(
            str_detect(metric, "%") ~ sprintf("%.1f%%", value),
            str_detect(metric, "Velo|Velocity") ~ sprintf("%.1f", value),
            TRUE ~ sprintf("%.2f", value)
          )),
          hjust = 0, size = 3, fontface = "bold"
        ) +
        scale_x_continuous(limits = c(0, 110), expand = c(0, 0)) +
        labs(
          title = sprintf("%s Percentile Rankings", pitcher_name),
          x = "Percentile (vs. League)",
          y = NULL
        ) +
        theme_minimal() +
        theme(
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line.x = element_line(),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.background = element_rect(fill = "white", color = NA)
        )
    }
    
    # Render plot
    output$percentile_plot <- renderPlot({
      req(pitcher_data())
      if (nrow(pitcher_data()) == 0) return(NULL)
      plot_pitcher_percentiles(data(), pitcher_name(), pitcher_data())
    })
    
    # Error message
    output$error_message <- renderText({
      if (is.null(pitcher_data()) || nrow(pitcher_data()) == 0) {
        "No data found for the selected pitcher and date."
      } else {
        ""
      }
    })
    
    # Download plot
    output$download_plot <- downloadHandler(
      filename = function() {
        paste0(pitcher_name(), "_percentiles_", input$selected_date, ".png")
      },
      content = function(file) {
        ggsave(file, plot = plot_pitcher_percentiles(data(), pitcher_name(), pitcher_data()),
               width = 10, height = 6, dpi = 300)
      }
    )
  })
}