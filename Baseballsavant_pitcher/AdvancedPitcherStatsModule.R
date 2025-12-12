library(tidyverse)
library(shiny)

# Calculate advanced pitching stats
getAdvancedPitching <- function(df, name) {
  df <- df %>% filter(!is.na(Pitcher), tolower(Pitcher) == tolower(name))
  if (nrow(df) == 0) return(NULL)
  
  safe_mean <- function(x) {
    val <- mean(x, na.rm = TRUE)
    if (is.nan(val) || is.na(val)) NA_real_ else round(val, digits = 3) # Round to 3 decimals
  }
  
  FastballVelo <- df %>%
    filter(TaggedPitchType %in% c("Fastball", "Sinker", "Cutter")) %>%
    summarize(val = safe_mean(RelSpeed)) %>%
    pull(val)
  
  avgExitVelo <- df %>%
    filter(!is.na(ExitSpeed)) %>%
    summarize(val = safe_mean(ExitSpeed)) %>%
    pull(val)
  
  hardHitPct <- df %>%
    filter(!is.na(ExitSpeed)) %>%
    summarize(val = mean(ExitSpeed >= 95, na.rm = TRUE)) %>%
    pull(val) %>%
    { if (is.nan(.) || is.na(.)) NA_real_ else round(., digits = 3) } # Round to 3 decimals
  
  k_data <- df %>%
    filter(!is.na(PlayResult) | !is.na(KorBB))
  total_PA <- nrow(k_data)
  strikeouts <- sum(k_data$PlayResult %in% c("StrikeoutLooking", "StrikeoutSwinging"), na.rm = TRUE)
  kPct <- if (total_PA == 0) NA_real_ else round(strikeouts / total_PA, digits = 3) # Round to 3 decimals
  
  walks <- sum(k_data$KorBB == "Walk", na.rm = TRUE)
  bbPct <- if (total_PA == 0) NA_real_ else round(walks / total_PA, digits = 3) # Round to 3 decimals
  
  whiffPct <- df %>%
    filter(!is.na(PitchCall)) %>%
    filter(!PitchCall %in% c("BallCalled", "HitByPitch", "StrikeCalled", "Ball")) %>%
    summarize(val = mean(PitchCall == "StrikeSwinging", na.rm = TRUE)) %>%
    pull(val) %>%
    { if (is.nan(.) || is.na(.)) NA_real_ else round(., digits = 3) } # Round to 3 decimals
  
  chasePct <- df %>%
    filter(IsStrike == FALSE) %>%
    summarize(val = mean(IsSwing == TRUE, na.rm = TRUE)) %>%
    pull(val) %>%
    { if (is.nan(.) || is.na(.)) NA_real_ else round(., digits = 3) } # Round to 3 decimals
  
  groundBallPct <- df %>%
    filter(!is.na(HitType)) %>%
    summarize(val = mean(HitType == "GroundBall", na.rm = TRUE)) %>%
    pull(val) %>%
    { if (is.nan(.) || is.na(.)) NA_real_ else round(., digits = 3) } # Round to 3 decimals
  
  xBA <- df %>%
    filter(!is.na(PlayResult)) %>%
    summarize(val = mean(predicted_xba, na.rm = TRUE)) %>% 
    pull(val) %>%
    { if (is.nan(.) || is.na(.)) NA_real_ else round(., digits = 3) } # Round to 3 decimals
  
  BarrelPct <- if ("Angle" %in% names(df)) {
    df %>%
      filter(!is.na(ExitSpeed), !is.na(Angle)) %>%
      summarize(val = mean(ExitSpeed > 88 & Angle >= 26 & Angle <= 30, na.rm = TRUE)) %>%
      pull(val) %>%
      { if (is.nan(.) || is.na(.)) NA_real_ else round(., digits = 3) } # Round to 3 decimals
  } else {
    NA_real_
  }
  
  list(
    xBA = xBA,
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

# UI (minimal, for data processing only)
advancedPitcherStatsUI <- function(id) {
  ns <- NS(id)
  tagList()
}

# SERVER
advancedPitcherStatsServer <- function(id, data_source, pitcher_name) {
  moduleServer(id, function(input, output, session) {
    # Compute pitcher metrics and percentiles
    player_data <- reactive({
      req(pitcher_name(), data_source())
      df <- data_source()
      
      # Validate input data
      if (!is.data.frame(df) || nrow(df) == 0) {
        message("Error: data_source is not a valid data frame or is empty.")
        return(NULL)
      }
      
      # Remove apostrophes from Pitcher names and ensure consistent case
      df <- df %>%
        mutate(Pitcher = gsub("'", "", Pitcher))
      
      # Log available pitchers
      league_pitchers <- unique(df$Pitcher[!is.na(df$Pitcher)])
      if (length(league_pitchers) == 0) {
        message("Error: No valid pitchers found in data_source.")
        return(NULL)
      }
      message("Available pitchers: ", paste(league_pitchers, collapse = ", "))
      
      # Calculate metrics for all pitchers
      advanced_by_pitcher <- tibble(
        Pitcher = league_pitchers,
        metrics = map(league_pitchers, ~getAdvancedPitching(df, .x))
      ) %>%
        filter(!map_lgl(metrics, is_null)) %>%  # Remove NULL metrics
        unnest_wider(metrics)
      
      # Check for valid metrics
      if (nrow(advanced_by_pitcher) == 0) {
        message("No pitchers with valid metrics after processing.")
        return(NULL)
      }
      
      # Ensure numeric columns and round to 3 decimals
      numeric_cols <- c("xBA", "FastballVelo", "avgExitVelo", "hardHitPct", 
                        "kPct", "bbPct", "whiffPct", "chasePct", "groundBallPct", "BarrelPct")
      advanced_by_pitcher <- advanced_by_pitcher %>%
        mutate(across(all_of(numeric_cols), as.numeric)) %>%
        mutate(across(all_of(numeric_cols), ~ round(., digits = 3))) # Round metrics to 3 decimals
      
      # Remove pitchers with all NA metrics (excluding Pitcher column)
      advanced_by_pitcher <- advanced_by_pitcher %>%
        filter(rowSums(!is.na(select(., all_of(numeric_cols)))) > 0)
      
      if (nrow(advanced_by_pitcher) == 0) {
        message("No pitchers with valid metrics after NA filtering.")
        return(NULL)
      }
      
      # Log metrics
      message("Metrics computed for ", nrow(advanced_by_pitcher), " pitchers: ", 
              paste(advanced_by_pitcher$Pitcher, collapse = ", "))
      
      # Compute percentiles and round to 3 decimals
      invert_cols <- c("bbPct", "hardHitPct", "BarrelPct", "xBA",'avgExitVelo')
      non_invert_cols <- setdiff(numeric_cols, invert_cols)
      
      advanced_by_pitcher <- advanced_by_pitcher %>%
        mutate(
          across(
            all_of(non_invert_cols),
            ~ if_else(is.na(.), NA_real_, round(dplyr::percent_rank(.) * 100, digits = 3)), # Round to 3 decimals
            .names = "{.col}_pct"
          ),
          across(
            all_of(invert_cols),
            ~ if_else(is.na(.), NA_real_, round(dplyr::percent_rank(-.) * 100, digits = 3)), # Round to 3 decimals
            .names = "{.col}_pct"
          ),
          across(
            ends_with("_pct"),
            ~ if_else(is.na(.) | is.nan(.) | . == 0, 100, .)
          )
        )
      
      # Filter for selected pitcher (remove apostrophes from input name)
      cleaned_pitcher_name <- gsub("'", "", pitcher_name())
      result <- advanced_by_pitcher %>% 
        filter(tolower(Pitcher) == tolower(cleaned_pitcher_name))
      
      if (nrow(result) == 0) {
        message("No data for selected pitcher: ", cleaned_pitcher_name, 
                ". Available pitchers: ", paste(advanced_by_pitcher$Pitcher, collapse = ", "))
        return(NULL)
      }
      
      result
    })
    
    return(player_data)
  })
}