library(tidyverse)
library(shiny)

# Calculate advanced pitching stats
getAdvancedPitching <- function(df, name) {
  df <- df %>% filter(!is.na(Pitcher), tolower(Pitcher) == tolower(name))
  if (nrow(df) == 0) return(NULL)
  
  safe_mean <- function(x) {
    val <- mean(x, na.rm = TRUE)
    if (is.nan(val) || is.na(val)) NA_real_ else val
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
    { if (is.nan(.) || is.na(.)) NA_real_ else . }
  
  k_data <- df %>%
    filter(!is.na(PlayResult) | !is.na(KorBB))
  total_PA <- nrow(k_data)
  strikeouts <- sum(k_data$PlayResult %in% c("StrikeoutLooking", "StrikeoutSwinging"), na.rm = TRUE)
  kPct <- if (total_PA == 0) NA_real_ else strikeouts / total_PA
  
  walks <- sum(k_data$KorBB == "Walk", na.rm = TRUE)
  bbPct <- if (total_PA == 0) NA_real_ else walks / total_PA
  
  whiffPct <- df %>%
    filter(!is.na(PitchCall)) %>%
    filter(!PitchCall %in% c("BallCalled", "HitByPitch", "StrikeCalled", "Ball")) %>%
    summarize(val = mean(PitchCall == "StrikeSwinging", na.rm = TRUE)) %>%
    pull(val) %>%
    { if (is.nan(.) || is.na(.)) NA_real_ else . }
  
  chasePct <- df %>%
    filter(IsStrike == FALSE) %>%
    summarize(val = mean(IsSwing == TRUE, na.rm = TRUE)) %>%
    pull(val) %>%
    { if (is.nan(.) || is.na(.)) NA_real_ else . }
  
  groundBallPct <- df %>%
    filter(!is.na(HitType) & HitType != "Throwdown") %>%
    summarize(val = mean(HitType == "GroundBall", na.rm = TRUE)) %>%
    pull(val) %>%
    { if (is.nan(.) || is.na(.)) NA_real_ else . }
  xBA  <- df %>%
    filter(!is.na(PlayResult)) %>%
    summarize(val = mean(predicted_xba,na.rm = TRUE)) %>% 
    pull(val)  %>%
    { if (is.nan(.) || is.na(.)) NA_real_ else . }
  
  BarrelPct <- if ("Angle" %in% names(df)) {
    df %>%
      filter(!is.na(ExitSpeed), !is.na(Angle)) %>%
      summarize(val = mean(ExitSpeed > 90 & Angle >= 26 & Angle <= 30, na.rm = TRUE)) %>%
      pull(val) %>%
      { if (is.nan(.) || is.na(.)) NA_real_ else . }
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
      req(pitcher_name())
      df <- data_source()
      
      # Log available pitchers
      league_pitchers <- unique(df$Pitcher)

      # Calculate metrics for all pitchers
      advanced_by_pitcher <- tibble(
        Pitcher = league_pitchers,
        metrics = map(league_pitchers, ~getAdvancedPitching(df, .x))
      ) %>%
        filter(!map_lgl(metrics, is_null)) %>%  # Remove NULL metrics
        unnest_wider(metrics) %>%
        filter(rowSums(!is.na(select(., -Pitcher))) > 0)  # Remove pitchers with all NA metrics
      
      if (nrow(advanced_by_pitcher) == 0) {
        message("No pitchers with valid metrics after processing.")
        return(NULL)
      }
      
      # Log metrics
      message("Metrics computed for ", nrow(advanced_by_pitcher), " pitchers: ", 
              paste(advanced_by_pitcher$Pitcher, collapse = ", "))
      
      # Compute percentiles
      invert_cols <- c("bbPct", "hardHitPct", "groundBallPct", "BarrelPct", "xBA")
      advanced_by_pitcher <- advanced_by_pitcher %>%
        mutate(
          across(
            .cols = setdiff(names(select(advanced_by_pitcher, where(is.numeric))), invert_cols),
            .fns = ~ if_else(is.na(.), NA_real_, percent_rank(.) * 100),
            .names = "{.col}_pct"
          ),
          across(
            .cols = all_of(invert_cols),
            .fns = ~ if_else(is.na(.), NA_real_, percent_rank(-.) * 100),
            .names = "{.col}_pct"
          )
        )
      
      advanced_by_pitcher <- advanced_by_pitcher %>%
        mutate(across(
          ends_with("_pct"),
          ~ if_else(is.na(.) | . == 0, 100, .)
        ))
      
      # Filter for selected pitcher
      result <- advanced_by_pitcher %>% 
        filter(tolower(Pitcher) == tolower(pitcher_name()))
      if (nrow(result) == 0) {
        message("No data for selected pitcher: ", pitcher_name(), 
                ". Available pitchers in metrics: ", paste(advanced_by_pitcher$Pitcher, collapse = ", "))
      }
      result
    })
    
    return(player_data)
  })
}