# ========================
# Title:        Plate Discipline Table Generator
# Description:  Creates a dataframe of plate discipline stats for a hitter
# Author:       Cam Cischke
# Created:      2025-06-18
# Last Updated: 2025-06-18
# Dependencies: tidyverse, readr
# Usage: source this file, then run generate_df(player_name)
# ========================

### SETUP ------------------------------------------------------------------

library(readr)
library(tidyverse)


### DATA IMPORT/CLEANING ---------------------------------------------------


kcl_files <- list.files("kclData/", pattern = "\\.csv$", full.names = T)
kcl_data <- bind_rows(lapply(kcl_files, read_csv))

belt_files <- list.files("CornbeltersData/", pattern = "\\.csv$", full.names = T)
belt_data <- bind_rows(lapply(belt_files, read_csv))


# Zone coordinate calculation values

box <- 2/3  # Width/height of one of the nine boxes of the zone
ball <- 2.9 / 12 # Width of ball (2.9 in converted to feet)

# Zone coords
zonex1 <- -1
zonex2 <- 1
zoney1 <- 1.5
zoney2 <- 3.5 

# Calculate meatball coords
meatballx1 <- zonex1 + box
meatballx2 <- zonex2 - box
meatbally1 <- zoney1 + box
meatbally2 <- zoney2 - box

# Calculate shadow zone coords
shadowx1 <- zonex1 - ball
shadowx2 <- zonex2 + ball
shadowy1 <- zoney1 - ball
shadowy2 <- zoney2 + ball

edgex1 <- zonex1 + ball
edgex2 <- zonex2 - ball
edgey1 <- zoney1 + ball
edgey2 <- zoney2 - ball


# Calculate edge zone coords


# Remove untagged pitches and no-ballflight pitches

yt_data <- bind_rows(kcl_data, belt_data) %>%
  filter(TaggedPitchType != "") %>%
  drop_na(PlateLocHeight, PlateLocSide)



### ZONE FILTERS --------------------------------------------------------------
yt_data$zone <- case_when(
  # Meatball
  yt_data$PlateLocSide >= meatballx1 & yt_data$PlateLocSide <= meatballx2 &
    yt_data$PlateLocHeight >= meatbally1 & yt_data$PlateLocHeight <= meatbally2
  ~ "Meatball",
  # Heart
  yt_data$PlateLocSide >= edgex1 & yt_data$PlateLocSide <= edgex2 & 
    yt_data$PlateLocHeight >= edgey1 & yt_data$PlateLocHeight <= edgey2
  ~ "Heart",
  # Edge
  yt_data$PlateLocSide >= zonex1 & yt_data$PlateLocSide <= zonex2 &
    yt_data$PlateLocHeight >= zoney1 & yt_data$PlateLocHeight <= zoney2
  ~ "Edge",
  # Shadow
  yt_data$PlateLocSide >= shadowx1 & yt_data$PlateLocSide <= shadowx2 &
    yt_data$PlateLocHeight >= shadowy1 & yt_data$PlateLocHeight <= shadowy2
  ~ "Shadow",
  TRUE ~ "Miss"
)



### HELPER FUNCTIONS ---------------------------------------------------------

# Get Player Data
get_player <- function(player){
  return(filter(yt_data, Batter == player))
}


# Zone
get_zone <- function(data){
  return(
    data %>%
      filter(zone %in% c("Meatball", "Heart", "Edge"))
  )
}


# Edge
get_edge <- function(data){
  return(
    data %>%
      filter(zone %in% c("Edge", "Shadow"))
  )
}


# Ball
get_ball <- function(data){
  return(
    data %>%
      filter(zone %in% c("Shadow", "Miss"))
  )
}

# Meatball
get_meatball <- function(data){
  return(
    data %>%
      filter(zone == "Meatball")
  )
}



### DATAFRAME GENERATOR -------------------------------------------------------
# Generates the table for output to Savant

generate_plate_disc <- function(player){
  
  # Create df for output
  result <- data.frame(matrix(nrow = 1, ncol = 12))
  colnames(result) <- c(
    "Pitches", "Zone %", "Zone Swing %", "Zone Contact %", "Chase %",
    "Chase Contact %", "Edge %", "1st Pitch Swing %", "Swing %", "Whiff %", 
    "Meatball %", "Meatball Swing %"
  )
  
  # Get data for player
  data <- get_player(player)
  
  
  # Get pitches thrown in each zone
  in_zone <- get_zone(data)
  on_edge <- get_edge(data)
  ball <- get_ball(data)
  meatball <- get_meatball(data)
  
  # Get total number of pitches
  n <- length(data$zone)
  result$Pitches <- n
  
  
  # Zone 
  result$`Zone %` <- length(in_zone$zone) / n
  
  # Zone Swing 
  result$`Zone Swing %` <- sum(in_zone$PitchCall %in% c(
    "StrikeSwinging", "Foul", "InPlay"
  )) / length(in_zone$zone)
  
  # Zone Contact 
  result$`Zone Contact %` <- sum(in_zone$PitchCall %in% c(
    "Foul", "InPlay"
  )) / length(in_zone$zone)
  
  # Chase
  result$`Chase %` <- sum(ball$PitchCall %in% c(
    "StrikeSwinging", "Foul", "InPlay"
  )) / length(ball$zone)
  
  # Chase Contact
  result$`Chase Contact %` <- sum(ball$PitchCall %in% c(
    "Foul", "InPlay"
  )) / length(ball)
  
  # Edge 
  result$`Edge %` <- length(on_edge$zone) / n
  
  # 1st Pitch Swing
  result$`1st Pitch Swing %` <- sum(
    (data$PitchCall %in% c("StrikeSwinging", "Foul", "InPlay")) &
      (data$PitchofPA == 1)
  ) / sum(data$PitchofPA == 1)
  
  # Swing
  result$`Swing %` <- sum(data$PitchCall %in% c(
    "StrikeSwinging", "Foul", "InPlay"
  )) / n
  
  
  # Whiff
  result$`Whiff %` <- sum(data$PitchCall == "StrikeSwinging") /
    sum(data$PitchCall %in% c("StrikeSwinging", "Foul", "InPlay"))
  
  # Meatball
  result$`Meatball %` <- sum(data$zone == "Meatball") / n
  
  # Meatball Contact
  result$`Meatball Swing %`  <- sum(meatball$PitchCall %in% c(
    "StrikeSwinging", "Foul", "InPlay"
  )) / length(meatball$zone)
  
  
  
  # Convert to percentages and round
  percentage_cols <- colnames(result[-1]) # all except "Pitches"
  result[percentage_cols] <- round(result[percentage_cols] * 100, 1)
  
  return(result)
}



