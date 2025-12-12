# ========================
# Title:        Plate Discipline Table Generator
# Description:  Creates a dataframe of plate discipline stats for a hitter
# Author:       Cam Cischke
# Created:      2025-06-18
# Last Updated: 2025-06-18
# Dependencies: tidyverse, readr
# Usage: run generate_plate_discipline_table(player_name, data)
# ========================

### SETUP ------------------------------------------------------------------

library(readr)
library(tidyverse)

### CLASSIFY FUNCTION ---------------------------------------------------------
# Classifies pitches based on zone location
classify <- function(data){
  
  # Remove untagged pitches and no-ballflight pitches
  data <- data %>%
    filter(TaggedPitchType != "") %>%
    drop_na(PlateLocHeight, PlateLocSide) %>%
    drop_na(PitchCall)
  
  result <- data
  
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
  
  # Zone filters
  result$zone <- case_when(
    # Meatball
    data$PlateLocSide >= meatballx1 & data$PlateLocSide <= meatballx2 &
      data$PlateLocHeight >= meatbally1 & data$PlateLocHeight <= meatbally2
    ~ "Meatball",
    # Heart
    data$PlateLocSide >= edgex1 & data$PlateLocSide <= edgex2 & 
      data$PlateLocHeight >= edgey1 & data$PlateLocHeight <= edgey2
    ~ "Heart",
    # Edge
    data$PlateLocSide >= zonex1 & data$PlateLocSide <= zonex2 &
      data$PlateLocHeight >= zoney1 & data$PlateLocHeight <= zoney2
    ~ "Edge",
    # Shadow
    data$PlateLocSide >= shadowx1 & data$PlateLocSide <= shadowx2 &
      data$PlateLocHeight >= shadowy1 & data$PlateLocHeight <= shadowy2
    ~ "Shadow",
    TRUE ~ "Miss"
  )
  
  return(result)
  
}




### HELPER FUNCTIONS ---------------------------------------------------------

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

generate_plate_discipline_table <- function(df){
  
  # Create df for output
  result <- data.frame(matrix(nrow = 1, ncol = 12))
  colnames(result) <- c(
    "Pitches", "Zone %", "Zone Swing %", "Zone Contact %", "Chase %",
    "Chase Contact %", "Edge %", "1st Pitch Swing %", "Swing %", "Whiff %", 
    "Meatball %", "Meatball Swing %"
  )
  
  # Classify data
  player_data_zoned <- classify(df)
  
  
  # Get pitches thrown in each zone
  in_zone <- get_zone(player_data_zoned)
  on_edge <- get_edge(player_data_zoned)
  ball <- get_ball(player_data_zoned)
  meatball <- get_meatball(player_data_zoned)
  
  # Get total number of pitches
  n <- length(player_data_zoned$zone)
  result$Pitches <- n
  
  
  # Zone 
  result$`Zone %` <- length(in_zone$zone) / n
  
  # Zone Swing 
  result$`Zone Swing %` <- sum(in_zone$PitchCall %in% c(
    "StrikeSwinging", "Foul", "InPlay"
  )) / nrow(in_zone)
  
  # Zone Contact 
  result$`Zone Contact %` <- sum(in_zone$PitchCall %in% c(
    "Foul", "InPlay"
  )) / nrow(in_zone)
  
  # Chase
  result$`Chase %` <- sum(ball$PitchCall %in% c(
    "StrikeSwinging", "Foul", "InPlay"
  )) / nrow(ball)
  
  # Chase Contact
  result$`Chase Contact %` <- sum(ball$PitchCall %in% c(
    "Foul", "InPlay"
  )) / nrow(ball)
  
  # Edge 
  result$`Edge %` <- length(on_edge$zone) / n
  
  # 1st Pitch Swing
  result$`1st Pitch Swing %` <- sum(
    (player_data_zoned$PitchCall %in% c("StrikeSwinging", "Foul", "InPlay")) &
      (player_data_zoned$PitchofPA == 1)
  ) / sum(player_data_zoned$PitchofPA == 1)
  
  # Swing
  result$`Swing %` <- sum(player_data_zoned$PitchCall %in% c(
    "StrikeSwinging", "Foul", "InPlay"
  )) / n
  
  
  # Whiff
  result$`Whiff %` <- sum(player_data_zoned$PitchCall == "StrikeSwinging") /
    sum(player_data_zoned$PitchCall %in% c("StrikeSwinging", "Foul", "InPlay"))
  
  # Meatball
  result$`Meatball %` <- sum(player_data_zoned$zone == "Meatball") / n
  
  # Meatball Contact
  result$`Meatball Swing %`  <- sum(meatball$PitchCall %in% c(
    "StrikeSwinging", "Foul", "InPlay"
  )) / nrow(meatball)
  
  
  
  # Convert to percentages and round
  percentage_cols <- colnames(result[-1]) # all except "Pitches"
  result[percentage_cols] <- round(result[percentage_cols] * 100, 1)
  
  return(result)
}


#–– UI function ––
plateDisciplineUI <- function(id) {
  ns <- NS(id)
  div(
    class = "basic-info-card",
    style = "
      display: flex;
      flex-direction: column;
      background: #fff;
      border: 1px solid #ccc;
      border-radius: 8px;
      overflow: hidden;
      box-shadow: 0 2px 6px rgba(0,0,0,0.1);
      width: 100%;
    ",
    
    div(
      style = "text-align: center; margin-bottom: 5px;",
      h3("Plate Discipline")
    ),
    
    div(
      style = "padding: 12px;",
      tableOutput(ns("discipline_table"))
    )
  )
}

#–– Server function ––
plateDisciplineServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$discipline_table <- renderTable({
      generate_plate_discipline_table(data())
    }, rownames = FALSE)
  })
}


