# Load required libraries
library(flexdashboard)  # For creating interactive dashboards (though not used directly in this script)
library(tidyverse)     # For data manipulation (dplyr, tidyr) and visualization (ggplot2)

# Read and preprocess game data from a CSV file
# The CSV contains pitch-by-pitch baseball data for a game between Burlington Bees and Normal Cornbelters
game_data <- read.csv("/Users/noahlippman/Downloads/05_28_2025 6_27_59 PM-Burlington Bees 2025@Normal Cornbelters.csv") %>%
  # Create a new column 'Swing_Type' based on PitchCall
  # - If PitchCall is "InPlay", "Foul", or "StrikeSwinging", classify as "Contact" (for InPlay) or "Whiff" (for Foul/StrikeSwinging)
  # - Otherwise, classify as "No Swing"
  mutate("Swing_Type" = if_else(PitchCall %in% c("InPlay", "Foul", "StrikeSwinging"), 
                                if_else(PitchCall == "InPlay", "Contact", "Whiff"), 
                                "No Swing")) 

# Add a 'Chase' column to indicate chase pitches (swings outside the strike zone)
# - A pitch is a chase (Chase = 1) if Swing_Type is "Contact" or "Whiff" and the pitch location is outside the strike zone
# - Strike zone is defined as: PlateLocSide between -1.66 and 0, PlateLocHeight between 1.5 and 3.5
# - Otherwise, Chase = 0
game_data <- game_data %>%
  mutate("Chase" = if_else(Swing_Type %in% c("Contact", "Whiff") & 
                             (PlateLocSide < -1.66 | PlateLocSide > 0 | 
                                PlateLocHeight < 1.5 | PlateLocHeight > 3.5), 1, 0))

# Define a function to create a plate discipline plot for a specific player
# Input: player_name (character string representing the batter's name)
Plate_discipline_plot <- function(player_name) {
  # Filter game_data for the specified player and remove rows with missing location data
  has_location_data <- game_data %>%
    drop_na(PlateLocHeight) %>%  # Remove rows where PlateLocHeight is NA
    drop_na(PlateLocSide) %>%    # Remove rows where PlateLocSide is NA
    filter(Batter == player_name) # Keep only rows where Batter matches player_name
  
  # Calculate chase rate: percentage of pitches outside the strike zone that the batter swung at
  Chase_Rate <- round(mean(has_location_data$Chase) * 100, 2)
  
  # Calculate in-zone swing rate: percentage of pitches in the strike zone that the batter swung at
  # Excludes chase pitches (Chase != 1) and non-swings (Swing_Type != "No Swing")
  Zone_Swing_Rate <- round(mean(has_location_data$Swing_Type != "No Swing" & 
                                  has_location_data$Chase != 1) * 100, 2)
  
  # Create a ggplot object for the plate discipline chart
  p <- ggplot(data = has_location_data, 
              aes(x = PlateLocSide,      # x-axis: horizontal pitch location
                  y = PlateLocHeight,    # y-axis: vertical pitch location
                  color = as.character(Swing_Type),  # Color points by Swing_Type
                  shape = TaggedPitchType,           # Shape points by pitch type
                  fill = ExitSpeed)) +               # Fill points by exit speed (if applicable)
    # Add title and subtitle with player name and calculated rates
    ggtitle(paste("Plate Discipline Chart for", player_name), 
            subtitle = paste("Overall Chase Rate: ", Chase_Rate,
                             "% | In Zone Swing Rate: ", Zone_Swing_Rate, "%")) + 
    xlim(-3, 3) +  # Set x-axis limits to show pitch locations from -3 to 3 feet
    ylim(0, 5) +   # Set y-axis limits to show pitch locations from 0 to 5 feet
    # Draw strike zone boundaries (rectangle from PlateLocSide: -1.66 to 0, PlateLocHeight: 1.5 to 3.5)
    geom_segment(aes(x = -1.66, y = 1.5, xend = 0, yend = 1.5), color = "black", inherit.aes = FALSE) +  # Bottom edge
    geom_segment(aes(x = -1.66, y = 3.5, xend = 0, yend = 3.5), color = "black", inherit.aes = FALSE) +  # Top edge
    geom_segment(aes(x = -1.66, y = 1.5, xend = -1.66, yend = 3.5), color = "black", inherit.aes = FALSE) +  # Left edge
    geom_segment(aes(x = 0, y = 1.5, xend = 0, yend = 3.5), color = "black", inherit.aes = FALSE) +  # Right edge
    geom_point() +  # Plot pitch locations as points
    theme(title = element_text(size = 5)) +  # Set title text size to 5
    labs(color = "Swing Type")  # Label the color legend as "Swing Type"
  
  return(p)  # Return the ggplot object
}

# Define a list of player names to generate plots for
player_list <- c("Connor Kave", "Ryan Skwarek", "Kooper Schulte", "Keanu Spenser",
                 "Conor Fitzpatric", "Caleb Seibers", "Dominic Texeira", "Dash Denton", 
                 "Noah Company")

# Loop through each player to generate and save a plate discipline plot
for (x in player_list) {
  # Use tryCatch to handle errors and warnings gracefully
  tryCatch({
    # Open a PNG graphics device to save the plot
    png(file = paste("/Users/noahlippman/Documents/Bees Graphics/", x, ".png"),
        width = 900, height = 525, res = 200)  # Set dimensions and resolution
    print(Plate_discipline_plot(x))  # Generate and print the plot
    dev.off()  # Close the graphics device
  }, warning = function(w) {
    # If a warning occurs, print the warning message and attempt to generate the plot again
    message(paste(x, "- Warning:", conditionMessage(w)))
    png(file = paste("/Users/noahlippman/Documents/Bees Graphics/", x, ".png"),
        width = 900, height = 525, res = 200)
    print(Plate_discipline_plot(x))
    dev.off()
  }, error = function(e) {
    # If an error occurs, print the error message and skip to the next player
    message(paste(x, "- Error:", conditionMessage(e)))
  })
}
