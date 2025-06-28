### Umpire Report Base Code


library(ggplot2)
library(tidyverse)
library(readr)
library(here)




#-----------------------------TEST DATA---------------------------------------#



kcl_files <- list.files("kclData/", pattern = "\\.csv$", full.names = T)
kcl_data <- bind_rows(lapply(kcl_files, read_csv))
here::i_am("Ump_Reports/ump_report.R")

df <- read_csv(here("kclData", "6-12 BlueCaps@Merchants.csv"))










#---------------------------------CLASSIFY-------------------------------------#

# This function cleans and classifies the data by correct pitch call
classify <- function(df){
  
  result <- df %>%
    drop_na(PitchCall, PlateLocSide, PlateLocHeight, PitcherThrows, BatterSide)
  
  result$CorrectCall <- ifelse(
    result$PlateLocSide >= -1 & result$PlateLocSide <= 1 &
      result$PlateLocHeight >= 1.5 & result$PlateLocHeight <= 3.5,
    "Strike",
    "Ball"
  )
  
  return(result)
}



#--------------------------------FILTERING-------------------------------------#


# This function filters the data by a couple of splits
filter <- function(df){
  # Filter by pitcher handedness
  lhp <- df %>%
    filter(PitcherThrows == "Left")
  rhp <- df %>%
    filter(PitcherThrows == "Right")
  
  # Filter by batter handedness
  lhb <- df%>%
    filter(BatterSide == "Left")
  rhb <- df %>%
    filter(BatterSide == "Right")
  
  # Filter by team 
  home <- df %>%
    filter(`Top/Bottom` == "Bottom")
  away <- df %>%
    filter(`Top/Bottom` == "Top")
  
  # In zone
  iz <- df %>%
    filter(CorrectCall == "Strike")
  oz <- df %>%
    filter(CorrectCall == "Ball")
  
  return(list(lhp, rhp, lhb, rhb, home, away, iz, oz))
}


#------------------------------RING GRAPHS-------------------------------------#


missed_balls <- game %>%
  filter(PitchCall == 'BallCalled', CorrectCall == "Strike")

ggplot(missed_balls, aes(x=PlateLocSide, y = PlateLocHeight, color = CorrectCall)) +
  geom_point(alpha = 0.7, size= 4
  ) +
  coord_fixed() +
  labs(
    title = "06/01 Called Balls",
    x = "Horizontal Pitch Location (ft)",
    y = "Vertical Plate Location (ft)",
    color = "Call Accuracy"
  ) + 
  scale_color_manual(
    values = c("Strike" = "red", "Ball" = "green")
  )+
  theme_minimal() +
  geom_rect(aes(xmin = -1, xmax = 1, ymin = 1.5, ymax = 3.5),
            color = "black", fill = NA, linetype = "dashed")


missed_strikes <- game %>%
  filter(PitchCall == "StrikeCalled", CorrectCall == "Ball")



ggplot(missed_balls, aes(x=PlateLocSide, y = PlateLocHeight, color = CorrectCall)) +
  geom_point(alpha = 0.7, size= 4
  ) +
  coord_fixed() +
  labs(
    title = "06/01 Called Balls",
    x = "Horizontal Pitch Location (ft)",
    y = "Vertical Plate Location (ft)",
    color = "Call Accuracy"
  ) + 
  scale_color_manual(
    values = c("Strike" = "red", "Ball" = "green")
  )+
  theme_minimal() +
  geom_rect(aes(xmin = -1, xmax = 1, ymin = 1.5, ymax = 3.5),
            color = "black", fill = NA, linetype = "dashed")