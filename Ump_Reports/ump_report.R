### Umpire Report Base Code


library(tidyverse)
library(here)




#-----------------------------TEST DATA---------------------------------------#



kcl_files <- list.files("kclData/", pattern = "\\.csv$", full.names = T)
kcl_data <- bind_rows(lapply(kcl_files, read_csv))
here::i_am("Ump_Reports/ump_report.R")

df <- read_csv(here("kclData", "6-12 BlueCaps@Merchants.csv"))










#---------------------------------CLASSIFY-------------------------------------#

# This function cleans and classifies the data by correct pitch call
classify <- function(df){
  
  # Drop NA's from important columns and select only called balls/strikes
  result <- df %>%
    drop_na(PitchCall, PlateLocSide, PlateLocHeight, PitcherThrows, BatterSide) %>%
    filter(PitchCall %in% c("StrikeCalled", "BallCalled"))
  
  # Add column for the correct call
  result$CorrectCall <- ifelse(
    result$PlateLocSide >= -1 & result$PlateLocSide <= 1 &
      result$PlateLocHeight >= 1.5 & result$PlateLocHeight <= 3.5,
    "StrikeCalled",
    "BallCalled"
  )
  
  result$Accuracy <- ifelse(
    result$PitchCall == result$CorrectCall,
    "Correct",
    "Incorrect"
  )
  return(result)
}



#--------------------------------FILTERING-------------------------------------#


# This function filters the cleaned data by a couple of splits
splits <- function(df){
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


rings <- function(df){
  
  # Get splits for passed dataframe
  split_list <- splits(df)
  
  # Helper values - overall accuracy
  correct <- sum(df$Accuracy == "Correct")
  total <- nrow(df)
  accuracy <- correct / total
  
  overall_help <- tibble(
    category = c("Correct", "Incorrect"),
    value = c(accuracy, 1 - accuracy)
  )
  
  # Helper values - in zone accuracy
  iz_correct <- sum(split_list$iz$Accuracy == "Correct")
  iz_total <- nrow(split_list$iz)
  iz_acc <- iz_correct / iz_total
  print(iz_acc)
  iz_help <- tibble(
    category = c("Correct", "Incorrect"),
    value = c(iz_acc, 1 - iz_acc)
  )
  
  # Helper values - out of zone accuracy
  oz_correct <- sum(split_list$oz$Accuracy == "Correct")
  oz_total <- nrow(split_list$oz)
  oz_acc <- oz_correct / oz_correct
  
  oz_help <- tibble(
    category = c("Correct", "Incorrect"),
    value = c(oz_acc, 1 - oz_acc)
  )
    
  
  overall <- ggplot(overall_help, aes(x=2, y = value, fill = category)) + 
    geom_col(color = "white", width = 1) +
    coord_polar(theta = "y") +
    xlim(0.5, 2.5) +
    scale_fill_manual(values = c("Correct" = "green", "Incorrect" = "red")) +
    theme_void()
  
  iz <- ggplot(iz_help, aes(x=2, y = value, fill = category)) + 
    geom_col(color = "white", width = 1) +
    coord_polar(theta = "y") +
    xlim(0.5, 2.5) +
    scale_fill_manual(values = c("Correct" = "green", "Incorrect" = "red")) +
    theme_void()
  
  oz <- ggplot(oz_help, aes(x=2, y = value, fill = category)) + 
    geom_col(color = "white", width = 1) +
    coord_polar(theta = "y") +
    xlim(0.5, 2.5) +
    scale_fill_manual(values = c("Correct" = "green", "Incorrect" = "red")) +
    theme_void()
  
  return(list(overall, iz, oz))
}


# 
# missed_balls <- game %>%
#   filter(PitchCall == 'BallCalled', CorrectCall == "Strike")
# 
# ggplot(missed_balls, aes(x=PlateLocSide, y = PlateLocHeight, color = CorrectCall)) +
#   geom_point(alpha = 0.7, size= 4
#   ) +
#   coord_fixed() +
#   labs(
#     title = "06/01 Called Balls",
#     x = "Horizontal Pitch Location (ft)",
#     y = "Vertical Plate Location (ft)",
#     color = "Call Accuracy"
#   ) + 
#   scale_color_manual(
#     values = c("Strike" = "red", "Ball" = "green")
#   )+
#   theme_minimal() +
#   geom_rect(aes(xmin = -1, xmax = 1, ymin = 1.5, ymax = 3.5),
#             color = "black", fill = NA, linetype = "dashed")
# 
# 
# missed_strikes <- game %>%
#   filter(PitchCall == "StrikeCalled", CorrectCall == "Ball")
# 
# 
# 
# ggplot(missed_balls, aes(x=PlateLocSide, y = PlateLocHeight, color = CorrectCall)) +
#   geom_point(alpha = 0.7, size= 4
#   ) +
#   coord_fixed() +
#   labs(
#     title = "06/01 Called Balls",
#     x = "Horizontal Pitch Location (ft)",
#     y = "Vertical Plate Location (ft)",
#     color = "Call Accuracy"
#   ) + 
#   scale_color_manual(
#     values = c("Strike" = "red", "Ball" = "green")
#   )+
#   theme_minimal() +
#   geom_rect(aes(xmin = -1, xmax = 1, ymin = 1.5, ymax = 3.5),
#             color = "black", fill = NA, linetype = "dashed")