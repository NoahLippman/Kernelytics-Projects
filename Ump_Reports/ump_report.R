### Umpire Report Base Code


library(tidyverse)
library(here)
library(ggforce)



  
#-----------------------------TEST DATA---------------------------------------#



# if (!here::here() %>% endsWith("/Kernelytics-Projects")) {
#   here::i_am("Ump_Reports/ump_report_test.Rmd")
# }
# 
# 
# df <- read_csv(here("kclData", "6-12 BlueCaps@Merchants.csv"))
# 
# dfc <- classify(df)
# 
# split_calls <- splits(dfc)
# 
# 
# accuracy_rings <- rings(dfc)




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
    filter(CorrectCall == "StrikeCalled")
  oz <- df %>%
    filter(CorrectCall == "BallCalled")
  
  return(list(
    lhp = lhp,
    rhp = rhp,
    lhb = lhb,
    rhb = rhb,
    home = home,
    away = away,
    iz = iz,
    oz = oz
    ))
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
  iz_help <- tibble(
    category = c("Correct", "Incorrect"),
    value = c(iz_acc, 1 - iz_acc)
  )
  
  # Helper values - out of zone accuracy
  oz_correct <- sum(split_list$oz$Accuracy == "Correct")
  oz_total <- nrow(split_list$oz)
  oz_acc <- oz_correct / oz_total 
  
  oz_help <- tibble(
    category = c("Correct", "Incorrect"),
    value = c(oz_acc, 1 - oz_acc)
  )
    
  
  overall <- ggplot(overall_help, aes(x=2, y = value, fill = category)) + 
    geom_col(color = "white", width = 1) +
    coord_polar(theta = "y") +
    xlim(0, 3) +
    scale_fill_manual(values = c("Correct" = "green", "Incorrect" = "red")) +
    theme_void() +
    annotate("text",
             x = 0, y = 0,
             label = paste0(
               " ",
               round(accuracy * 100, 1),
               "%\n",
               correct,
               "/",
               total),
               size = 4,
               color = "black",
               hjust = 0.5,
               vjust = 0.5) +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
    labs(title = "Overall Accuracy")
  
  iz <- ggplot(iz_help, aes(x=2, y = value, fill = category)) + 
    geom_col(color = "white", width = 1) +
    coord_polar(theta = "y") +
    xlim(0, 3) +
    scale_fill_manual(values = c("Correct" = "green", "Incorrect" = "red")) +
    theme_void() + 
    annotate("text",
             x = 0, y = 0,
             label = paste0(
               " ",
               round(iz_acc * 100, 1),
               "%\n",
               iz_correct,
               "/",
               iz_total),
             size = 4,
             color = "black",
             hjust = 0.5,
             vjust = 0.5) +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
    labs(title = "In Zone Accuracy")
  
  oz <- ggplot(oz_help, aes(x=2, y = value, fill = category)) + 
    geom_col(color = "white", width = 1) +
    coord_polar(theta = "y") +
    xlim(0, 3) +
    scale_fill_manual(values = c("Correct" = "green", "Incorrect" = "red")) +
    theme_void() +
    annotate("text",
             x = 0, y = 0,
             label = paste0(
               " ",
               round(oz_acc * 100, 1),
               "%\n",
               oz_correct,
               "/",
               oz_total),
             size = 4,
             color = "black",
             hjust = 0.5,
             vjust = 0.5) +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
    labs(title = "Out of Zone Accuracy")
  
  return(list(overall = overall,
              iz = iz,
              oz = oz
              ))
}


#---------------------------- STRIKE ZONE GRAPHICS-----------------------------#

zones <- function(incorrect_balls, incorrect_strikes){
  
  oz_plot <- ggplot(incorrect_balls, aes(x=PlateLocSide, y = PlateLocHeight,
                                      color = "red")) +
    geom_point(alpha = 0.7, size= 4
    ) +
    coord_fixed() +
    scale_x_continuous(limits = c(-3,3)) +
    scale_y_continuous(limits = c(1,4)) + 
    labs(
      title = "Incorrect Ball Calls",
      x = "Horizontal Pitch Location (ft)",
      y = "Vertical Plate Location (ft)",
      color = "Call Accuracy"
    ) +
    theme_void() +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
    geom_rect(aes(xmin = -1, xmax = 1, ymin = 1.5, ymax = 3.5),
              color = "black", fill = NA, linetype = "dashed")
  
  iz_plot <- ggplot(incorrect_strikes, aes(x=PlateLocSide, y = PlateLocHeight,
                                        color = "red")) +
    geom_point(alpha = 0.7, size= 4
    ) +
    coord_fixed() + 
    scale_x_continuous(limits = c(-3,3)) +
    scale_y_continuous(limits = c(1,4)) + 
    labs(
      title = "Incorrect Strike Calls",
      x = "Horizontal Pitch Location (ft)",
      y = "Vertical Plate Location (ft)",
      color = "Call Accuracy"
    ) +
    theme_void() +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
    geom_rect(aes(xmin = -1, xmax = 1, ymin = 1.5, ymax = 3.5),
              color = "black", fill = NA, linetype = "dashed")
  
  return(list(oz_plot = oz_plot,
              iz_plot = iz_plot)
         )
  
}


# missed_balls <- game %>%
#   filter(PitchCall == 'BallCalled', CorrectCall == "Strike")
# 
# 
# 
# 
# missed_strikes <- game %>%
#   filter(PitchCall == "StrikeCalled", CorrectCall == "Ball")



