### Plate Discipline Savant Table

library(tidyverse)
library(ggplot2) # for testing

# Data Import/Cleaning ----


kcl_files <- list.files("kclData", pattern = "\\.csv$", full.names = T)
kcl_data <- bind_rows(lapply(yt_files, read.csv))

belt_files <- list.files("CornbeltersData", pattern = "\\.csv$", full.names = T)
belt_data <- bind_rows(lapply(belt_files, read.csv))


# Zone coordinate calculation values - easier to keep track of them this way
  
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
edgex1 <- zonex1 - ball
edgex2 <- zonex2 + ball
edgey1 <- zoney1 - ball
edgey2 <- zoney2 + ball


# Remove untagged pitches and no-ballflight pitches

yt_data <- bind_rows(kcl_data, belt_data) %>%
  filter(TaggedPitchType != "") %>%
  drop_na(PlateLocHeight, PlateLocSide)


# Tag pitches by zone location
yt_data$zone <- case_when(
  # Meatball
  yt_data$PlateLocSide >= meatballx1 & yt_data$PlateLocSide <= meatballx2 &
    yt_data$PlateLocHeight >= meatbally1 & yt_data$PlateLocHeight <= meatbally2
  ~ "Meatball",
  # Edge in zone
  yt_data$PlateLocSide >= edgex1 & yt_data$PlateLocSide <= edgex2 &
    yt_data$PlateLocHeight >= edgey1 & yt_data$PlateLocHeight <= edgey2 &
    yt_data$PlateLocSide >= zonex1 & yt_data$PlateLocSide <= zonex2 &
    yt_data$PlateLocHeight >= zoney1 & yt_data$PlateLocHeight <= zoney2
  ~ "Edge",
  # Zone
  yt_data$PlateLocSide >= zonex1 & yt_data$PlateLocSide <= zonex2 &
    yt_data$PlateLocHeight >= zoney1 & yt_data$PlateLocHeight <= zoney2
  ~ "Zone",
  yt_data$PlateLocSide >= edgex1 & yt_data$PlateLocSide <= edgex2 &
    yt_data$PlateLocHeight >= edgey1 & yt_data$PlateLocHeight <= edgey2
  ~ "Shadow",
  # Miss
  TRUE ~ "Miss"
)  
  
test <- filter(yt_data, Batter == "Caleb Royer")
# Zone percentage

zone_perc <- function(name){
  data <- filter(yt_data, Batter == name)
  return(sum(data$zone == "Zone") / nrow(data)) 
}
















# Plot for testing
ggplot(yt_data, aes(x = PlateLocSide, y = PlateLocHeight, color = zone)) +
  geom_point(alpha = 0.7, size = 4) +
  coord_fixed() +
  labs(title = "Shadow Zone") +
  theme_minimal() +
  geom_rect(aes(xmin = zonex1, xmax = zonex2, ymin = zoney1, ymax = zoney2), 
            color = "black",fill = NA) +
  geom_rect(aes(xmin = edgex1, xmax = edgex2, ymin = edgey1, ymax = edgey2),
            color = "black", fill = NA, linetype = "dashed")
  
  



    

