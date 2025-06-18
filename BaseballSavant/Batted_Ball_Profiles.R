

library(tidyverse)
library(dplyr)

# Adding KCL files
kcl_all_files <- list.files(
  path = "/Users/wyattsherman/Downloads/Kernelytics-Projects-main 4/kclData",
  pattern = "\\.csv$",
  full.names = TRUE
)

# Adding Belters files
belters_all_files <- list.files(
  path = "/Users/wyattsherman/Downloads/Kernelytics-Projects-main 4/CornBeltersData",
  pattern = "\\.csv$",
  full.names = TRUE
)


kcl_list_dfs <- kcl_all_files |>
  map(read_csv)

belters_list_dfs <- belters_all_files |>
  map(read_csv)

KCLyakkertechData <- bind_rows(kcl_list_dfs)
#write_csv(KCLyakkertechData, "KCLYakkertechData.csv")

BeltersyakkertechData <- bind_rows(belters_list_dfs)
#write_csv(BeltersyakkertechData, "BeltersYakkertechData.csv")


# Name Corrections 
corrections <- c(
  "Justin Trusner" = "Jacob Trusner",
  "Brooks Neuhof" = "Brooks Neuhoff",
  "Teagan Disharoom" = "Teagan Disharoon",
  "Kam  Ross" = "Kam Ross",
  "Brayden Windy" = "Brayden Windy",
  "Sammy Descarpentrie" = "Sam DesCarpentrie"
)

fixNames <- function(df, column, lookup){
  old_values <- df[[column]]
  needs_fix <- old_values %in% names(lookup)
  df[[column]] <- ifelse(
    needs_fix,
    lookup[old_values],
    old_values
  )
  return(df)
}

combined_data <-bind_rows(KCLyakkertechData,BeltersyakkertechData)
gamedata <- fixNames(combined_data,"Batter",corrections)

#Filtering for the teams project pertains to
batted_balls <- gamedata %>%
  filter(!is.na(HitType),BatterTeam %in% c("Normal cornbelters","Kcl groundsloths 2025",
                                           "Kcl merchants 2025","Kcl bluecaps 2025","Kcl bobcats 2025","Kcl merchants 2026", "Kcl groundsloths 2026", "Kcl bluecaps 2026", "Kcl bobcats 2026")) %>%
  filter(HitType!="Throwdown",HitType!="Foul")

# Finding the Batted Ball Data Profiles
batted_ball_summary <- batted_balls %>%
  mutate(
    Valid_Bearing = !is.na(Bearing),
  # Spray Direction Percentage
  Spray_Direction =case_when(
    BatterSide == "Right" & Bearing <= -15 ~ "Pull",
    BatterSide == "Right" & Bearing >= 15 ~ "Opposite",
    BatterSide =="Left" & Bearing >= 15 ~ "Pull",
    BatterSide == 'Left' & Bearing <=-15 ~ "Opposite",
    !is.na(Bearing) & Bearing > -15 & Bearing < 15 ~ "Straight",
    TRUE ~ NA_character_
  ),
BallType = case_when(
  HitType == "GroundBall" ~ "GB",
  HitType %in% c("FlyBall","LineDrive","Popup") ~ "AIR",
  TRUE ~ NA_character_
  )
)

batted_ball_summary <- batted_ball_summary %>%
  filter(!is.na(Bearing)) %>%
  group_by(Batter) %>%
summarize(
  BBE=n(),
  Valid_Bearing =sum(Valid_Bearing),
  GB=sum(HitType=="GroundBall"),
  GB_Percent=round(100*GB/BBE,1),
  AIR = sum(HitType %in% c("FlyBall","LineDrive","Popup")),
  AIR_Percent=(100-GB_Percent),
  FB=sum(HitType=="FlyBall"),
  FB_Percent=round(100*FB/BBE,1),
  LD=sum(HitType=="LineDrive"),
  LD_Percent=round(100*LD/BBE,1),
  PU=sum(HitType=="Popup"),
  PU_Percent=round(100*PU/BBE,1),
  #Pull Percent 
  Pull_Percent = ifelse(
    Valid_Bearing > 0,
    round(100*sum((BatterSide=="Right" & Bearing <= -15)|
            (BatterSide=="Left" & Bearing >= 15), na.rm = TRUE) /Valid_Bearing, 1),0),
#Straight Percent
Sraight_Percent = ifelse(
  Valid_Bearing > 0, 
  round(100*sum(Bearing > -15 & Bearing < 15, na.rm = TRUE)/Valid_Bearing,1),0),
# Opposite Percent
Opposite_Percent =ifelse(
  Valid_Bearing > 0, 
  round(100*sum((BatterSide=="Right" & Bearing >=15)|
          (BatterSide =="Left" & Bearing <= -15),na.rm= TRUE)/Valid_Bearing, 1),0),


# Directional GB and AIR Data 
Pulled_GB_Percent = ifelse(GB >0, round(100* sum(BallType == "GB" & Spray_Direction == "Pull") / BBE, 1), 0),
Opposite_GB_Percent = ifelse(GB >0, round(100* sum(BallType == "GB" & Spray_Direction == "Opposite")/BBE, 1), 0),
Straight_GB_Percent = ifelse(GB >0, round(100* sum(BallType == "GB" & Spray_Direction == "Straight")/BBE, 1), 0),
Pull_AIR_Percent = ifelse(AIR >0, round(100* sum(BallType == "AIR" & Spray_Direction == "Pull")/ BBE, 1), 0),
Opposite_AIR_Percent = ifelse(AIR >0, round(100* sum(BallType == "AIR" & Spray_Direction == "Opposite")/ BBE,1), 0),
Straight_AIR_Percent = ifelse(AIR >0, round(100* sum(BallType == "AIR" & Spray_Direction =="Straight")/ BBE, 1), 0)
) %>%
  arrange(desc(BBE)) %>%
  select(-GB,-FB,-LD,-PU,-AIR, -Valid_Bearing)

View(batted_ball_summary)



   
 

  