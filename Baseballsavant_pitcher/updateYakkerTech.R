library(tidyverse)
library(mgcv)
library(purrr)

#"C:/Users/isu_mvquirk_admin/Documents/GitHub/Kernelytics-Projects/kclData"
#"C:/Users/maxim/Desktop/Kernelytics-Projects/kclData"
#Put your file path
kcl_all_files <- list.files(
  path = "C:/Users/isu_mvquirk_admin/Documents/GitHub/Kernelytics-Projects/kclData",
  pattern = "\\.csv$",
  full.names = TRUE
)

#"C:/Users/maxim/Desktop/Kernelytics-Projects/CornBeltersData",
#"C:/Users/isu_mvquirk_admin/Documents/GitHub/Kernelytics-Projects/CornBeltersData"
belters_all_files <- list.files(
  path = "C:/Users/isu_mvquirk_admin/Documents/GitHub/Kernelytics-Projects/CornBeltersData",
  pattern = "\\.csv$",
  full.names = TRUE
)


kcl_list_dfs <- kcl_all_files |> 
  map(read_csv)

belters_list_dfs <- belters_all_files |> 
  map(read_csv)

#Create dataframes with combined yakkertech data for both leagues
KCLyakkertechData <- bind_rows(kcl_list_dfs)
BeltersyakkertechData <- bind_rows(belters_list_dfs)


corrections <- c(
  "Justin Trusner" = "Jacob Trusner",
  "Brooks Neuhof" = "Brooks Neuhoff",
  "Teagan Disharoom" = 	"Teagan Disharoon",
  "Kam  Ross" = "Kam Ross",
  "Brayden  Windy" = "Brayden Windy",
  "Sammy Descarpentrie" = "Sam DesCarpentrie",
  "Kannan Kleine" = "Kannon Kleine",
  "Nolan Mccrossin" = "Nolan McCrossin"
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

KCLyakkertechData <- fixNames(KCLyakkertechData, "Batter", corrections)
KCLyakkertechData <- fixNames(KCLyakkertechData, "Pitcher", corrections)


BeltersyakkertechData <- BeltersyakkertechData |> 
  filter(PitcherTeam == "Normal cornbelters")

BeltersyakkertechData <- fixNames(BeltersyakkertechData, "Batter", corrections)
BeltersyakkertechData <- fixNames(BeltersyakkertechData, "Pitcher", corrections)



KCLyakkertechData <- KCLyakkertechData |> 
  mutate(
    IsStrike = PlateLocHeight >= 1.5 &
      PlateLocHeight <= 3.5 &
      PlateLocSide   >= -1 &
      PlateLocSide   <=  1
  ) |> 
  mutate(
    IsSwing = PitchCall == "InPlay" |
      PitchCall == "StrikeSwinging" |
      PitchCall == "Foul"
  )

BeltersyakkertechData <- BeltersyakkertechData |> 
  mutate(
    IsStrike = PlateLocHeight >= 1.5 &
      PlateLocHeight <= 3.5 &
      PlateLocSide   >= -1 &
      PlateLocSide   <=  1
  ) |> 
  mutate(
    IsSwing = PitchCall == "InPlay" |
      PitchCall == "StrikeSwinging" |
      PitchCall == "Foul"
  )

KCLyakkertechData <- KCLyakkertechData |> 
  mutate(
    zone = case_when(
      PlateLocHeight > 2.833333 & PlateLocHeight <= 3.5 & PlateLocSide >= -1 & PlateLocSide <= -.3333333           ~ 1,
      PlateLocHeight > 2.833333 & PlateLocHeight <= 3.5 & PlateLocSide > -.3333333 & PlateLocSide <= .3333333      ~ 2,
      PlateLocHeight > 2.833333 & PlateLocHeight <= 3.5 & PlateLocSide > .3333333 & PlateLocSide <= 1              ~ 3,
      PlateLocHeight > 2.1666666 & PlateLocHeight <= 2.833333 & PlateLocSide >= -1 & PlateLocSide <= -.3333333      ~ 4,
      PlateLocHeight > 2.1666666 & PlateLocHeight <= 2.833333 & PlateLocSide > -.3333333 & PlateLocSide <= .3333333 ~ 5,
      PlateLocHeight > 2.1666666 & PlateLocHeight <= 2.833333 & PlateLocSide > .3333333 & PlateLocSide <= 1         ~ 6,
      PlateLocHeight >= 1.5 & PlateLocHeight <= 2.1666666 & PlateLocSide >= -1 & PlateLocSide <= -.3333333          ~ 7,
      PlateLocHeight >= 1.5 & PlateLocHeight <= 2.1666666 & PlateLocSide > -.3333333 & PlateLocSide <= .3333333     ~ 8,
      PlateLocHeight >= 1.5 & PlateLocHeight <= 2.1666666 & PlateLocSide > .3333333 & PlateLocSide <= 1             ~ 9,
      TRUE ~ NA_integer_
    )
  )

BeltersyakkertechData <- BeltersyakkertechData |> 
  mutate(
    zone = case_when(
      PlateLocHeight > 2.833333 & PlateLocHeight <= 3.5 & PlateLocSide >= -1 & PlateLocSide <= -.3333333           ~ 3,
      PlateLocHeight > 2.833333 & PlateLocHeight <= 3.5 & PlateLocSide > -.3333333 & PlateLocSide <= .3333333      ~ 2,
      PlateLocHeight > 2.833333 & PlateLocHeight <= 3.5 & PlateLocSide > .3333333 & PlateLocSide <= 1              ~ 1,
      PlateLocHeight > 2.1666666 & PlateLocHeight <= 2.833333 & PlateLocSide >= -1 & PlateLocSide <= -.3333333      ~ 6,
      PlateLocHeight > 2.1666666 & PlateLocHeight <= 2.833333 & PlateLocSide > -.3333333 & PlateLocSide <= .3333333 ~ 5,
      PlateLocHeight > 2.1666666 & PlateLocHeight <= 2.833333 & PlateLocSide > .3333333 & PlateLocSide <= 1         ~ 4,
      PlateLocHeight >= 1.5 & PlateLocHeight <= 2.1666666 & PlateLocSide >= -1 & PlateLocSide <= -.3333333          ~ 9,
      PlateLocHeight >= 1.5 & PlateLocHeight <= 2.1666666 & PlateLocSide > -.3333333 & PlateLocSide <= .3333333     ~ 8,
      PlateLocHeight >= 1.5 & PlateLocHeight <= 2.1666666 & PlateLocSide > .3333333 & PlateLocSide <= 1             ~ 7,
      TRUE ~ NA_integer_
    )
  )

KCLyakkertechData <- KCLyakkertechData |> 
  mutate(
    PitchCategory = case_when(
      TaggedPitchType %in% c("Cutter", "Fastball", "Sinker") ~ "Fastball",
      TaggedPitchType %in% c("Changeup", "Splitter") ~ "Offspeed",
      TaggedPitchType %in% c("Curveball", "Slider") ~ "Breaking",
      TRUE ~ "Other"
    )
  ) |> 
  mutate(
    bases = case_when(
      PlayResult == "Single" ~ 1,
      PlayResult == "Double" ~ 2,
      PlayResult == "Triple" ~ 3,
      PlayResult == "HomeRun" ~ 4,
      TRUE ~ 0
    )
  )

BeltersyakkertechData <- BeltersyakkertechData |> 
  mutate(
    PitchCategory = case_when(
      TaggedPitchType %in% c("Cutter", "Fastball", "Sinker") ~ "Fastball",
      TaggedPitchType %in% c("Changeup", "Splitter") ~ "Offspeed",
      TaggedPitchType %in% c("Curveball", "Slider") ~ "Breaking",
      TRUE ~ "Other"
    )
  ) |> 
  mutate(
    bases = case_when(
      PlayResult == "Single" ~ 1,
      PlayResult == "Double" ~ 2,
      PlayResult == "Triple" ~ 3,
      PlayResult == "HomeRun" ~ 4,
      TRUE ~ 0
    )
  )


savant <- read_csv("C:/Users/isu_mvquirk_admin/Documents/GitHub/Kernelytics-Projects/Baseballsavant_pitcher/savantData25.csv")

set.seed(42)

fit_xba <- gam(
  expected_babip ~ te(launch_speed, launch_angle),
  data = savant,
  family = gaussian(),
  method = "REML"
)

fit_xwoba <- gam(
  expected_woba ~ te(launch_speed, launch_angle),
  data = savant,
  family = gaussian(),
  method = "REML"
)

fit_xslg <- gam(
  estimated_slg_using_speedangle ~ te(launch_speed, launch_angle),
  data = savant,
  family = gaussian(),
  method = "REML"
)



KCLyakkertechData <- KCLyakkertechData |> 
  mutate(row_id = row_number())

modelYakkertechData <- KCLyakkertechData |> 
  select(row_id, ExitSpeed, Angle) |> 
  filter(!is.na(ExitSpeed) & !is.na(Angle)) |> 
  rename(
    launch_speed = ExitSpeed,
    launch_angle = Angle
  )

savant <- savant |> 
  filter(!is.na(launch_speed) & !is.na(launch_angle)) |> 
  filter(expected_babip > 0 & expected_babip < 1)

modelYakkertechData$predicted_xba <- predict(fit_xba, newdata = modelYakkertechData)
modelYakkertechData$predicted_xba <- pmax(0.0001, modelYakkertechData$predicted_xba)

KCLyakkertechData <- KCLyakkertechData |> 
  left_join(
    modelYakkertechData |> select(row_id, predicted_xba),
    by = "row_id"
  )

BeltersyakkertechData <- BeltersyakkertechData |> 
  mutate(row_id = row_number())

modelYakkertechData <- BeltersyakkertechData |> 
  select(row_id, ExitSpeed, Angle) |> 
  filter(!is.na(ExitSpeed) & !is.na(Angle)) |> 
  rename(
    launch_speed = ExitSpeed,
    launch_angle = Angle
  )

savant <- savant |> 
  filter(!is.na(launch_speed) & !is.na(launch_angle)) |> 
  filter(expected_babip > 0 & expected_babip < 1)

modelYakkertechData$predicted_xba <- predict(fit_xba, newdata = modelYakkertechData)
modelYakkertechData$predicted_xba <- pmax(0.0001, modelYakkertechData$predicted_xba)

BeltersyakkertechData <- BeltersyakkertechData |> 
  left_join(
    modelYakkertechData |> select(row_id, predicted_xba),
    by = "row_id"
  )

modelYakkertechData <- KCLyakkertechData |> 
  select(row_id, ExitSpeed, Angle) |> 
  filter(!is.na(ExitSpeed) & !is.na(Angle)) |> 
  rename(
    launch_speed = ExitSpeed,
    launch_angle = Angle
  )

modelYakkertechData$predicted_xwoba <- predict(fit_xwoba, newdata = modelYakkertechData)
modelYakkertechData$predicted_xwoba <- pmax(0.0001, modelYakkertechData$predicted_xwoba)

KCLyakkertechData <- KCLyakkertechData |> 
  left_join(
    modelYakkertechData |> select(row_id, predicted_xwoba),
    by = "row_id"
  )

KCLyakkertechData <- KCLyakkertechData |> 
  mutate(predicted_xwoba = case_when(
    KorBB == "Walk" | PlayResult == "Walk" ~ 0.695,
    TRUE ~ predicted_xwoba
  ))

modelYakkertechData <- BeltersyakkertechData |> 
  select(row_id, ExitSpeed, Angle) |> 
  filter(!is.na(ExitSpeed) & !is.na(Angle)) |> 
  rename(
    launch_speed = ExitSpeed,
    launch_angle = Angle
  )

modelYakkertechData$predicted_xwoba <- predict(fit_xwoba, newdata = modelYakkertechData)
modelYakkertechData$predicted_xwoba <- pmax(0.0001, modelYakkertechData$predicted_xwoba)

BeltersyakkertechData <- BeltersyakkertechData |> 
  left_join(
    modelYakkertechData |> select(row_id, predicted_xwoba),
    by = "row_id"
  )

BeltersyakkertechData <- BeltersyakkertechData |> 
  mutate(predicted_xwoba = case_when(
    KorBB == "Walk" | PlayResult == "Walk" ~ 0.695,
    TRUE ~ predicted_xwoba
  ))

modelYakkertechData <- KCLyakkertechData |> 
  select(row_id, ExitSpeed, Angle) |> 
  filter(!is.na(ExitSpeed) & !is.na(Angle)) |> 
  rename(
    launch_speed = ExitSpeed,
    launch_angle = Angle
  )

modelYakkertechData$predicted_xslg <- predict(fit_xslg, newdata = modelYakkertechData)
modelYakkertechData$predicted_xslg <- pmax(0.0001, modelYakkertechData$predicted_xslg)


KCLyakkertechData <- KCLyakkertechData |> 
  left_join(
    modelYakkertechData |> select(row_id, predicted_xslg),
    by = "row_id"
  )

KCLyakkertechData <- KCLyakkertechData |> 
  mutate(predicted_xba = replace_na(predicted_xba, 0),
         predicted_xwoba = replace_na(predicted_xwoba, 0),
         predicted_xslg = replace_na(predicted_xslg, 0))


modelYakkertechData <- BeltersyakkertechData |> 
  select(row_id, ExitSpeed, Angle) |> 
  filter(!is.na(ExitSpeed) & !is.na(Angle)) |> 
  rename(
    launch_speed = ExitSpeed,
    launch_angle = Angle
  )

modelYakkertechData$predicted_xslg <- predict(fit_xslg, newdata = modelYakkertechData)
modelYakkertechData$predicted_xslg <- pmax(0.0001, modelYakkertechData$predicted_xslg)

BeltersyakkertechData <- BeltersyakkertechData |> 
  left_join(
    modelYakkertechData |> select(row_id, predicted_xslg),
    by = "row_id"
  )

BeltersyakkertechData <- BeltersyakkertechData |> 
  mutate(predicted_xba = replace_na(predicted_xba, 0),
         predicted_xwoba = replace_na(predicted_xwoba, 0),
         predicted_xslg = replace_na(predicted_xslg, 0))

yakkertechData <- bind_rows(KCLyakkertechData, BeltersyakkertechData)
write_csv(KCLyakkertechData, "KCLYakkertechData.csv")
write_csv(BeltersyakkertechData, "BeltersYakkertechData.csv")
write_csv(yakkertechData, "YakkertechData.csv")