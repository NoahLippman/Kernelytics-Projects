### Pitch Pairing Study

library(dplyr)

# Load data

yt_files <- list.files("kclData", pattern = "\\.csv$", full.names = TRUE)

yt_data <- bind_rows(lapply(yt_files, read.csv))


# Filter data for example pitcher - Roy Rolston

rolston <- yt_data %>%
  filter(Pitcher == "Roy Rolston") %>%
  filter(TaggedPitchType != "")

rolston_mix <- unique(rolston$TaggedPitchType)

# Get all row indices of fastballs

fb <- which(rolston$TaggedPitchType == "Fastball")
fb <- fb[fb < nrow(rolston)]

fb_pair <- rolston[c(fb, fb + 1), ]
