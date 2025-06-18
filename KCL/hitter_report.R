# Load required packages
library(dplyr)
library(gridExtra)
library(grid)
library(scales)
library(readr)
library(tidyverse)

# Define color palettes
rank_colors <- colorRampPalette(c("#E1463E", "white", "#00840D"))(18)
rank_colors2 <- colorRampPalette(c("#00840D", "white", "#E1463E"))(18)
na_color <- "#D3D3D3"

# Color ranking functions
color_by_rank <- function(rank_vector, n_colors) {
  rank_vector <- as.numeric(rank_vector)
  rank_vector[!is.finite(rank_vector)] <- NA
  if (all(is.na(rank_vector))) {
    stop("Rank vector has no valid values to process.")
  }
  min_rank <- min(rank_vector, na.rm = TRUE)
  max_rank <- max(rank_vector, na.rm = TRUE)
  if (!is.finite(min_rank) | !is.finite(max_rank)) {
    stop("Rank vector must contain finite values for color mapping.")
  }
  breaks <- seq(min_rank, max_rank, length.out = n_colors + 1)
  color_index <- findInterval(rank_vector, breaks, rightmost.closed = TRUE)
  rank_colors[color_index]
}

color_by_rank2 <- function(rank_vector, n_colors) {
  rank_vector <- as.numeric(rank_vector)
  rank_vector[!is.finite(rank_vector)] <- NA
  if (all(is.na(rank_vector))) {
    stop("Rank vector has no valid values to process.")
  }
  min_rank <- min(rank_vector, na.rm = TRUE)
  max_rank <- max(rank_vector, na.rm = TRUE)
  if (!is.finite(min_rank) | !is.finite(max_rank)) {
    stop("Rank vector must contain finite values for color mapping.")
  }
  breaks <- seq(min_rank, max_rank, length.out = n_colors + 1)
  color_index <- findInterval(rank_vector, breaks, rightmost.closed = TRUE)
  rank_colors2[color_index]
}

# Load data
df <- read_csv("/Users/maxim/Desktop/Kernelytics-Projects/CornBelters/Data/2025.csv")

# Filter and mutate data
df <- df %>%
  filter(
    BatterTeam == 'Normal cornbelters'
  ) %>%
  filter(
    TaggedPitchType != 'Undefined'
  ) %>%
  mutate(
    WhiffIndicator = ifelse(
      PitchCall == 'StrikeSwinging', 1, 0
    ),
    StrikeZoneIndicator = ifelse(
      PlateLocSide >= -0.8333 & PlateLocSide <= 0.8333 & 
        PlateLocHeight >= 1.5 & PlateLocHeight <= 3.37467, 
      1, 0),
    SwingIndicator = ifelse(
      PitchCall %in% c("StrikeSwinging", "FoulBallNotFieldable", "FoulBall", "InPlay"), 
      1, 0),
    BIPind = ifelse(
      PitchCall == 'InPlay' & HitType != 'Bunt', 1, 0
    ),
    Zwhiffind = ifelse(
      WhiffIndicator == 1 & StrikeZoneIndicator == 1, 1, 0),
    Zswing = ifelse(
      StrikeZoneIndicator == 1 & SwingIndicator == 1, 1, 0),
    LA1030ind = ifelse(PitchCall == 'InPlay' & Angle >= 10 & Angle <= 30, 1, 0),
    Barrelind = ifelse(PitchCall == 'InPlay' & ExitSpeed >= 95 & Angle >= 10 & Angle <= 32, 1, 0),
    HHind = ifelse(PitchCall == 'InPlay' & ExitSpeed >= 95, 1, 0),
    SCind = ifelse(
      (PitchCall == "InPlay" & 
         ((ExitSpeed > 95 & Angle >= 0 & Angle <= 35) | 
            (ExitSpeed > 92 & Angle >= 8 & Angle <= 35))), 1, 0),
    GBindicator = ifelse(HitType == 'GroundBall', 1, 0),
    LDind = ifelse(HitType == 'LineDrive', 1, 0),
    FBind = ifelse(HitType == 'FlyBall', 1, 0),
    Popind = ifelse(HitType == 'Popup', 1, 0),
    Chaseindicator = ifelse(SwingIndicator == 1 & StrikeZoneIndicator == 0, 1, 0),
    OutofZone = ifelse(StrikeZoneIndicator == 0, 1, 0),
    BatterTeam = ifelse(BatterTeam == 'COA_CHA' | BatterTeam == 'CCU_PRA', 'Coastal', BatterTeam),  # Note: Redundant due to filter
    PAindicator = ifelse(
      PitchCall %in% c("InPlay", "HitByPitch", "CatchersInterference") | 
        KorBB %in% c("Walk", "Strikeout"), 
      1, 0),
    OutIndicator = ifelse(
      (PlayResult %in% c("Out", "FieldersChoice") | KorBB == "Strikeout"), 
      1, 0),
    EV100ind = ifelse(PitchCall == 'InPlay' & ExitSpeed >= 100 & HitType != 'Bunt', 1, 0),
    WalkIndicator = ifelse(KorBB == 'Walk', 1, 0)
  )

# Summarize data for individual batters
summary_data <- df %>%
  group_by(Batter) %>%
  summarize(
    PA = sum(PAindicator, na.rm = TRUE),
    'BB%' = round(ifelse(sum(PAindicator) > 0, sum(WalkIndicator, na.rm = TRUE) / sum(PAindicator), NA_real_), 3) * 100,
    'SO%' = round(ifelse(sum(PAindicator) > 0, sum(KorBB == "Strikeout", na.rm = TRUE) / sum(PAindicator), NA_real_), 3) * 100,
    '90th EV' = round(quantile(ExitSpeed[PitchCall == 'InPlay' & HitType != 'Bunt'], probs = 0.9, na.rm = TRUE), 1),
    '100+ EV' = sum(EV100ind, na.rm = TRUE),
    'Avg EV' = round(mean(ExitSpeed[PitchCall == 'InPlay' & HitType != 'Bunt'], na.rm = TRUE), 1),
    'Max EV' = round(max(ExitSpeed, na.rm = TRUE), 1),
    'SC%' = round(ifelse(sum(BIPind) > 0, sum(SCind, na.rm = TRUE) / sum(BIPind, na.rm = TRUE), NA_real_), 3) * 100,
    '10-30%' = round(ifelse(sum(BIPind) > 0, sum(LA1030ind, na.rm = TRUE) / sum(BIPind, na.rm = TRUE), NA_real_), 3) * 100,
    'HH%' = round(ifelse(sum(BIPind) > 0, sum(HHind, na.rm = TRUE) / sum(BIPind, na.rm = TRUE), NA_real_), 3) * 100,
    'Barrel%' = round(ifelse(sum(BIPind) > 0, sum(Barrelind, na.rm = TRUE) / sum(BIPind, na.rm = TRUE), NA_real_), 3) * 100,
    'Whiff%' = round(ifelse(sum(SwingIndicator) > 0, sum(WhiffIndicator, na.rm = TRUE) / sum(SwingIndicator, na.rm = TRUE), NA_real_), 3) * 100,
    'Z Whiff%' = round(ifelse(sum(Zswing) > 0, sum(Zwhiffind, na.rm = TRUE) / sum(Zswing, na.rm = TRUE), NA_real_), 3) * 100,
    'Chase%' = round(ifelse(sum(SwingIndicator) > 0, sum(Chaseindicator, na.rm = TRUE) / sum(SwingIndicator, na.rm = TRUE), NA_real_), 3) * 100,
    'GB%' = round(ifelse(sum(BIPind) > 0, sum(GBindicator, na.rm = TRUE) / sum(BIPind, na.rm = TRUE), NA_real_), 3) * 100,
    'LD%' = round(ifelse(sum(BIPind) > 0, sum(LDind, na.rm = TRUE) / sum(BIPind, na.rm = TRUE), NA_real_), 3) * 100,
    'FB%' = round(ifelse(sum(BIPind) > 0, sum(FBind, na.rm = TRUE) / sum(BIPind, na.rm = TRUE), NA_real_), 3) * 100,
    'Pop%' = round(ifelse(sum(BIPind) > 0, sum(Popind, na.rm = TRUE) / sum(BIPind, na.rm = TRUE), NA_real_), 3) * 100
  )

# Calculate ranks for color assignments
summary_data3 <- summary_data %>%
  arrange(desc(`Avg EV`)) %>%
  mutate(
    Rank_AvgVelo = rank(`Avg EV`),
    Rank_Velo = rank(`Max EV`),
    Rank_90thEV = rank(`90th EV`),
    Rank_100EV = rank(`100+ EV`),
    Rank_SC = rank(`SC%`),
    Rank_swsp = rank(`10-30%`),
    Rankhh = rank(`HH%`),
    Rankbarrell = rank(`Barrel%`),
    RankWhiff = rank(`Whiff%`),
    RankZWhiff = rank(`Z Whiff%`),
    RankChase = rank(`Chase%`),
    RankGB = rank(`GB%`),
    RankLD = rank(`LD%`),
    RankFB = rank(`FB%`),
    RankPU = rank(`Pop%`),
    Rank_BB = rank(`BB%`),
    Rank_SO = rank(`SO%`)
  )

# Summarize data for team
summary_data6 <- df %>%
  group_by(BatterTeam) %>%
  summarize(
    PA = sum(PAindicator, na.rm = TRUE),
    'BB%' = round(ifelse(sum(PAindicator) > 0, sum(WalkIndicator, na.rm = TRUE) / sum(PAindicator), NA_real_), 3) * 100,
    'SO%' = round(ifelse(sum(PAindicator) > 0, sum(KorBB == "Strikeout", na.rm = TRUE) / sum(PAindicator), NA_real_), 3) * 100,
    '90th EV' = round(quantile(ExitSpeed[PitchCall == 'InPlay' & HitType != 'Bunt'], probs = 0.9, na.rm = TRUE), 1),
    '100+ EV' = sum(EV100ind, na.rm = TRUE),
    'Avg EV' = round(mean(ExitSpeed[PitchCall == 'InPlay'], na.rm = TRUE), 1),
    'Max EV' = round(max(ExitSpeed, na.rm = TRUE), 1),
    'SC%' = round(ifelse(sum(BIPind) > 0, sum(SCind, na.rm = TRUE) / sum(BIPind, na.rm = TRUE), NA_real_), 3) * 100,
    '10-30%' = round(ifelse(sum(BIPind) > 0, sum(LA1030ind, na.rm = TRUE) / sum(BIPind, na.rm = TRUE), NA_real_), 3) * 100,
    'HH%' = round(ifelse(sum(BIPind) > 0, sum(HHind, na.rm = TRUE) / sum(BIPind, na.rm = TRUE), NA_real_), 3) * 100,
    'Barrel%' = round(ifelse(sum(BIPind) > 0, sum(Barrelind, na.rm = TRUE) / sum(BIPind, na.rm = TRUE), NA_real_), 3) * 100,
    'Whiff%' = round(ifelse(sum(SwingIndicator) > 0, sum(WhiffIndicator, na.rm = TRUE) / sum(SwingIndicator, na.rm = TRUE), NA_real_), 3) * 100,
    'Z Whiff%' = round(ifelse(sum(Zswing) > 0, sum(Zwhiffind, na.rm = TRUE) / sum(Zswing, na.rm = TRUE), NA_real_), 3) * 100,
    'Chase%' = round(ifelse(sum(SwingIndicator) > 0, sum(Chaseindicator, na.rm = TRUE) / sum(SwingIndicator, na.rm = TRUE), NA_real_), 3) * 100,
    'GB%' = round(ifelse(sum(BIPind) > 0, sum(GBindicator, na.rm = TRUE) / sum(BIPind, na.rm = TRUE), NA_real_), 3) * 100,
    'LD%' = round(ifelse(sum(BIPind) > 0, sum(LDind, na.rm = TRUE) / sum(BIPind, na.rm = TRUE), NA_real_), 3) * 100,
    'FB%' = round(ifelse(sum(BIPind) > 0, sum(FBind, na.rm = TRUE) / sum(BIPind, na.rm = TRUE), NA_real_), 3) * 100,
    'Pop%' = round(ifelse(sum(BIPind) > 0, sum(Popind, na.rm = TRUE) / sum(BIPind, na.rm = TRUE), NA_real_), 3) * 100
  )

# Save tables as PDF
pdf(file = "/Users/maxim/Desktop/Kernelytics-Projects/KCL/hitter_report.pdf", width = 11, height = 8.5)  # Landscape dimensions
grid.newpage()
grid.text("Advanced Offensive Statistics", x = 0.5, y = 0.95, gp = gpar(fontsize = 20, fontface = "bold"))
grid.table(
  summary_data3 %>% 
    arrange(desc(`Avg EV`)) %>%
    select(Batter, PA, `BB%`, `SO%`, `90th EV`, `100+ EV`, `Avg EV`, `Max EV`, `SC%`, `10-30%`, `HH%`, `Barrel%`, 
           `Whiff%`, `Z Whiff%`, `Chase%`, `GB%`, `LD%`, `FB%`, `Pop%`),
  theme = ttheme_default(base_size = 5)  # Font size for 19 columns
)
grid.newpage()
grid.text("Team Advanced Offensive Statistics", x = 0.5, y = 0.95, gp = gpar(fontsize = 20, fontface = "bold"))
grid.table(
  summary_data6 %>%
    select(BatterTeam, PA, `BB%`, `SO%`, `90th EV`, `100+ EV`, `Avg EV`, `Max EV`, `SC%`, `10-30%`, `HH%`, `Barrel%`, 
           `Whiff%`, `Z Whiff%`, `Chase%`, `GB%`, `LD%`, `FB%`, `Pop%`),
  theme = ttheme_default(base_size = 8)
)
dev.off()