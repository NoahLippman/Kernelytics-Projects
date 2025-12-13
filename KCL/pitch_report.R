# Load required packages
library(dplyr)
library(gridExtra)
library(grid)
library(scales)
library(readr)
library(tidyverse)

# Define color palettes
rank_colors <- colorRampPalette(c("#E1463E", "white", "#00840D"))(18)  # Red (low) to Green (high)
rank_colors2 <- colorRampPalette(c("#00840D", "white", "#E1463E"))(18)  # Green (low) to Red (high)
na_color <- "#D3D3D3"

# Color ranking functions (modified to return na_color instead of stopping)
color_by_rank <- function(rank_vector, n_colors) {
  rank_vector <- as.numeric(rank_vector)
  rank_vector[!is.finite(rank_vector)] <- NA
  if (all(is.na(rank_vector))) {
    return(rep(na_color, length(rank_vector)))  # Return gray for all-NA
  }
  min_rank <- min(rank_vector, na.rm = TRUE)
  max_rank <- max(rank_vector, na.rm = TRUE)
  if (!is.finite(min_rank) | !is.finite(max_rank)) {
    return(rep(na_color, length(rank_vector)))  # Return gray for non-finite
  }
  breaks <- seq(min_rank, max_rank, length.out = n_colors + 1)
  color_index <- findInterval(rank_vector, breaks, rightmost.closed = TRUE)
  rank_colors[color_index]
}

color_by_rank2 <- function(rank_vector, n_colors) {
  rank_vector <- as.numeric(rank_vector)
  rank_vector[!is.finite(rank_vector)] <- NA
  if (all(is.na(rank_vector))) {
    return(rep(na_color, length(rank_vector)))  # Return gray for all-NA
  }
  min_rank <- min(rank_vector, na.rm = TRUE)
  max_rank <- max(rank_vector, na.rm = TRUE)
  if (!is.finite(min_rank) | !is.finite(max_rank)) {
    return(rep(na_color, length(rank_vector)))  # Return gray for non-finite
  }
  breaks <- seq(min_rank, max_rank, length.out = n_colors + 1)
  color_index <- findInterval(rank_vector, breaks, rightmost.closed = TRUE)
  rank_colors2[color_index]
}

# Load data
data_path <- "C:/Users/isu_mvquirk_admin/Documents/GitHub/Kernelytics-Projects/KCL/Data/2025.csv"

if (!file.exists(data_path)) {
  stop("Data file not found: ", data_path)
}
df <- read_csv(data_path, col_types = cols(.default = "c")) %>%  # Read as character to avoid type issues
  mutate(
    RelSpeed = as.numeric(RelSpeed),  # Convert to numeric
    PlateLocSide = as.numeric(PlateLocSide),
    PlateLocHeight = as.numeric(PlateLocHeight),
    ExitSpeed = as.numeric(ExitSpeed),
    Angle = as.numeric(Angle),
    Balls = as.numeric(Balls),
    Strikes = as.numeric(Strikes)
  )

# Debug: Check column names and unique PitcherTeam values
message("Column names in dataset: ", paste(names(df), collapse = ", "))
message("Unique PitcherTeam values: ", paste(unique(df$PitcherTeam), collapse = ", "))

# Filter and mutate data
df <- df %>%
  filter(
    Pitcher %in% c('Thomas Mickels'),
    !is.na(RelSpeed),
    !(TaggedPitchType %in% "Undefined"),
    !(PitchCall %in% "Undefined")
   
  ) %>%
  mutate(
    FBindicator = ifelse(TaggedPitchType %in% c("Fastball", "Sinker"), 1, 0),
    OSindicator = ifelse(TaggedPitchType %in% c("Slider", "Cutter", "Curveball", "ChangeUp"), 1, 0),
    EarlyIndicator = ifelse(
      (Balls == 0 & Strikes == 0 & PitchCall %in% "InPlay") |
        (Balls == 1 & Strikes == 0 & PitchCall %in% "InPlay") |
        (Balls == 0 & Strikes == 1 & PitchCall %in% "InPlay") |
        (Balls == 1 & Strikes == 1 & PitchCall %in% "InPlay"), 
      1, 0),
    AheadIndicator = ifelse(
      ((Balls == 0 & Strikes == 1) & PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable", "FoulBall")) |
        ((Balls == 1 & Strikes == 1) & PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable", "FoulBall")), 
      1, 0),
    StrikeZoneIndicator = ifelse(
      PlateLocSide >= -0.8333 & PlateLocSide <= 0.8333 & 
        PlateLocHeight >= 1.5 & PlateLocHeight <= 3.37467, 
      1, 0),
    EdgeHeightIndicator = ifelse(
      (PlateLocHeight > 14/12 & PlateLocHeight < 22/12) |
        (PlateLocHeight > 38/12 & PlateLocHeight < 46/12), 
      1, 0),
    EdgeZoneHtIndicator = ifelse(
      PlateLocHeight > 16/12 & PlateLocHeight < 45.2/12, 
      1, 0),
    EdgeZoneWIndicator = ifelse(
      PlateLocSide > -13.4/12 & PlateLocSide < 13.4/12, 
      1, 0),
    EdgeWidthIndicator = ifelse(
      (PlateLocSide > -13.3/12 & PlateLocSide < -6.7/12) |
        (PlateLocSide < 13.3/12 & PlateLocSide > 6.7/12), 
      1, 0),
    HeartIndicator = ifelse(
      PlateLocSide >= -0.5583 & PlateLocSide <= 0.5583 & 
        PlateLocHeight >= 1.83 & PlateLocHeight <= 3.5, 
      1, 0),
    StrikeIndicator = ifelse(
      PitchCall %in% c("StrikeSwinging", "StrikeCalled", "FoulBallNotFieldable", "FoulBall", "InPlay"), 
      1, 0),
    WhiffIndicator = ifelse(PitchCall %in% "StrikeSwinging", 1, 0),
    SwingIndicator = ifelse(
      PitchCall %in% c("StrikeSwinging", "FoulBallNotFieldable", "FoulBall", "InPlay"), 
      1, 0),
    LHHindicator = ifelse(BatterSide %in% "Left", 1, 0),
    RHHindicator = ifelse(BatterSide %in% "Right", 1, 0),
    ABindicator = ifelse(
      PlayResult %in% c("Error", "FieldersChoice", "Out", "Single", "Double", "Triple", "HomeRun") | 
        KorBB %in% "Strikeout", 
      1, 0),
    HitIndicator = ifelse(
      PlayResult %in% c("Single", "Double", "Triple", "HomeRun"), 
      1, 0),
    FPindicator = ifelse(Balls == 0 & Strikes == 0, 1, 0),
    PAindicator = ifelse(
      PitchCall %in% c("InPlay", "HitByPitch") | 
        KorBB %in% c("Walk", "Strikeout"), 
      1, 0),
    LeadOffIndicator = ifelse(
      (PAofInning == 1 & (!(PlayResult %in% "Undefined") | !(KorBB %in% "Undefined") | 
                            PitchCall %in% "HitByPitch")), 
      1, 0),
    HBPIndicator = ifelse(PitchCall %in% "HitByPitch", 1, 0),
    WalkIndicator = ifelse(KorBB %in% "Walk", 1, 0),
    BIPind = ifelse(PitchCall %in% "InPlay", 1, 0),
    SolidContact = ifelse(
      (PitchCall %in% "InPlay" & 
         ((ExitSpeed > 95 & Angle >= 0 & Angle <= 40) | 
            (ExitSpeed > 92 & Angle >= 8 & Angle <= 40))), 1, 0),
    HHindicator = ifelse(PitchCall %in% "InPlay" & ExitSpeed > 95, 1, 0),
    biphh = ifelse(PitchCall %in% "InPlay" & ExitSpeed > 15, 1, 0),
    FBstrikeind = ifelse(
      (PitchCall %in% c("StrikeSwinging", "StrikeCalled", "Foul", "InPlay")) & 
        (FBindicator == 1), 
      1, 0),
    OSstrikeind = ifelse(
      (PitchCall %in% c("StrikeSwinging", "StrikeCalled", "Foul", "FoulBallNotFieldable", "InPlay")) & 
        (OSindicator == 1), 
      1, 0),
    EdgeIndicator = ifelse(
      (EdgeHeightIndicator == 1 & EdgeZoneWIndicator == 1) | 
        (EdgeWidthIndicator == 1 & EdgeZoneHtIndicator == 1), 
      1, 0),
    QualityPitchIndicator = ifelse(StrikeZoneIndicator == 1 | EdgeIndicator == 1, 1, 0),
    FPSindicator = ifelse(
      PitchCall %in% c("StrikeCalled", "StrikeSwinging", "Foul", "InPlay") &
        (FPindicator == 1), 
      1, 0),
    OutIndicator = ifelse(
      (PlayResult %in% c("Out", "FieldersChoice") | KorBB %in% "Strikeout") & HBPIndicator == 0, 
      1, 0),
    LOOindicator = ifelse(LeadOffIndicator == 1 & OutIndicator == 1, 1, 0),
    Zwhiffind = ifelse(WhiffIndicator == 1 & StrikeZoneIndicator == 1, 1, 0),
    Zswing = ifelse(StrikeZoneIndicator == 1 & SwingIndicator == 1, 1, 0),
    GBindicator = ifelse(TaggedHitType %in% "GroundBall", 1, 0),
    Chaseindicator = ifelse(SwingIndicator == 1 & StrikeZoneIndicator == 0, 1, 0),
    OutofZone = ifelse(StrikeZoneIndicator == 0, 1, 0),
    OnBaseindicator = ifelse(
      PlayResult %in% c("Single", "Double", "Triple", "HomeRun") | 
        KorBB %in% "Walk" | 
        PitchCall %in% "HitByPitch", 
      1, 0),
    totalbases = ifelse(PlayResult %in% "Single", 1, 
                        ifelse(PlayResult %in% "Double", 2, 
                               ifelse(PlayResult %in% "Triple", 3, 
                                      ifelse(PlayResult %in% "HomeRun", 4, 0))))
  )

# Debug: Check if data remains after filtering
if (nrow(df) == 0) {
  stop("No data remains after filtering for PitcherTeam and non-Undefined pitches")
}

# Summarize data
summary_data <- df %>%
  group_by(Pitcher) %>%
  summarize(
    IP = sum(OutIndicator, na.rm = TRUE) / 3,
    Pitches = n(),
    `FB Velo` = round(
      ifelse(
        sum(TaggedPitchType %in% c("Fastball", "Sinker") & !is.na(RelSpeed)) > 0,
        mean(RelSpeed[TaggedPitchType %in% c("Fastball", "Sinker")], na.rm = TRUE),
        NA_real_
      ), 1),
    `Max FB Velo` = round(
      ifelse(
        sum(TaggedPitchType %in% c("Fastball", "Sinker") & !is.na(RelSpeed)) > 0,
        max(RelSpeed[TaggedPitchType %in% c("Fastball", "Sinker")], na.rm = TRUE),
        NA_real_
      ), 1),
    `Strike %` = round(sum(PitchCall %in% c("StrikeCalled", "StrikeSwinging", "Foul", "InPlay")) / n(), 3) * 100,
    `FB Strike %` = round(sum(FBstrikeind) / sum(FBindicator), 3) * 100,
    `OS Strike %` = round(sum(OSstrikeind) / sum(OSindicator), 3) * 100,
    `E+A %` = round((sum(EarlyIndicator, na.rm = TRUE) + sum(AheadIndicator, na.rm = TRUE)) / sum(PAindicator, na.rm = TRUE), 3) * 100,
    `1PK %` = round(sum(FPSindicator) / sum(FPindicator), 3) * 100,
    `Whiff %` = round(sum(WhiffIndicator) / sum(SwingIndicator), 3) * 100,
    `IZ Whiff %` = round(sum(Zwhiffind, na.rm = TRUE) / sum(Zswing, na.rm = TRUE), 3) * 100,
    `Chase %` = round(sum(Chaseindicator, na.rm = TRUE) / sum(OutofZone, na.rm = TRUE), 3) * 100,
    `K %` = round(sum(KorBB %in% c("Strikeout")) / sum(PAindicator, na.rm = TRUE), 3) * 100,
    `BB %` = round(sum(WalkIndicator) / sum(PAindicator, na.rm = TRUE), 3) * 100,
    `HH %` = round(sum(HHindicator, na.rm = TRUE) / sum(biphh, na.rm = TRUE), 3) * 100,
    `GB %` = round(sum(GBindicator, na.rm = TRUE) / sum(BIPind, na.rm = TRUE), 3) * 100
  )

# Debug: Check if summary_data is empty
message("Number of rows in summary_data: ", nrow(summary_data))
if (nrow(summary_data) == 0) {
  stop("summary_data is empty after summarization")
}

# Prepare data for plotting
data_to_plot <- summary_data %>% 
  arrange(desc(`FB Velo`)) %>%
  select(Pitcher, IP, Pitches, `FB Velo`, `Max FB Velo`, `Strike %`, `FB Strike %`, `OS Strike %`, 
         `E+A %`, `1PK %`, `Whiff %`, `IZ Whiff %`, `Chase %`, `K %`, `BB %`, `HH %`, `GB %`)

# Calculate percentile ranks for numeric columns
percentile_rank <- function(x) {
  if (is.numeric(x)) {
    rank(x, ties.method = "average", na.last = "keep") / sum(!is.na(x))
  } else {
    rep(NA, length(x))  # Non-numeric columns (e.g., Pitcher) get NA
  }
}

percentile_data <- data_to_plot %>%
  mutate(across(.cols = where(is.numeric), .fns = percentile_rank))

# Define which columns use which color scheme
high_is_good <- c("IP", "Pitches", "FB Velo", "Max FB Velo", "Strike %", "FB Strike %", 
                  "OS Strike %", "E+A %", "1PK %", "Whiff %", "IZ Whiff %", "Chase %", "K %", "GB %")
low_is_good <- c("BB %", "HH %")

# Create a matrix of background colors based on percentile ranks
bg_colors <- matrix(na_color, nrow = nrow(data_to_plot), ncol = ncol(data_to_plot), 
                    dimnames = list(NULL, names(data_to_plot)))
for (col in names(data_to_plot)) {
  if (col == "Pitcher") next  # Skip non-numeric column
  ranks <- percentile_data[[col]]
  if (col %in% high_is_good) {
    bg_colors[, col] <- color_by_rank(ranks, length(rank_colors))  # Fixed: Use rank_colors
  } else if (col %in% low_is_good) {
    bg_colors[, col] <- color_by_rank2(ranks, length(rank_colors2))  # Fixed: Use rank_colors2
  }
}
bg_colors[is.na(bg_colors)] <- na_color  # Replace NA with na_color

# Define custom table theme
custom_theme <- ttheme_default(
  core = list(
    fg_params = list(fontsize = 7, col = "black"),  # Text color for table content
    bg_params = list(fill = bg_colors)  # Apply percentile-based background colors
  ),
  colhead = list(
    fg_params = list(fontsize = 8, col = "white", fontface = "bold"),
    bg_params = list(fill = "#CC0000")  # Header background color
  )
)
#C:/Users/isu_mvquirk_admin/Documents/GitHub/Kernelytics-Projects/KCL/pitcher_reports/carter_sellers.pdf
#C:/Users/maxim/Desktop/Kernelytics-Projects/CornBelters/Data/2025.csv
# Save table and glossary as PDF
pdf_path <- "C:/Users/isu_mvquirk_admin/Documents/GitHub/Kernelytics-Projects/KCL/pitcher_reports/mickels.pdf"
pdf(file = pdf_path, width = 11, height = 8.5)

# Create a new page for the table
grid.newpage()
grid.text("Pitching Leaders", x = 0.5, y = 0.95, gp = gpar(fontsize = 20, fontface = "bold", col = "black"))
grid.table(
  data_to_plot,
  theme = custom_theme,
  rows = NULL  # Suppress row indices
)

# Create a new page for the glossary
grid.newpage()
grid.text("Glossary of Terms", x = 0.5, y = 0.95, gp = gpar(fontsize = 14, fontface = "bold", col = "black"))
grid.text(
  paste(
    "Glossary of Terms:\n",
    "IP: Innings Pitched - The number of innings pitched, calculated as outs recorded divided by 3.\n",
    "Pitches: Total Pitches - The total number of pitches thrown by the pitcher.\n",
    "FB Velo: Fastball Velocity - The average velocity (in mph) of fastballs and sinkers thrown.\n",
    "Max FB Velo: Maximum Fastball Velocity - The highest velocity (in mph) of fastballs or sinkers thrown.\n",
    "Strike %: Strike Percentage - The percentage of pitches resulting in a strike (called, swinging, foul, or in-play).\n",
    "FB Strike %: Fastball Strike Percentage - The percentage of fastballs/sinkers resulting in a strike.\n",
    "OS Strike %: Off-Speed Strike Percentage - The percentage of off-speed pitches (slider, cutter, curveball, changeup) resulting in a strike.\n",
    "E+A %: Early and Ahead Percentage - The percentage of plate appearances with outcomes in early (0-0, 1-0, 0-1, 1-1) or ahead (0-1, 1-1) counts.\n",
    "1PK %: First-Pitch Strike Percentage - The percentage of first pitches (0-0 count) resulting in a strike.\n",
    "Whiff %: Whiff Percentage - The percentage of swings resulting in a miss (swinging strike).\n",
    "IZ Whiff %: In-Zone Whiff Percentage - The percentage of swings on pitches in the strike zone resulting in a miss.\n",
    "Chase %: Chase Percentage - The percentage of swings on pitches outside the strike zone.\n",
    "K %: Strikeout Percentage - The percentage of plate appearances resulting in a strikeout.\n",
    "BB %: Walk Percentage - The percentage of plate appearances resulting in a walk.\n",
    "HH %: Hard Hit Percentage - The percentage of batted balls with an exit velocity above 95 mph.\n",
    "GB %: Ground Ball Percentage - The percentage of batted balls classified as ground balls."
  ),
  x = 0.5,              # Center horizontally
  y = 0.9,              # Start near the top of the page
  just = c("center", "top"),  # Center-align and anchor at top
  gp = gpar(fontsize = 12, fontface = "plain", col = "black", lineheight = 0.7)  # Larger font, compact spacing
)

# Close the PDF device
dev.off()