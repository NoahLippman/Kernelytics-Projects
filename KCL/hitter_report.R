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

#"C:/Users/isu_mvquirk_admin/Documents/GitHub/Kernelytics-Projects/KCL/Data/2025.csv"
#"C:/Users/maxim/Desktop/Kernelytics-Projects/CornBelters/Data/2025.csv"
# Load data
data_path <- "GitHub/Kernelytics-Projects/KCL/Data/2025.csv"
if (!file.exists(data_path)) {
  stop("Data file not found: ", data_path)
}
df <- read_csv(data_path)

# Assign team names (corrected to match filter)
df$BatterTeam[df$Batter %in% c("Will Vogel", "Kam Ross", "Bennett Summers", 
                               "Brayden Windy", "Sammy Descarpentrie", 
                               "Michael Lucas")] <- "Kcl groundsloths 2025"

# Debug: Check column names and unique BatterTeam values
message("Column names in dataset: ", paste(names(df), collapse = ", "))
message("Unique BatterTeam values: ", paste(unique(df$BatterTeam), collapse = ", "))

# Filter data
df <- df %>%
  filter(TaggedPitchType != "Undefined",
         BatterTeam == "Kcl bobcats 2025")
#BatterTeam == "Kcl merchants 2025",
# Debug: Check if data remains after filtering
if (nrow(df) == 0) {
  stop("No data remains after filtering for BatterTeam == 'Kcl bobcats 2025' and TaggedPitchType != 'Undefined'")
}

# Mutate data with indicators
df <- df %>%
  mutate(
    WhiffIndicator = ifelse(PitchCall %in% "StrikeSwinging", 1, 0),
    StrikeZoneIndicator = ifelse(PlateLocSide >= -1 & PlateLocSide <= 1 & 
                                   PlateLocHeight >= 1.5 & PlateLocHeight <= 3.5, 1, 0),
    SwingIndicator = ifelse(PitchCall %in% c("StrikeSwinging",  "FoulBall", "InPlay"), 1, 0),
    BIPind = ifelse(PitchCall %in% "InPlay" & !(HitType %in% "Bunt"), 1, 0),
    Zwhiffind = ifelse(WhiffIndicator == 1 & StrikeZoneIndicator == 1, 1, 0),
    Zswing = ifelse(StrikeZoneIndicator == 1 & SwingIndicator == 1, 1, 0),
    LA1030ind = ifelse(PitchCall %in% "InPlay" & Angle >= 10 & Angle <= 30, 1, 0),
    Barrelind = ifelse(PitchCall %in% "InPlay" & ExitSpeed >= 95 & Angle >= 10 & Angle <= 32, 1, 0),
    HHind = ifelse(PitchCall %in% "InPlay" & ExitSpeed >= 95, 1, 0),
    SCind = ifelse(PitchCall %in% "InPlay" & 
                     ((ExitSpeed > 95 & Angle >= 0 & Angle <= 35) | 
                        (ExitSpeed > 92 & Angle >= 8 & Angle <= 35)), 1, 0),
    GBindicator = ifelse(HitType %in% "GroundBall", 1, 0),
    LDind = ifelse(HitType %in% "LineDrive", 1, 0),
    FBind = ifelse(HitType %in% "FlyBall", 1, 0),
    Popind = ifelse(HitType %in% "Popup", 1, 0),
    Chaseindicator = ifelse(SwingIndicator == 1 & StrikeZoneIndicator == 0, 1, 0),
    OutofZone = ifelse(StrikeZoneIndicator == 0, 1, 0),
    PAindicator = ifelse(PitchCall %in% c("InPlay", "HitByPitch", "CatchersInterference") | 
                           KorBB %in% c("Walk", "Strikeout"), 1, 0),
    OutIndicator = ifelse(PlayResult %in% c("Out", "FieldersChoice") | KorBB %in% "Strikeout", 1, 0),
    EV100ind = ifelse(PitchCall %in% "InPlay" & ExitSpeed >= 100 & !(HitType %in% "Bunt"), 1, 0),
    WalkIndicator = ifelse(KorBB %in% "Walk", 1, 0),
    ZStrikeTakeInd = ifelse(StrikeZoneIndicator == 1 & PitchCall %in% "StrikeCalled", 1, 0)
  )

# Summarize data for individual batters
summary_data <- df %>%
  group_by(Batter) %>%
  summarize(
    PA = sum(PAindicator, na.rm = TRUE),
    `BB%` = round(ifelse(sum(PAindicator) > 0, sum(WalkIndicator, na.rm = TRUE) / sum(PAindicator), NA_real_), 3) * 100,
    `SO%` = round(ifelse(sum(PAindicator) > 0, sum(KorBB %in% "Strikeout", na.rm = TRUE) / sum(PAindicator), NA_real_), 3) * 100,
    `IZ Strike Take%` = round(ifelse(sum(StrikeZoneIndicator) > 0, sum(ZStrikeTakeInd, na.rm = TRUE) / sum(StrikeZoneIndicator), NA_real_), 3) * 100,
    `90th EV` = round(quantile(ExitSpeed[PitchCall %in% "InPlay" & !(HitType %in% "Bunt")], probs = 0.9, na.rm = TRUE), 1),
    `100+ EV` = sum(EV100ind, na.rm = TRUE),
    `Avg EV` = round(mean(ExitSpeed[PitchCall %in% "InPlay" & !(HitType %in% "Bunt")], na.rm = TRUE), 1),
    `Max EV` = round(max(ExitSpeed, na.rm = TRUE), 1),
    `SC%` = round(ifelse(sum(BIPind) > 0, sum(SCind, na.rm = TRUE) / sum(BIPind, na.rm = TRUE), NA_real_), 3) * 100,
    `10-30%` = round(ifelse(sum(BIPind) > 0, sum(LA1030ind, na.rm = TRUE) / sum(BIPind, na.rm = TRUE), NA_real_), 3) * 100,
    `HH%` = round(ifelse(sum(BIPind) > 0, sum(HHind, na.rm = TRUE) / sum(BIPind, na.rm = TRUE), NA_real_), 3) * 100,
    `Barrel%` = round(ifelse(sum(BIPind) > 0, sum(Barrelind, na.rm = TRUE) / sum(BIPind, na.rm = TRUE), NA_real_), 3) * 100,
    `Whiff%` = round(ifelse(sum(SwingIndicator) > 0, sum(WhiffIndicator, na.rm = TRUE) / sum(SwingIndicator, na.rm = TRUE), NA_real_), 3) * 100,
    `Z Whiff%` = round(ifelse(sum(Zswing) > 0, sum(Zwhiffind, na.rm = TRUE) / sum(Zswing, na.rm = TRUE), NA_real_), 3) * 100,
    `Chase%` = round(ifelse(sum(SwingIndicator) > 0, sum(Chaseindicator, na.rm = TRUE) / sum(SwingIndicator, na.rm = TRUE), NA_real_), 3) * 100,
    `GB%` = round(ifelse(sum(BIPind) > 0, sum(GBindicator, na.rm = TRUE) / sum(BIPind, na.rm = TRUE), NA_real_), 3) * 100,
    `LD%` = round(ifelse(sum(BIPind) > 0, sum(LDind, na.rm = TRUE) / sum(BIPind, na.rm = TRUE), NA_real_), 3) * 100,
    `FB%` = round(ifelse(sum(BIPind) > 0, sum(FBind, na.rm = TRUE) / sum(BIPind, na.rm = TRUE), NA_real_), 3) * 100,
    `Pop%` = round(ifelse(sum(BIPind) > 0, sum(Popind, na.rm = TRUE) / sum(BIPind, na.rm = TRUE), NA_real_), 3) * 100
  )

# Debug: Check if summary_data is empty
message("Number of rows in summary_data: ", nrow(summary_data))
if (nrow(summary_data) == 0) {
  stop("summary_data is empty after summarization")
}

# Prepare data for plotting (remove redundant ranking)
data_to_plot <- summary_data %>% 
  arrange(desc(`Avg EV`)) %>%
  select(Batter, PA, `BB%`, `SO%`, `IZ Strike Take%`, `90th EV`, `100+ EV`, `Avg EV`, `Max EV`, 
         `SC%`, `10-30%`, `HH%`, `Barrel%`, `Whiff%`, `Z Whiff%`, `Chase%`, `GB%`, `LD%`, `FB%`, `Pop%`)

# Calculate percentile ranks for numeric columns
percentile_rank <- function(x) {
  if (is.numeric(x)) {
    rank(x, ties.method = "average", na.last = "keep") / sum(!is.na(x))
  } else {
    rep(NA, length(x))  # Non-numeric columns (e.g., Batter) get NA
  }
}

percentile_data <- data_to_plot %>%
  mutate(across(.cols = where(is.numeric), .fns = percentile_rank))

# Define which columns use which color scheme
high_is_good <- c("PA", "BB%", "90th EV", "100+ EV", "Avg EV", "Max EV", "HH%", "Barrel%", "LD%", "10-30%", "SC%")
low_is_good <- c("SO%", "IZ Strike Take%", "Whiff%", "Z Whiff%", "Chase%", "GB%", "FB%", "Pop%")

# Create a matrix of background colors based on percentile ranks
bg_colors <- matrix(na_color, nrow = nrow(data_to_plot), ncol = ncol(data_to_plot), 
                    dimnames = list(NULL, names(data_to_plot)))  # Assign column names
for (col in names(data_to_plot)) {
  if (col == "Batter") next  # Skip non-numeric column
  ranks <- percentile_data[[col]]
  if (col %in% high_is_good) {
    bg_colors[, col] <- color_by_rank(ranks, length(rank_colors))
  } else if (col %in% low_is_good) {
    bg_colors[, col] <- color_by_rank2(ranks, length(rank_colors2))
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

#"C:/Users/maxim/Desktop/Kernelytics-Projects/CornBelters/reports/hitter_report.pdf"
#".pdf"
# Define PDF output path
# Define PDF output path
pdf_path <- "GitHub/Kernelytics-Projects/KCL/hitter_reports/driscoll.pdf"

# Open PDF device with landscape dimensions
pdf(file = pdf_path, width = 11, height = 8.5)

# Create a new page
grid.newpage()

# Add title with bold formatting and color
grid.text("Advanced Offensive Statistics", x = 0.5, y = 0.95, 
          gp = gpar(fontsize = 20, fontface = "bold", col = "#000000"))

# Create and render the table without row indices
grid.table(
  data_to_plot,
  theme = custom_theme,
  rows = NULL  # Suppress row indices
)

# Add glossary text below the table
grid.newpage()
grid.text("Glossary of Terms", x = 0.5, y = 0.95, gp = gpar(fontsize = 14, fontface = "bold", col = "black"))

grid.text(
  paste(
    "PA: Plate Appearance - A batter's turn at bat, resulting in outcomes like hits, walks, strikeouts, or other results.\n",
    "BB%: Walk Percentage - The percentage of plate appearances resulting in a walk.\n",
    "SO%: Strikeout Percentage - The percentage of plate appearances resulting in a strikeout.\n",
    "IZ Strike Take%: In-Zone Strike Take Percentage - The percentage of pitches in the strike zone taken for a called strike.\n",
    "90th EV: 90th Percentile Exit Velocity - The exit velocity at the 90th percentile of a batter's batted balls, excluding bunts.\n",
    "100+ EV: 100+ MPH Exit Velocity Count - The number of batted balls with an exit velocity of 100 mph or higher, excluding bunts.\n",
    "Avg EV: Average Exit Velocity - The mean exit velocity of batted balls, excluding bunts.\n",
    "Max EV: Maximum Exit Velocity - The highest exit velocity recorded for a batted ball.\n",
    "SC%: Solid Contact Percentage - The percentage of batted balls > 92 with a launch angle above 8\n",
    "10-30%: 10-30 Degree Launch Angle Percentage - The percentage of batted balls with a launch angle between 10 and 30 degrees.\n",
    "HH%: Hard Hit Percentage - The percentage of batted balls with an exit velocity of 95 mph or higher.\n",
    "Barrel%: Barrel Percentage - The percentage of batted balls with an exit velocity of 95 mph or higher and a launch angle between 10 and 32 degrees.\n",
    "Whiff%: Whiff Percentage - The percentage of swings resulting in a miss (swinging strike).\n",
    "Z Whiff%: In-Zone Whiff Percentage - The percentage of swings on pitches in the strike zone resulting in a miss.\n",
    "Chase%: Chase Percentage - The percentage of swings on pitches outside the strike zone.\n",
    "GB%: Ground Ball Percentage - The percentage of batted balls classified as ground balls.\n",
    "LD%: Line Drive Percentage - The percentage of batted balls classified as line drives.\n",
    "FB%: Fly Ball Percentage - The percentage of batted balls classified as fly balls.\n",
    "Pop%: Popup Percentage - The percentage of batted balls classified as popups."
  ),
  x = 0.5,              # Center horizontally
  y = 0.9,              # Start near the top of the page
  just = c("center", "top"),  # Center-align and anchor at top
  gp = gpar(fontsize = 12, fontface = "plain", col = "black", lineheight = 0.7)  # Larger font, compact spacing
)





# Close the PDF device
dev.off()