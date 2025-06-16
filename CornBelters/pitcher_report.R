# Load required libraries
library(knitr)
library(kableExtra)
library(dplyr)
library(readxl)
library(scales)

# Define color palettes
higher_lower <- c("#780008", "#bd3e47", "#ffbabf", "#ffffff", "#E6FFE8", "#A2CEA6", "#00840D")
lower_higher <- c("#00840D", "#A2CEA6", "#E6FFE8", "#ffbabf", "#ebebeb", "#bd3e47", "#780008")

# Define thresholds data frame
thresholds <- data.frame(
  AvgFB = c(92, 91, 90, 89, 88),
  MaxVel = c(100, 99, 97, 96, 95),
  Strikeper = c(69, 66, 63, 62, 61),
  FBstr = c(66, 65, 64, 63, 62),
  OSstr = c(64, 62, 60, 59, 58),
  EA = c(73, 72, 69, 68, 66),
  FPS = c(63, 62, 59, 58, 57),
  whiff = c(30, 27, 25, 23, 22),
  IZwhiff = c(19, 18, 17, 15, 14),
  chase = c(29, 28, 26, 25, 24),
  K = c(26, 24, 22, 20, 19),
  BB = c(8, 9, 10, 11, 12),
  BA = c(0.236, 0.248, 0.268, 0.276, 0.285),
  OPS = c(0.706, 0.737, 0.789, 0.822, 0.850),
  HH = c(45, 44, 39, 38, 37),
  GB = c(45, 44, 41, 40, 39)
)

# Read and process data
df <- read.csv("C:/Users/isu_mvquirk_admin/Documents/GitHub/Kernelytics-Projects/CornBelters/Data/2025.csv") %>%
  mutate(
    RelSpeed = as.numeric(as.character(RelSpeed))  # Ensure RelSpeed is numeric
  )

# Filter data
df <- df %>%
  filter(
    PitcherTeam == 'Normal cornbelters' | PitcherTeam == 'Normal Cornbelters'
  )

df <- df %>%
  mutate(
    PitcherTeam == 'Normal cornbelters'
  )

df <- df %>%
  filter(
    !is.na(RelSpeed)
  )

df <- df %>%
  filter(
    TaggedPitchType != 'Undefined'
  )

df <- df %>%
  filter(
    PitchCall != 'Undefined'
  ) %>%
  mutate(
    FBindicator = ifelse(TaggedPitchType == 'Fastball' | TaggedPitchType == 'Sinker', 1, 0),
    OSindicator = ifelse(TaggedPitchType %in% c("Slider", "Cutter", "Curveball", "ChangeUp"), 
                         1, 0),
    EarlyIndicator = ifelse(
      ((Balls == 0 & Strikes == 0 & PitchCall == "InPlay") |
         (Balls == 1 & Strikes == 0 & PitchCall == "InPlay") |
         (Balls == 0 & Strikes == 1 & PitchCall == "InPlay") |
         (Balls == 1 & Strikes == 1 & PitchCall == "InPlay")), 
      1, 0),
    AheadIndicator = ifelse(
      ((Balls == 0 & Strikes == 1) & (PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable", "FoulBall"))) |
        ((Balls == 1 & Strikes == 1) & (PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable", "FoulBall"))), 
      1, 0),
    StrikeZoneIndicator = ifelse(
      PlateLocSide >= -0.8333 & PlateLocSide <= 0.8333 & 
        PlateLocHeight >= 1.5 & PlateLocHeight <= 3.37467, 
      1, 0),
    EdgeHeightIndicator = ifelse(
      ((PlateLocHeight > 14/12 & PlateLocHeight < 22/12) |
         (PlateLocHeight > 38/12 & PlateLocHeight < 46/12)), 
      1, 0),
    EdgeZoneHtIndicator = ifelse(
      PlateLocHeight > 16/12 & PlateLocHeight < 45.2/12, 
      1, 0),
    EdgeZoneWIndicator = ifelse(
      PlateLocSide > -13.4/12 & PlateLocSide < 13.4/12, 
      1, 0),
    EdgeWidthIndicator = ifelse(
      ((PlateLocSide > -13.3/12 & PlateLocSide < -6.7/12) |
         (PlateLocSide < 13.3/12 & PlateLocSide > 6.7/12)), 
      1, 0),
    HeartIndicator = ifelse(
      PlateLocSide >= -0.5583 & PlateLocSide <= 0.5583 & 
        PlateLocHeight >= 1.83 & PlateLocHeight <= 3.5, 
      1, 0),
    StrikeIndicator = ifelse(
      PitchCall %in% c("StrikeSwinging", "StrikeCalled", "FoulBall", "FoulBall", "InPlay"), 
      1, 0),
    WhiffIndicator = ifelse(
      PitchCall == 'StrikeSwinging', 1, 0
    ),
    SwingIndicator = ifelse(
      PitchCall %in% c("StrikeSwinging", "FoulBallNotFieldable", "FoulBall", "InPlay"), 
      1, 0),
    LHHindicator = ifelse(
      BatterSide == 'Left', 1, 0
    ),
    RHHindicator = ifelse(
      BatterSide == 'Right', 1, 0
    ),
    ABindicator = ifelse(
      PlayResult %in% c("Error", "FieldersChoice", "Out", "Single", "Double", "Triple", "HomeRun") | 
        KorBB == "Strikeout", 
      1, 0),
    HitIndicator = ifelse(
      PlayResult %in% c("Single", "Double", "Triple", "HomeRun"), 
      1, 0),
    FPindicator = ifelse(Balls == 0 & Strikes == 0, 1, 0),
    PAindicator = ifelse(
      PitchCall %in% c("InPlay", "HitByPitch", "CatchersInterference") | 
        KorBB %in% c("Walk", "Strikeout"), 
      1, 0),
    LeadOffIndicator = ifelse(
      (PAofInning == 1 & (PlayResult != "Undefined" | KorBB != "Undefined")) | 
        PitchCall == "HitByPitch", 
      1, 0),
    HBPIndicator = ifelse(
      PitchCall == 'HitByPitch', 1, 0),
    WalkIndicator = ifelse(
      KorBB == 'Walk', 1, 0
    ),
    BIPind = ifelse(
      PitchCall == 'InPlay', 1, 0
    ),
    SolidContact = ifelse(
      (PitchCall == "InPlay" & 
         ((ExitSpeed > 95 & Angle >= 0 & Angle <= 40) | 
            (ExitSpeed > 92 & Angle >= 8 & Angle <= 40))), 1, 0),
    HHindicator = ifelse(PitchCall == 'InPlay' & ExitSpeed > 95, 1, 0),
    biphh = ifelse(PitchCall == 'InPlay' & ExitSpeed > 15, 1, 0)
  )

# Additional mutations
df <- df %>%
  mutate(
    FBstrikeind = ifelse(
      (PitchCall %in% c("StrikeSwinging", "StrikeCalled", "FoulBall", "FoulBallNotFieldable", "InPlay")) & 
        (FBindicator == 1), 
      1, 0),
    OSstrikeind = ifelse(
      (PitchCall %in% c("StrikeSwinging", "StrikeCalled", "FoulBall", "FoulBallNotFieldable", "InPlay")) & 
        (OSindicator == 1), 
      1, 0),
    EdgeIndicator = ifelse(
      (EdgeHeightIndicator == 1 & EdgeZoneWIndicator == 1) | 
        (EdgeWidthIndicator == 1 & EdgeZoneHtIndicator == 1), 
      1, 0),
    QualityPitchIndicator = ifelse(
      StrikeZoneIndicator == 1 | EdgeIndicator == 1, 
      1, 0),
    FPSindicator = ifelse(
      PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable", "FoulBall", "InPlay") &
        (FPindicator == 1),
      1, 0),
    OutIndicator = ifelse(
      (PlayResult %in% c("Out", "FieldersChoice") | KorBB == "Strikeout") & HBPIndicator == 0, 
      1, 0),
    LOOindicator = ifelse(
      LeadOffIndicator == 1 & OutIndicator == 1, 
      1, 0),
    Zwhiffind = ifelse(
      WhiffIndicator == 1 & StrikeZoneIndicator == 1, 1, 0),
    Zswing = ifelse(
      StrikeZoneIndicator == 1 & SwingIndicator == 1, 1, 0
    ),
    GBindicator = ifelse(HitType == 'GroundBall', 1, 0),
    Chaseindicator = ifelse(SwingIndicator == 1 & StrikeZoneIndicator == 0, 1, 0),
    OutofZone = ifelse(StrikeZoneIndicator == 0, 1, 0),
    OnBaseindicator = ifelse(
      PlayResult %in% c("Single", "Double", "Triple", "HomeRun") | 
        KorBB == "Walk" | 
        PitchCall == "HitByPitch", 
      1, 0),
    totalbases = ifelse(PlayResult == "Single", 1, 
                        ifelse(PlayResult == "Double", 2, 
                               ifelse(PlayResult == "Triple", 3, 
                                      ifelse(PlayResult == "HomeRun", 4, 0))))
  )

# Summarize data
summary_data <- df %>%
  group_by(Pitcher) %>%
  summarize(
    `Avg FB Velo` = round(
      ifelse(
        sum(TaggedPitchType %in% c("Fastball", "Sinker") & !is.na(RelSpeed)) > 0,
        mean(RelSpeed[TaggedPitchType %in% c("Fastball", "Sinker")], na.rm = TRUE),
        NA_real_
      ), 1
    ),
    `Max FB Velo` = round(
      ifelse(
        sum(TaggedPitchType %in% c("Fastball", "Sinker") & !is.na(RelSpeed)) > 0,
        max(RelSpeed[TaggedPitchType %in% c("Fastball", "Sinker")], na.rm = TRUE),
        NA_real_
      ), 1
    ),
    `Strike %` = round(sum(PitchCall %in% c("StrikeCalled", "StrikeSwinging", "Foul", "InPlay")) / n(), 3) * 100,
    `FB Strike %` = round(sum(FBstrikeind) / sum(FBindicator), 3) * 100,
    `OS Strike %` = round(sum(OSstrikeind) / sum(OSindicator), 3) * 100,
    `E+A %` = round((sum(EarlyIndicator, na.rm = TRUE) + sum(AheadIndicator, na.rm = TRUE)) / sum(PAindicator, na.rm = TRUE), 3) * 100,
    `1PK %` = round(sum(FPSindicator) / sum(FPindicator), 3) * 100,
    `Whiff %` = round(sum(WhiffIndicator) / sum(SwingIndicator), 3) * 100,
    `IZ Whiff %` = round(sum(Zwhiffind, na.rm = TRUE) / sum(Zswing, na.rm = TRUE), 3) * 100,
    `Chase %` = round(sum(Chaseindicator, na.rm = TRUE) / sum(OutofZone, na.rm = TRUE), 3) * 100,
    `K %` = round(sum(KorBB == "Strikeout") / sum(PAindicator), 3) * 100,
    `BB %` = round(sum(WalkIndicator) / sum(PAindicator), 3) * 100,
    AVG = round(sum(HitIndicator) / sum(ABindicator), 3),
    OPS = round((sum(OnBaseindicator) / sum(PAindicator)) + (sum(totalbases) / sum(ABindicator)), 3),
    `HH %` = round(sum(HHindicator, na.rm = TRUE) / sum(biphh, na.rm = TRUE), 3) * 100,
    `GB %` = round(sum(GBindicator, na.rm = TRUE) / sum(BIPind, na.rm = TRUE), 3) * 100
  )

# Define color palettes for visualization
red_white_green <- colorRampPalette(c("#E1463E", "white", "#00840D"))
green_white_red <- colorRampPalette(c("#00840d", "white", "#E1463E"))

velocity_palette <- col_numeric(palette = red_white_green(98), domain = c(82, 90, 98))
maxvelopal <- col_numeric(palette = red_white_green(102), domain = c(85, 93, 102))
strikeperpal <- col_numeric(palette = red_white_green(75), domain = c(40, 61, 75))
fbstrpal <- col_numeric(palette = red_white_green(76), domain = c(46, 63, 76))
osstrpal <- col_numeric(palette = red_white_green(74), domain = c(40, 60, 74))
epapal <- col_numeric(palette = red_white_green(90), domain = c(40, 69, 90))
fpspal <- col_numeric(palette = red_white_green(82), domain = c(25, 58, 82))
whiffpal <- col_numeric(palette = red_white_green(50), domain = c(2, 24, 50))
zonewhiff <- col_numeric(palette = red_white_green(40), domain = c(0, 16, 40))
kpal <- col_numeric(palette = red_white_green(45), domain = c(0, 21, 45))
bbpal <- col_numeric(palette = green_white_red(35), domain = c(0, 11, 35))
avgpal <- col_numeric(palette = green_white_red(450), domain = c(.1, .264, .450))
opspal <- col_numeric(palette = green_white_red(1368), domain = c(.389, .836, 1.368))
hhpal <- col_numeric(palette = green_white_red(54), domain = c(8, 33, 54))
gbpal <- col_numeric(palette = red_white_green(100), domain = c(0, 50, 100))
chasepal <- col_numeric(palette = red_white_green(31), domain = c(20, 26, 31))

# Add color columns to summary data
summary_data2 <- summary_data %>%
  arrange(desc(`Avg FB Velo`)) %>%
  mutate(
    Velocity_Color = case_when(
      `Avg FB Velo` < 82 ~ "#e1463e",
      `Avg FB Velo` > 98 ~ "#00840d",
      TRUE ~ velocity_palette(`Avg FB Velo`)
    ),
    MaxColor = case_when(
      `Max FB Velo` < 85 ~ "#e1463e",
      `Max FB Velo` > 102 ~ "#00840d",
      TRUE ~ maxvelopal(`Max FB Velo`)
    ),
    StrikePerColor = case_when(
      `Strike %` < 40 ~ "#e1463e",
      `Strike %` > 75 ~ "#00840d",
      TRUE ~ strikeperpal(`Strike %`)),
    FBStrkColor = case_when(
      `FB Strike %` < 46 ~ "#e1463e",
      `FB Strike %` > 76 ~ "#00840d",
      TRUE ~ fbstrpal(`FB Strike %`)),
    OSStrikeCol = case_when(
      `OS Strike %` < 40 ~ "#e1463e",
      `OS Strike %` > 74 ~ "#00840d",
      TRUE ~ osstrpal(`OS Strike %`)),
    EPAcol = case_when(
      `E+A %` < 40 ~ '#e1463e',
      `E+A %` > 90 ~ '#00840d',
      TRUE ~ epapal(`E+A %`)),
    FPScolor = case_when(
      `1PK %` < 25 ~ "#e1463e",
      `1PK %` > 82 ~ "#00840d",
      TRUE ~ fpspal(`1PK %`)),
    Whiffcolor = case_when(
      `Whiff %` < 2 ~ '#e1463e',
      `Whiff %` > 50 ~ '#00840d',
      TRUE ~ whiffpal(`Whiff %`)),
    zwhiffcolor = case_when(
      `IZ Whiff %` < 0 ~ '#e1463e',
      `IZ Whiff %` > 40 ~ '#00840d',
      TRUE ~ zonewhiff(`IZ Whiff %`)),
    Kcolor = case_when(
      `K %` < 0 ~ '#e1463e',
      `K %` > 45 ~ '#00840d',
      TRUE ~ kpal(`K %`)),
    BBcolor = case_when(
      `BB %` < 0 ~ "#00840d",
      `BB %` > 35 ~ "#e1463e",
      TRUE ~ bbpal(`BB %`)),
    AVGcolor = case_when(
      AVG < .1 ~ '#00840d',
      AVG > .45 ~ '#e1463e',
      TRUE ~ avgpal(AVG)
    ),
    OPScol = case_when(
      OPS < .389 ~ '#00840d',
      OPS > 1.368 ~ '#e1463e',
      TRUE ~ opspal(OPS)
    ),
    HHcol = case_when(
      `HH %` < 8 ~ '#00840d',
      `HH %` > 54 ~ '#e1463e',
      TRUE ~ hhpal(`HH %`)
    ),
    GBcol = gbpal(`GB %`),
    chasecol = case_when(
      `Chase %` < 20 ~ '#e1463e',
      `Chase %` > 31 ~ '#00840d',
      TRUE ~ chasepal(`Chase %`)
    )
  )

# Define NCAA average data
average_row <- tibble(
  Pitcher = "NCAA Average",
  AvgFBVelo = 88.9,
  MaxFBVelo = 102.5,
  StrikePct = 60.7,
  FBStrikePct = 62.4,
  OSStrikePct = 58.6,
  FPSPct = 57.6,
  WhiffPct = 23.8,
  BBPct = 11.1,
  KPct = 19.7,
  AVG = 0.278,
  OPS = 0.826
)