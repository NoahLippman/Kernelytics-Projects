import pandas as pd
import numpy as np
from tabulate import tabulate
import os
from matplotlib.colors import LinearSegmentedColormap
from matplotlib.cm import ScalarMappable
import matplotlib.pyplot as plt
from reportlab.lib.pagesizes import letter
from reportlab.platypus import SimpleDocTemplate, Table, TableStyle, Paragraph
from reportlab.lib import colors
from reportlab.lib.styles import getSampleStyleSheet
# Define color palettes
higher_lower = ["#780008", "#bd3e47", "#ffbabf", "#ffffff", "#E6FFE8", "#A2CEA6", "#00840D"]
lower_higher = ["#00840D", "#A2CEA6", "#E6FFE8", "#ffbabf", "#ebebeb", "#bd3e47", "#780008"]

# Define thresholds
thresholds = pd.DataFrame({
    'AvgFB': [92, 91, 90, 89, 88],
    'MaxVel': [100, 99, 97, 96, 95],
    'Strikeper': [69, 66, 63, 62, 61],
    'FBstr': [66, 65, 64, 63, 62],
    'OSstr': [64, 62, 60, 59, 58],
    'EA': [73, 72, 69, 68, 66],
    'FPS': [63, 62, 59, 58, 57],
    'whiff': [30, 27, 25, 23, 22],
    'IZwhiff': [19, 18, 17, 15, 14],
    'chase': [29, 28, 26, 25, 24],
    'K': [26, 24, 22, 20, 19],
    'BB': [8, 9, 10, 11, 12],
    'BA': [0.236, 0.248, 0.268, 0.276, 0.285],
    'OPS': [0.706, 0.737, 0.789, 0.822, 0.850],
    'HH': [45, 44, 39, 38, 37],
    'GB': [45, 44, 41, 40, 39]
})

# Load data with error handling
data_file = "C:/Users/isu_mvquirk_admin/Documents/GitHub/Kernelytics-Projects/CornBelters/Data/2025.csv"
if not os.path.exists(data_file):
    raise FileNotFoundError(f"Data file not found: {data_file}")

df = pd.read_csv(data_file)

# Convert RelSpeed to numeric, handling non-numeric values
df['RelSpeed'] = pd.to_numeric(df['RelSpeed'], errors='coerce')

# Filter data
df = df[df['PitcherTeam'].isin(['Normal cornbelters', 'Normal Cornbelters'])]
df['PitcherTeam'] = df['PitcherTeam'].replace(['CCU_PRA', 'COA_CHA'], 'CCU')
df = df[df['RelSpeed'].notna()]
df = df[df['TaggedPitchType'] != 'Undefined']
df = df[df['PitchCall'] != 'Undefined']

# Add indicators
df = df.assign(
    FBindicator=lambda x: np.where(x['TaggedPitchType'].isin(['Fastball', 'Sinker']), 1, 0),
    OSindicator=lambda x: np.where(x['TaggedPitchType'].isin(['Slider', 'Cutter', 'Curveball', 'ChangeUp']), 1, 0),
    EarlyIndicator=lambda x: np.where(
        ((x['Balls'] == 0) & (x['Strikes'] == 0) & (x['PitchCall'] == 'InPlay')) |
        ((x['Balls'] == 1) & (x['Strikes'] == 0) & (x['PitchCall'] == 'InPlay')) |
        ((x['Balls'] == 0) & (x['Strikes'] == 1) & (x['PitchCall'] == 'InPlay')) |
        ((x['Balls'] == 1) & (x['Strikes'] == 1) & (x['PitchCall'] == 'InPlay')), 1, 0),
    AheadIndicator=lambda x: np.where(
        (((x['Balls'] == 0) & (x['Strikes'] == 1)) | ((x['Balls'] == 1) & (x['Strikes'] == 1))) &
        (x['PitchCall'].isin(['StrikeCalled', 'StrikeSwinging', 'FoulBallNotFieldable', 'FoulBall'])), 1, 0),
    StrikeZoneIndicator=lambda x: np.where(
        (x['PlateLocSide'] >= -0.8333) & (x['PlateLocSide'] <= 0.8333) &
        (x['PlateLocHeight'] >= 1.5) & (x['PlateLocHeight'] <= 3.37467), 1, 0),
    EdgeHeightIndicator=lambda x: np.where(
        ((x['PlateLocHeight'] > 14/12) & (x['PlateLocHeight'] < 22/12)) |
        ((x['PlateLocHeight'] > 38/12) & (x['PlateLocHeight'] < 46/12)), 1, 0),
    EdgeZoneHtIndicator=lambda x: np.where(
        (x['PlateLocHeight'] > 16/12) & (x['PlateLocHeight'] < 45.2/12), 1, 0),
    EdgeZoneWIndicator=lambda x: np.where(
        (x['PlateLocSide'] > -13.4/12) & (x['PlateLocSide'] < 13.4/12), 1, 0),
    EdgeWidthIndicator=lambda x: np.where(
        ((x['PlateLocSide'] > -13.3/12) & (x['PlateLocSide'] < -6.7/12)) |
        ((x['PlateLocSide'] < 13.3/12) & (x['PlateLocSide'] > 6.7/12)), 1, 0),
    HeartIndicator=lambda x: np.where(
        (x['PlateLocSide'] >= -0.5583) & (x['PlateLocSide'] <= 0.5583) &
        (x['PlateLocHeight'] >= 1.83) & (x['PlateLocHeight'] <= 3.5), 1, 0),
    StrikeIndicator=lambda x: np.where(
        x['PitchCall'].isin(['StrikeSwinging', 'StrikeCalled', 'FoulBallNotFieldable', 'FoulBall', 'InPlay']), 1, 0),
    WhiffIndicator=lambda x: np.where(x['PitchCall'] == 'StrikeSwinging', 1, 0),
    SwingIndicator=lambda x: np.where(
        x['PitchCall'].isin(['StrikeSwinging', 'FoulBallNotFieldable', 'FoulBall', 'InPlay']), 1, 0),
    LHHindicator=lambda x: np.where(x['BatterSide'] == 'Left', 1, 0),
    RHHindicator=lambda x: np.where(x['BatterSide'] == 'Right', 1, 0),
    ABindicator=lambda x: np.where(
        x['PlayResult'].isin(['Error', 'FieldersChoice', 'Out', 'Single', 'Double', 'Triple', 'HomeRun']) |
        (x['KorBB'] == 'Strikeout'), 1, 0),
    HitIndicator=lambda x: np.where(
        x['PlayResult'].isin(['Single', 'Double', 'Triple', 'HomeRun']), 1, 0),
    FPindicator=lambda x: np.where((x['Balls'] == 0) & (x['Strikes'] == 0), 1, 0),
    PAindicator=lambda x: np.where(
        x['PitchCall'].isin(['InPlay', 'HitByPitch', 'CatchersInterference']) |
        x['KorBB'].isin(['Walk', 'Strikeout']), 1, 0),
    LeadOffIndicator=lambda x: np.where(
        (x['PAofInning'] == 1) & ((x['PlayResult'] != 'Undefined') | (x['KorBB'] != 'Undefined')) |
        (x['PitchCall'] == 'HitByPitch'), 1, 0),
    HBPIndicator=lambda x: np.where(x['PitchCall'] == 'HitByPitch', 1, 0),
    WalkIndicator=lambda x: np.where(x['KorBB'] == 'Walk', 1, 0),
    BIPind=lambda x: np.where(x['PitchCall'] == 'InPlay', 1, 0),
    SolidContact=lambda x: np.where(
        (x['PitchCall'] == 'InPlay') & (
            ((x['ExitSpeed'] > 95) & (x['Angle'] >= 0) & (x['Angle'] <= 40)) |
            ((x['ExitSpeed'] > 92) & (x['Angle'] >= 8) & (x['Angle'] <= 40))), 1, 0),
    HHindicator=lambda x: np.where((x['PitchCall'] == 'InPlay') & (x['ExitSpeed'] > 95), 1, 0),
    biphh=lambda x: np.where((x['PitchCall'] == 'InPlay') & (x['ExitSpeed'] > 15), 1, 0),
    FBstrikeind=lambda x: np.where(
        x['PitchCall'].isin(['StrikeSwinging', 'StrikeCalled', 'FoulBall', 'FoulBallNotFieldable', 'InPlay']) &
        (x['FBindicator'] == 1), 1, 0),
    OSstrikeind=lambda x: np.where(
        x['PitchCall'].isin(['StrikeSwinging', 'StrikeCalled', 'FoulBall', 'FoulBallNotFieldable', 'InPlay']) &
        (x['OSindicator'] == 1), 1, 0),
    EdgeIndicator=lambda x: np.where(
        ((x['EdgeHeightIndicator'] == 1) & (x['EdgeZoneWIndicator'] == 1)) |
        ((x['EdgeWidthIndicator'] == 1) & (x['EdgeZoneHtIndicator'] == 1)), 1, 0),
    QualityPitchIndicator=lambda x: np.where((x['StrikeZoneIndicator'] == 1) | (x['EdgeIndicator'] == 1), 1, 0),
    FPSindicator=lambda x: np.where(
        x['PitchCall'].isin(['StrikeCalled', 'StrikeSwinging', 'FoulBallNotFieldable', 'FoulBall', 'InPlay']) &
        (x['FPindicator'] == 1), 1, 0),
    OutIndicator=lambda x: np.where(
        (x['PlayResult'].isin(['Out', 'FieldersChoice']) | (x['KorBB'] == 'Strikeout')) &
        (x['HBPIndicator'] == 0), 1, 0),
    LOOindicator=lambda x: np.where((x['LeadOffIndicator'] == 1) & (x['OutIndicator'] == 1), 1, 0),
    Zwhiffind=lambda x: np.where((x['WhiffIndicator'] == 1) & (x['StrikeZoneIndicator'] == 1), 1, 0),
    Zswing=lambda x: np.where((x['StrikeZoneIndicator'] == 1) & (x['SwingIndicator'] == 1), 1, 0),
    GBindicator=lambda x: np.where(x['HitType'] == 'GroundBall', 1, 0),
    Chaseindicator=lambda x: np.where((x['SwingIndicator'] == 1) & (x['StrikeZoneIndicator'] == 0), 1, 0),
    OutofZone=lambda x: np.where(x['StrikeZoneIndicator'] == 0, 1, 0),
    OnBaseindicator=lambda x: np.where(
        x['PlayResult'].isin(['Single', 'Double', 'Triple', 'HomeRun']) |
        (x['KorBB'] == 'Walk') | (x['PitchCall'] == 'HitByPitch'), 1, 0),
    totalbases=lambda x: np.where(
        x['PlayResult'] == 'Single', 1,
        np.where(x['PlayResult'] == 'Double', 2,
                 np.where(x['PlayResult'] == 'Triple', 3,
                          np.where(x['PlayResult'] == 'HomeRun', 4, 0))))
)

# Summarize data by pitcher
summary_data = df.groupby('Pitcher').agg({
    'Inning': lambda x: len(set(zip(x, df.loc[x.index, 'Batter'], df.loc[x.index, 'PAofInning']))),
    'RelSpeed': [
        lambda x: round(np.mean(x[df.loc[x.index, 'TaggedPitchType'].isin(['Fastball', 'Sinker'])]), 1) if len(x[df.loc[x.index, 'TaggedPitchType'].isin(['Fastball', 'Sinker'])]) > 0 else np.nan,
        lambda x: round(np.max(x[df.loc[x.index, 'TaggedPitchType'].isin(['Fastball', 'Sinker'])]), 1) if len(x[df.loc[x.index, 'TaggedPitchType'].isin(['Fastball', 'Sinker'])]) > 0 else np.nan
    ],
    'PitchCall': lambda x: round(len(x[x.isin(['StrikeCalled', 'StrikeSwinging', 'FoulBall', 'FoulBallNotFieldable', 'InPlay'])]) / len(x), 3) * 100,
    'FBstrikeind': lambda x: round(x.sum() / df.loc[x.index, 'FBindicator'].sum(), 3) * 100 if df.loc[x.index, 'FBindicator'].sum() > 0 else np.nan,
    'OSstrikeind': lambda x: round(x.sum() / df.loc[x.index, 'OSindicator'].sum(), 3) * 100 if df.loc[x.index, 'OSindicator'].sum() > 0 else np.nan,
    'EarlyIndicator': lambda x: x.sum(),
    'AheadIndicator': lambda x: x.sum(),
    'PAindicator': lambda x: x.sum(),
    'FPSindicator': lambda x: round(x.sum() / df.loc[x.index, 'FPindicator'].sum(), 3) * 100 if df.loc[x.index, 'FPindicator'].sum() > 0 else np.nan,
    'WhiffIndicator': lambda x: round(x.sum() / df.loc[x.index, 'SwingIndicator'].sum(), 3) * 100 if df.loc[x.index, 'SwingIndicator'].sum() > 0 else np.nan,
    'Zwhiffind': lambda x: round(x.sum() / df.loc[x.index, 'Zswing'].sum(), 3) * 100 if df.loc[x.index, 'Zswing'].sum() > 0 else np.nan,
    'Chaseindicator': lambda x: round(x.sum() / df.loc[x.index, 'OutofZone'].sum(), 3) * 100 if df.loc[x.index, 'OutofZone'].sum() > 0 else np.nan,
    'KorBB': lambda x: round(len(x[x == 'Strikeout']) / df.loc[x.index, 'PAindicator'].sum(), 3) * 100 if df.loc[x.index, 'PAindicator'].sum() > 0 else np.nan,
    'WalkIndicator': lambda x: round(x.sum() / df.loc[x.index, 'PAindicator'].sum(), 3) * 100 if df.loc[x.index, 'PAindicator'].sum() > 0 else np.nan,
    'HitIndicator': lambda x: round(x.sum() / df.loc[x.index, 'ABindicator'].sum(), 3) if df.loc[x.index, 'ABindicator'].sum() > 0 else np.nan,
    'ABindicator': lambda x: x.sum(),  # Added ABindicator aggregation
    'OnBaseindicator': lambda x: x.sum(),
    'totalbases': lambda x: x.sum(),
    'HHindicator': lambda x: round(x.sum() / df.loc[x.index, 'biphh'].sum(), 3) * 100 if df.loc[x.index, 'biphh'].sum() > 0 else np.nan,
    'GBindicator': lambda x: round(x.sum() / df.loc[x.index, 'BIPind'].sum(), 3) * 100 if df.loc[x.index, 'BIPind'].sum() > 0 else np.nan
}).reset_index()

# Rename columns
summary_data.columns = [
    'Pitcher', 'BF', 'Avg FB Velo', 'Max FB Velo', 'Strike %', 'FB Strike %', 'OS Strike %',
    'EarlyIndicator', 'AheadIndicator', 'PAindicator', '1PK %', 'Whiff %', 'IZ Whiff %',
    'Chase %', 'K %', 'BB %', 'AVG', 'ABindicator', 'OnBaseindicator', 'totalbases', 'HH %', 'GB %'
]

# Calculate E+A % and OPS
summary_data['E+A %'] = round((summary_data['EarlyIndicator'] + summary_data['AheadIndicator']) / summary_data['PAindicator'], 3) * 100
summary_data['OPS'] = round(
    (summary_data['OnBaseindicator'] / summary_data['PAindicator']) +
    (summary_data['totalbases'] / summary_data['ABindicator']), 3
)
summary_data = summary_data.drop(columns=['EarlyIndicator', 'AheadIndicator', 'PAindicator', 'OnBaseindicator', 'totalbases', 'ABindicator'])

# Define color mapping functions
red_white_green = LinearSegmentedColormap.from_list('red_white_green', ['#E1463E', 'white', '#00840D'])
green_white_red = LinearSegmentedColormap.from_list('green_white_red', ['#00840D', 'white', '#E1463E'])

def col_numeric(palette, domain):
    norm = plt.Normalize(domain[0], domain[2])
    sm = ScalarMappable(cmap=palette, norm=norm)
    def map_value(x):
        if pd.isna(x):
            return '#FFFFFF'
        return plt.cm.colors.to_hex(sm.to_rgba(x))
    return map_value

velocity_palette = col_numeric(red_white_green, [82, 90, 98])
maxvelopal = col_numeric(red_white_green, [85, 93, 102])
strikeperpal = col_numeric(red_white_green, [40, 61, 75])
fbstrpal = col_numeric(red_white_green, [46, 63, 76])
osstrpal = col_numeric(red_white_green, [40, 60, 74])
epapal = col_numeric(red_white_green, [40, 69, 90])
fpspal = col_numeric(red_white_green, [25, 58, 82])
whiffpal = col_numeric(red_white_green, [2, 24, 50])
zonewhiff = col_numeric(red_white_green, [0, 16, 40])
kpal = col_numeric(red_white_green, [0, 21, 45])
bbpal = col_numeric(green_white_red, [0, 11, 35])
avgpal = col_numeric(green_white_red, [0.1, 0.264, 0.450])
opspal = col_numeric(green_white_red, [0.389, 0.836, 1.368])
hhpal = col_numeric(green_white_red, [8, 33, 54])
gbpal = col_numeric(red_white_green, [0, 50, 100])
chasepal = col_numeric(red_white_green, [20, 26, 31])

# Apply colors
summary_data2 = summary_data.sort_values('Avg FB Velo', ascending=False).copy()
summary_data2 = summary_data2.assign(
    Velocity_Color=lambda x: np.where(x['Avg FB Velo'] < 82, '#E1463E',
                                      np.where(x['Avg FB Velo'] > 98, '#00840D',
                                               x['Avg FB Velo'].apply(velocity_palette))),
    MaxColor=lambda x: np.where(x['Max FB Velo'] < 85, '#E1463E',
                                np.where(x['Max FB Velo'] > 102, '#00840D',
                                         x['Max FB Velo'].apply(maxvelopal))),
    StrikePerColor=lambda x: np.where(x['Strike %'] < 40, '#E1463E',
                                      np.where(x['Strike %'] > 75, '#00840D',
                                               x['Strike %'].apply(strikeperpal))),
    FBStrkColor=lambda x: np.where(x['FB Strike %'] < 46, '#E1463E',
                                   np.where(x['FB Strike %'] > 76, '#00840D',
                                            x['FB Strike %'].apply(fbstrpal))),
    OSStrikeCol=lambda x: np.where(x['OS Strike %'] < 40, '#E1463E',
                                   np.where(x['OS Strike %'] > 74, '#00840D',
                                            x['OS Strike %'].apply(osstrpal))),
    EPAcol=lambda x: np.where(x['E+A %'] < 40, '#E1463E',
                              np.where(x['E+A %'] > 90, '#00840D',
                                       x['E+A %'].apply(epapal))),
    FPScolor=lambda x: np.where(x['1PK %'] < 25, '#E1463E',
                                np.where(x['1PK %'] > 82, '#00840D',
                                         x['1PK %'].apply(fpspal))),
    Whiffcolor=lambda x: np.where(x['Whiff %'] < 2, '#E1463E',
                                  np.where(x['Whiff %'] > 50, '#00840D',
                                           x['Whiff %'].apply(whiffpal))),
    zwhiffcolor=lambda x: np.where(x['IZ Whiff %'] < 0, '#E1463E',
                                   np.where(x['IZ Whiff %'] > 40, '#00840D',
                                            x['IZ Whiff %'].apply(zonewhiff))),
    Kcolor=lambda x: np.where(x['K %'] < 0, '#E1463E',
                              np.where(x['K %'] > 45, '#00840D',
                                       x['K %'].apply(kpal))),
    BBcolor=lambda x: np.where(x['BB %'] < 0, '#00840D',
                               np.where(x['BB %'] > 35, '#E1463E',
                                        x['BB %'].apply(bbpal))),
    AVGcolor=lambda x: np.where(x['AVG'] < 0.1, '#00840D',
                                np.where(x['AVG'] > 0.45, '#E1463E',
                                         x['AVG'].apply(avgpal))),
    OPScol=lambda x: np.where(x['OPS'] < 0.389, '#00840D',
                              np.where(x['OPS'] > 1.368, '#E1463E',
                                       x['OPS'].apply(opspal))),
    HHcol=lambda x: np.where(x['HH %'] < 8, '#00840D',
                             np.where(x['HH %'] > 54, '#E1463E',
                                      x['HH %'].apply(hhpal))),
    GBcol=lambda x: x['GB %'].apply(gbpal),
    chasecol=lambda x: np.where(x['Chase %'] < 20, '#E1463E',
                                np.where(x['Chase %'] > 31, '#00840D',
                                         x['Chase %'].apply(chasepal)))
)

# Summarize data by team
summary_data_team = df.groupby('PitcherTeam').agg({
    'Inning': lambda x: len(set(zip(x, df.loc[x.index, 'Batter'], df.loc[x.index, 'PAofInning']))),
    'RelSpeed': [
        lambda x: round(np.mean(x[df.loc[x.index, 'TaggedPitchType'].isin(['Fastball', 'Sinker'])]), 1) if len(x[df.loc[x.index, 'TaggedPitchType'].isin(['Fastball', 'Sinker'])]) > 0 else np.nan,
        lambda x: round(np.max(x[df.loc[x.index, 'TaggedPitchType'].isin(['Fastball', 'Sinker'])]), 1) if len(x[df.loc[x.index, 'TaggedPitchType'].isin(['Fastball', 'Sinker'])]) > 0 else np.nan
    ],
    'PitchCall': lambda x: round(len(x[x.isin(['StrikeCalled', 'StrikeSwinging', 'FoulBallNotFieldable', 'FoulBall', 'InPlay'])]) / len(x), 3) * 100,
    'FBstrikeind': lambda x: round(x.sum() / df.loc[x.index, 'FBindicator'].sum(), 3) * 100 if df.loc[x.index, 'FBindicator'].sum() > 0 else np.nan,
    'OSstrikeind': lambda x: round(x.sum() / df.loc[x.index, 'OSindicator'].sum(), 3) * 100 if df.loc[x.index, 'OSindicator'].sum() > 0 else np.nan,
    'EarlyIndicator': lambda x: x.sum(),
    'AheadIndicator': lambda x: x.sum(),
    'PAindicator': lambda x: x.sum(),
    'FPSindicator': lambda x: round(x.sum() / df.loc[x.index, 'FPindicator'].sum(), 3) * 100 if df.loc[x.index, 'FPindicator'].sum() > 0 else np.nan,
    'WhiffIndicator': lambda x: round(x.sum() / df.loc[x.index, 'SwingIndicator'].sum(), 3) * 100 if df.loc[x.index, 'SwingIndicator'].sum() > 0 else np.nan,
    'Zwhiffind': lambda x: round(x.sum() / df.loc[x.index, 'Zswing'].sum(), 3) * 100 if df.loc[x.index, 'Zswing'].sum() > 0 else np.nan,
    'Chaseindicator': lambda x: round(x.sum() / df.loc[x.index, 'OutofZone'].sum(), 3) * 100 if df.loc[x.index, 'OutofZone'].sum() > 0 else np.nan,
    'KorBB': lambda x: round(len(x[x == 'Strikeout']) / df.loc[x.index, 'PAindicator'].sum(), 3) * 100 if df.loc[x.index, 'PAindicator'].sum() > 0 else np.nan,
    'WalkIndicator': lambda x: round(x.sum() / df.loc[x.index, 'PAindicator'].sum(), 3) * 100 if df.loc[x.index, 'PAindicator'].sum() > 0 else np.nan,
    'HitIndicator': lambda x: round(x.sum() / df.loc[x.index, 'ABindicator'].sum(), 3) if df.loc[x.index, 'ABindicator'].sum() > 0 else np.nan,
    'ABindicator': lambda x: x.sum(),  # Added ABindicator aggregation
    'OnBaseindicator': lambda x: x.sum(),
    'totalbases': lambda x: x.sum(),
    'HHindicator': lambda x: round(x.sum() / df.loc[x.index, 'biphh'].sum(), 3) * 100 if df.loc[x.index, 'biphh'].sum() > 0 else np.nan,
    'GBindicator': lambda x: round(x.sum() / df.loc[x.index, 'BIPind'].sum(), 3) * 100 if df.loc[x.index, 'BIPind'].sum() > 0 else np.nan
}).reset_index()

# Rename columns
summary_data_team.columns = [
    'PitcherTeam', 'BF', 'Avg FB Velo', 'Max FB Velo', 'Strike %', 'FB Strike %', 'OS Strike %',
    'EarlyIndicator', 'AheadIndicator', 'PAindicator', '1PK %', 'Whiff %', 'IZ Whiff %',
    'Chase %', 'K %', 'BB %', 'AVG', 'ABindicator', 'OnBaseindicator', 'totalbases', 'HH %', 'GB %'
]

# Calculate E+A % and OPS
summary_data_team['E+A %'] = round((summary_data_team['EarlyIndicator'] + summary_data_team['AheadIndicator']) / summary_data_team['PAindicator'], 3) * 100
summary_data_team['OPS'] = round(
    (summary_data_team['OnBaseindicator'] / summary_data_team['PAindicator']) +
    (summary_data_team['totalbases'] / summary_data_team['ABindicator']), 3
)
summary_data_team = summary_data_team.drop(columns=['EarlyIndicator', 'AheadIndicator', 'PAindicator', 'OnBaseindicator', 'totalbases', 'ABindicator'])

# Print tables
print("\nPitching Leaders")
print(tabulate(summary_data2[summary_data2.columns[:16]], headers='keys', tablefmt='psql', showindex=False))

print("\nTeam Summary")
print(tabulate(summary_data_team[summary_data_team.columns[:16]], headers='keys', tablefmt='psql', showindex=False))

# Save to CSV
summary_data2[summary_data2.columns[:16]].to_csv('output.csv', index=False)
summary_data_team[summary_data_team.columns[:16]].to_csv('team_output.csv', index=False)

# Optional: Generate PDF with weasyprint (requires installation)

# Save to CSV
summary_data2[summary_data2.columns[:16]].to_csv('output.csv', index=False)
summary_data_team[summary_data_team.columns[:16]].to_csv('team_output.csv', index=False)

# Generate PDF with reportlab
def create_pdf(summary_data, team_data, filename='pitcher_report.pdf'):
    doc = SimpleDocTemplate(filename, pagesize=letter)
    elements = []
    styles = getSampleStyleSheet()

    # Title for Pitching Leaders
    elements.append(Paragraph("Pitching Leaders", styles['Heading1']))

    # Prepare data for Pitching Leaders table
    data = [summary_data.columns[:16].tolist()]  # Headers
    for _, row in summary_data[summary_data.columns[:16]].iterrows():
        data.append([str(val) for val in row.values])

    # Create table for Pitching Leaders
    table = Table(data)
    table.setStyle(TableStyle([
        ('BACKGROUND', (0, 0), (-1, 0), colors.grey),
        ('TEXTCOLOR', (0, 0), (-1, 0), colors.whitesmoke),
        ('ALIGN', (0, 0), (-1, -1), 'CENTER'),
        ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
        ('FONTSIZE', (0, 0), (-1, 0), 12),
        ('BOTTOMPADDING', (0, 0), (-1, 0), 12),
        ('BACKGROUND', (0, 1), (-1, -1), colors.beige),
        ('GRID', (0, 0), (-1, -1), 1, colors.black),
    ]))
    elements.append(table)

    # Title for Team Summary
    elements.append(Paragraph("Team Summary", styles['Heading1']))

    # Prepare data for Team Summary table
    data_team = [team_data.columns[:16].tolist()]  # Headers
    for _, row in team_data[team_data.columns[:16]].iterrows():
        data_team.append([str(val) for val in row.values])

    # Create table for Team Summary
    table_team = Table(data_team)
    table_team.setStyle(TableStyle([
        ('BACKGROUND', (0, 0), (-1, 0), colors.grey),
        ('TEXTCOLOR', (0, 0), (-1, 0), colors.whitesmoke),
        ('ALIGN', (0, 0), (-1, -1), 'CENTER'),
        ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
        ('FONTSIZE', (0, 0), (-1, 0), 12),
        ('BOTTOMPADDING', (0, 0), (-1, 0), 12),
        ('BACKGROUND', (0, 1), (-1, -1), colors.beige),
        ('GRID', (0, 0), (-1, -1), 1, colors.black),
    ]))
    elements.append(table_team)

    # Build the PDF
    doc.build(elements)
    print(f"PDF generated: {filename}")

# Call the function to generate the PDF
create_pdf(summary_data2, summary_data_team, 'pitcher_report.pdf')