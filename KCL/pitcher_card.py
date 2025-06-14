import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
import pandas as pd
import seaborn as sns
import matplotlib.colors as mcolors
import numpy as np
from matplotlib.ticker import FuncFormatter
from matplotlib.patches import Rectangle
import os

# Define the stats dictionary for the new box score metrics
stats_dict = {
    'IP': {'table_header': '$\\bf{IP}$', 'format': '.1f'},  # Innings Pitched
    'P': {'table_header': '$\\bf{P}$', 'format': '.1f'},     # Number of Pitches
    'R': {'table_header': '$\\bf{R}$', 'format': '.0f'},     # Runs
    'H': {'table_header': '$\\bf{H}$', 'format': '.0f'},     # Hits
    'BB': {'table_header': '$\\bf{BB}$', 'format': '.0f'},   # Walks
    'K': {'table_header': '$\\bf{K}$', 'format': '.0f'}      # Strikeouts
}

# Function to compute pitching stats from local data
def local_pitching_stats(pitcher_name: str, team: str, season: int, data_path: str):
    dtypes = {
        'Pitcher': str,
        'PitcherTeam': str,
        'TaggedPitchType': str,
        'RelSpeed': float,
        'HorzBreak': float,
        'InducedVertBreak': float,
        'SpinRate': float,
        'RelSide': float,
        'RelHeight': float,
        'Extension': float,
        'Swing?': float,
        'Swing Strike?': float,
        'Strike?': float,
        'Chase?': float,
        'PitchCall': str,
        'PlayResult': str,
        'Date': str,
        'BatterSide': str,
        'PlateLocSide': float,
        'PlateLocHeight': float,
        'OutsOnPlay': float,
        'RunsScored': float,
        'KorBB': str
    }
    try:
        df = pd.read_csv(data_path, dtype=dtypes)
    except ValueError as e:
        print(f"Error reading CSV with specified dtypes: {e}")
        print("Falling back to low_memory=False")
        df = pd.read_csv(data_path, low_memory=False)

    df['Year'] = pd.to_datetime(df['Date'], errors='coerce').dt.year
    pitcher_data = df[(df['Pitcher'] == pitcher_name) & (df['PitcherTeam'] == team) & (df['Year'] == season)]

    if pitcher_data.empty:
        print(f"No data found for {pitcher_name} from {team} in {season}")
        return pd.DataFrame()

    # Calculate box score metrics
    ip = pitcher_data['OutsOnPlay'].sum() / 3.0  # Innings Pitched = Total Outs / 3
    runs = pitcher_data['RunsScored'].sum()      # Runs Scored
    hits = pitcher_data[pitcher_data['PlayResult'].isin(['Single', 'Double', 'Triple', 'HomeRun'])].shape[0]  # Hits
    walks = pitcher_data[pitcher_data['KorBB'] == 'Walk'].shape[0]  # Walks
    strikeouts = pitcher_data[pitcher_data['PlayResult'] == 'StrikeoutLooking'].shape[0] + pitcher_data[pitcher_data['PlayResult'] == 'StrikeoutSwinging'].shape[0]  # Strikeouts
    pitches = pitcher_data['TaggedPitchType'].count()
    stats = {
        'IP': ip,
        'P': pitches,
        'R': runs,
        'H': hits,
        'BB': walks,
        'K': strikeouts
    }
    return pd.DataFrame([stats])

# Function to create the pitching stats table
def local_pitcher_stats_table(pitcher_name: str, team: str, season: int, ax: plt.Axes, stats: list, fontsize: int, data_path: str):
    df_stats = local_pitching_stats(pitcher_name, team, season, data_path)

    if df_stats.empty:
        print("No stats to display")
        return

    df_stats = df_stats[stats].reset_index(drop=True)

    df_stats.loc[0] = [
        format(df_stats[x][0], stats_dict[x]['format'])
        if df_stats[x][0] != '---' and not pd.isna(df_stats[x][0])
        else '---'
        for x in df_stats
    ]

    table_fg = ax.table(
        cellText=df_stats.values,
        colLabels=stats,
        cellLoc='center',
        bbox=[0.00, 0.0, 1, 1]
    )

    table_fg.set_fontsize(fontsize)

    new_column_names = [stats_dict[x]['table_header'] if x in df_stats else '---' for x in stats]
    for i, col_name in enumerate(new_column_names):
        table_fg.get_celld()[(0, i)].get_text().set_text(col_name)

    ax.axis('off')

# Unchanged functions (strike_zone_plot and break_plot remain the same)
def strike_zone_plot(df: pd.DataFrame, ax: plt.Axes, pitcher_name: str, batter_side: str, title: str):
    df = df.rename(columns={
        'HorzBreak': 'pfx_x',
        'InducedVertBreak': 'pfx_z',
        'TaggedPitchType': 'pitch_type',
        'PitcherThrows': 'p_throws',
        'PlateLocSide': 'plate_x',
        'PlateLocHeight': 'plate_z'
    })

    pitch_mapping = {
        'Fastball': '4-SEAM FASTBALL',
        'Sinker': 'SINKER',
        'Curveball': 'CURVEBALL',
        'Changeup': 'CHANGEUP',
        'Sweeper': 'SWEEPER',
        'Slider': 'SLIDER',
        'Cutter': 'CUTTER',
        'Splitter': 'SPLITTER',
        'Undefined': 'UNKNOWN',
        'Knuckleball': 'KNUCKLEBALL'
    }

    df['pitch_type'] = df['pitch_type'].fillna('UNKNOWN').astype(str).map(pitch_mapping).fillna('UNKNOWN')

    dict_colour = {
        '4-SEAM FASTBALL': 'pink',
        'SINKER': 'purple',
        'CURVEBALL': 'blue',
        'CHANGEUP': 'orange',
        'SWEEPER': 'red',
        'SLIDER': 'green',
        'CUTTER': 'yellow',
        'SPLITTER': 'black',
        'UNKNOWN': 'gray',
        'KNUCKLEBALL': 'brown'
    }

    missing_pitches = set(df['pitch_type']) - set(dict_colour.keys())
    if missing_pitches:
        raise ValueError(f"The palette dictionary is missing keys: {missing_pitches}")

    font_properties = {'fontsize': 10}
    font_properties_axes = {'fontsize': 12, 'weight': 'bold'}
    font_properties_titles = {'fontsize': 14, 'weight': 'bold'}

    df_side = df[df['BatterSide'] == batter_side]
    if df_side.empty:
        ax.text(0.5, 0.5, f'No data for {batter_side} batters', ha='center', va='center', fontsize=12)
        ax.set_xlim(-3.66, 2)
        ax.set_ylim(0, 4)
        ax.set_aspect('equal', adjustable='box')
        ax.axis('off')
        return

    sns.scatterplot(ax=ax,
                    x=df_side['plate_x'],
                    y=df_side['plate_z'],
                    hue=df_side['pitch_type'],
                    palette=dict_colour,
                    ec='black',
                    alpha=0.8,
                    zorder=2)

    strike_zone = Rectangle((-1.66, 1.5), 1.7, 2.0, fill=False, edgecolor='black', linewidth=2, zorder=1)
    ax.add_patch(strike_zone)

    ax.set_xlabel('Horizontal Location (ft, Catcher\'s View)', fontdict=font_properties_axes)
    ax.set_ylabel('Vertical Location (ft)', fontdict=font_properties_axes)
    ax.set_title(f"{title}\n{pitcher_name}", fontdict=font_properties_titles)

    ax.set_xlim(-3, 1)
    ax.set_ylim(0, 4)
    ax.set_aspect('equal', adjustable='box')

    ax.xaxis.set_major_formatter(FuncFormatter(lambda x, _: int(x) if x.is_integer() else x))
    ax.yaxis.set_major_formatter(FuncFormatter(lambda x, _: int(x) if x.is_integer() else x))

    ax.get_legend().remove()

def break_plot(df: pd.DataFrame, ax: plt.Axes, pitcher_name: str):
    df = df.rename(columns={
        'HorzBreak': 'pfx_x',
        'InducedVertBreak': 'pfx_z',
        'TaggedPitchType': 'pitch_type',
        'PitcherThrows': 'p_throws',
        'VerticleArmAngle' : 'arm_angle'
    })

    pitch_mapping = {
        'Fastball': '4-SEAM FASTBALL',
        'Sinker': 'SINKER',
        'Curveball': 'CURVEBALL',
        'Changeup': 'CHANGEUP',
        'Sweeper': 'SWEEPER',
        'Slider': 'SLIDER',
        'Cutter': 'CUTTER',
        'Splitter': 'SPLITTER',
        'FourSeamFastBall': '4-SEAM FASTBALL',
        'Undefined': 'UNKNOWN',
        'Knuckleball': 'KNUCKLEBALL'
    }

    df['pitch_type'] = df['pitch_type'].fillna('UNKNOWN').astype(str).map(pitch_mapping).fillna('UNKNOWN')

    dict_colour = {
        '4-SEAM FASTBALL': 'pink',
        'SINKER': 'purple',
        'CURVEBALL': 'blue',
        'CHANGEUP': 'orange',
        'SWEEPER': 'red',
        'SLIDER': 'green',
        'CUTTER': 'yellow',
        'SPLITTER': 'black',
        'FOURSEAM': 'pink',
        'UNKNOWN': 'gray',
        'KNUCKLEBALL': 'brown'
    }

    missing_pitches = set(df['pitch_type']) - set(dict_colour.keys())
    if missing_pitches:
        raise ValueError(f"The palette dictionary is missing keys: {missing_pitches}")

    font_properties = {'fontsize': 10}
    font_properties_axes = {'fontsize': 12, 'weight': 'bold'}
    font_properties_titles = {'fontsize': 14, 'weight': 'bold'}

    sns.scatterplot(ax=ax,
                    x=df['pfx_x'],
                    y=df['pfx_z'],
                    hue=df['pitch_type'],
                    palette=dict_colour,
                    ec='black',
                    alpha=0.8,
                    zorder=2)
    
    pitching_side = df['p_throws'][df['p_throws'].notna()].iloc[0] if df['p_throws'].notna().any() else 'Unknown'
    
    ax.axhline(y=0, color='#808080', alpha=0.5, linestyle='--', zorder=1)
    ax.axvline(x=0, color='#808080', alpha=0.5, linestyle='--', zorder=1)

    ax.set_xlabel('Horizontal Break (in, Pitcher\'s View)', fontdict=font_properties_axes)
    ax.set_ylabel('Induced Vertical Break (in)', fontdict=font_properties_axes)
    ax.set_title(f"Pitch Breaks\n{pitcher_name} - {pitching_side}", fontdict=font_properties_titles)

    ax.set_xticks(range(-20, 21, 10))
    ax.set_xticklabels(range(-20, 21, 10), fontdict=font_properties)
    ax.set_yticks(range(-20, 21, 10))
    ax.set_yticklabels(range(-20, 21, 10), fontdict=font_properties)

    ax.set_xlim((-25, 25))
    ax.set_ylim((-25, 25))

    ax.text(-24.2, -24.2, s='← First Base', fontstyle='italic', ha='left', va='bottom',
            bbox=dict(facecolor='white', edgecolor='black'), fontsize=10, zorder=3)
    ax.text(24.2, -24.2, s='Third Base →', fontstyle='italic', ha='right', va='bottom',
            bbox=dict(facecolor='white', edgecolor='black'), fontsize=10, zorder=3)

    ax.set_aspect('equal', adjustable='box')

    ax.xaxis.set_major_formatter(FuncFormatter(lambda x, _: int(x)))
    ax.yaxis.set_major_formatter(FuncFormatter(lambda x, _: int(x)))

    ax.get_legend().remove()

# Modified df_grouping to apply pitcher-specific filtering and use TaggedPitchType.count()
def df_grouping(df: pd.DataFrame, pitcher_name: str, team: str, season: int):
    df = df.rename(columns={
        'TaggedPitchType': 'pitch_type',
        'RelSpeed': 'release_speed',
        'HorzBreak': 'pfx_x',
        'InducedVertBreak': 'pfx_z',
        'SpinRate': 'release_spin_rate',
        'RelSide': 'release_pos_x',
        'RelHeight': 'release_pos_z',
        'Extension': 'release_extension'
    })

    # Apply pitcher, team, and season filters
    df['Year'] = pd.to_datetime(df['Date'], errors='coerce').dt.year
    pitcher_data = df[(df['Pitcher'] == pitcher_name) & (df['PitcherTeam'] == team) & (df['Year'] == season)]

    if pitcher_data.empty:
        print(f"No data found for {pitcher_name} from {team} in {season} for pitch matrix")
        return pd.DataFrame(), []

    pitch_mapping = {
        'Fastball': '4-SEAM FASTBALL',
        'Sinker': 'SINKER',
        'Curveball': 'CURVEBALL',
        'Changeup': 'CHANGEUP',
        'Sweeper': 'SWEEPER',
        'Splitter':'SPLITTER',
        'Slider': 'SLIDER',
        'Cutter': 'CUTTER',
        'Undefined': 'UNKNOWN',
        'Knuckleball': 'KNUCKLEBALL'
    }

    pitcher_data = pitcher_data.copy()  # Avoid SettingWithCopyWarning
    pitcher_data['pitch_type'] = pitcher_data['pitch_type'].fillna('UNKNOWN').astype(str).map(pitch_mapping)

    df_group = pitcher_data.groupby('pitch_type').agg(
        pitch=('pitch_type', 'count'),
        release_speed=('release_speed', 'mean'),
        pfx_z=('pfx_z', 'mean'),
        pfx_x=('pfx_x', 'mean'),
        release_spin_rate=('release_spin_rate', 'mean'),
        release_pos_x=('release_pos_x', 'mean'),
        release_pos_z=('release_pos_z', 'mean'),
        release_extension=('release_extension', 'mean'),
        swing=('Swing?', 'sum'),
        whiff=('Swing Strike?', 'sum'),
        in_zone=('Strike?', 'sum'),
        chase=('Chase?', 'sum')
    ).reset_index()

    df_group['pitch_usage'] = df_group['pitch'] / df_group['pitch'].sum()
    df_group['whiff_rate'] = df_group['whiff'] / df_group['swing'].replace(0, np.nan)
    df_group['in_zone_rate'] = df_group['in_zone'] / df_group['pitch']
    df_group['chase_rate'] = df_group['chase'] / df_group['pitch']

    df_group['pitch_description'] = df_group['pitch_type']
    dict_colour = {
        '4-SEAM FASTBALL': 'pink',
        'SINKER': 'purple',
        'CURVEBALL': 'blue',
        'CHANGEUP': 'orange',
        'SWEEPER': 'red',
        'SLIDER': 'green',
        'SPLITTER' : 'black',
        'CUTTER': 'yellow',
        'UNKNOWN': 'gray',
        'KNUCKLEBALL': 'brown'
    }
    df_group['colour'] = df_group['pitch_type'].map(dict_colour).fillna('gray')

    df_group = df_group.sort_values(by='pitch_usage', ascending=False)
    colour_list = df_group['colour'].tolist()
    colour_list = ['gray' if pd.isna(col) else col for col in colour_list]

    # Use TaggedPitchType.count() for total pitch count, consistent with local_pitching_stats
    total_pitches = pitcher_data['pitch_type'].count()

    plot_table_all = pd.DataFrame(data={
        'pitch_type': 'All',
        'pitch_description': 'All',
        'pitch': total_pitches,
        'pitch_usage': 1.0,
        'release_speed': pitcher_data['release_speed'].mean(),
        'pfx_z': pitcher_data['pfx_z'].mean(),
        'pfx_x': pitcher_data['pfx_x'].mean(),
        'release_spin_rate': pitcher_data['release_spin_rate'].mean(),
        'release_pos_x': pitcher_data['release_pos_x'].mean(),
        'release_pos_z': pitcher_data['release_pos_z'].mean(),
        'release_extension': pitcher_data['release_extension'].mean(),
        'swing': pitcher_data['Swing?'].sum(),
        'whiff': pitcher_data['Swing Strike?'].sum(),
        'in_zone': pitcher_data['Strike?'].sum(),
        'chase': pitcher_data['Chase?'].sum(),
        'whiff_rate': pitcher_data['Swing Strike?'].sum() / pitcher_data['Swing?'].sum() if pitcher_data['Swing?'].sum() > 0 else np.nan,
        'in_zone_rate': pitcher_data['Strike?'].sum() / total_pitches if total_pitches > 0 else np.nan,
        'chase_rate': pitcher_data['Chase?'].sum() / total_pitches if total_pitches > 0 else np.nan
    }, index=[0])

    df_plot = pd.concat([df_group, plot_table_all], ignore_index=True)
    return df_plot, colour_list

pitch_stats_dict = {
    'pitch': {'table_header': '$\\bf{Count}$', 'format': '.0f'},
    'pitch_usage': {'table_header': '$\\bf{Pitch\%}$', 'format': '.1%'},
    'release_speed': {'table_header': '$\\bf{Velocity}$', 'format': '.1f'},
    'pfx_z': {'table_header': '$\\bf{iVB}$', 'format': '.1f'},
    'pfx_x': {'table_header': '$\\bf{HB}$', 'format': '.1f'},
    'release_spin_rate': {'table_header': '$\\bf{Spin}$', 'format': '.0f'},
    'release_pos_x': {'table_header': '$\\bf{hRel}$', 'format': '.1f'},
    'release_pos_z': {'table_header': '$\\bf{vRel}$', 'format': '.1f'},
    'release_extension': {'table_header': '$\\bf{Ext.}$', 'format': '.1f'},
    'whiff_rate': {'table_header': '$\\bf{Whiff\%}$', 'format': '.1%'},
    'in_zone_rate': {'table_header': '$\\bf{Zone\%}$', 'format': '.1%'},
    'chase_rate': {'table_header': '$\\bf{Chase\%}$', 'format': '.1%'}
}

table_columns = [
    'pitch_description',
    'pitch',
    'pitch_usage',
    'release_speed',
    'pfx_z',
    'pfx_x',
    'release_spin_rate',
    'release_pos_x',
    'release_pos_z',
    'release_extension',
    'whiff_rate',
    'in_zone_rate',
    'chase_rate'
]

def plot_pitch_format(df: pd.DataFrame):
    df_group = df[table_columns].fillna('—')
    for column, props in pitch_stats_dict.items():
        if column in df_group.columns:
            df_group[column] = df_group[column].apply(lambda x: format(x, props['format']) if isinstance(x, (int, float)) else x)
    return df_group

cmap_sum = mcolors.LinearSegmentedColormap.from_list("", ['#648FFF', '#FFFFFF', '#FFB000'])
colour_stats = ['release_speed', 'whiff_rate', 'in_zone_rate', 'chase_rate']

def get_cell_colours(df_group: pd.DataFrame):
    colour_list_df = []
    for pt in df_group['pitch_type'].unique():
        colour_list_df_inner = []
        select_df = df_group[df_group['pitch_type'] == pt]
        for tb in table_columns:
            if tb in colour_stats and isinstance(select_df[tb].values[0], (int, float)):
                value = pd.to_numeric(select_df[tb], errors='coerce').mean()
                if np.isnan(value):
                    colour_list_df_inner.append('#ffffff')
                elif tb == 'release_speed':
                    normalize = mcolors.Normalize(vmin=df_group[tb].mean() * 0.95, vmax=df_group[tb].mean() * 1.05)
                    colour_list_df_inner.append(mcolors.to_hex(cmap_sum(normalize(value))))
                else:
                    normalize = mcolors.Normalize(vmin=df_group[tb].mean() * 0.7, vmax=df_group[tb].mean() * 1.3)
                    colour_list_df_inner.append(mcolors.to_hex(cmap_sum(normalize(value))))
            else:
                colour_list_df_inner.append('#ffffff')
        colour_list_df.append(colour_list_df_inner)
    return colour_list_df

# Modified pitch_table to pass pitcher_name, team, and season
def pitch_table(df: pd.DataFrame, ax: plt.Axes, pitcher_name: str, team: str, season: int, fontsize: int = 12):
    df_group, colour_list = df_grouping(df, pitcher_name, team, season)
    if df_group.empty:
        ax.text(0.5, 0.5, 'No pitch data available', ha='center', va='center', fontsize=12)
        ax.axis('off')
        return

    colour_list_df = get_cell_colours(df_group)
    df_plot = plot_pitch_format(df_group)

    table_plot = ax.table(cellText=df_plot.values, colLabels=table_columns, cellLoc='center',
                          bbox=[0, -0.1, 1, 1], colWidths=[0.15] + [0.07] * (len(table_columns) - 1),
                          cellColours=colour_list_df)

    table_plot.auto_set_font_size(False)
    table_plot.set_fontsize(fontsize)
    table_plot.scale(1, 0.5)

    new_column_names = ['$\\bf{Pitch\\ Name}$'] + [pitch_stats_dict.get(x, {'table_header': x})['table_header'] for x in table_columns[1:]]
    for i, col_name in enumerate(new_column_names):
        table_plot.get_celld()[(0, i)].get_text().set_text(col_name)

    for i in range(len(df_plot)):
        cell = table_plot.get_celld()[(i + 1, 0)]
        cell.get_text().set_fontweight('bold')
        if i < len(colour_list):
            cell.set_facecolor(colour_list[i])
            cell.set_text_props(color='#000000' if df_plot.iloc[i]['pitch_description'] in ['CHANGEUP', 'SLIDER', 'CUTTER'] else '#FFFFFF')

    ax.axis('off')

# Modified pitching_dashboard to pass pitcher_name, team, and season to pitch_table
def pitching_dashboard(df: pd.DataFrame, stats: list, pitcher_name: str, team: str, season: int, data_path: str):
    if df.empty:
        print(f"No data found for pitcher {pitcher_name} from {team} in {season}. Exiting dashboard creation.")
        return

    fig = plt.figure(figsize=(20, 20))

    gs = gridspec.GridSpec(6, 8,
                           height_ratios=[2, 20, 9, 36, 36, 7],
                           width_ratios=[1, 18, 18, 18, 18, 18, 18, 1])

    ax_headshot = fig.add_subplot(gs[1, 1:3])
    ax_bio = fig.add_subplot(gs[1, 3:5])
    ax_logo = fig.add_subplot(gs[1, 5:7])
    ax_season_table = fig.add_subplot(gs[2, 1:7])
    ax_plot_1 = fig.add_subplot(gs[3, 1:3])
    ax_plot_2 = fig.add_subplot(gs[3, 3:5])
    ax_plot_3 = fig.add_subplot(gs[3, 5:7])
    ax_table = fig.add_subplot(gs[4, 1:7])
    ax_footer = fig.add_subplot(gs[-1, 1:7])
    ax_header = fig.add_subplot(gs[0, 1:7])
    ax_left = fig.add_subplot(gs[:, 0])
    ax_right = fig.add_subplot(gs[:, -1])

    # Add bold title in the header
    ax_header.text(0.5, 0.5, f"{pitcher_name} - {team} - May 28, 2025",
                   ha='center', va='center', fontsize=24, fontweight='bold')
    ax_header.axis('off')

    ax_footer.axis('off')
    ax_headshot.axis('off')
    ax_bio.axis('off')
    ax_logo.axis('off')
    ax_left.axis('off')
    ax_right.axis('off')

    fontsize = 16
    local_pitcher_stats_table(pitcher_name, team, season, ax_season_table, stats, fontsize=20, data_path=data_path)
    strike_zone_plot(df, ax_plot_1, pitcher_name, 'Left', 'Pitch Locations vs LHH')
    break_plot(df, ax_plot_2, pitcher_name)
    strike_zone_plot(df, ax_plot_3, pitcher_name, 'Right', 'Pitch Locations vs RHH')
    pitch_table(df, ax_table, pitcher_name, team, season, fontsize=fontsize)

    ax_footer.text(0, 1, 'By: Max Q', ha='left', va='top', fontsize=24)
    ax_footer.text(0.5, 1, 'Pitch Matrix Colour Coding Compares to League Average', ha='center', va='top', fontsize=16)
    ax_footer.text(1, 1, 'Data: MLB, Fangraphs\nImages: MLB, ESPN', ha='right', va='top', fontsize=24)

    plt.tight_layout()
    filename = f"./KCL/cards/{pitcher_name.replace(', ', '_')}_pitching_dashboard.png"
    os.makedirs(os.path.dirname(filename), exist_ok=True)
    plt.savefig(filename, bbox_inches='tight', dpi=300)
    plt.close()

# Main script with updated for loop
data_path = 'KCL\Data\modified_yakkertech_file.csv'
stats = ['IP', 'P', 'R', 'H', 'BB', 'K']  # Updated stats for box score
season = 2025

# Load the Yakkertech DataFrame
dtypes = {
    'Pitcher': str,
    'PitcherTeam': str,
    'TaggedPitchType': str,
    'RelSpeed': float,
    'HorzBreak': float,
    'InducedVertBreak': float,
    'SpinRate': float,
    'RelSide': float,
    'RelHeight': float,
    'Extension': float,
    'Swing?': float,
    'Swing Strike?': float,
    'Strike?': float,
    'Chase?': float,
    'PitchCall': str,
    'PlayResult': str,
    'Date': str,
    'BatterSide': str,
    'PlateLocSide': float,
    'PlateLocHeight': float,
    'OutsOnPlay': float,
    'RunsScored': float,
    'KorBB': str,
    'VerticleArmAngle': float
}

try:
    df = pd.read_csv(data_path, dtype=dtypes)
except ValueError as e:
    print(f"Error reading CSV with specified dtypes: {e}")
    print("Falling back to low_memory=False")
    df = pd.read_csv(data_path, low_memory=False)

# Convert Date to datetime
df['Date'] = pd.to_datetime(df['Date'], errors='coerce')

# Extract unique pitchers and their most recent team
pitcher_teams = (df.sort_values('Date', ascending=False)
                 .groupby('Pitcher')
                 .agg({'PitcherTeam': 'first'})
                 .reset_index())
pitcher_teams.columns = ['Pitcher', 'PitcherTeam']

# For loop to run pitching_dashboard for each pitcher
for _, row in pitcher_teams.iterrows():
    pitcher_name = row['Pitcher']
    team = row['PitcherTeam']
    
    # Filter DataFrame for the current pitcher and season
    pitcher_df = df[(df['Pitcher'] == pitcher_name) & (df['PitcherTeam'] == team) & (df['Date'].dt.year == season)]
    
    # Call the pitching_dashboard function
    try:
        pitching_dashboard(pitcher_df, stats, pitcher_name, team, season, data_path)
        print(f"Dashboard generated for {pitcher_name} ({team})")
    except Exception as e:
        print(f"Error generating dashboard for {pitcher_name}: {e}")

print("All dashboards generated.")