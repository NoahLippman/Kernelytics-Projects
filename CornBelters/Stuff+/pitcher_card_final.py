import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
import pandas as pd
import seaborn as sns
import matplotlib.colors as mcolors
import numpy as np
from matplotlib.ticker import FuncFormatter
from matplotlib.patches import Rectangle

# Define the stats dictionary for the new metrics
stats_dict = {
    'K_LHH': {'table_header': '$\\bf{K\\ vs\\ LHH}$', 'format': '.0f'},
    'K_RHH': {'table_header': '$\\bf{K\\ vs\\ RHH}$', 'format': '.0f'},
    'BB_LHH': {'table_header': '$\\bf{BB\\ vs\\ LHH}$', 'format': '.0f'},
    'BB_RHH': {'table_header': '$\\bf{BB\\ vs\\ RHH}$', 'format': '.0f'},
    'GB%_LHH': {'table_header': '$\\bf{GB\%\\ vs\\ LHH}$', 'format': '.1%'},
    'GB%_RHH': {'table_header': '$\\bf{GB\%\\ vs\\ RHH}$', 'format': '.1%'},
    'FB%_LHH': {'table_header': '$\\bf{FB\%\\ vs\\ LHH}$', 'format': '.1%'},
    'FB%_RHH': {'table_header': '$\\bf{FB\%\\ vs\\ RHH}$', 'format': '.1%'}
}

# Function to compute pitching stats from local data
def local_pitching_stats(pitcher_name: str, team: str, season: int, data_path: str):
    dtypes = {
        'Pitcher': str,
        'PitcherTeam': str,
        'AutoPitchType': str,
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
        'PlateLocHeight': float
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

    # Split by batter side
    df_lhh = pitcher_data[pitcher_data['BatterSide'] == 'Left']
    df_rhh = pitcher_data[pitcher_data['BatterSide'] == 'Right']

    # Calculate metrics for LHH
    k_lhh = df_lhh[df_lhh['KorBB'] == 'Strikeout'].shape[0]
    bb_lhh = df_lhh[df_lhh['KorBB'] == 'Walk'].shape[0]
    groundouts_lhh = df_lhh[df_lhh['TaggedHitType'] == 'GroundBall'].shape[0]
    flyouts_lhh = df_lhh[df_lhh['TaggedHitType'] == 'FlyBall'].shape[0]
    total_bip_lhh = groundouts_lhh + flyouts_lhh + df_lhh[df_lhh['PlayResult'].isin(['Single', 'Double', 'Triple', 'HomeRun'])].shape[0]
    gb_percent_lhh = groundouts_lhh / total_bip_lhh if total_bip_lhh > 0 else 0.0
    fb_percent_lhh = flyouts_lhh / total_bip_lhh if total_bip_lhh > 0 else 0.0

    # Calculate metrics for RHH
    k_rhh = df_rhh[df_rhh['KorBB'] == 'Strikeout'].shape[0]
    bb_rhh = df_rhh[df_rhh['KorBB'] == 'Walk'].shape[0]
    groundouts_rhh = df_rhh[df_rhh['TaggedHitType'] == 'GroundBall'].shape[0]
    flyouts_rhh = df_rhh[df_rhh['TaggedHitType'] == 'FlyBall'].shape[0]
    total_bip_rhh = groundouts_rhh + flyouts_rhh + df_rhh[df_rhh['PlayResult'].isin(['Single', 'Double', 'Triple', 'HomeRun'])].shape[0]
    gb_percent_rhh = groundouts_rhh / total_bip_rhh if total_bip_rhh > 0 else 0.0
    fb_percent_rhh = flyouts_rhh / total_bip_rhh if total_bip_rhh > 0 else 0.0

    stats = {
        'K_LHH': k_lhh,
        'K_RHH': k_rhh,
        'BB_LHH': bb_lhh,
        'BB_RHH': bb_rhh,
        'GB%_LHH': gb_percent_lhh,
        'GB%_RHH': gb_percent_rhh,
        'FB%_LHH': fb_percent_lhh,
        'FB%_RHH': fb_percent_rhh
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

# Function to plot strike zone for LHH or RHH
def strike_zone_plot(df: pd.DataFrame, ax: plt.Axes, pitcher_name: str, batter_side: str, title: str):
    df = df.rename(columns={
        'HorzBreak': 'pfx_x',
        'InducedVertBreak': 'pfx_z',
        'AutoPitchType': 'pitch_type',
        'PitcherThrows': 'p_throws',
        'PlateLocSide': 'plate_x',
        'PlateLocHeight': 'plate_z'
    })

    pitch_mapping = {
        'Four-Seam': '4-SEAM FASTBALL',
        'Sinker': 'SINKER',
        'Curveball': 'CURVEBALL',
        'Changeup': 'CHANGEUP',
        'Sweeper': 'SWEEPER',
        'Slider': 'SLIDER',
        'Cutter': 'CUTTER',
        'Splitter': 'SPLITTER',
        'Undefined': 'UNKNOWN'
    }

    # Handle NaN values by converting to 'UNKNOWN'
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
        'UNKNOWN': 'gray'
    }

    missing_pitches = set(df['pitch_type']) - set(dict_colour.keys())
    if missing_pitches:
        raise ValueError(f"The palette dictionary is missing keys: {missing_pitches}")

    font_properties = {'fontsize': 10}
    font_properties_axes = {'fontsize': 12, 'weight': 'bold'}
    font_properties_titles = {'fontsize': 14, 'weight': 'bold'}

    # Filter by batter side
    df_side = df[df['BatterSide'] == batter_side]
    if df_side.empty:
        ax.text(0.5, 0.5, f'No data for {batter_side} batters', ha='center', va='center', fontsize=12)
        ax.set_xlim(-2, 2)
        ax.set_ylim(0, 4)
        ax.set_aspect('equal', adjustable='box')
        ax.axis('off')
        return

    # Plot pitches
    sns.scatterplot(ax=ax,
                    x=df_side['plate_x'],
                    y=df_side['plate_z'],
                    hue=df_side['pitch_type'],
                    palette=dict_colour,
                    ec='black',
                    alpha=0.8,
                    zorder=2)

    # Draw strike zone
    strike_zone = Rectangle((-0.85, 1.5), 1.7, 2.0, fill=False, edgecolor='black', linewidth=2, zorder=1)
    ax.add_patch(strike_zone)

    # Set labels and title
    ax.set_xlabel('Horizontal Location (ft, Catcher\'s View)', fontdict=font_properties_axes)
    ax.set_ylabel('Vertical Location (ft)', fontdict=font_properties_axes)
    ax.set_title(f"{title}\n{pitcher_name}", fontdict=font_properties_titles)

    # Set axis limits
    ax.set_xlim(-2, 2)
    ax.set_ylim(0, 4)

    # Set equal aspect ratio
    ax.set_aspect('equal', adjustable='box')

    # Format ticks
    ax.xaxis.set_major_formatter(FuncFormatter(lambda x, _: int(x) if x.is_integer() else x))
    ax.yaxis.set_major_formatter(FuncFormatter(lambda x, _: int(x) if x.is_integer() else x))

    # Remove legend
    ax.get_legend().remove()

# Break Plot Function
def break_plot(df: pd.DataFrame, ax: plt.Axes, pitcher_name: str):
    df = df.rename(columns={
        'HorzBreak': 'pfx_x',
        'InducedVertBreak': 'pfx_z',
        'AutoPitchType': 'pitch_type',
        'PitcherThrows': 'p_throws'
    })

    pitch_mapping = {
        'Four-Seam': '4-SEAM FASTBALL',
        'Sinker': 'SINKER',
        'Curveball': 'CURVEBALL',
        'Changeup': 'CHANGEUP',
        'Sweeper': 'SWEEPER',
        'Slider': 'SLIDER',
        'Cutter': 'CUTTER',
        'Splitter': 'SPLITTER',
        'FourSeamFastBall': '4-SEAM FASTBALL',
        'Undefined': 'UNKNOWN'
    }

    # Handle NaN values by converting to 'UNKNOWN'
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
        'UNKNOWN': 'gray'
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

    # Remove legend
    ax.get_legend().remove()

def df_grouping(df: pd.DataFrame):
    df = df.rename(columns={
        'AutoPitchType': 'pitch_type',
        'RelSpeed': 'release_speed',
        'HorzBreak': 'pfx_x',
        'InducedVertBreak': 'pfx_z',
        'SpinRate': 'release_spin_rate',
        'RelSide': 'release_pos_x',
        'RelHeight': 'release_pos_z',
        'Extension': 'release_extension',
        'Stuff+': 'stuff+',  # Ensure consistent column name
        'xWhiff' : 'xWhiff'
    })

    pitch_mapping = {
        'Four-Seam': '4-SEAM FASTBALL',
        'Sinker': 'SINKER',
        'Curveball': 'CURVEBALL',
        'Changeup': 'CHANGEUP',
        'Sweeper': 'SWEEPER',
        'Slider': 'SLIDER',
        'Cutter': 'CUTTER',
        'Undefined': 'UNKNOWN'
    }

    # Handle NaN values by converting to 'UNKNOWN'
    df['pitch_type'] = df['pitch_type'].fillna('UNKNOWN').astype(str).map(pitch_mapping).fillna('UNKNOWN')

    df_group = df.groupby('pitch_type').agg(
        pitch=('pitch_type', 'count'),
        release_speed=('release_speed', 'mean'),
        max_speed = ('release_speed', 'max'),
        pfx_z=('pfx_z', 'mean'),
        pfx_x=('pfx_x', 'mean'),
        release_spin_rate=('release_spin_rate', 'mean'),
        release_pos_x=('release_pos_x', 'mean'),
        release_pos_z=('release_pos_z', 'mean'),
        release_extension=('release_extension', 'mean'),
        swing=('Swing?', 'sum'),
        whiff=('Swing Strike?', 'sum'),
        in_zone=('Strike?', 'sum'),
        chase=('Chase?', 'sum'),
        stuffplus=('stuff+', 'mean'),  # Use mean for Stuff+
        expwhiff=('xWhiff','mean')
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
        'CUTTER': 'yellow',
        'UNKNOWN': 'gray'
    }
    df_group['colour'] = df_group['pitch_type'].map(dict_colour).fillna('gray')

    df_group = df_group.sort_values(by='pitch_usage', ascending=False)
    colour_list = df_group['colour'].tolist()
    colour_list = ['gray' if pd.isna(col) else col for col in colour_list]

    plot_table_all = pd.DataFrame(data={
        'pitch_type': 'All',
        'pitch_description': 'All',
        'pitch': df['pitch_type'].count(),
        'pitch_usage': 1.0,
        'release_speed': df['release_speed'].mean(),
        'max_speed': df['release_speed'].max(),
        'pfx_z': df['pfx_z'].mean(),
        'pfx_x': df['pfx_x'].mean(),
        'release_spin_rate': df['release_spin_rate'].mean(),
        'release_pos_x': df['release_pos_x'].mean(),
        'release_pos_z': df['release_pos_z'].mean(),
        'release_extension': df['release_extension'].mean(),
        'swing': df['Swing?'].sum(),
        'whiff': df['Swing Strike?'].sum(),
        'in_zone': df['Strike?'].sum(),
        'chase': df['Chase?'].sum(),
        'whiff_rate': df['Swing Strike?'].sum() / df['Swing?'].sum() if df['Swing?'].sum() > 0 else np.nan,
        'in_zone_rate': df['Strike?'].sum() / df['pitch_type'].count(),
        'chase_rate': df['Chase?'].sum() / df['pitch_type'].count(),
        'stuffplus': df['stuff+'].mean(),  # Calculate mean Stuff+ for "All"
        'expwhiff' : df['xWhiff'].mean()
    }, index=[0])

    df_plot = pd.concat([df_group, plot_table_all], ignore_index=True)
    return df_plot, colour_list

pitch_stats_dict = {
    'pitch': {'table_header': '$\\bf{Count}$', 'format': '.0f'},
    'pitch_usage': {'table_header': '$\\bf{Pitch\%}$', 'format': '.1%'},
    'release_speed': {'table_header': '$\\bf{Velocity}$', 'format': '.1f'},
    'max_speed': {'table_header': '$\\bf{Max Velo}$', 'format': '.1f'},
    'pfx_z': {'table_header': '$\\bf{iVB}$', 'format': '.1f'},
    'pfx_x': {'table_header': '$\\bf{HB}$', 'format': '.1f'},
    'release_spin_rate': {'table_header': '$\\bf{Spin}$', 'format': '.0f'},
    'release_pos_x': {'table_header': '$\\bf{hRel}$', 'format': '.1f'},
    'release_pos_z': {'table_header': '$\\bf{vRel}$', 'format': '.1f'},
    'release_extension': {'table_header': '$\\bf{Ext.}$', 'format': '.1f'},
    'whiff_rate': {'table_header': '$\\bf{Whiff\%}$', 'format': '.1%'},
    'in_zone_rate': {'table_header': '$\\bf{Zone\%}$', 'format': '.1%'},
    'chase_rate': {'table_header': '$\\bf{Chase\%}$', 'format': '.1%'},
    'stuffplus': {'table_header': '$\\bf{Stuff+}$', 'format': '.1f'},
    'expwhiff': {'table_header': '$\\bf{xWhiff}$', 'format': '.1f'}  # Updated format
}

table_columns = [
    'pitch_description',
    'pitch',
    'pitch_usage',
    'release_speed',
    'max_speed',
    'pfx_z',
    'pfx_x',
    'release_spin_rate',
    'release_pos_x',
    'release_pos_z',
    'release_extension',
    'whiff_rate',
    'in_zone_rate',
    'chase_rate',
    'stuffplus',  # Updated to match df_grouping
    'expwhiff'
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

def pitch_table(df: pd.DataFrame, ax: plt.Axes, fontsize: int = 12):
    df_group, colour_list = df_grouping(df)
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

# Modified pitching_dashboard to handle data_path and empty DataFrame
def pitching_dashboard(df: pd.DataFrame, stats: list, pitcher_name: str, team: str, season: int, data_path: str):
    if df.empty:
        print(f"No data found for pitcher {pitcher_name} from {team} in {season}. Exiting dashboard creation.")
        return

    fig = plt.figure(figsize=(20, 20))

    gs = gridspec.GridSpec(6, 8,
                           height_ratios=[2,20,9,36,36,7],
                           width_ratios=[1,18,18,18,18,18,18,1])

    ax_headshot = fig.add_subplot(gs[1,1:3])
    ax_bio = fig.add_subplot(gs[1,3:5])
    ax_logo = fig.add_subplot(gs[1,5:7])
    ax_season_table = fig.add_subplot(gs[2,1:7])
    ax_plot_1 = fig.add_subplot(gs[3,1:3])
    ax_plot_2 = fig.add_subplot(gs[3,3:5])
    ax_plot_3 = fig.add_subplot(gs[3,5:7])
    ax_table = fig.add_subplot(gs[4,1:7])
    ax_footer = fig.add_subplot(gs[-1,1:7])
    ax_header = fig.add_subplot(gs[0,1:7])
    ax_left = fig.add_subplot(gs[:,0])
    ax_right = fig.add_subplot(gs[:,-1])

    ax_footer.axis('off')
    ax_header.axis('off')
    ax_left.axis('off')
    ax_right.axis('off')
    ax_headshot.axis('off')
    ax_bio.axis('off')
    ax_logo.axis('off')

    fontsize = 16
    local_pitcher_stats_table(pitcher_name, team, season, ax_season_table, stats, fontsize=20, data_path=data_path)
    strike_zone_plot(df, ax_plot_1, pitcher_name, 'Left', 'Pitch Locations vs LHH')
    break_plot(df, ax_plot_2, pitcher_name)
    strike_zone_plot(df, ax_plot_3, pitcher_name, 'Right', 'Pitch Locations vs RHH')
    pitch_table(df, ax_table, fontsize=fontsize)

    ax_footer.text(0, 1, 'By: Max Q', ha='left', va='top', fontsize=24)
    ax_footer.text(0.5, 1, 'Pitch Matrix Colour Coding Compares to League Average', ha='center', va='top', fontsize=16)
    ax_footer.text(1, 1, 'Data: MLB, Fangraphs\nImages: MLB, ESPN', ha='right', va='top', fontsize=24)

    plt.tight_layout()
    filename = f"./cards/{pitcher_name.replace(', ', '_')}_pitching_dashboard.png"
    plt.savefig(filename, bbox_inches='tight', dpi=300)
    plt.close()

# Example usage
stats = ['K_LHH', 'K_RHH', 'BB_LHH', 'BB_RHH', 'GB%_LHH', 'GB%_RHH', 'FB%_LHH', 'FB%_RHH']
season = 2025
data_path = 'CornBelters/Stuff+/2025wStuff+.csv'  # Specify your CSV file here
dtypes = {
    'Pitcher': str,
    'PitcherTeam': str,
    'AutoPitchType': str,
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
    'PitcherThrows': str,
    'PitchCall': str,
    'PlayResult': str,
    'Date': str,
    'BatterSide': str,
    'PlateLocSide': float,
    'PlateLocHeight': float,
    'stuff+' : float,
    'xWhiff' : float
}
try:
    df = pd.read_csv(data_path, dtype=dtypes)
except ValueError as e:
    print(f"Error reading CSV with specified dtypes: {e}")
    print("Falling back to low_memory=False")
    df = pd.read_csv(data_path, low_memory=False)

# Convert Date to datetime
df['Date'] = pd.to_datetime(df['Date'], errors='coerce')

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