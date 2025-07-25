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
    'P': {'table_header': '$\\bf{P}$', 'format': '.0f'},     # Number of Pitches
    'R': {'table_header': '$\\bf{R}$', 'format': '.0f'},     # Runs
    'H': {'table_header': '$\\bf{H}$', 'format': '.0f'},     # Hits
    'BB': {'table_header': '$\\bf{BB}$', 'format': '.0f'},   # Walks
    'K': {'table_header': '$\\bf{K}$', 'format': '.0f'}      # Strikeouts
}

# Modified local_pitching_stats to use provided DataFrame
def local_pitching_stats(pitcher_data: pd.DataFrame):
    if pitcher_data.empty:
        print("No data found for the selected pitcher and date")
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

# Modified local_pitcher_stats_table to use provided DataFrame and avoid dtype mismatch
def local_pitcher_stats_table(pitcher_data: pd.DataFrame, ax: plt.Axes, stats: list, fontsize: int):
    df_stats = local_pitching_stats(pitcher_data)

    if df_stats.empty:
        print("No stats to display")
        return

    df_stats = df_stats[stats].reset_index(drop=True)

    # Create a list of formatted values for display, keeping df_stats numeric
    display_values = [
        format(df_stats[x][0], stats_dict[x]['format'])
        if not pd.isna(df_stats[x][0])
        else '---'
        for x in stats
    ]

    table_fg = ax.table(
        cellText=[display_values],  # Use formatted strings for display
        colLabels=stats,
        cellLoc='center',
        bbox=[0.00, 0.0, 1, 1]
    )

    table_fg.set_fontsize(fontsize)

    new_column_names = [stats_dict[x]['table_header'] for x in stats]
    for i, col_name in enumerate(new_column_names):
        table_fg.get_celld()[(0, i)].get_text().set_text(col_name)

    ax.axis('off')

# [UNCHANGED] strike_zone_plot function
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
        'Undefined': 'UNKNOWN'
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
        'UNKNOWN': 'gray'
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
        ax.set_xlim(-2, 2)
        ax.set_ylim(0, 4)
        ax.set_aspect('equal', adjustable='box')
        ax.axis('off')
        return

    grouped = df_side.groupby('pitch_type').agg({
        'plate_x': 'mean',
        'plate_z': 'mean',
        'pitch_type': 'count'
    }).rename(columns={'pitch_type': 'count'}).reset_index()

    sns.scatterplot(ax=ax,
                    x=grouped['plate_x'],
                    y=grouped['plate_z'],
                    hue=grouped['pitch_type'],
                    size=grouped['count'],
                    sizes=(50, 500),
                    palette=dict_colour,
                    ec='black',
                    alpha=0.8,
                    zorder=2)

    strike_zone = Rectangle((-1, 1.5), 2, 2.0, fill=False, edgecolor='black', linewidth=2, zorder=1)
    ax.add_patch(strike_zone)

    ax.set_xlabel('Horizontal Location (ft, Catcher\'s View)', fontdict=font_properties_axes)
    ax.set_ylabel('Vertical Location (ft)', fontdict=font_properties_axes)
    ax.set_title(f"{title}\n{pitcher_name}", fontdict=font_properties_titles)

    ax.set_xlim(-2, 2)
    ax.set_ylim(0, 4)
    ax.set_aspect('equal', adjustable='box')

    ax.xaxis.set_major_formatter(FuncFormatter(lambda x, _: int(x) if x.is_integer() else x))
    ax.yaxis.set_major_formatter(FuncFormatter(lambda x, _: int(x) if x.is_integer() else x))

    ax.get_legend().remove()

# [UNCHANGED] break_plot function
def break_plot(df: pd.DataFrame, ax: plt.Axes, pitcher_name: str):
    df = df.rename(columns={
        'HorzBreak': 'pfx_x',
        'InducedVertBreak': 'pfx_z',
        'TaggedPitchType': 'pitch_type',
        'PitcherThrows': 'p_throws'
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
        'Undefined': 'UNKNOWN'
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

    ax.get_legend().remove()

# [UNCHANGED] df_grouping function
def df_grouping(df: pd.DataFrame, pitcher_name: str, team: str, season: int):
    df = df.rename(columns={
        'TaggedPitchType': 'pitch_type',
        'RelSpeed': 'release_speed',
        'VertApprAngle': 'vaa',
        'HorzBreak': 'pfx_x',
        'InducedVertBreak': 'pfx_z',
        'SpinRate': 'release_spin_rate',
        'RelSide': 'release_pos_x',
        'RelHeight': 'release_pos_z',
        'Extension': 'release_extension'
    })

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
        'Splitter': 'SPLITTER',
        'Slider': 'SLIDER',
        'Cutter': 'CUTTER',
        'Undefined': 'UNKNOWN'
    }

    pitcher_data = pitcher_data.copy()
    pitcher_data['pitch_type'] = pitcher_data['pitch_type'].fillna('UNKNOWN').astype(str).map(pitch_mapping)

    df_group = pitcher_data.groupby('pitch_type').agg(
        pitch=('pitch_type', 'count'),
        release_speed=('release_speed', 'mean'),
        max_velo=('release_speed', 'max'),
        vaa=('vaa', 'mean'),
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
        'SPLITTER': 'black',
        'CUTTER': 'yellow',
        'UNKNOWN': 'gray'
    }
    df_group['colour'] = df_group['pitch_type'].map(dict_colour).fillna('gray')

    df_group = df_group.sort_values(by='pitch_usage', ascending=False)
    colour_list = df_group['colour'].tolist()
    colour_list = ['gray' if pd.isna(col) else col for col in colour_list]

    total_pitches = pitcher_data['pitch_type'].count()

    plot_table_all = pd.DataFrame(data={
        'pitch_type': 'All',
        'pitch_description': 'All',
        'pitch': total_pitches,
        'pitch_usage': 1.0,
        'release_speed': pitcher_data['release_speed'].mean(),
        'max_velo': pitcher_data['release_speed'].max(),
        'vaa': pitcher_data['vaa'].mean(),
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

# [UNCHANGED] pitch_stats_dict, table_columns, plot_pitch_format, get_cell_colours
pitch_stats_dict = {
    'pitch': {'table_header': '$\\bf{Count}$', 'format': '.0f'},
    'pitch_usage': {'table_header': '$\\bf{Pitch\%}$', 'format': '.1%'},
    'release_speed': {'table_header': '$\\bf{Velocity}$', 'format': '.1f'},
    'max_velo': {'table_header': '$\\bf{Max Velo}$', 'format': '.1f'},
    'vaa': {'table_header': '$\\bf{VAA}$', 'format': '.1f'},
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
    'max_velo',
    'vaa',
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

# [UNCHANGED] pitch_table function
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

def time_to_angle(time_str):
    try:
        # Parse HH:MM format
        hour, minute = map(int, time_str.split(':'))
        total_minutes = hour * 60 + minute
        if total_minutes >= 12 * 60:
            total_minutes -= 12 * 60  # Normalize to 0-720 minutes
        # Map to standard clock angles: 12:00 = 0°, 3:00 = 90°, 6:00 = 180°, 9:00 = 270°
        angle_deg = (total_minutes / (12 * 60)) * 360
        return np.radians(angle_deg)
    except (ValueError, AttributeError):
        return 0  # Default to 12:00
def plot_pitcher_tilt(df: pd.DataFrame, ax: plt.Axes, pitcher_name: str, team: str, season: int, full_df: pd.DataFrame):
    # Calculate angles for each tilt
    df = df.copy()
    df['Angle'] = df['Tilt'].apply(time_to_angle)  # Assuming time_to_angle is defined

    # Do NOT create a new fig, ax here!
    # fig, ax = plt.subplots(subplot_kw={'projection': 'polar'})  # REMOVE THIS LINE

    # Get unique pitch types for coloring
    pitch_types = df['TaggedPitchType'].unique()
    colors = sns.color_palette("husl", len(pitch_types))  # Distinct colors
    pitch_type_colors = dict(zip(pitch_types, colors))

    # Calculate average tilt and spin rate for each pitch type
    avg_stats = df.groupby('TaggedPitchType').agg(
        avg_tilt=('Tilt', lambda x: x.mode()[0] if not x.mode().empty else x.iloc[0]),  # Most common tilt
        avg_spinrate=('SpinRate', 'mean')
    ).reset_index()

    # Plot circles for each pitch, colored by TaggedPitchType, with SpinRate as radial distance
    for pitch_type in pitch_types:
        avg_tilt = avg_stats.loc[avg_stats['TaggedPitchType'] == pitch_type, 'avg_tilt'].values[0]
        avg_spin = avg_stats.loc[avg_stats['TaggedPitchType'] == pitch_type, 'avg_spinrate'].values[0]
        label = f"{pitch_type}\nAvg Tilt: {avg_tilt}\nAvg Spin: {avg_spin:.0f}"
        df_pitch = df[df['TaggedPitchType'] == pitch_type]
        radial = df_pitch['SpinRate'] / 3000
        sizes = 50
        ax.scatter(df_pitch['Angle'], radial, 
                   s=sizes, c=[pitch_type_colors[pitch_type]], label=label, alpha=0.7)

    # Customize the plot
    ax.set_theta_direction(-1)  # Clockwise
    ax.set_theta_zero_location('N')  # 12 o'clock at the top (0°)
    ax.set_rlim(0, .5)  # Radial limit for visibility (adjust based on SpinRate scaling)
    # Set radial labels to correspond to SpinRate
    spin_rate_labels = ['500', '1000', '1500', '2000', '2500', '3000']
    radial_ticks = [float(x) / 3000 for x in spin_rate_labels]  # Scale ticks to match SpinRate scaling
    ax.set_yticks(radial_ticks)
    ax.set_yticklabels(spin_rate_labels)

    # Set clock face labels
    key_times = ['12:00', '01:30', '03:00', '04:30', '06:00', '07:30', '09:00', '10:30']
    key_angles = [time_to_angle(t) for t in key_times]
    ax.set_xticks(key_angles)
    ax.set_xticklabels(key_times)

    ax.legend(loc='upper right', bbox_to_anchor=(1.3, 1.1))
    ax.set_title('Pitches by Tilt (Clock Time), Spin Rate (Radial Distance), and Pitch Type (Color)')
# Function to map clock time (HH:MM) to angle (in radians)


# Modified pitching_dashboard to handle "ALL" option and use provided DataFrame
def pitching_dashboard(df: pd.DataFrame, stats: list, pitcher_name: str, team: str, season: int, date: str):
    if df.empty:
        print(f"No data found for pitcher {pitcher_name} from {team} in {season} on {date}. Exiting dashboard creation.")
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


    # Set header based on date
    header_date = "All Dates" if date == "ALL" else f"{date}/2025"
    ax_header.text(0.5, 0.5, f"{pitcher_name} - {team} - {header_date}",
                   ha='center', va='center', fontsize=24, fontweight='bold')
    ax_header.axis('off')

    ax_footer.axis('off')
    ax_headshot.axis('off')
    ax_bio.axis('off')
    ax_logo.axis('off')
    ax_left.axis('off')
    ax_right.axis('off')

    fontsize = 16
    local_pitcher_stats_table(df, ax_season_table, stats, fontsize=20)
    strike_zone_plot(df, ax_plot_1, pitcher_name, 'Left', 'Pitch Locations vs LHH')
    break_plot(df, ax_plot_2, pitcher_name)
    strike_zone_plot(df, ax_plot_3, pitcher_name, 'Right', 'Pitch Locations vs RHH')
    pitch_table(df, ax_table, pitcher_name, team, season, fontsize=fontsize)

    ax_footer.text(0, 1, 'By: Max Quirk @mqstats', ha='left', va='top', fontsize=24)
    ax_footer.text(0.5, 1, 'Pitch Matrix Colour Coding Compares to League Average', ha='center', va='top', fontsize=16)
    ax_footer.text(1, 1, 'Data: Yakkertech', ha='right', va='top', fontsize=24)

    plt.tight_layout()
    # Use provided date for file path
    filename = f"CornBelters/Cards/{date}/{pitcher_name.replace(', ', '_')}_pitching.png"
    os.makedirs(os.path.dirname(filename), exist_ok=True)
    plt.savefig(filename, bbox_inches='tight', dpi=300)
    plt.close()