import pandas as pd
import numpy as np 
import matplotlib.pyplot as plt
import seaborn as sns
import glob

def plot_pitch_velocity_with_line(pitcher_name, df,date):

    # Load the dataset
    
    # Filter data for the specified pitcher
    pitcher_data = df[df['Pitcher'] == pitcher_name]
    
    if pitcher_data.empty:
        print(f"No data found for pitcher: {pitcher_name}")
        return
    
    # Get unique pitch types for this pitcher
    pitch_types = pitcher_data['TaggedPitchType'].unique()
    
    if len(pitch_types) == 0:
        print(f"No pitch types found for pitcher: {pitcher_name}")
        return
    
    plt.figure(figsize=(10, 7))
    
    # Plot each pitch type on the same figure
    for pitch_type in pitch_types:
        pitch_type_data = pitcher_data[pitcher_data['TaggedPitchType'] == pitch_type].sort_values('PitchNo')
        if pitch_type_data.empty:
            continue
        pitch_type_data = pitch_type_data.copy()
        pitch_type_data['PitchTypeCount'] = range(1, len(pitch_type_data) + 1)
        
        plt.scatter(
            pitch_type_data['PitchTypeCount'],
            pitch_type_data['RelSpeed'],
            alpha=0.6,
            s=50,
            label=f'{pitch_type} Pitches'
        )
        plt.plot(
            pitch_type_data['PitchTypeCount'],
            pitch_type_data['RelSpeed'],
            linestyle='-',
            linewidth=1
        )
    
    plt.title(f'{pitcher_name} - Velocity vs. Pitch Type Count (All Pitches)')
    plt.xlabel('Pitch Count (per type)')
    plt.ylabel('Release Velocity (mph)')
    plt.legend()
    plt.grid(True, linestyle='--', alpha=0.7)
    plt.tight_layout()
    plt.savefig(f'CornBelters/velocity/{date}/{pitcher_name}_velocity_with_line.png', dpi=300, bbox_inches='tight')
    plt.close()


def plot_pitch_usage(pitcher_name, df, date):
    counts = ['0-0', '0-1', '0-2', '1-0', '1-1', '1-2', '2-0', '2-1', '2-2', '3-0', '3-1', '3-2']
    df = df.copy()  # Ensure we're not modifying a slice
    df.loc[:, 'Count'] = df['Balls'].astype(str) + '-' + df['Strikes'].astype(str)
    # Filter for the pitcher
    df_pitcher = df[df['Pitcher'] == pitcher_name]

    # Pivot table: index=pitch type, columns=count, values=counts
    usage_table = pd.pivot_table(
        df_pitcher,
        index='TaggedPitchType',
        columns='Count',
        values='PitchNo',
        aggfunc='count',
        fill_value=0
    )

    # Calculate usage % for each count (column)
    usage_pct = usage_table.div(usage_table.sum(axis=0), axis=1) * 100

    # Reindex columns to ensure all counts are present and in order
    usage_pct = usage_pct.reindex(columns=counts, fill_value=0)

    # Sort pitch types alphabetically
    usage_pct = usage_pct.sort_index()
    # Plot the usage percentage table as a heatmap
    plt.figure(figsize=(12, 6))
    sns.heatmap(usage_pct, annot=True, fmt=".1f", cmap="coolwarm", cbar_kws={'label': 'Usage %'})
    plt.title(f"Pitch Usage % by Count for {pitcher_name}")
    plt.ylabel("Pitch Type")
    plt.xlabel("Count")
    plt.tight_layout()
    plt.savefig(f'CornBelters/usage/{date}/{pitcher_name}_pitch_usage.png', dpi=300, bbox_inches='tight')


def calculate_pitcher_metrics(df,pitcher_name):
    """
    Calculate key metrics for a given pitcher.
    Metrics: fastball velo, chase%, whiff%, k%, bb%, barrel%, hard-hit%, gb%, extension%
    """
    pitcher_df = df[df['Pitcher'] == pitcher_name].copy()
    metrics = {}

    # Fastball velo (mean RelSpeed for fastball, sinker, cutter)
    fastballs = pitcher_df[pitcher_df['TaggedPitchType'].isin(['Fastball', 'Sinker', 'Cutter'])]
    metrics['Fastball Velo'] = fastballs['RelSpeed'].mean()

    # Chase% (Chase? == 1 or True)
    if 'Chase?' in pitcher_df.columns:
        chase_n = pitcher_df['Chase?'].sum()
        chase_d = pitcher_df['Chase?'].count()
        metrics['Chase%'] = 100 * chase_n / chase_d if chase_d > 0 else np.nan
    else:
        metrics['Chase%'] = np.nan

    # Whiff% (Swing? == 1 and Swing Strike? == 1)
    if 'Swing?' in pitcher_df.columns and 'Swing Strike?' in pitcher_df.columns:
        swings = pitcher_df[pitcher_df['Swing?'] == 1]
        whiffs = swings['Swing Strike?'].sum()
        swings_n = swings['Swing?'].count()
        metrics['Whiff%'] = 100 * whiffs / swings_n if swings_n > 0 else np.nan
    else:
        metrics['Whiff%'] = np.nan

    # K% (PlayResult == 'Strikeout')
    if 'KorBB' in pitcher_df.columns:
        k_n = (pitcher_df['KorBB'] == 'Strikeout').sum()
        k_d = pitcher_df['PlayResult'].notna().sum()
        metrics['K%'] = 100 * k_n / k_d if k_d > 0 else np.nan
    else:
        metrics['K%'] = np.nan

    # BB% (PlayResult == 'Walk')
    if 'KorBB' in pitcher_df.columns:
        bb_n = (pitcher_df['KorBB'] == 'Walk').sum()
        bb_d = pitcher_df['PlayResult'].notna().sum()
        metrics['BB%'] = 100 * bb_n / bb_d if bb_d > 0 else np.nan
    else:
        metrics['BB%'] = np.nan

    # Barrel% (Barrel == 1 or True)
    # Barrel% (ExitSpeed > 90 and 26 <= Angle <= 30)
    if 'ExitSpeed' in pitcher_df.columns and 'Angle' in pitcher_df.columns:
        batted_balls = pitcher_df[pitcher_df['ExitSpeed'].notna() & pitcher_df['Angle'].notna()]
        barrel_mask = (batted_balls['ExitSpeed'] > 90) & (batted_balls['Angle'] >= 26) & (batted_balls['Angle'] <= 30)
        barrel_n = barrel_mask.sum()
        barrel_d = len(batted_balls)
        metrics['Barrel%'] = 100 * barrel_n / barrel_d if barrel_d > 0 else np.nan
    else:
        metrics['Barrel%'] = np.nan

    # Hard-Hit% (ExitSpeed >= 95)
    if 'ExitSpeed' in pitcher_df.columns:
        hardhit_n = (pitcher_df['ExitSpeed'] >= 92).sum()
        hardhit_d = pitcher_df['ExitSpeed'].notna().sum()
        metrics['Hard-Hit%'] = 100 * hardhit_n / hardhit_d if hardhit_d > 0 else np.nan
    else:
        metrics['Hard-Hit%'] = np.nan

    # GB% (Ground Ball? == 1 or True)
    if 'Ground Ball?' in pitcher_df.columns:
        gb_n = pitcher_df['Ground Ball?'].sum()
        gb_d = pitcher_df['Ground Ball?'].count()
        metrics['GB%'] = 100 * gb_n / gb_d if gb_d > 0 else np.nan
    else:
        metrics['GB%'] = np.nan


    return metrics


def plot_pitcher_percentiles(df, pitcher_name, save_path=None):
    """
    Plots percentile rankings for a pitcher compared to league for key metrics.
    Bubble color is from coolwarm colormap by percentile.
    The bubble shows the percentile, and the actual value is shown as a column to the right.
    """
    import matplotlib.pyplot as plt
    import matplotlib as mpl
    from scipy.stats import percentileofscore

    # Metrics in desired order and whether higher is better
    metric_info = [
        ('Fastball Velo', True),
        ('Avg Exit Velocity', False),
        ('Chase%', True),
        ('Whiff%', True),
        ('K%', True),
        ('BB%', False),
        ('Barrel%', False),
        ('Hard-Hit%', False),
        ('GB%', True)
    ]

    # Calculate pitcher metrics
    pitcher_metrics = calculate_pitcher_metrics(df, pitcher_name)
    # Add Avg Exit Velocity calculation if not present
    if 'Avg Exit Velocity' not in pitcher_metrics:
        if 'ExitSpeed' in df.columns:
            pitcher_metrics['Avg Exit Velocity'] = df[df['Pitcher'] == pitcher_name]['ExitSpeed'].mean()
        else:
            pitcher_metrics['Avg Exit Velocity'] = np.nan

    # Calculate league metrics for each pitcher
    league_pitchers = df['Pitcher'].dropna().unique()
    league_metrics = {m[0]: [] for m in metric_info}
    for p in league_pitchers:
        m = calculate_pitcher_metrics(df, p)
        # Add Avg Exit Velocity for each pitcher
        if 'ExitSpeed' in df.columns:
            m['Avg Exit Velocity'] = df[df['Pitcher'] == p]['ExitSpeed'].mean()
        else:
            m['Avg Exit Velocity'] = np.nan
        for k, _ in metric_info:
            league_metrics[k].append(m[k])

    # Calculate percentiles
    percentiles = []
    for metric, higher_is_better in metric_info:
        league_vals = np.array(league_metrics[metric])
        league_vals = league_vals[~np.isnan(league_vals)]
        player_val = pitcher_metrics[metric]
        if np.isnan(player_val) or len(league_vals) == 0:
            percentiles.append(np.nan)
            continue
        pct = percentileofscore(league_vals, player_val, kind='rank')
        if not higher_is_better:
            pct = 100 - pct
        percentiles.append(pct)

    # Plot
    fig, ax = plt.subplots(figsize=(10, 6))
    metrics = [m[0] for m in metric_info]
    cmap = plt.get_cmap('coolwarm')
    norm = mpl.colors.Normalize(vmin=0, vmax=100)
    bars = ax.barh(metrics, percentiles, color=[cmap(norm(p)) if not np.isnan(p) else '#cccccc' for p in percentiles], edgecolor='black')
    ax.set_xlim(0, 100 )  # Add space for value column
    ax.set_xlabel('Percentile (vs. League)')
    ax.set_title(f'{pitcher_name} Percentile Rankings')

    for i, (p, metric) in enumerate(zip(percentiles, metrics)):
        value = pitcher_metrics.get(metric, np.nan)
        # Draw bubble with percentile inside
        if not np.isnan(p):
            bubble_text = f"{int(round(p))}"
            ax.scatter(p, i, s=400, color=cmap(norm(p)), edgecolors='black', zorder=5)
            ax.text(p, i, bubble_text, va='center', ha='center', color='black', fontsize=9, zorder=6, fontweight='bold')
        # Draw value column to the right
        if not np.isnan(value):
            # Format value for display
            if '%' in metric:
                value_text = f"{value:.1f}%"
            elif 'Velo' in metric or 'Velocity' in metric:
                value_text = f"{value:.1f}"
            else:
                value_text = f"{value:.2f}"
            ax.text(104, i, value_text, va='center', ha='left', color='black', fontsize=9, fontweight='bold')

    # Draw a vertical separator line for the value column
    ax.axvline(102, color='gray', linestyle='--', lw=1)
    # Remove y-axis line on the right for a cleaner look
    ax.spines['right'].set_visible(False)

    plt.tight_layout()
    plt.savefig(f'CornBelters/percentiles/{pitcher_name}_percentiles.png', dpi=300, bbox_inches='tight')
    plt.close()
# Example usage:
# df = pd.read_csv('CornBelters/pitching_dashboard/Data/2025.csv')

def plot_pitcher_tilt(pitcher_name, df):
    # Calculate angles for each tilt
    df['Angle'] = df['Tilt'].apply(time_to_angle)  # Assuming time_to_angle is defined

    # Set up polar plot
    fig, ax = plt.subplots(subplot_kw={'projection': 'polar'})

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
    plt.title('Pitches by Tilt (Clock Time), Spin Rate (Radial Distance), and Pitch Type (Color)')

    # Show plot
    plt.show()
# Function to map clock time (HH:MM) to angle (in radians)
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
        print(f"Invalid time format: {time_str}. Defaulting to 12:00.")
        return 0  # Default to 12:00
