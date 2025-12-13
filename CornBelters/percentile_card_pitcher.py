import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
import os
import matplotlib.gridspec as gridspec
import matplotlib as mpl
from scipy.stats import percentileofscore

import pandas as pd
import numpy as np

def calculate_pitcher_metrics(df, pitcher_name):
    """
    Calculate key metrics for a given pitcher.
    Metrics: xBA, xWOBA, fastball velo, chase%, whiff%, k%, bb%, barrel%, hard-hit%, gb%
    """
    pitcher_df = df[df['Pitcher'] == pitcher_name].copy()
    metrics = {}

    # xBA and xWOBA (calculated for batted ball events where PitchCall == 'InPlay')
    batted_balls = pitcher_df[pitcher_df['PitchCall'] == 'InPlay']
    if 'xBA' in pitcher_df.columns:
        metrics['xBA'] = batted_balls['xBA'].mean() if len(batted_balls) > 0 else np.nan
    else:
        metrics['xBA'] = np.nan

    if 'xWOBA' in pitcher_df.columns:
        metrics['xWOBA'] = batted_balls['xWOBA'].mean() if len(batted_balls) > 0 else np.nan
    else:
        metrics['xWOBA'] = np.nan
    

    metrics['Stuff+'] = pitcher_df['Stuff+'].mean() if 'Stuff+' in pitcher_df.columns else np.nan
    # Fastball velo (mean RelSpeed for fastball, sinker, cutter)
    fastballs = pitcher_df[pitcher_df['TaggedPitchType'].isin(['Fastball', 'Sinker'])]
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

    # K% (KorBB == 'Strikeout')
    if 'KorBB' in pitcher_df.columns:
        k_n = (pitcher_df['KorBB'] == 'Strikeout').sum()
        k_d = pitcher_df['PlayResult'].notna().sum()
        metrics['K%'] = 100 * k_n / k_d if k_d > 0 else np.nan
    else:
        metrics['K%'] = np.nan

    # BB% (KorBB == 'Walk')
    if 'KorBB' in pitcher_df.columns:
        bb_n = (pitcher_df['KorBB'] == 'Walk').sum()
        bb_d = pitcher_df['PlayResult'].notna().sum()
        metrics['BB%'] = 100 * bb_n / bb_d if bb_d > 0 else np.nan
    else:
        metrics['BB%'] = np.nan

    # Barrel% (ExitSpeed > 88 and 25 <= Angle <= 30)
    if 'ExitSpeed' in pitcher_df.columns and 'Angle' in pitcher_df.columns:
        batted_balls = pitcher_df[pitcher_df['ExitSpeed'].notna() & pitcher_df['Angle'].notna()]
        barrel_mask = (batted_balls['ExitSpeed'] > 88) & (batted_balls['Angle'] >= 25) & (batted_balls['Angle'] <= 35)
        barrel_n = barrel_mask.sum()
        barrel_d = len(batted_balls)
        metrics['Barrel%'] = 100 * barrel_n / barrel_d if barrel_d > 0 else np.nan
    else:
        metrics['Barrel%'] = np.nan

    # Hard-Hit% (ExitSpeed >= 90)
    if 'ExitSpeed' in pitcher_df.columns:
        hardhit_n = (pitcher_df['ExitSpeed'] >= 90).sum()
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

# Function to compute pitching stats from local data
def local_pitching_stats(pitcher_name: str, df: pd.DataFrame):
    df = df.copy()  # Avoid SettingWithCopyWarning
    df['Year'] = pd.to_datetime(df['Date'], errors='coerce').dt.year
    pitcher_data = df[(df['Pitcher'] == pitcher_name)]

    if pitcher_data.empty:
        print(f"No data found for {pitcher_name}")
        return pd.DataFrame()

    # Calculate box score metrics
    ip = pitcher_data['OutsOnPlay'].sum() / 3.0  # Innings Pitched = Total Outs / 3
    runs = pitcher_data['RunsScored'].sum()      # Runs Scored (assuming this column exists and sums earned/unearned)
    hits = pitcher_data[pitcher_data['PlayResult'].isin(['Single', 'Double', 'Triple', 'HomeRun'])].shape[0]  # Hits
    walks = pitcher_data[pitcher_data['KorBB'] == 'Walk'].shape[0]  # Walks
    strikeouts = pitcher_data[pitcher_data['KorBB'] == 'Strikeout'].shape[0] # Simplified K count
    pitches = pitcher_data['TaggedPitchType'].count()
    stats = {
        'IP': float(ip),
        'P': int(pitches),
        'R': int(runs),
        'H': int(hits),
        'BB': int(walks),
        'K': int(strikeouts)
    }
    return pd.DataFrame([stats])

def local_pitcher_stats_table(pitcher_name: str, ax: plt.Axes, stats: list, fontsize: int, df: pd.DataFrame):
    stats_dict = {
        'IP': {'table_header': '$\\bf{IP}$', 'format': '.1f'},
        'P': {'table_header': '$\\bf{P}$', 'format': '.0f'},
        'R': {'table_header': '$\\bf{R}$', 'format': '.0f'},
        'H': {'table_header': '$\\bf{H}$', 'format': '.0f'},
        'BB': {'table_header': '$\\bf{BB}$', 'format': '.0f'},
        'K': {'table_header': '$\\bf{K}$', 'format': '.0f'}
    }

    df_stats = local_pitching_stats(pitcher_name, df)

    if df_stats.empty:
        ax.text(0.5, 0.5, "No stats to display", ha='center', va='center', fontsize=fontsize, color='red')
        ax.axis('off')
        return

    # Ensure all required stats are in df_stats, fill with NaN if not present
    for stat_key in stats:
        if stat_key not in df_stats.columns:
            df_stats[stat_key] = np.nan

    df_stats = df_stats[stats].reset_index(drop=True)

    # Format values for display
    display_values = [
        [format(df_stats[x][0], stats_dict[x]['format']) if not pd.isna(df_stats[x][0]) else '---' for x in df_stats]
    ]

    table_fg = ax.table(
        cellText=display_values,
        colLabels=[stats_dict[s]['table_header'] for s in stats],
        cellLoc='center',
        bbox=[-.05, 0.0, 1, 1]
    )

    table_fg.set_fontsize(fontsize)
    for i in range(len(stats)):
        table_fg.get_celld()[(0, i)].set_width(1/len(stats))  # Distribute width evenly
        table_fg.get_celld()[(0, i)].set_height(0.1)  # Adjust row height

    ax.axis('off')


def plot_pitcher_percentiles(df, pitcher_name, save_path=None):
    """
    Plots percentile rankings for a pitcher compared to league for key metrics.
    Bubble color is from coolwarm colormap by percentile.
    The bubble shows the percentile, and the actual value is shown as a column to the right.
    Includes pitcher name at the top and a season stats table at the bottom, centered.
    """
    # Metrics in desired order and whether higher is better
    metric_info = [
        ('xBA', False),
        ('xWOBA', False),
        ('Stuff+', True),
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

    # Define 'metrics' here by extracting names from metric_info
    metrics = [m[0] for m in metric_info]

    # Stats for the season table
    season_stats_keys = ['IP', 'P', 'R', 'H', 'BB', 'K']

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
        player_val = pitcher_metrics.get(metric, np.nan)
        if np.isnan(player_val) or len(league_vals) == 0:
            percentiles.append(np.nan)
            continue
        pct = percentileofscore(league_vals, player_val, kind='rank')
        if not higher_is_better:
            pct = 100 - pct
        percentiles.append(pct)

    # --- Plotting ---
    fig = plt.figure(figsize=(14, 14))  # Adjusted figure size for better centering

    # Define a custom GridSpec for better layout control
    # Rows: Header (0.5), Spacer (0.2), Stats Table (1.5), Spacer (0.2), Percentiles (7), Footer (0.5)
    gs = gridspec.GridSpec(6, 1, height_ratios=[0.5, 0.2, 1.5, 0.2, 7, 0.5])

    ax_title = fig.add_subplot(gs[0, 0])
    ax_stats_table = fig.add_subplot(gs[2, 0])
    ax_percentiles = fig.add_subplot(gs[4, 0])
    ax_footer = fig.add_subplot(gs[5, 0])

    # 1. Pitcher Name as Title
    ax_title.text(0.45, 0.5, f"{pitcher_name} Percentile Report",
                  ha='center', va='center', fontsize=20, fontweight='bold')
    ax_title.axis('off')

    # 2. Percentile Plot
    ax = ax_percentiles
    y_pos = np.arange(len(metrics))
    cmap = plt.get_cmap('coolwarm')
    norm = mpl.colors.Normalize(vmin=0, vmax=100)

    # Plot bars
    bars = ax.barh(y_pos, percentiles, color=[cmap(norm(p)) if not np.isnan(p) else '#cccccc' for p in percentiles], edgecolor='black', height=0.7)

    ax.set_yticks(y_pos)
    ax.set_yticklabels(metrics, fontsize=10)
    ax.set_xlim(0, 115)
    ax.set_xlabel('Percentile (vs. League)', fontsize=10)
    ax.tick_params(axis='x', labelsize=8)
    ax.grid(axis='x', linestyle='--', alpha=0.7)
    ax.invert_yaxis()

    # Add bubbles and values
    for i, (p, metric) in enumerate(zip(percentiles, metrics)):
        value = pitcher_metrics.get(metric, np.nan)
        # Draw bubble with percentile inside
        if not np.isnan(p):
            bubble_text = f"{int(round(p))}"
            ax.scatter(p, i, s=500, color=cmap(norm(p)), edgecolors='black', zorder=5)
            ax.text(p, i, bubble_text, va='center', ha='center', color='black', fontsize=9, zorder=6, fontweight='bold')
        # Draw value column to the right
        if not np.isnan(value):
            if '%' in metric:
                value_text = f"{value:.1f}%"
            elif 'Velo' in metric or 'Velocity' in metric:
                value_text = f"{value:.1f}"
            else:
                value_text = f"{value:.2f}"
            ax.text(107, i, value_text, va='center', ha='left', color='black', fontsize=9, fontweight='bold')

    # Draw a vertical separator line for the value column
    ax.axvline(102, color='gray', linestyle='--', lw=1)
    ax.spines['right'].set_visible(False)
    ax.spines['top'].set_visible(False)
    ax.spines['bottom'].set_visible(False)

    # 3. Season Stats Table (Centered)
    ax_stats_table.axis('off')  # Turn off axes for the table
    # Assuming local_pitcher_stats_table creates a matplotlib table
    # Call the function to create the table
    local_pitcher_stats_table(pitcher_name, ax_stats_table, season_stats_keys, fontsize=12, df=df)
    
    # 4. Footer
    ax_footer.text(-.1, 0.5, 'By: Max', ha='left', va='center', fontsize=10)
    ax_footer.text(0.45, 0.5, 'Pitch Matrix Colour Coding Compares to League Average', ha='center', va='center', fontsize=8)
    ax_footer.text(1, 0.5, 'Data: Yakkertech', ha='right', va='center', fontsize=10)
    ax_footer.axis('off')

    plt.tight_layout(rect=[0.00, 0.00, 0.95, 0.97])  # Adjust layout to prevent overlap
    # Save path
    if save_path is None:
        save_dir = os.path.join("CornBelters", "percentiles")
        os.makedirs(save_dir, exist_ok=True)
        save_path = os.path.join(save_dir, f"{pitcher_name.replace(' ','')}_percentiles.png")

    plt.savefig(save_path, dpi=300, bbox_inches='tight')
    plt.close(fig)

#    --- Main execution ---
data_path = 'CornBelters/Data/2025.csv'

# Ensure the base directory exists
output_base_dir = "CornBelters"
os.makedirs(output_base_dir, exist_ok=True)
os.makedirs(os.path.join(output_base_dir, "percentiles"), exist_ok=True)

try:
    df = pd.read_csv(data_path)
except ValueError as e:
    print(f"Error reading CSV with specified dtypes: {e}")
    print("Falling back to low_memory=False")
    df = pd.read_csv(data_path, low_memory=False)

df = df[df['PitcherTeam'] == 'Normal cornbelters']  # Filter for Normal cornbelters team

# Convert Date to datetime
player_name_map = {
     "Zach O'donnell": "Zach O'Donnell",
}
df['Date'] = pd.to_datetime(df['Date'], errors='coerce')
df['Pitcher'] = df['Pitcher'].replace(player_name_map)
df['Year'] = df['Date'].dt.year # Ensure 'Year' column is available for local_pitching_stats



    # Filter DataFrame for the current pitcher and season (already done by the outer loop for 'df')
    # The `plot_pitcher_percentiles` function expects the full league df for percentile calculation,
    # but `local_pitching_stats` within it will filter for the specific pitcher and season.
pitchers = df['Pitcher'].dropna().unique()

for pitcher_name in pitchers:
    try:
        plot_pitcher_percentiles(df, "Nicholas Currie")
        print(f"Percentile card generated for {pitcher_name}")
    except Exception as e:
        print(f"Error generating percentile card for {pitcher_name}: {e}")

print("All percentile cards generated.")