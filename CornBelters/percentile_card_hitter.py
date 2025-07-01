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

def calculate_batter_metrics(df, batter_name):
    """
    Calculate key metrics for a given batter.
    Metrics: xWOBA, xBA xSLG, avg exit velocity, barrel%, hard-hit%, la sweet-spot%, chase%, whiff%, k%, bb%
    """
    batter_df = df[df['Batter'] == batter_name].copy()
    metrics = {}

    # xBA and xWOBA (calculated for batted ball events where PitchCall == 'InPlay')
    batted_balls = batter_df[batter_df['PitchCall'] == 'InPlay']
    if 'xWOBA' in batter_df.columns:
        metrics['xWOBA'] = batted_balls['xWOBA'].mean() if len(batted_balls) > 0 else np.nan
    else:
        metrics['xWOBA'] = np.nan
    if 'xBA' in batter_df.columns:
        metrics['xBA'] = batted_balls['xBA'].mean() if len(batted_balls) > 0 else np.nan
    else:
        metrics['xBA'] = np.nan

    if 'xSLG' in batter_df.columns:
        metrics['xSLG'] = batted_balls['xSLG'].mean() if len(batted_balls) > 0 else np.nan  
    else:
        metrics['xSLG'] = np.nan
    
    if 'ExitSpeed' in batter_df.columns:
        # Avg Exit Velocity (mean of ExitSpeed)
        metrics['Avg Exit Velocity'] = batter_df['ExitSpeed'].mean() if len(batted_balls) > 0 else np.nan   
    else:
        metrics['Avg Exit Velocity'] = np.nan

    # Barrel% (ExitSpeed > 88 and 25 <= Angle <= 30)
    if 'ExitSpeed' in batter_df.columns and 'Angle' in batter_df.columns:
        batted_balls = batter_df[batter_df['ExitSpeed'].notna() & batter_df['Angle'].notna()]
        barrel_mask = (batted_balls['ExitSpeed'] > 88) & (batted_balls['Angle'] >= 25) & (batted_balls['Angle'] <= 35)
        barrel_n = barrel_mask.sum()
        barrel_d = len(batted_balls)
        metrics['Barrel%'] = 100 * barrel_n / barrel_d if barrel_d > 0 else np.nan
    else:
        metrics['Barrel%'] = np.nan
    
    # Hard-Hit% (ExitSpeed >= 90)
    if 'ExitSpeed' in batter_df.columns:
        hardhit_n = (batter_df['ExitSpeed'] >= 90).sum()
        hardhit_d = batter_df['ExitSpeed'].notna().sum()
        metrics['Hard-Hit%'] = 100 * hardhit_n / hardhit_d if hardhit_d > 0 else np.nan
    else:
        metrics['Hard-Hit%'] = np.nan


    if 'Angle' in batter_df.columns:
        # Launch Angle Sweet Spot% (8 <= Angle <= 32)
        la_sweet_spot_mask = (batter_df['Angle'] >= 8) & (batter_df['Angle'] <= 32)
        la_sweet_spot_n = la_sweet_spot_mask.sum()
        la_sweet_spot_d = batter_df['Angle'].notna().sum()
        metrics['LA Sweet Spot%'] = 100 * la_sweet_spot_n / la_sweet_spot_d if la_sweet_spot_d > 0 else np.nan



    # Chase% (Chase? == 1 or True)
    if 'Chase?' in batter_df.columns:
        chase_n = batter_df['Chase?'].sum()
        chase_d = batter_df['Chase?'].count()
        metrics['Chase%'] = 100 * chase_n / chase_d if chase_d > 0 else np.nan
    else:
        metrics['Chase%'] = np.nan

    # Whiff% (Swing? == 1 and Swing Strike? == 1)
    if 'Swing?' in batter_df.columns and 'Swing Strike?' in batter_df.columns:
        swings = batter_df[batter_df['Swing?'] == 1]
        whiffs = swings['Swing Strike?'].sum()
        swings_n = swings['Swing?'].count()
        metrics['Whiff%'] = 100 * whiffs / swings_n if swings_n > 0 else np.nan
    else:
        metrics['Whiff%'] = np.nan

    # K% (KorBB == 'Strikeout')
    if 'KorBB' in batter_df.columns:
        k_n = (batter_df['KorBB'] == 'Strikeout').sum()
        k_d = batter_df['PlayResult'].notna().sum()
        metrics['K%'] = 100 * k_n / k_d if k_d > 0 else np.nan
    else:
        metrics['K%'] = np.nan

    # BB% (KorBB == 'Walk')
    if 'KorBB' in batter_df.columns:
        bb_n = (batter_df['KorBB'] == 'Walk').sum()
        bb_d = batter_df['PlayResult'].notna().sum()
        metrics['BB%'] = 100 * bb_n / bb_d if bb_d > 0 else np.nan
    else:
        metrics['BB%'] = np.nan




    return metrics

# Function to compute pitching stats from local data
def local_batter_stats(batter_name: str, df: pd.DataFrame) -> pd.DataFrame:
    # Validate input DataFrame
    required_columns = ['Batter', 'PlayResult', 'KorBB']
    if not all(col in df.columns for col in required_columns):
        raise ValueError(f"DataFrame must contain columns: {required_columns}")

    # Filter for the batter and create a copy
    batter_data = df[df['Batter'] == batter_name].copy()

    if batter_data.empty:
        print(f"No data found for {batter_name}")
        return pd.DataFrame()

    # At Bats (AB): Exclude Walk, Sacrifice, Error, FieldersChoice
    ab_mask = batter_data['PlayResult'].isin(['Out', 'Single', 'Double', 'Triple', 'HomeRun', 'StrikeoutLooking', 'StrikeoutSwinging'])
    ab = batter_data[ab_mask].shape[0]

    # Hits
    hits_mask = batter_data['PlayResult'].isin(['Single', 'Double', 'Triple', 'HomeRun'])
    hits = batter_data[hits_mask].shape[0]

    # Walks
    walks = batter_data['KorBB'].eq('Walk').sum()

    # Strikeouts
    strikeouts = batter_data['KorBB'].eq('Strikeout').sum()

    # Total Bases
    tb_map = {'Single': 1, 'Double': 2, 'Triple': 3, 'HomeRun': 4}
    batter_data['TotalBases'] = batter_data['PlayResult'].map(tb_map).fillna(0)
    total_bases = batter_data['TotalBases'].sum()

    # Plate Appearances (PA)
    pa = batter_data['PlayResult'].notna().sum()

    # Batting Average (AVG)
    avg = hits / ab if ab > 0 else np.nan

    # On-Base Percentage (OBP)
    obp = (hits + walks) / pa if pa > 0 else np.nan

    # Slugging Percentage (SLG)
    slg = total_bases / ab if ab > 0 else np.nan

    # OPS
    ops = obp + slg if not (np.isnan(obp) or np.isnan(slg)) else np.nan

    # Round to three decimal places
    stats = {
        'AB': int(ab),
        'H': int(hits),
        'AVG': round(avg, 3) if not np.isnan(avg) else np.nan,
        'OBP': round(obp, 3) if not np.isnan(obp) else np.nan,
        'SLG': round(slg, 3) if not np.isnan(slg) else np.nan,
        'OPS': round(ops, 3) if not np.isnan(ops) else np.nan,
        'BB': int(walks),
        'K': int(strikeouts)
    }
    return pd.DataFrame([stats])

def local_batter_stats_table(batter_name: str, ax: plt.Axes, stats: list, fontsize: int, df: pd.DataFrame):
    stats_dict = {
        'AB': {'table_header': '$\\bf{AB}$', 'format': '.0f'},
        'AVG': {'table_header': '$\\bf{AVG}$', 'format': '.3f'},
        'OBP': {'table_header': '$\\bf{OBP}$', 'format': '.3f'},
        'SLG': {'table_header': '$\\bf{SLG}$', 'format': '.3f'},
        'OPS': {'table_header': '$\\bf{OPS}$', 'format': '.3f'},
        'BB': {'table_header': '$\\bf{BB}$', 'format': '.0f'},
        'K': {'table_header': '$\\bf{K}$', 'format': '.0f'}
    }
    df_stats = local_batter_stats(batter_name, df)

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


def plot_batter_percentiles(df, batter_name, save_path=None):
    """
    Plots percentile rankings for a batter compared to league for key metrics.
    Bubble color is from coolwarm colormap by percentile.
    The bubble shows the percentile, and the actual value is shown as a column to the right.
    Includes batter name at the top and a season stats table at the bottom, centered.
    """
    # Metrics in desired order and whether higher is better
    metric_info = [
    ('xWOBA', True),
    ('xBA', True),
    ('xSLG', True),
    ('Avg Exit Velocity', True),
    ('Barrel%', True),
    ('Hard-Hit%', True),
    ('LA Sweet Spot%', True),
    ('Chase%', False),
    ('Whiff%', False),
    ('K%', False),
    ('BB%', True)
    ]

    # Define 'metrics' here by extracting names from metric_info
    metrics = [m[0] for m in metric_info]

    # Stats for the season table
    season_stats_keys = ['AB', 'AVG', 'OBP', 'SLG','OPS', 'BB', 'K']

    # Calculate batter metrics
    batter_metrics = calculate_batter_metrics(df, batter_name)
    # Add Avg Exit Velocity calculation if not present
    if 'Avg Exit Velocity' not in batter_metrics:
        if 'ExitSpeed' in df.columns:
            batter_metrics['Avg Exit Velocity'] = df[df['Batter'] == batter_name]['ExitSpeed'].mean()
        else:
            batter_metrics['Avg Exit Velocity'] = np.nan

    # Calculate league metrics for each batter
    league_batters = df['Batter'].dropna().unique()
    league_metrics = {m[0]: [] for m in metric_info}
    for p in league_batters:
        m = calculate_batter_metrics(df, p)
        # Add Avg Exit Velocity for each batter
        if 'ExitSpeed' in df.columns:
            m['Avg Exit Velocity'] = df[df['Batter'] == p]['ExitSpeed'].mean()
        else:
            m['Avg Exit Velocity'] = np.nan
        for k, _ in metric_info:
            league_metrics[k].append(m[k])

    # Calculate percentiles
    percentiles = []
    for metric, higher_is_better in metric_info:
        league_vals = np.array(league_metrics[metric])
        league_vals = league_vals[~np.isnan(league_vals)]
        player_val = batter_metrics.get(metric, np.nan)
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

    # 1. batter Name as Title
    ax_title.text(0.45, 0.5, f"{batter_name} Percentile Report",
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
        value = batter_metrics.get(metric, np.nan)
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
    # Assuming local_batter_stats_table creates a matplotlib table
    # Call the function to create the table
    local_batter_stats_table(batter_name, ax_stats_table, season_stats_keys, fontsize=12, df=df)
    
    # 4. Footer
    ax_footer.text(-.1, 0.5, 'By: Max', ha='left', va='center', fontsize=10)
    ax_footer.text(0.45, 0.5, 'Pitch Matrix Colour Coding Compares to League Average', ha='center', va='center', fontsize=8)
    ax_footer.text(1, 0.5, 'Data: Yakkertech', ha='right', va='center', fontsize=10)
    ax_footer.axis('off')

    plt.tight_layout(rect=[0.00, 0.00, 0.95, 0.97])  # Adjust layout to prevent overlap
    # Save path
    if save_path is None:
        save_dir = os.path.join("CornBelters", "percentiles_hitters")
        os.makedirs(save_dir, exist_ok=True)
        save_path = os.path.join(save_dir, f"{batter_name.replace(' ','')}_percentiles.png")

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

df = df[df['BatterTeam'] == 'Normal cornbelters']  # Filter for Normal cornbelters team

# Convert Date to datetime

df['Date'] = pd.to_datetime(df['Date'], errors='coerce')
df['Year'] = df['Date'].dt.year # Ensure 'Year' column is available for local_pitching_stats



    # Filter DataFrame for the current batter and season (already done by the outer loop for 'df')
    # The `plot_batter_percentiles` function expects the full league df for percentile calculation,
    # but `local_pitching_stats` within it will filter for the specific batter and season.
batters = df['Batter'].dropna().unique()

for batter_name in batters:
    try:
        plot_batter_percentiles(df, batter_name)
        print(f"Percentile card generated for {batter_name}")
    except Exception as e:
        print(f"Error generating percentile card for {batter_name}: {e}")

print("All percentile cards generated.")