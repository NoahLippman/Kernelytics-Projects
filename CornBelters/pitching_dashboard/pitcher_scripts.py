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


def calculate_pitcher_metrics(df, pitcher_name):
    """
    Calculate key metrics for a given pitcher.
    Metrics: fastball velo, chase%, whiff%, k%, bb%, barrel%, hard-hit%, gb%, extension%
    """
    pitcher_df = df[df['Pitcher'] == pitcher_name].copy()
    metrics = {}

    # Fastball velo (mean RelSpeed for fastball, sinker, cutter)
    fastballs = pitcher_df[pitcher_df['TaggedPitchType'].isin(['Fastball', 'Sinker', 'Cutter'])]
    metrics['Fastball Velo'] = fastballs['RelSpeed'].mean()

    # Chase%
    if 'Chase?' in pitcher_df.columns:
        chase_n = pitcher_df['Chase?'].sum()
        chase_d = pitcher_df['Chase?'].count()
        metrics['Chase%'] = 100 * chase_n / chase_d if chase_d > 0 else np.nan
    else:
        metrics['Chase%'] = np.nan

    # Whiff%
    if 'Swing?' in pitcher_df.columns and 'Swing Strike?' in pitcher_df.columns:
        swings = pitcher_df[pitcher_df['Swing?'] == 1]
        whiffs = swings['Swing Strike?'].sum()
        swings_n = swings['Swing?'].count()
        metrics['Whiff%'] = 100 * whiffs / swings_n if swings_n > 0 else np.nan
    else:
        metrics['Whiff%'] = np.nan

    # K%
    if 'KorBB' in pitcher_df.columns:
        k_n = (pitcher_df['KorBB'] == 'Strikeout').sum()
        k_d = pitcher_df['PlayResult'].notna().sum()
        metrics['K%'] = 100 * k_n / k_d if k_d > 0 else np.nan
    else:
        metrics['K%'] = np.nan

    # BB%
    if 'KorBB' in pitcher_df.columns:
        bb_n = (pitcher_df['KorBB'] == 'Walk').sum()
        bb_d = pitcher_df['PlayResult'].notna().sum()
        metrics['BB%'] = 100 * bb_n / bb_d if bb_d > 0 else np.nan
    else:
        metrics['BB%'] = np.nan

    # Barrel% (ExitSpeed > 90 and 26 <= Angle <= 30) -- only for batted balls (ExitSpeed notna)
    if 'ExitSpeed' in pitcher_df.columns and 'Angle' in pitcher_df.columns:
        batted_balls = pitcher_df[pitcher_df['ExitSpeed'].notna() & pitcher_df['Angle'].notna()]
        barrel_mask = (batted_balls['ExitSpeed'] > 90) & (batted_balls['Angle'] >= 26) & (batted_balls['Angle'] <= 30)
        barrel_n = barrel_mask.sum()
        barrel_d = len(batted_balls)
        metrics['Barrel%'] = 100 * barrel_n / barrel_d if barrel_d > 0 else np.nan
    else:
        metrics['Barrel%'] = np.nan

    # Hard-Hit% (ExitSpeed >= 92) -- only for batted balls
    if 'ExitSpeed' in pitcher_df.columns:
        batted_balls = pitcher_df[pitcher_df['ExitSpeed'].notna()]
        hardhit_n = (batted_balls['ExitSpeed'] >= 92).sum()
        hardhit_d = len(batted_balls)
        metrics['Hard-Hit%'] = 100 * hardhit_n / hardhit_d if hardhit_d > 0 else np.nan
    else:
        metrics['Hard-Hit%'] = np.nan

    # GB% (Ground Ball? == 1) -- only for balls in play (where 'Ground Ball?' is notna)
    if 'Ground Ball?' in pitcher_df.columns:
        gb_bip = pitcher_df[pitcher_df['Ground Ball?'].notna()]
        gb_n = gb_bip['Ground Ball?'].sum()
        gb_d = len(gb_bip)
        metrics['GB%'] = 100 * gb_n / gb_d if gb_d > 0 else np.nan
    else:
        metrics['GB%'] = np.nan


    return metrics

def plot_pitcher_percentiles(df, pitcher_name, save_path=None):
    """
    Plots percentile rankings for a pitcher compared to league for key metrics.
    Uses seaborn coolwarm colormap, tight bar spacing, and a less vertical layout.
    """
    import matplotlib.pyplot as plt
    import seaborn as sns
    import numpy as np
    from matplotlib import colors
    from scipy.stats import percentileofscore

    # Metrics with display names, whether higher is better, and formatting
    metric_info = [
        ('Fastball_Velo', 'Fastball Velocity (mph)', True, '{:.1f}'),
        ('Avg Exit Velocity', 'Avg Exit Velocity (mph)', False, '{:.1f}'),
        ('Chase%', 'Chase Rate (%)', True, '{:.1f}%'),
        ('Whiff%', 'Whiff Rate (%)', True, '{:.1f}%'),
        ('K%', 'Strikeout Rate (%)', True, '{:.1f}%'),
        ('BB%', 'Walk Rate (%)', False, '{:.1f}%'),
        ('Barrel%', 'Barrel Rate (%)', False, '{:.1f}%'),
        ('Hard-Hit%', 'Hard-Hit Rate (%)', False, '{:.1f}%'),
        ('GB%', 'Ground Ball Rate (%)', False, '{:.1f}%'),
    ]

    # Calculate pitcher metrics
    try:
        pitcher_metrics = calculate_pitcher_metrics(df, pitcher_name)
    except Exception as e:
        raise ValueError(f"Error calculating metrics for {pitcher_name}: {e}")

    # Add Avg Exit Velocity if not present
    if 'Avg Exit Velocity' not in pitcher_metrics:
        if 'ExitSpeed' in df.columns:
            pitcher_metrics['Avg Exit Velocity'] = df[df['Pitcher'] == pitcher_name]['ExitSpeed'].mean()
        else:
            pitcher_metrics['Avg Exit Velocity'] = np.nan

    # Calculate league metrics for all pitchers
    league_pitchers = df['Pitcher'].dropna().unique()
    league_metrics = {m[0]: [] for m in metric_info}
    for p in league_pitchers:
        try:
            m = calculate_pitcher_metrics(df, p)
            if 'ExitSpeed' in df.columns:
                m['Avg Exit Velocity'] = df[df['Pitcher'] == p]['ExitSpeed'].mean()
            else:
                m['Avg Exit Velocity'] = np.nan
            for key, _, _, _ in metric_info:
                league_metrics[key].append(m.get(key, np.nan))
        except:
            continue  # Skip pitchers with calculation errors

    # Calculate percentiles
    percentiles = []
    for metric_key, _, higher_is_better, _ in metric_info:
        league_vals = np.array([v for v in league_metrics[metric_key] if not np.isnan(v)])
        player_val = pitcher_metrics.get(metric_key, np.nan)
        if np.isnan(player_val) or len(league_vals) == 0:
            percentiles.append(np.nan)
            continue
        pct = percentileofscore(league_vals, player_val, kind='rank')
        if not higher_is_better:
            pct = 100 - pct
        percentiles.append(pct)

    # Set up the plot
    metrics_display = [m[1] for m in metric_info]
    formats = [m[3] for m in metric_info]
    # Use seaborn's coolwarm colormap
    cmap = sns.color_palette("coolwarm", as_cmap=True)
    norm = colors.Normalize(vmin=0, vmax=100)

    fig, ax = plt.subplots(figsize=(10, 4.5))  # Less vertical

    # Plot horizontal bars with tighter spacing
    bars = ax.barh(
        metrics_display,
        percentiles,
        height=0.28,  # Smaller height for closer bars
        color=[cmap(norm(p)) if not np.isnan(p) else '#d3d3d3' for p in percentiles],
        edgecolor='black',
        alpha=0.85
    )

    # Customize axes
    # Customize axes
    ax.set_xlim(0, 100)  # Only go to 100 for percentile
    ax.set_xlabel('Percentile (vs. League)', fontsize=12, labelpad=10)
    ax.set_title(f'{pitcher_name} - Pitching Metrics Percentile Rankings', fontsize=15, pad=16, fontweight='bold')
    ax.grid(True, axis='x', linestyle='--', alpha=0.3)

    # Add percentile bubbles and metric values
    for i, (pct, metric_key, metric_display, fmt) in enumerate(zip(percentiles, [m[0] for m in metric_info], metrics_display, formats)):
        value = pitcher_metrics.get(metric_key, np.nan)
        if not np.isnan(pct):
            # Percentile bubble
            ax.scatter(pct, i, s=320, color=cmap(norm(pct)), edgecolors='black', zorder=5, alpha=0.95)
            ax.text(pct, i, f"{int(round(pct))}", va='center', ha='center', color='white', fontsize=9, fontweight='bold', zorder=6)
        # Metric value
        if not np.isnan(value):
            value_text = fmt.format(value)
            ax.text(103, i, value_text, va='center', ha='left', color='black', fontsize=10, fontweight='bold')
        # If value is NaN, do not display anything

    # Add value column separator and label
    ax.axvline(101, color='gray', linestyle='--', lw=1, alpha=0.5)
    ax.text(103, len(metrics_display) - 0.2, 'Value', va='bottom', ha='left', color='black', fontsize=10, fontweight='bold')

    # Style adjustments
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.spines['left'].set_color('gray')
    ax.spines['bottom'].set_color('gray')
    ax.tick_params(axis='both', labelsize=10)
    ax.invert_yaxis()  # Top-to-bottom metric display

    plt.tight_layout()
    plt.savefig(f'CornBelters/percentiles/{pitcher_name}_percentiles.png', dpi=300, bbox_inches='tight')
    plt.close()


