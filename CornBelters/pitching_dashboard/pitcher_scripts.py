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