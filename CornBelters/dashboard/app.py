from pathlib import Path
from shiny import App, render, ui, reactive
import pandas as pd
from tempfile import NamedTemporaryFile
import os
from pitcher_card import pitching_dashboard
from pitcher_scripts import plot_pitch_velocity_with_line

here = Path(__file__).parent
data_path = here / "Data/2025.csv"

# Load the dataset to populate dropdowns
try:
    dtypes = {
        'Date': str,
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
        'VertApprAngle': float,
        'BatterSide': str,
        'PlateLocSide': float,
        'PlateLocHeight': float,
        'OutsOnPlay': float,
        'RunsScored': float,
        'KorBB': str
    }
    df = pd.read_csv(data_path, dtype=dtypes)

    # Parse mixed date formats
    def parse_dates(date_str):
        if pd.isna(date_str) or not isinstance(date_str, str):
            return pd.NaT
        try:
            # Try MM/DD/YYYY format (e.g., 06/05/2025)
            return pd.to_datetime(date_str, format='%m/%d/%Y')
        except ValueError:
            try:
                # Try M/D/YY format (e.g., 6/3/25)
                return pd.to_datetime(date_str, format='%m/%d/%y')
            except ValueError:
                return pd.NaT

    df['Date'] = df['Date'].apply(parse_dates)
    # Log unparseable dates for debugging
    invalid_dates = df[df['Date'].isna()]['Date'].drop_duplicates().tolist()
    if invalid_dates:
        print(f"Unparseable dates in 2025.csv: {invalid_dates}")
except Exception as e:
    print(f"Error loading data: {e}")
    df = pd.DataFrame()

# Get unique pitchers and their most recent team
if not df.empty:
    pitcher_teams = (df.sort_values('Date', ascending=False)
                     .groupby('Pitcher')
                     .agg({'PitcherTeam': 'first'})
                     .reset_index())
    pitchers = sorted(pitcher_teams['Pitcher'].tolist())
    pitcher_team_map = dict(zip(pitcher_teams['Pitcher'], pitcher_teams['PitcherTeam']))
else:
    pitchers = []
    pitcher_team_map = {}

# Define UI
app_ui = ui.page_fluid(
    ui.panel_title("Pitcher Dashboard Viewer"),
    ui.card(
        ui.card_header("Search Parameters"),
        ui.layout_columns(
            ui.input_select(
                "pitcher_name",
                "Select Pitcher:",
                choices=[""] + pitchers,
                selected="",
                width="100%"
            ),
            ui.input_select(
                "date",
                "Select Date:",
                choices=[""],
                selected="",
                width="100%"
            ),
            col_widths={"xs": (6, 6), "md": (4, 4)}
        ),
        style="max-width: 800px; margin: 20px auto; display: block !important; visibility: visible !important;"
    ),
    ui.card(
        ui.card_header("Dashboard"),
        ui.output_image("image_output"),
        ui.output_text("error_message"),
        ui.card_footer(
            ui.download_button(
                "download_img",
                "Download Dashboard",
                class_="btn-primary",
                style="width: 100%;"
            )
        ),
        style="max-width: 800px; margin: 20px auto; display: block !important; visibility: visible !important;"
    ),
    ui.include_css(here / "styles.css") if (here / "styles.css").exists() else ui.tags.style("""
        body {
            background-color: #f5f5f5;
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Open Sans', 'Helvetica Neue', sans-serif;
        }
        .card {
            border-radius: 8px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
            padding: 1rem;
            display: block !important;
            visibility: visible !important;
        }
        .btn-primary {
            background-color: #007bff;
            border-color: #007bff;
            transition: all 0.2s;
        }
        .btn-primary:hover {
            background-color: #0056b3;
            border-color: #0056b3;
        }
        .text-muted {
            color: #6c757d !important;
        }
        .text-danger {
            color: #dc3545 !important;
        }
        img {
            border-radius: 4px;
            border: 1px solid #dee2e6;
        }
    """)
)

def server(input, output, session):
    # Reactive effect to update date dropdown based on pitcher selection
    @reactive.Effect
    @reactive.event(input.pitcher_name)
    def update_date_dropdown():
        pitcher_name = input.pitcher_name().strip()
        if not pitcher_name:
            ui.update_select("date", choices=[""], selected="")
            return

        # Get dates for the selected pitcher
        pitcher_df = df[df['Pitcher'] == pitcher_name]
        valid_dates = pitcher_df['Date'].dropna()
        dates = sorted(valid_dates.dt.strftime('%m-%d').unique().tolist())
        # Add "ALL" option
        dates = ["ALL"] + dates
        ui.update_select("date", choices=[""] + dates, selected="")

    @render.image
    def image_output():
        pitcher_name = input.pitcher_name().strip()
        date = input.date()
        if not pitcher_name or not date:
            return None

        team = pitcher_team_map.get(pitcher_name, "")
        if not team:
            return None

        season = 2025
        stats = ['IP', 'P', 'R', 'H', 'BB', 'K']
        
        if date == "ALL":
            # Filter for all dates for the pitcher
            pitcher_df = df[(df['Pitcher'] == pitcher_name) & (df['PitcherTeam'] == team)]
        else:
            # Convert date from MM-DD to MM/DD/YYYY for filtering
            try:
                full_date = pd.to_datetime(f"2025-{date}", format="%Y-%m-%d").strftime("%m/%d/%Y")
            except ValueError:
                return None
            pitcher_df = df[(df['Pitcher'] == pitcher_name) &
                            (df['PitcherTeam'] == team) &
                            (df['Date'].dt.strftime('%m/%d/%Y') == full_date)]

        if pitcher_df.empty:
            return None

        try:
            with NamedTemporaryFile(delete=False, suffix=".png") as tmp_file:
                tmp_path = tmp_file.name
                pitching_dashboard(pitcher_df, stats, pitcher_name, team, season, date)
                plot_pitch_velocity_with_line(pitcher_name, data_path=data_path)
                pitch_types = pitcher_df['TaggedPitchType'].unique()
                pitcher_card = here / f"CornBelters/Cards/{date}/{pitcher_name.replace(', ', '_')}_pitching.png"
                for pitch_type in pitch_types:
                    velocity_card = here / f"CornBelters/velocity/{pitcher_name}_{pitch_type}_velocity_with_line.png".format(pitch_type)
                
                if pitcher_card.exists() and velocity_card.exists():
                    return {
                        "src": str(pitcher_card,velocity_card),
                        "style": "display: block; margin: 0 auto;",
                        "width": "100%",
                        "height": "auto",
                        "alt": f"{pitcher_name} dashboard for {date}"
                    }
                else:
                    return None
        except Exception as e:
            print(f"Error generating dashboard for {pitcher_name} on {date}: {e}")
            return None
        finally:
            if 'tmp_path' in locals() and os.path.exists(tmp_path):
                os.unlink(tmp_path)

    @render.text
    def error_message():
        pitcher_name = input.pitcher_name().strip()
        date = input.date()
        if not pitcher_name or not date:
            return "Please select a pitcher and date."
        team = pitcher_team_map.get(pitcher_name, "")
        if not team:
            return "No team found for the selected pitcher."
        
        if date == "ALL":
            pitcher_df = df[(df['Pitcher'] == pitcher_name) & (df['PitcherTeam'] == team)]
        else:
            try:
                full_date = pd.to_datetime(f"2025-{date}", format="%Y-%m-%d").strftime("%m/%d/%Y")
            except ValueError:
                return "Invalid date format."
            pitcher_df = df[(df['Pitcher'] == pitcher_name) &
                            (df['PitcherTeam'] == team) &
                            (df['Date'].dt.strftime('%m/%d/%Y') == full_date)]
        
        if pitcher_df.empty:
            return "No data found for the selected pitcher and date."
        pitcher_card = here / f"CornBelters/Cards/{date}/{pitcher_name.replace(', ', '_')}_pitching.png"
        return "" if pitcher_card.exists() else "Failed to generate dashboard. Please try again."

    @render.download(
        filename=lambda: f"{input.pitcher_name().strip()}_dashboard_{input.date()}.png"
    )
    def download_img():
        pitcher_name = input.pitcher_name().strip()
        date = input.date()
        if not pitcher_name or not date:
            return
        pitcher_card = here / f"CornBelters/Cards/{date}/{pitcher_name.replace(', ', '_')}_pitching.png"
        if pitcher_card.exists():
            with open(pitcher_card, "rb") as f:
                yield f.read()

app = App(app_ui, server)