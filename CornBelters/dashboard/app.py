from pathlib import Path
from shiny import App, render, ui, reactive
import pandas as pd
from tempfile import NamedTemporaryFile
import os
from pitcher_card import pitching_dashboard
from pitcher_scripts import plot_pitch_velocity_with_line, plot_pitch_usage, plot_pitcher_percentiles

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
        'KorBB': str,
        'ExitSpeed': float,
        'Angle': float,
        'HitType': str,
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

def df_grouping(pitcher_name, date):
    if not pitcher_name or not date:
        return None

    team = pitcher_team_map.get(pitcher_name, "")
    if not team:
        return None

    season = 2025
    
    if date == "ALL":
    # Filter for all dates for the pitcher
        pitcher_df = df[(df['Pitcher'] == pitcher_name) & (df['PitcherTeam'] == team)]
        if pitcher_df.empty:
            return None
        return pitcher_df
    else:
        # ... existing code ...
        pitcher_df = df[
            (df['Pitcher'] == pitcher_name) &
            (df['PitcherTeam'] == team) &
            (df['Date'].dt.strftime('%m-%d') == date)
        ]
        if pitcher_df.empty:
            return None
        return pitcher_df

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
    # 2x2 grid for dashboards
    ui.layout_columns(
        # First row
        ui.layout_columns(
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
            ),
            ui.card(
                ui.card_header("Velocity Plot"),
                ui.output_image("image_output1"),
                ui.output_text("error_message1"),
                ui.card_footer(
                    ui.download_button(
                        "download_img1",
                        "Download Velocity Plot",
                        class_="btn-primary",
                        style="width: 100%;"
                    )
                ),
            ),
            width=6  # Each card takes half the row
        ),
        # Second row
        ui.layout_columns(
            ui.card(
                ui.card_header("Pitch Usage"),
                ui.output_image("image_output2"),
                ui.output_text("error_message2"),
                # Add footer or download if needed
            ),
            ui.card(
                ui.card_header("Percentile Rankings"),
                ui.output_image("image_output3"),
                ui.output_text("error_message3"),
                # Add footer or download if needed
            ),
            width=6
        ),
        col_widths={"xs": (6, 6), "md": (6, 6)}
        
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
    """),
    ui.tags.div(
        ui.tags.img(id="fullscreen-img", src="", style="max-width:95vw; max-height:95vh; border:4px solid white; border-radius:8px;"),
        ui.tags.button("Exit Full Screen", id="exit-fullscreen", style="position:absolute; top:30px; right:30px; font-size:1.5rem; padding:10px 20px;"),
        id="fullscreen-modal",
        style="display:none; position:fixed; top:0; left:0; width:100vw; height:100vh; background:rgba(0,0,0,0.95); z-index:9999; justify-content:center; align-items:center;"
    ),
    ui.tags.script("""
    document.addEventListener('DOMContentLoaded', function() {
        document.body.addEventListener('click', function(e) {
            if(e.target.tagName === 'IMG' && e.target.closest('.card')) {
                var modal = document.getElementById('fullscreen-modal');
                var modalImg = document.getElementById('fullscreen-img');
                modalImg.src = e.target.src;
                modal.style.display = 'flex';
            }
        });
        document.getElementById('exit-fullscreen').onclick = function() {
            document.getElementById('fullscreen-modal').style.display = 'none';
            document.getElementById('fullscreen-img').src = '';
        };
        document.getElementById('fullscreen-modal').onclick = function(e) {
            if(e.target === this) {
                this.style.display = 'none';
                document.getElementById('fullscreen-img').src = '';
            }
        };
    });
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
        pitcher_df = df_grouping(pitcher_name, date)

        season = 2025
        stats = ['IP', 'P', 'R', 'H', 'BB', 'K']
        
        if pitcher_df is None:
            return None
        if pitcher_df.empty:
            return None

        try:
            with NamedTemporaryFile(delete=False, suffix=".png") as tmp_file:
                tmp_path = tmp_file.name
                pitching_dashboard(pitcher_df, stats, pitcher_name, team, season, date)

                generated_path = here / f"CornBelters/Cards/{date}/{pitcher_name.replace(', ', '_')}_pitching.png"
                if generated_path.exists():
                    return {
                        "src": str(generated_path),
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
        
        pitcher_df = df_grouping(pitcher_name, date)
        if pitcher_df is None:
            return "No data found for the selected pitcher and date."
        if pitcher_df.empty:
            return "No data found for the selected pitcher and date."
        generated_path = here / f"CornBelters/Cards/{date}/{pitcher_name.replace(', ', '_')}_pitching.png"
        return "" if generated_path.exists() else "Failed to generate dashboard. Please try again."

    @render.download(
        filename=lambda: f"{input.pitcher_name().strip()}_dashboard_{input.date()}.png"
    )
    def download_img():
        pitcher_name = input.pitcher_name().strip()
        date = input.date()
        if not pitcher_name or not date:
            return
        generated_path = here / f"CornBelters/Cards/{date}/{pitcher_name.replace(', ', '_')}_pitching.png"
        if generated_path.exists():
            with open(generated_path, "rb") as f:
                yield f.read()

    @render.image
    def image_output1():
        pitcher_name = input.pitcher_name().strip()
        date = input.date()
        pitcher_df = df_grouping(pitcher_name, date)
        if pitcher_df is None:
            return None

        try:
            with NamedTemporaryFile(delete=False, suffix=".png") as tmp_file:
                tmp_path = tmp_file.name
                plot_pitch_velocity_with_line(pitcher_name, pitcher_df,date)

                generated_path = here / f"CornBelters/velocity/{date}/{pitcher_name}_velocity_with_line.png"
                if generated_path.exists():
                    return {
                        "src": str(generated_path),
                        "width": "100%",
                        "height": "auto",
                        "alt": f"{pitcher_name} velocity chart for {date}"
                    }
                else:
                    return None
        except Exception as e:
            print(f"Error generating pitching velocity chart for {pitcher_name} on {date}: {e}")
            return None
        finally:
            if 'tmp_path' in locals() and os.path.exists(tmp_path):
                os.unlink(tmp_path)

    @render.text
    def error_message1():
        pitcher_name = input.pitcher_name().strip()
        date = input.date()
        if not pitcher_name or not date:
            return "Please select a pitcher and date."
        team = pitcher_team_map.get(pitcher_name, "")
        if not team:
            return "No team found for the selected pitcher."
        
        pitcher_df = df_grouping(pitcher_name, date)
        if pitcher_df is None:
            return "No data found for the selected pitcher and date."
        if pitcher_df.empty:
            return "No data found for the selected pitcher and date."
        generated_path = here / f"CornBelters/velocity/{date}/{pitcher_name}_velocity_with_line.png"
        return "" if generated_path.exists() else "Failed to generate velocity chart. Please try again."
    @render.download(
        filename=lambda: f"{input.pitcher_name().strip()}_dashboard_{input.date()}.png"
    )
    def download_img1():
        pitcher_name = input.pitcher_name().strip()
        date = input.date()
        if not pitcher_name or not date:
            return
        generated_path = here / f"CornBelters/velocity/{date}/{pitcher_name}_velocity_with_line.png"
        if generated_path.exists():
            with open(generated_path, "rb") as f:
                yield f.read()
    @render.image
    def image_output2():
        pitcher_name = input.pitcher_name().strip()
        date = input.date()
        pitcher_df = df_grouping(pitcher_name, date)
        if pitcher_df is None:
            return None

        try:
            with NamedTemporaryFile(delete=False, suffix=".png") as tmp_file:
                tmp_path = tmp_file.name
                plot_pitch_usage(pitcher_name, pitcher_df,date)

                generated_path = here / f"CornBelters/usage/{date}/{pitcher_name}_pitch_usage.png"
                if generated_path.exists():
                    return {
                        "src": str(generated_path),
                        "width": "100%",
                        "height": "auto",
                        "alt": f"{pitcher_name} velocity chart for {date}"
                    }
                else:
                    return None
        except Exception as e:
            print(f"Error generating pitching velocity chart for {pitcher_name} on {date}: {e}")
            return None
        finally:
            if 'tmp_path' in locals() and os.path.exists(tmp_path):
                os.unlink(tmp_path)
    @render.text        
    def error_message2():
        pitcher_name = input.pitcher_name().strip()
        date = input.date()
        if not pitcher_name or not date:
            return "Please select a pitcher and date."
        team = pitcher_team_map.get(pitcher_name, "")
        if not team:
            return "No team found for the selected pitcher."
        
        pitcher_df = df_grouping(pitcher_name, date)
        if pitcher_df is None:
            return "No data found for the selected pitcher and date."
        if pitcher_df.empty:
            return "No data found for the selected pitcher and date."
        generated_path = here / f"CornBelters/usage/{date}/{pitcher_name}_pitch_usage.png"
        return "" if generated_path.exists() else "Failed to generate percentile chart. Please try again."
    @render.image
    def image_output3():
        pitcher_name = input.pitcher_name().strip()
        pitcher_df = df
        if pitcher_df is None:
            return None

        try:
            with NamedTemporaryFile(delete=False, suffix=".png") as tmp_file:
                tmp_path = tmp_file.name
                plot_pitcher_percentiles(df,pitcher_name)

                generated_path = here / f'CornBelters/percentiles/{pitcher_name}_percentiles.png'
                if generated_path.exists():
                    return {
                        "src": str(generated_path),
                        "width": "100%",
                        "height": "auto",
                        "alt": f"{pitcher_name} percentile for ALL dates"
                    }
                else:
                    return None
        except Exception as e:
            print(f"Error generating pitching percentile {pitcher_name} : {e}")
            return None
        finally:
            if 'tmp_path' in locals() and os.path.exists(tmp_path):
                os.unlink(tmp_path)
    @render.text        
    def error_message3():
        pitcher_name = input.pitcher_name().strip()
        date = input.date()
        if not pitcher_name or not date:
            return "Please select a pitcher and date."
        team = pitcher_team_map.get(pitcher_name, "")
        if not team:
            return "No team found for the selected pitcher."
        
        pitcher_df = df_grouping(pitcher_name, date)
        if pitcher_df is None:
            return "No data found for the selected pitcher and date."
        if pitcher_df.empty:
            return "No data found for the selected pitcher and date."
        generated_path = here / f"CornBelters/usage/{date}/{pitcher_name}_pitch_usage.png"
        return "" if generated_path.exists() else "Failed to generate velocity chart. Please try again."



app = App(app_ui, server)