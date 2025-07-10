### Report Rendering

# CHANGE THESE VALUES

file = "5-31-2025_BlueCaps_at_Merchants_SQUEAKYCLEAN.csv"
home = "Merchants"
away = "Bluecaps"
month = "05"
day = "31"
league = "KCL"


# RUN THIS TO RENDER
# OUTPUT: Ump_Reports/output
rmarkdown::render(
  "umpire_report.Rmd",
  params = list(
    game_data = file,
    home_team = home,
    away_team = away,
    game_date = paste0(month, "/", day),
    league = league
  ),
  output_file = paste0(
    "output/", home, "_vs_", away, "_", month, "_", day, "_Report.pdf"
  )
)