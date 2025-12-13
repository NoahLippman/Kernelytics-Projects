### Report Rendering

# CHANGE THESE VALUES

file = "TEST-07_13_2025 7_21_43 PM-KCL BlueCaps 2025@KCL Merchants 2025.csv"
home = "Merchants"
away = "Bluecaps"
month = "07"
day = "13"
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
    "output/",  month, "_", day, "_", home, "_vs_", away, "_Report.pdf"
  )
)