library(shiny)
library(tidyverse)
library(rlang)

# Source modules
source("AdvancedPitcherStatsModule.R")
source("PitcherPercentilePlotModule.R")
source("zoneChartModule.R")


# Load data
kclYakkertechData <- read_csv("KCLYakkertechData.csv")
beltersYakkertechData <- read_csv("BeltersYakkertechData.csv")
YakkertechData <- read_csv("YakkertechData.csv")

kclAdvancedData <- read_csv("KCLSavantData.csv")
beltersAdvancedData <- read_csv("BeltersSavantData.csv")
AdvancedData <- read_csv("SavantData.csv")

filtered_data <- YakkertechData %>%
  filter(PitcherTeam %in% c(
    "Normal cornbelters",
    "Kcl bluecaps 2025",
    "Kcl bobcats 2025",
    "Kcl groundsloths 2025",
    "Kcl merchants 2025"
  ))

# Step 2: Standardize pitcher names (lowercase + trim whitespace)
filtered_data <- filtered_data %>%
  filter(!is.na(Pitcher)) %>%
  mutate(Pitcher = trimws(tolower(as.character(Pitcher))))

# Step 3: Get distinct pitcher → team mapping
pitcher_teams <- filtered_data %>%
  distinct(Pitcher, PitcherTeam)

# Step 4: Get sorted list of pitcher names
pitchers <- sort(unique(pitcher_teams$Pitcher))

# Step 5: Create named vector mapping pitcher → team
pitcher_team_map <- setNames(pitcher_teams$PitcherTeam, pitcher_teams$Pitcher)

# Log pitcher names for debugging
message("SelectInput pitchers: ", paste(pitchers, collapse = ", "))

# UI
ui <- fluidPage(
  style = "position: relative; height: 100vh;",
  
  absolutePanel(
    top = "5%", left = "2%", width = "250px",
    selectInput(
      inputId = "selected_pitcher",
      label = "Choose a pitcher:",
      choices = c("", pitchers),
      selected = NULL
    )
  ),
  
  absolutePanel(
    top    = "5%", 
    bottom = "5%",
    left   = "30%",   
    width  = "35%", 
    pitcherPercentilePlotUI("pitcherPercentileChart")
  ),
  tags$div(
    style = "
          position: absolute;
          bottom: 5%;
          right: 2%;
          width: 34%;
          max-width: 500px;
          display: flex;
          flex-direction: column;
          gap: 15px;
        ",
    zoneChartUI("zoneChart")
  )
)

# Server
server <- function(input, output, session) {
  # Get the pitcher
  selected_pitcher <- reactive({
    req(input$selected_pitcher)
    tolower(trimws(input$selected_pitcher))  # Standardize selected name
  })
  
  # Compute pitcher stats
  player_data <- advancedPitcherStatsServer(
    id = "advancedPitcherChart",
    data_source = reactive(YakkertechData),
    pitcher_name = selected_pitcher
  )
  
  # Render percentile plot
  pitcherPercentilePlotServer(
    id = "pitcherPercentileChart",
    player_data = player_data
  )
  zoneChartServer(
    id          = "zoneChart",
    data        = reactive(YakkertechData),
    player_name = selected_pitcher
  )
}

# Launch the app
shinyApp(ui = ui, server = server)