library(shiny)
library(tidyverse)
library(rlang)

# Put all the R files here
source("zoneChartModule.R")
source("SprayChart.R")
source("advancedStatsModule.R")
source("basicStatsModule.R")

# Load data
kclYakkertechData <- read_csv("KCLYakkertechData.csv")
beltersYakkertechData <- read_csv("BeltersYakkertechData.csv")
YakkertechData <- read_csv("YakkertechData.csv")


kclAdvancedData <- read_csv("KCLSavantData.csv")
beltersAdvancedData <- read_csv("BeltersSavantData.csv")
AdvancedData <- read_csv("SavantData.csv")

playerDetails <- read_csv("playerDetails.csv")


# UI
ui <- fluidPage(
  style = "position: relative; height: 100vh;",
  
  absolutePanel(
    top   = "5%", left = "2%", width = "250px",
    selectInput(
      inputId  = "selected_player",
      label    = "Choose a player:",
      choices  = sort(unique(YakkertechData$Batter)),
      selected = NULL
    )
  ),
  
  absolutePanel(
    top    = "15%", 
    left   = "2%",
    width  = "26%",
    basicStatsUI("profileChart")
  ),
  
  absolutePanel(
    top    = "5%", 
    bottom = "5%",
    left   = "30%",   
    width  = "35%", 
    advancedStatsUI("advancedChart")
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
    sprayChartUI("sprayChart"),
    zoneChartUI("zoneChart")
  )
)

# Server
server <- function(input, output, session) {
  #Get the player
  selected_player <- reactive({
    req(input$selected_player)
    input$selected_player
  })
  
  #Get player details
  player_details <- reactive({
    playerDetails |> 
      filter(Name == selected_player()) |> 
      select(
        name       = Name,
        position   = Position,
        college    = College,
        side       = Side
      ) %>%
      slice(1)
  })
  
  #Get basic stats
  player_stats <- reactive({
    req(selected_player())
    AdvancedData %>%
      filter(Batter == selected_player()) %>%
      select(AB, H, HR, SB, AVG, OBP, SLG, OPS)
  })
  
  #Get player photos
  action_photo <- reactive({
    normalizePath(
      file.path("www", "actions", paste0(selected_player(), "_action.png")),
      mustWork = FALSE
    )
  })
  
  headshot_photo <- reactive({
    normalizePath(
      file.path("www", "headshots", paste0(selected_player(), "_headshot.png")),
      mustWork = FALSE
    )
  })
  
  #Get basic stats server
  basicStatsServer(
    id           = "profileChart",
    details_df   = player_details,
    stats_df     = player_stats,
    action_img   = action_photo,
    headshot_img = headshot_photo
  )
  
  #Get advanced stats plot
  advancedStatsServer(
    id          = "advancedChart",
    data_source = reactive(AdvancedData),
    player_name = selected_player,
    stat_cols = c("avgExitVelo", "maxExitVelo", "LASweetSpot", "hardHitPct", "squaredUpPct",
                  "kPct", "bbPct", "whiffPct", "chasePct", "xBA", "xSLG", "xWOBA_2",
                  "xBABIP", "babip")
  )
  
  #Get zone chart plot
  zoneChartServer(
    id          = "zoneChart",
    data        = reactive(YakkertechData),
    player_name = selected_player
  )
  
  #Get spray chart plot
  sprayChartServer(
    id          = "sprayChart",
    data        = reactive(YakkertechData),
    player_name = selected_player
  )
}

# Launch the app
shinyApp(ui = ui, server = server)
