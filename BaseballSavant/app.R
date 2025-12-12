library(shiny)
library(tidyverse)
library(rlang)

# Put all the R files here
source("zoneChartModule.R")
source("SprayChart.R")
source("advancedStatsModule.R")
source("basicStatsModule.R")
source("referenceStats.R")
source("rollingXWOBAModule.R")


# Load data
YakkertechData <- read_csv("yakkertechData.csv")
AdvancedData <- read_csv("SavantData.csv")

playerDetails <- read_csv("playerDetails.csv")


# UI
ui <- fluidPage(
  
  conditionalPanel(
    condition = "!output.login_successful",
    div(style = "
          position: absolute;
          top: 30%;
          left: 35%;
          width: 400px
        ",
        wellPanel(
          h2("Baseball Savant Login", style = "text-align: center;"),
          textInput("user", "Username", value = ""),
          textInput("password", "Password", value = ""),
          actionButton("go_to_main_page", "Enter")
        )
    )
  ),
  
  style = "padding: 0; margin: 0; overflow-x: hidden;",
  
  conditionalPanel(
    condition = "output.login_successful",
    div(
      style = "min-height: 100vh; position: relative;",
      
      # Fixed player selector
      absolutePanel(
        top   = "5%", left = "2%", width = "250px",
        selectInput(
          inputId  = "selected_player",
          label    = "Choose a player:",
          choices  = NULL,
          selected = NULL
        )
      ),
      
      # Team Logo and Team Name
      absolutePanel(
        top   = "0%",
        right = "2%",
        width = "230px",
        uiOutput("teamDisplay")
      ),
      
      # Basic Stats Panel
      absolutePanel(
        top    = "13%", 
        left   = "2%",
        width  = "26%",
        basicStatsUI("profileChart")
      ),
      
      # Advanced Stats
      absolutePanel(
        top    = "5%", 
        bottom = "2%",
        left   = "30%",   
        width  = "35%", 
        advancedStatsUI("advancedChart")
      ),
      
      # Rolling XWOBA
      absolutePanel(
        top = "61%",
        bottom = "2%",
        left   = "2%",   
        width  = "26%", 
        rollingXWOBAUI("rollingChart")
      ),
      
      # Zone & Spray Chart
      tags$div(
        style = "
          position: absolute;
          bottom: 2%;
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
    ),
    
    # Scrollable section below fixed panels
    div(
      style = "
        margin-top: 0vh;
        padding: 20px 5%;
      ",
      referenceStatsUI("refStats")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Process Login Info
  permissions <- c("bobcats8729", "groundsloths4528", "merchants9253", "bluecaps0827", "admin1928")
  
  login_status <- reactiveVal(FALSE)
  
  observeEvent(input$go_to_main_page, {
    creds <- paste0(tolower(input$user), input$password)
    login_status(creds %in% permissions)
  })
  
  output$login_successful <- reactive({
    login_status()
  })
  
  #Filter Dataset
  filtered_data <- reactive({
    req(input$user)
    if(tolower(input$user) != "admin"){
      YakkertechData %>%
        filter(BatterTeam == paste("Kcl", tolower(input$user), "2025")) |> 
        arrange(Batter)
    }
    else{
      YakkertechData |> 
        arrange(Batter)
    }
  })
  
  observeEvent(filtered_data(), {
    updateSelectInput(
      session,
      "selected_player",
      choices = unique(filtered_data()$Batter)
    )
  })
  
  outputOptions(output, "login_successful", suspendWhenHidden = FALSE)
  
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
        side       = Side,
        team       = Team
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
      file.path("www", "actions", paste0(selected_player(), "_action.jpg")),
      mustWork = FALSE
    )
  })
  
  headshot_photo <- reactive({
    normalizePath(
      file.path("www", "headshots", paste0(selected_player(), "_headshot.jpg")),
      mustWork = FALSE
    )
  })
  
  #Get Team Display and Logo
  output$teamDisplay <- renderUI({
    team_name <- player_details()$team
    logo_src  <- paste0(team_name, ".png")
    
    div(
      style = "
      display: flex;
      align-items: center;
      justify-content: flex-start;
      gap: 20px;
      padding: 5px;
    ",
      
      # Logo
      tags$img(
        src   = logo_src,
        style = "height: 130px; max-width: 130px; object-fit: contain;"
      )
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
  
  #Reference Stats Server
  referenceStatsServer(
    id = "refStats",
    stats_df = reactive({
      AdvancedData %>%
        filter(Batter == selected_player())
    })
  )
  
  #Get Rolling XWOBA Plot
  rollingXWOBAServer(
    id          = "rollingChart",
    data_source = reactive(YakkertechData),
    player_name = selected_player
  )
  
  #Get advanced stats plot
  advancedStatsServer(
    id          = "advancedChart",
    data_source = reactive(AdvancedData),
    player_name = selected_player,
    stat_cols = c("avgExitVelo", "maxExitVelo", "LASweetSpot", "hardHitPct", "squaredUpPct",
                  "kPct", "bbPct", "whiffPct", "chasePct", "xBA", "xSLG", "xWOBA", "xWOBA_2",
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