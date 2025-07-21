library(shiny)
library(tidyverse)

## R Files for plot outputs ##
source("playsResponsibleMap.R")
source("defenseSprayChart.R")
source("rangePercentileChart.R")
## Load Data ##

startingPositions <- read.csv("/Users/noahlippman/Documents/GitHub/Kernelytics-Projects/KCL_Defense/startingPositions.csv") %>%
  mutate(startingX = xCord) %>%
  mutate(startingY = yCord) %>%
  select(Position, startingX, startingY)

playsData <- read.csv("/Users/noahlippman/Documents/GitHub/Kernelytics-Projects/KCL_Defense/playerScores.csv") %>%
  left_join(startingPositions, by = c("playerPosition" = "Position")) %>%
  mutate(distanceFromAverageStart = sqrt((startingX - X_Cord)^2 + (startingY - Y_Cord)^2)) %>%
  mutate(outOrHit = if_else(PlayResult %in% c("Out", "Sacrifice"), "Out","Hit"))

## Load data ##

players <- unique(playsData$Player)

## UI ##
ui <- fluidPage(
  
  # Player Selector #
  absolutePanel(
    top = "3%", left = "2%", width = "300px",
    wellPanel(
      selectInput(
        inputId = "selected_player",
        label = "Choose a player",
        choices = players,
        selected = NULL
      )
    )
  ),
  div("Range Percentile Chart",
      style = "position: absolute; left: 9.5%; top: 52%; font-weight: bold; font-size: 22px;"),
  
  # OAA Dataframe #
  absolutePanel(
    top = "17%", left = "2%", width = "30%",
    oAATableUI("oAATable")
  ),
  
  # Range Percentile Chart #
  absolutePanel(
    top = "57%", left = "2%", width = "30%",
    rangePercentileChartUI("rangePercentileChart")
  ),
  
  # Position & Hang Time Selector #
  absolutePanel(
    top = "57%", left = "63%", width = "20%", height = "300px",
    wellPanel(
      sliderInput(
        inputId = "hang_times",
        label = "Select a Hang Time Range",
        min = 0, max = 10,
        value = c(0,10)
      ),
      checkboxGroupInput(
        inputId = "position",
        label = "Choose a Position",
        selected = 6,
        choices = NULL
      )
    )
  ),
  
  absolutePanel(
    top = "17%", left = "33%", width = "30%",
    playsResponsibleUI("playsResponsibleChart")
  ),
  
  absolutePanel(
    top = "57%", left = "33%", width = "30%",
    defenseSprayChartUI("defenseSprayChart")
  )
)

server <- function(input, output, session) {
  
  # Get the player
  selected_player <- reactive({
    req(input$selected_player)
    input$selected_player
  })
  
  options_selected <- reactiveVal(FALSE)
  
  observeEvent(selected_player(), {
    updateCheckboxGroupInput(
      session,
      "position",
      choices = unique((playsData %>% filter(Player == selected_player()))$playerPosition),
      selected =  unique((playsData %>% filter(Player == selected_player()))$playerPosition)
    )
  })
  
  selected_position <- reactive({
    req(input$position)
    input$position
  })
  
  selected_hang_time_lower <- reactive({
    req(input$hang_times)
    input$hang_times[1]
  })
  
  selected_hang_time_upper <- reactive({
    req(input$hang_times)
    input$hang_times[2]
  })
  
  playsResponsibleServer(
    id = "playsResponsibleChart",
    player_name = selected_player
  )
  
  defenseSprayChartServer(
    id = "defenseSprayChart",
    player_name = selected_player,
    position = selected_position,
    hang_time_lower = selected_hang_time_lower,
    hang_time_upper = selected_hang_time_upper
  )
  
  oAATableServer(
    id = "oAATable", 
    player_name = selected_player
  )
  
  rangePercentileChartServer(
    id = "rangePercentileChart",
    player_name = selected_player
  )
  
  
}

shinyApp(ui = ui, server = server)

