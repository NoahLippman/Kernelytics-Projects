library(shiny)
library(tidyverse)
library(rsconnect)

tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);")

## R Files for plot outputs ##
source("/Users/noahlippman/Documents/GitHub/Kernelytics-Projects/KCL_Defense/playsResponsibleMap.R")
source("/Users/noahlippman/Documents/GitHub/Kernelytics-Projects/KCL_Defense/defenseSprayChart.R")
source("/Users/noahlippman/Documents/GitHub/Kernelytics-Projects/KCL_Defense/rangePercentileChart.R")
source("/Users/noahlippman/Documents/GitHub/Kernelytics-Projects/KCL_Defense/oAAtable.R")
source("/Users/noahlippman/Documents/GitHub/Kernelytics-Projects/KCL_Defense/catchProbChart.R")

## Load Data ##
startingPositions <- read.csv("/Users/noahlippman/Documents/GitHub/Kernelytics-Projects/KCL_Defense/startingPositions.csv") %>%
  mutate(startingX = xCord) %>%
  mutate(startingY = yCord) %>%
  select(Position, startingX, startingY)

playsData <- read.csv("/Users/noahlippman/Documents/GitHub/Kernelytics-Projects/KCL_Defense/playerScores.csv") %>%
  left_join(startingPositions, by = c("playerPosition" = "Position")) %>%
  mutate(distanceFromAverageStart = sqrt((startingX - X_Cord)^2 + (startingY - Y_Cord)^2)) %>%
  mutate(outOrHit = if_else(PlayResult %in% c("Out", "Sacrifice"), "Out","Hit"))

## Position Options ##
players <- unique(playsData$Player)
numToPositionApp <- c('3' = "First Base", '4' = "Second Base", '5' = "Third Base", '6' = 'Short Stop', 
                 '7' = 'Leftfield', '8' = 'Centerfield', '9' = 'Rightfield')

## UI ##
ui <- fluidPage(
  
  # Player Selector #
  absolutePanel(
    top = "3.5%", left = "2%", width = "350px",
    wellPanel(
      selectInput(
        inputId = "selected_player",
        label = "Choose a player",
        choices = players,
        selected = "Adan Nieves"
      )
    )
  ),
  
  div("Catch Probability chart by HangTime and Direction",
      style = "position: absolute; left: 7.5%; top: 23%; font-weight: bold; font-size: 22px;"),
  div("OAA Total Values by Direction", 
      style = "position: absolute; left: 40%; top: 81%; font-weight: bold; font-size: 22px"),
  div("Catch Probability Interactive Spray Chart",
      style = "position: absolute; left: 62.5%; top: 23%; font-weight: bold; font-size: 22px"),
  div("Range Percentile Chart (Batter's View)", 
      style = "position: absolute; left: 11%; top: 121%; font-weight: bold; font-size: 22px"),
  div("Defensive Spray Chart", 
      style = "position: absolute; left: 69%; top: 121%; font-weight: bold; font-size: 22px"), 
  div("Click a Point on Left Graph", 
      style = "position: absolute; left: 42.7%; top: 43%; font-weight: bold; font-size: 17px"),
  
  div(icon("arrow-right", class = "fa-5x"),
      style = "position: absolute; left: 47%; top: 46%"),
  div("h",
      style = "position: absolute; top: 180%; font-size: 1px"),
  h2(" ", style = "border-top: 1.5px solid #000000; margin-top: 52.45%; margin-bottom: 0%; width: 39%; margin-left: 0%;"), 
  h2(" ", style = "border-top: 1.5px solid #000000; margin-top: 0%; margin-bottom: 0%; margin-left: 59%;"), 
  
  # OAA Dataframe #
  absolutePanel(
    top = "86%", left = "17%", width = "65%",
    oAATableUI("oAATable")
  ),
  
  # Range Percentile Chart #
  absolutePanel(
    top = "126%", left = "4%", width = "38%",
    rangePercentileChartUI("rangePercentileChart")
  ),
  
  # Position & Hang Time Selector #
  absolutePanel(
    top = "168%", left = "56%", width = "38%",
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
        selected = "ShortStop",
        choices = NULL,
        inline = TRUE
      )
    )
  ),
  
  absolutePanel(
    top = "28%", left = "4%", width = "38%",
    playsResponsibleUI("playsResponsibleChart")
  ),
  
  absolutePanel(
    top = "126%", left = "56%", width = "38%",
    defenseSprayChartUI("defenseSprayChart")
  ), 
  
  absolutePanel(
    top = "28%", left = "56%", width = "38%", 
    catchProbUI("catchProbChart")
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
      choices = unname(numToPositionApp[unique(as.character((playsData %>% filter(Player == selected_player()))$playerPosition))]),
      selected =  unname(numToPositionApp[unique(as.character((playsData %>% filter(Player == selected_player()))$playerPosition))])
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
  
  clicked_point <- reactive({nearPoints(playsData %>% filter(Player == selected_player() & HangTime != 0), 
                              input$plot_click, 
                              threshold = 10, 
                              maxpoints = 1)
  })
  print(clicked_point)
  
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
  
  observeEvent(clicked_point(), {
    catchProbServer(
      id = "catchProbChart", 
      Position = clicked_point()$playerPosition, 
      X_Cord = clicked_point()$X_Cord,
      Y_Cord = clicked_point()$Y_Cord, 
      hangTime = round(clicked_point()$HangTime * 2)/2, 
      outOrHit = clicked_point()$outOrHit
    )
  })
  
  
}

shinyApp(ui = ui, server = server)

