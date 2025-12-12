library(shiny)
library(tidyverse)

# --------------------------------------------------
# UI: Basic Player Info Card
# --------------------------------------------------
basicStatsUI <- function(id) {
  ns <- NS(id)
  div(
    class = "basic-info-card",
    style = "
      display: flex;
      flex-direction: column;
      background: #fff;
      border: 1px solid #ccc;
      border-radius: 8px;
      overflow: hidden;
      box-shadow: 0 2px 6px rgba(0,0,0,0.1);
      width: 100%;
    ",
    
    # Action photo container (200px tall)
    div(
      style = "
        position: relative;
        width: 100%;
        height: 200px;
        overflow: hidden;
        display: flex;
        justify-content: center;
        align-items: center;
        background: #fff;
      ",
      # Placeholder for action shot
      imageOutput(ns("actionPhoto"), width = "auto", height = "200px"),
      
      # Headshot overlay centered bottom
      tags$div(
        style = "
          position: absolute;
          bottom: 5px;
          left: 50%;
          transform: translateX(-50%);
          width: 100px;
          height: 100px;
          border-radius: 50%;
          overflow: hidden;
          border: 2px solid #fff;
          background: #fff;
        ",
        # Placeholder for headshot
        imageOutput(ns("headshot"), width = "60px", height = "60px")
      )
    ),
    
    # Details and stats below images
    div(
      style = "padding: 12px;",
      uiOutput(ns("playerDetails")),
      tableOutput(ns("basicStats"))
    )
  )
}

# --------------------------------------------------
# SERVER: Basic Player Info Card Logic
# --------------------------------------------------
basicStatsServer <- function(id,
                             details_df,    # reactive 1-row df: name, bat_hand, throw_hand, college
                             stats_df,      # reactive 1-row df: AB, HR, AVG, OBP, OPS, etc.
                             action_img,    # reactive string path/URL for action photo
                             headshot_img   # reactive string path/URL for headshot
) {
  moduleServer(id, function(input, output, session) {
    output$actionPhoto <- renderImage({
      req(action_img())
      list(
        src         = action_img(),
        contentType = "image/png",
        alt         = "Action shot",
        width       = NULL,
        height      = 200
      )
    }, deleteFile = FALSE)
    
    output$headshot <- renderImage({
      req(headshot_img())
      list(
        src         = headshot_img(),
        contentType = "image/png",
        alt         = "Headshot",
        width       = 100,
        height      = 100
      )
    }, deleteFile = FALSE)
    
    # Player details list
    output$playerDetails <- renderUI({
      df <- details_df()
      tags$ul(
        style = "list-style: none; padding-left: 0; margin: 0 0 12px 0;",
        tags$li(tags$strong("Name: "),    df$name),
        tags$li(tags$strong("Position: "),    df$position),
        tags$li(tags$strong("College: "),  df$college),
        tags$li(tags$strong("Throws/Bats: "), df$side)
      )
    })
    
    # Basic stats table
    output$basicStats <- renderTable({
      stats_df() %>% select(AB, H, HR, SB, AVG, OBP, SLG, OPS) |> 
        transmute(
          AB  = as.integer(round(AB, 0)),
          H   = as.integer(round(H, 0)),
          HR  = as.integer(round(HR, 0)),
          SB  = as.integer(round(SB, 0)),
          AVG = sprintf("%.3f", AVG),
          OBP = sprintf("%.3f", OBP),
          SLG = sprintf("%.3f", SLG),
          OPS = sprintf("%.3f", OPS)
        )
    }, rownames = FALSE, spacing = "l", align = "c")
  })
}