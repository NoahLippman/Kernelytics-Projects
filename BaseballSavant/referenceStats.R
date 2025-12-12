library(tidyverse)



# --------------------------------------------------
# UI: Reference Stats For Player
# --------------------------------------------------
referenceStatsUI <- function(id) {
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
    
    div(
      style = "text-align: center; margin-bottom: 5px;",
      h3("Standard Hitting")
    ),
    
    div(
      style = "padding: 12px;",
      tableOutput(ns("referenceStats"))
    )
  )
}

# --------------------------------------------------
# SERVER: Reference Stats For Player
# --------------------------------------------------
referenceStatsServer <- function(id, stats_df) {
  moduleServer(id, function(input, output, session) {
    
    # Reference stats table
    output$referenceStats <- renderTable({
      stats_df() %>% select(GP, PA, AB, R, H, `2B`, `3B`, HR, RBI, SB, CS, BB, SO, AVG, OBP, SLG, OPS, `OPS+`, wOBA, `wRC+`, oWAR) |> 
        transmute(
          GP  = as.integer(round(GP, 0)),
          PA  = as.integer(round(PA, 0)),
          AB  = as.integer(round(AB, 0)),
          R  = as.integer(round(R, 0)),
          H   = as.integer(round(H, 0)),
          `2B` = as.integer(round(`2B`, 0)),
          `3B` = as.integer(round(`3B`, 0)),
          HR  = as.integer(round(HR, 0)),
          RBI  = as.integer(round(RBI, 0)),
          SB  = as.integer(round(SB, 0)),
          CS  = as.integer(round(CS, 0)),
          BB  = as.integer(round(BB, 0)),
          SO  = as.integer(round(SO, 0)),
          AVG = sprintf("%.3f", AVG),
          OBP = sprintf("%.3f", OBP),
          SLG = sprintf("%.3f", SLG),
          OPS = sprintf("%.3f", OPS),
          `OPS+`  = as.integer(round(`OPS+`, 0)),
          wOBA = sprintf("%.3f", wOBA),
          `wRC+`  = as.integer(round(`wRC+`, 0)),
          oWAR = sprintf("%.1f", oWAR)
        )
    }, rownames = FALSE, spacing = "l", align = "c")
  })
}