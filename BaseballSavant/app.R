library(shiny)
library(tidyverse)
library(rlang)

kclData <- read_csv("../KCLYakkertechData.csv")
beltersData <- read_csv("../BeltersYakkertechData.csv")

kclData <- kclData |> 
  mutate(
    PitchCategory = case_when(
      TaggedPitchType %in% c("Cutter", "Fastball", "Sinker") ~ "Fastball",
      TaggedPitchType %in% c("Changeup", "Splitter") ~ "Offspeed",
      TaggedPitchType %in% c("Curveball", "Slider") ~ "Breaking",
      TRUE ~ "Other"
    )
  ) |> 
  mutate(
    bases = case_when(
      PlayResult == "Single" ~ 1,
      PlayResult == "Double" ~ 2,
      PlayResult == "Triple" ~ 3,
      PlayResult == "HomeRun" ~ 4,
      TRUE ~ 0
    )
  )

beltersData <- beltersData |> 
  mutate(
    PitchCategory = case_when(
      TaggedPitchType %in% c("Cutter", "Fastball", "Sinker") ~ "Fastball",
      TaggedPitchType %in% c("Changeup", "Splitter") ~ "Offspeed",
      TaggedPitchType %in% c("Curveball", "Slider") ~ "Breaking",
      TRUE ~ "Other"
    )
  ) |> 
  mutate(
    bases = case_when(
      PlayResult == "Single" ~ 1,
      PlayResult == "Double" ~ 2,
      PlayResult == "Triple" ~ 3,
      PlayResult == "HomeRun" ~ 4,
      TRUE ~ 0
    )
  )

getZoneMetric <- function(df, metric, collapse_zones = FALSE) {
  df2 <- df |> 
    mutate(
      is_AB  = !PlayResult %in% c("Walk", "Sacrifice", NA_character_),
      is_hit = PlayResult %in% c("Single","Double","Triple","HomeRun")
    )
  
  if (collapse_zones) {
    if (metric == "BA") {
      total_H  <- sum(df2$is_hit, na.rm = TRUE)
      total_AB <- sum(df2$is_AB,  na.rm = TRUE)
      value <- if (total_AB > 0) total_H / total_AB else NA_real_
      return(tibble(zone = "All", value = value))
    }
    if (metric == "SLG") {
      total_TB <- sum(df2$bases, na.rm = TRUE)
      total_AB <- sum(df2$is_AB,  na.rm = TRUE)
      value <- if (total_AB > 0) total_TB / total_AB else NA_real_
      return(tibble(zone = "All", value = value))
    }
    if (metric == "xBA") {
      df3   <- df2 %>% filter(!is.na(PlayResult))
      value <- if (nrow(df3) > 0) mean(df3$predicted_xba, na.rm = TRUE) else NA_real_
      return(tibble(zone = "All", value = value))
    }
    if (metric == "xSLG") {
      df3   <- df2 %>% filter(!is.na(PlayResult))
      value <- if (nrow(df3) > 0) mean(df3$predicted_xslg, na.rm = TRUE) else NA_real_
      return(tibble(zone = "All", value = value))
    }
    if (metric == "xWOBA") {
      df3   <- df2 %>% filter(!is.na(PlayResult))
      value <- if (nrow(df3) > 0) mean(df3$predicted_xwoba, na.rm = TRUE) else NA_real_
      return(tibble(zone = "All", value = value))
    }
    stop("Unsupported metric: ", metric)
  }
  
  if (metric == "BA") {
    return(
      df2 %>%
        group_by(zone) %>%
        summarize(
          H  = sum(is_hit, na.rm = TRUE),
          AB = sum(is_AB,  na.rm = TRUE),
          value = if_else(AB > 0, H / AB, NA_real_),
          .groups = "drop"
        ) %>%
        mutate(zone = as.character(zone)) %>%
        select(zone, value)
    )
  }
  if (metric == "SLG") {
    return(
      df2 %>%
        group_by(zone) %>%
        summarize(
          TB = sum(bases, na.rm = TRUE),
          AB = sum(is_AB,  na.rm = TRUE),
          value = if_else(AB > 0, TB / AB, NA_real_),
          .groups = "drop"
        ) %>%
        mutate(zone = as.character(zone)) %>%
        select(zone, value)
    )
  }
  if (metric == "xBA") {
    return(
      df2 %>%
        filter(!is.na(PlayResult)) %>%
        group_by(zone) %>%
        summarize(
          value = mean(predicted_xba, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(zone = as.character(zone))
    )
  }
  if (metric == "xSLG") {
    return(
      df2 %>%
        filter(!is.na(PlayResult)) %>%
        group_by(zone) %>%
        summarize(
          value = mean(predicted_xslg, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(zone = as.character(zone))
    )
  }
  if (metric == "xWOBA") {
    return(
      df2 %>%
        filter(!is.na(PlayResult)) %>%
        group_by(zone) %>%
        summarize(
          value = mean(predicted_xwoba, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(zone = as.character(zone))
    )
  }
  
  stop("Unsupported metric: ", metric)
}

ui <- fluidPage(
  titlePanel("Strike‐Zone Heatmap Tool"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        "league", "Choose League:",
        choices = c("KCL" = "kcl", "CornBelters" = "belters"),
        selected = "kcl"
      ),
      
      radioButtons(
        "viewBy", "View by:",
        choices = c("Player" = "player", "Team" = "team"),
        selected = "player"
      ),
      
      conditionalPanel(
        condition = "input.viewBy == 'player'",
        selectInput("batter", "Choose Batter:", choices = sort(unique(kclData$Batter)))
      ),
      
      conditionalPanel(
        condition = "input.viewBy == 'team'",
        selectInput("team", "Choose Team:", choices = sort(unique(kclData$BatterTeam)))
      ),
      selectInput(
        "metric", "Statistic:",
        choices = c("BA"   = "BA",
                    "SLG"  = "SLG",
                    "xBA"  = "xBA",
                    "xSLG" = "xSLG",
                    "xWOBA"= "xWOBA")
      ),
      radioButtons(
        "hand", "Pitcher Handedness:",
        choices = c("All" = "All", "Left" = "Left", "Right" = "Right"),
        selected = "All"
      ),
      checkboxGroupInput(
        "pitches", "Pitch Category:",
        choices  = c("Fastball","Offspeed","Breaking"),
        selected = c("Fastball","Offspeed","Breaking")
      ),
      radioButtons(
        "granularity", "Zone Granularity:",
        choices = c("3×3 Zones" = "zones", "Whole Zone" = "whole"),
        selected = "zones"
      )
    ),
    
    mainPanel(
      plotOutput("zonePlot", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$league, {
    if (input$league == "kcl") {
      updateSelectInput(session, "batter",
                        choices = sort(unique(kclData$Batter)),
                        selected = sort(unique(kclData$Batter))[1]
      )
      updateSelectInput(session, "team",
                        choices = sort(unique(kclData$BatterTeam)),
                        selected = sort(unique(kclData$BatterTeam))[1]
      )
    } else {
      updateSelectInput(session, "batter",
                        choices = sort(unique(beltersData$Batter)),
                        selected = sort(unique(beltersData$Batter))[1]
      )
      updateSelectInput(session, "team",
                        choices = sort(unique(beltersData$BatterTeam)),
                        selected = sort(unique(beltersData$BatterTeam))[1]
      )
    }
  })
  
  
  filtered_data <- reactive({
    df <- kclData
    if(input$league == "kcl"){
      df <- kclData
    } else{
      df <- beltersData
    }
    
    if(input$viewBy == "player"){
      df <- df |> filter(Batter == input$batter)
    } else{
      df <- df |> filter(BatterTeam == input$team)
    }
    
    if (input$hand != "All") {
      df <- df |>  filter(PitcherThrows == input$hand)
    }
    
    df2 <- df |>  filter(PitchCategory %in% input$pitches)
    
  })
  
  zone_values <- reactive({
    collapse <- (input$granularity == "whole")
    getZoneMetric(df             = filtered_data(),
                  metric         = input$metric,
                  collapse_zones = collapse)
  })
  
  output$zonePlot <- renderPlot({
    vals <- zone_values()
    
    if (input$granularity == "whole") {
      overall <- vals$value[1]
      
      zone_dist <- getZoneMetric(
        df             = filtered_data(),
        metric         = input$metric,
        collapse_zones = FALSE
      )
      zd_vals <- zone_dist$value[!is.na(zone_dist$value)]
      
      if (length(zd_vals) > 0) {
        qs       <- quantile(zd_vals, probs = c(0.10, 0.50, 0.90), na.rm = TRUE)
        low_10   <- unname(qs[1])
        mid_value<- unname(qs[2])
        high_90  <- unname(qs[3])
      } else {
        low_10   <- 0
        mid_value<- 0.5
        high_90  <- 1
      }
      
      buffer    <- (high_90 - low_10) * 0.05
      lower_lim <- low_10   - buffer
      upper_lim <- high_90  + buffer
      
      ggplot(tibble(x = 1, y = 1, value = overall), aes(x = x, y = y, fill = value)) +
        geom_tile(color = "black", width = 1, height = 1) +
        geom_text(aes(label = sprintf("%.3f", value)), size = 10, color = "black") +
        scale_fill_gradientn(
          colours = c("#08519C", "#3182BD", "snow2", "#FC9272", "#CB181D"),
          values  = c(0, 0.25, 0.5, 0.75, 1),
          limits  = c(lower_lim, upper_lim),
          oob     = scales::squish,
          na.value= "snow3"
        ) +
        coord_fixed(xlim = c(0.5, 1.5), ylim = c(0.5, 1.5), expand = FALSE) +
        theme_void() +
        labs(
          title = paste(input$metric, "for", input$batter,
                        if (input$hand == "All") "" else paste("vs", input$hand, "SP"),
                        "\n(", paste(input$pitches, collapse = ", "), ")")
        )
    }
    
    else {
      # 3×3 grid
      plot_data <- vals |> 
        mutate(
          zone = as.integer(zone),
          x = ((zone - 1) %% 3) + 1,
          y = ((zone - 1) %/% 3) + 1
        )
      
      all_zones <- tibble(zone = 1:9) |> 
        mutate(
          x = ((zone - 1) %% 3) + 1,
          y = ((zone - 1) %/% 3) + 1
        )
      
      plot_data <- all_zones |> 
        left_join(plot_data |>  select(zone, value), by = "zone")
      
      #Color Stuff
      zd_vals <- plot_data$value[!is.na(plot_data$value)]
      
      if (length(zd_vals) > 0) {
        qs       <- quantile(zd_vals, probs = c(0.10, 0.50, 0.90), na.rm = TRUE)
        low_10   <- unname(qs[1])
        mid_value<- unname(qs[2])
        high_90  <- unname(qs[3])
      } else {
        low_10   <- 0
        mid_value<- 0.5
        high_90  <- 1
      }
      
      buffer    <- (high_90 - low_10) * 0.05
      lower_lim <- low_10   - buffer
      upper_lim <- high_90  + buffer
      
      #Start Plotting
      ggplot(plot_data, aes(x = x, y = y, fill = value)) +
        geom_tile(color = "black") +
        geom_text(aes(label = ifelse(is.na(value), "", sprintf("%.3f", value))),
                  size = 5, color = "black") +
        scale_fill_gradientn(
          colours = c("#08519C", "#3182BD", "snow2", "#FC9272", "#CB181D"),
          values  = c(0, 0.25, 0.5, 0.75, 1),
          limits  = c(lower_lim, upper_lim),
          oob     = scales::squish,
          na.value= "snow3"
        ) +
        coord_fixed(
          xlim = c(0.5, 3.5), 
          ylim = c(0.5, 3.5),
          expand = FALSE
        ) +
        scale_x_continuous(breaks = 1:3, labels = NULL, expand = c(0, 0)) +
        scale_y_continuous(breaks = 1:3, labels = NULL, expand = c(0, 0)) +
        theme_void() +
        labs(
          title = paste(input$metric, "in each zone for", 
                        if (input$viewBy == "player") {
                          input$batter
                        } else {
                          input$team
                        },
                        if (input$hand == "All") "" else paste("vs", input$hand, "SP"),
                        "\n(", paste(input$pitches, collapse = ", "), ")")
        )
    }
  }
  
  # output$percentilePlot <- renderPlot({
  #   
  # })
  
  
  , res = 96)
}

shinyApp(ui = ui, server = server)



