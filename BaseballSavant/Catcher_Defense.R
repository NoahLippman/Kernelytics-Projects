library(rbind)
# Data Collection
kcl_path = "/Users/noahlippman/Documents/Github/Kernelytics-Projects/kclData"
kcl_files <- list.files(path = kcl_path)
game_data <- read.csv(paste(kcl_path, "/", kcl_files[1], sep = ""))

for(i in kcl_files[2:length(kcl_files)]){
  game_data = bind_rows(game_data, read.csv(paste(kcl_path,"/",i, sep = "")))
}

belters_path = "/Users/noahlippman/Documents/GitHub/Kernelytics-Projects/CornBeltersData"
belters_files <- list.files(path = belters_path)

for(i in belters_files[1:length(belters_files)]){
  game_data = bind_rows(game_data, read.csv(paste(belters_path, "/", i, sep = "")))
}

#Fix Names in DataFrame
corrections <- c(
  "Justin Trusner" = "Jacob Trusner",
  "Brooks Neuhof" = "Brooks Neuhoff",
  "Teagan Disharoom" = "Teagan Disharoon",
  "Kam Ross" = "Kam Ross",
  "Brayden Windy" = "Brayden Windy",
  "Sammy Descarpentrie" = "Sam DesCarpentrie"
)

fixNames <- function(df, column, lookup){
  old_values <- df[[column]]
  needs_fix <- old_values %in% names(lookup)
  df[[column]] <- ifelse(
    needs_fix,
    lookup[old_values],
    old_values
  )
  return(df)
}
game_data <- fixNames(game_data, "Catcher", corrections)

#Assign Strikezone boxes to each of the pitches

strike_zone_augment <- function(data) {
  strike_zone <- data %>%
  filter(!is.na(PlateLocSide) & !is.na(PlateLocHeight)
         & PitchCall %in% c("StrikeCalled", "BallCalled")) %>%
  mutate(xZone = case_when(
    PlateLocSide <= 1.3333 & PlateLocSide >= 1 ~ 0,
    PlateLocSide < 1 & PlateLocSide >= .3333 ~ 1,
    PlateLocSide < .3333 & PlateLocSide >= -.3333 ~ 2,
    PlateLocSide < -.3333 & PlateLocSide >= -1 ~ 3,
    PlateLocSide < -1 & PlateLocSide >= -1.3333 ~ 4
  )) %>%
  mutate(yZone = case_when(
    PlateLocHeight >= .84 & PlateLocHeight <= 1.5 ~ 0,
    PlateLocHeight > 1.5 & PlateLocHeight <= 2.17 ~ 1,
    PlateLocHeight > 2.17 & PlateLocHeight <= 2.83 ~ 2,
    PlateLocHeight > 2.83 & PlateLocHeight <= 3.5 ~ 3,
    PlateLocHeight > 3.5 & PlateLocHeight <= 4.16 ~ 4
  )) %>%
  mutate(CalledStrike = if_else(PitchCall == "StrikeCalled",1,0))

  strike_zone <- strike_zone %>%
    filter(!is.na(xZone) & !is.na(yZone))
  return(strike_zone)
}

#Process data frame into a grouped matrix for a given player and league
data_processer <- function(player_name, league, data) {
  total_Matrix <- data %>%
    {
      if(league != ""){
        filter(., grepl(league,PitcherTeam, fixed = TRUE))
      }
      else{
        filter(., !(grepl("Kcl", PitcherTeam, fixed = TRUE)))
      } 
    } %>%
    group_by(yZone, xZone) %>%
    summarize(Called_Strike_Rate = mean(CalledStrike), .groups = "drop")
  
  zone_Matrix <- data %>%
    filter(Catcher == player_name) %>%
    group_by(yZone, xZone) %>%
    summarize(Called_Strike_Rate = mean(CalledStrike), .groups = "drop") %>%
    full_join(total_Matrix, by = c("yZone", "xZone")) %>%
    mutate(CS_Rate_aboveAverage = Called_Strike_Rate.x - Called_Strike_Rate.y) %>%
    select(yZone, xZone, CS_Rate_aboveAverage) %>%
    mutate(height = 1) %>%
    mutate(width  = 1)
  
  xOut_Matrix <- zone_Matrix %>%
    filter(xZone == 0 | xZone == 4 & yZone != 0 & yZone != 4) %>%
    group_by(xZone) %>%
    summarise(CS_Rate_aboveAverage = mean(CS_Rate_aboveAverage)) %>%
    mutate(yZone = 2) %>%
    mutate(height = 3) %>%
    mutate(width  = 1)
  
  yOut_Matrix <- zone_Matrix %>%
    filter(yZone == 0 | yZone == 4 & xZone != 0 & xZone != 4) %>%
    group_by(yZone) %>%
    summarise(CS_Rate_aboveAverage = mean(CS_Rate_aboveAverage)) %>%
    mutate(xZone = 2) %>%
    mutate(height = 1) %>%
    mutate(width  = 3)
  
  corner_Matrix <- zone_Matrix %>%
    filter((xZone == 0 & yZone == 0)|
             (xZone == 0 & yZone == 4)|
             (xZone == 4 & yZone == 0)|
             (xZone == 4 & yZone == 4)) %>%
    mutate(height = 1) %>%
    mutate(width  = 1)
  
  full <- rbind(zone_Matrix %>% filter(xZone > 0 & yZone > 0 & xZone < 4 & yZone < 4), xOut_Matrix, yOut_Matrix, corner_Matrix)
  return(full %>% as.matrix())
}

# Find a Player's called strikes added or lost compared to league average
CS_above_average <- function(player_name, league, data){
  total_Matrix <- data %>%
    {
      if(league != ""){
        filter(., grepl(league,PitcherTeam, fixed = TRUE))
      }
      else{
        filter(., !(grepl("Kcl", PitcherTeam, fixed = TRUE)))
      } 
    } %>%
    group_by(yZone, xZone) %>%
    summarize(Called_Strike_Rate = mean(CalledStrike), .groups = "drop")
  
  zone_Matrix <- data %>%
    filter(Catcher == player_name) %>%
    group_by(yZone, xZone) %>%
    summarize(Called_Strike_Opportunities = length(CalledStrike),
              Called_Strike_Total = sum(CalledStrike), .groups = "drop") %>%
    full_join(total_Matrix, by = c("yZone", "xZone")) %>%
    mutate(CS_Above_Average = case_when(
      is.na(Called_Strike_Opportunities) ~ 0,
      Called_Strike_Rate != 0 ~ Called_Strike_Total * (1 - Called_Strike_Rate) - ((Called_Strike_Opportunities - Called_Strike_Total) *     (Called_Strike_Rate)),
      Called_Strike_Rate == 0 ~ Called_Strike_Total
    ))
  return(c(round(sum(zone_Matrix$CS_Above_Average),0), round(sum(zone_Matrix$Called_Strike_Opportunities, na.rm = TRUE),0)))
}

# Plot a Player's CS% HeatMap
Catcher_Framing_Map <- function(player_name, league, data){
  strike_zone_data <- strike_zone_augment(game_data)
  data <- data_processer(player_name, league, strike_zone_data)
  CS_Added <- CS_above_average(player_name, league, strike_zone_data)
  p <- ggplot(data, aes(xZone, yZone, fill = CS_Rate_aboveAverage)) +
    geom_tile(aes(height = height, width = width)) +
    ylim(-2,6) +
    xlim(-6, 10) + 
    theme_void() +
    scale_fill_gradientn(
      colours = c("#1E90FF", "#87CEEB", "#ADD8E6", "white", "#FFC1CC", "#FF6666", "#FF6666"),
      values = scales::rescale(c(-1, -0.5, -0.2, 0, 0.2, 0.5, 1)),
      limits = c(-1, 1),
      na.value = "grey90"
    ) +
    labs(fill = "CS% above average") + 
    theme(legend.position = c(.85,.5)) + 
    geom_text(aes(label = round(CS_Rate_aboveAverage,2)), color = "black") +
    geom_segment(aes(x = .5, y = .5, xend = 3.5, yend = .5), linewidth = 1) + 
    geom_segment(aes(x = .5, y = 3.5, xend = 3.5, yend = 3.5), linewidth = 1) + 
    geom_segment(aes(x = .5, y = .5, xend = .5, yend = 3.5), linewidth = 1) +  
    geom_segment(aes(x = 3.5, y = .5, xend = 3.5, yend = 3.5), linewidth = 1) +
    geom_segment(aes(x = .5, y = -1, xend = 3.5, yend = -1), linewidth = 1) +
    geom_segment(aes(x = .5, y = -1, xend = .5, yend = -1.5), linewidth = 1) +
    geom_segment(aes(x = 3.5, y = -1, xend = 3.5, yend = -1.5), linewidth = 1) +
    geom_segment(aes(x = .5, y = -1.5, xend = 2, yend = -2), linewidth = 1) +
    geom_segment(aes(x = 3.5, y = -1.5, xend = 2, yend = -2), linewidth = 1) + 
    { if(CS_Added[1] >= 0){
      ggtitle(paste("Called Strike Rate Above League Average for", player_name), 
              subtitle = paste(CS_Added[1], "Strikes Added in", CS_Added[2], "Opportunities"))
    }
      else{
        ggtitle(paste("Called Strike Rate Above League Average for", player_name), 
                subtitle = paste(abs(CS_Added[1]), "Strikes Lost in", CS_Added[2], "Opportunities"))
      }
      
    } +
    theme(plot.title = element_text(hjust = .5)) + 
    theme(plot.title = element_text(vjust = -10)) +
    theme(plot.subtitle = element_text(hjust =.5, vjust = -20))
  return(p)
}
Catcher_Framing_Map("Lawson Alwan", "Kcl")

# -----------------------------
# Shiny Module UI/Server
# -----------------------------
CatcherFramingUI <- function(id) {
  ns <- NS(id)
  div(
    style = "
      display: flex;
      flex-direction: row;
      gap: 10px;
      background: #f9f9f9;
      padding: 10px;
      border: 1px solid #ccc;
      border-radius: 8px;
    ",
    # the plot itself
    div(
      style = "flex: 1 1 auto; min-width: 0;",
      plotOutput(ns("Catcher_Framing"), height = "300px")
    )
  )
}

CatcherFramingServer <- function(id, data_source, player_name, league) {
  moduleServer(id, function(input, output, session) {
    output$Catcher_Framing <- renderPlot({
      # make sure we have a catcher
      req(player_name())
      df <- data_source()
      league <- league()
      # call your function
      Catcher_Framing_Map(df, player_name(), league)
    })
  })
}


