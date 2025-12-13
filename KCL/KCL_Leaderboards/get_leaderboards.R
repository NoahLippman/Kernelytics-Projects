### KCL Leaderboard CSV Production Script
library(tidyverse)
library(stringr)

### DATA IMPORT / CLEANING ----

bc_raw <- read_csv("bluecaps.csv", col_names = F)
b_raw <- read_csv("bobcats.csv", col_names = F)
gs_raw <- read_csv("groundsloths.csv", col_names = F)
m_raw <- read_csv("merchants.csv", col_names = F)





master_col <- janitor::make_clean_names(as.character(unlist(bc_raw[2, ])))




convert_ip <- function(ip_string) {
  # Converts "4.2" to 4 + (2/3)
  ip_numeric <- as.numeric(ip_string)
  floor_ip <- floor(ip_numeric)
  decimal <- ip_numeric - floor_ip
  outs <- round(decimal * 10)
  ip_final <- floor_ip + (outs / 3)
  return(ip_final)
}



# Data Cleaning Function
clean_stats <- function(stats){
  total_rows <- nrow(stats)
  result <- stats[-c(1,2, total_rows - 2, total_rows), ]
  colnames(result) <- master_col
  
  # Set important columns to numeric
  result$pa <- as.numeric(result$pa)
  result$avg <- as.numeric(result$avg)
  result$hr <- as.numeric(result$hr)
  result$h <- as.numeric(result$h)
  result$rbi <- as.numeric(result$rbi)
  result$ops <- as.numeric(result$ops)
  result$sb <- as.numeric(result$sb)
  
  result$ip <- as.numeric(result$ip)
  result$er <- as.numeric(result$er)
  result$so_2 <- as.numeric(result$so_2)
  result$whip <- as.numeric(result$whip)
  result$k_bb <- as.numeric(result$k_bb)
  result$w <- as.numeric(result$w)
  
  # Calculate ERA for 9 inning game
  result$ip_num <- convert_ip(result$ip)
  result$er <- as.numeric(result$er)
  
  result$era <- (result$er / result$ip_num) * 9
  
  # Create a name column
  result$Name <- paste(result$first, result$last)
  
  # Remove HR Derby Hitters/Pitchers
  result <- result[!grepl("HR", result$Name), ]
  


  
  
  return(result)
}





bc <- clean_stats(bc_raw)
b <- clean_stats(b_raw)
gs <- clean_stats(gs_raw)
m <- clean_stats(m_raw)




# Add a team column to each dataframe
bc$Team <- "Blue Caps"
b$Team <- "Bobcats"
gs$Team <- "Ground Sloths"
m$Team <- "Merchants"


full_stats <- bind_rows(bc, b, gs, m)




### FUNCTIONS ----

# Stat Leaderboard Function for descending values

get_leaderboard_desc <- function(data, stat){
  results <- data %>%
    drop_na(Name, {{stat}} ) %>%
    arrange(desc( {{stat}}), desc(gp), Name) %>%
    select(Name, Team, {{stat}} ) %>%
    head(5)
}

# Stat leaderboard function for ascending values

get_leaderboard_asc <- function(data, stat){
  results <- data %>%
    drop_na(Name, {{stat}} ) %>%
    arrange({{stat}}, desc(gp), Name) %>%
    select(Name, Team, {{stat}} ) %>%
    head(5)
}



### INDIVIDUAL LEADEBOARDS ----

# Create csvs for the hitting leaderboards

hr <- get_leaderboard_desc(full_stats, hr) %>%
  filter(hr > 0)
colnames(hr) <- c("Name", "Team", "Home Runs")
write.csv(hr, "hr_leaders.csv", row.names = FALSE)


avg <- get_leaderboard_desc(filter(full_stats, pa > 10), avg)
avg$avg <- sprintf("%.3f", avg$avg)
avg$avg <- sub("^0", "", avg$avg)
colnames(avg) <- c("Name", "Team", "Batting Average")
write.csv(avg, "avg_leaders.csv", row.names = FALSE)


rbi <- get_leaderboard_desc(full_stats, rbi)
colnames(rbi) <- c("Name", "Team", "Runs Batted In")
write.csv(rbi, "rbi_leaders.csv", row.names = FALSE)

h <- get_leaderboard_desc(full_stats, h)
colnames(h) <- c("Name", "Team", "Hits")
write.csv(h, "hit_leaders.csv", row.names = FALSE)

sb <- get_leaderboard_desc(full_stats, sb)
colnames(sb) <- c("Name", "Team", "Stolen Bases")
write.csv(sb, "sb_leaders.csv", row.names = FALSE)

ops <- get_leaderboard_desc(filter(full_stats, pa > 10), ops)
ops$ops <- sprintf("%.3f", ops$ops)
colnames(ops) <- c("Name", "Team", "On-Base Plus Slugging")
write.csv(ops, "ops_leaders.csv", row.names = FALSE)



# Create csvs for the pitching leaderboards

era <- get_leaderboard_asc(filter(full_stats, ip > 5), era)
era$era <- sprintf("%.2f", era$era)
colnames(era) <- c("Name", "Team", "ERA")
write.csv(era, "era_leaders.csv", row.names = FALSE)

so <- get_leaderboard_desc(full_stats, so_2)
colnames(so) <- c("Name", "Team", "Strikeouts")
write.csv(so, "so_leaders.csv", row.names = FALSE)

w <- get_leaderboard_desc(full_stats, w)
colnames(w) <- c("Name", "Team", "Wins")
write.csv(w, "w_leaders.csv", row.names = FALSE)

ip <- get_leaderboard_desc(full_stats, ip)
ip$ip <- sprintf("%.1f", ip$ip)
colnames(ip) <- c("Name", "Team", "Innings Pitched")
write.csv(ip, "ip_leaders.csv", row.names = FALSE)

whip <- get_leaderboard_asc(filter(full_stats, ip > 3), whip)
whip$whip <- as.numeric(whip$whip)
whip$whip <- sprintf("%.2f", whip$whip)
colnames(whip) <- c("Name", "Team", "WHIP")
write.csv(whip, "whip_leaders.csv",row.names = FALSE)

kbb <- get_leaderboard_desc(full_stats, k_bb)
colnames(kbb) <- c("Name", "Team", "Strikeout to Walk Ratio")
write.csv(kbb, "kbb_leaders.csv", row.names = FALSE)



### TEAM LEADERBOARDS ----


# Collect total values and compile into one dataframe
bc_tot <- bc_raw[nrow(bc_raw) - 2, ]
colnames(bc_tot) <- master_col
bc_tot$Team <- "Blue Caps"


b_tot <- b_raw[nrow(b_raw) - 2,]
colnames(b_tot) <- master_col
b_tot$Team <- "Bobcats"


gs_tot <- gs_raw[nrow(gs_raw) - 2, ]
colnames(gs_tot) <- master_col
gs_tot$Team <- "Ground Sloths"

m_tot <- m_raw[nrow(m_raw) - 2, ]
colnames(m_tot) <- master_col
m_tot$Team <- "Merchants"


team_stats <- bind_rows(bc_tot, b_tot, gs_tot, m_tot)


# Select hitting stats
team_hit <- team_stats %>%
  select(Team, gp, pa, hr, r, rbi, sb, avg, obp, slg)
colnames(team_hit) <- c("Team", "GP", "PA", "HR", "R", "RBI", "SB", "AVG",
                        "OBP", "SLG")

# Select pitching stats
team_pitch <- team_stats %>%
  select(Team, gp, ip, so_2, bb_2, hr_2, era, whip, fip)
colnames(team_pitch) <- c("Team", "GP", "IP", "SO", "BB","HR", "ERA", "WHIP",
                          "WHIP","FIP")

# Output
write.csv(team_hit, "team_hitting.csv", row.names = FALSE)
write.csv(team_pitch, "team_pitching.csv", row.names = FALSE)
