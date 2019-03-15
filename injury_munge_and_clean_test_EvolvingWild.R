# Install and load packages
install.packages("tidyverse")
library(tidyverse)

# Load WAR data, injury data 
skater_war_df <- read_csv("Evolving_Hockey_GAA_GAR_Skaters_2019-03-07.csv")
team_war_df <- read_csv("Evolving_Hockey_League_GAR_2019-03-07.csv")
injury_df <- read_csv("NHL_Injury_Database_data.csv")
team_statistics_df <- read_csv("NHL_team_data.csv")

# Create a new WAR df with the required data 
war_gp_df <- skater_war_df %>%
  select(player, position, season, team = Team, games_played = GP, toi = TOI_all, war = WAR) %>%
  # Create games_missed var, for comparison to games_missed_injury; account for absences for personal reasons, etc.
  mutate(games_missed = 82 - games_played,
         war_gp = war / games_played)

# Determine a weighted WAR for each player
weight_war <- function(last3_war) {
  player_season <- as.numeric(stringr::str_split_fixed(last3_war, " ", 3))
  if (is.na(player_season[2]))
    player_season[1]
  else if (is.na(player_season[3]))
    weighted.mean(player_season[1:2], c(0.542, 0.458))
  else
    weighted.mean(player_season, c(0.417, 0.333, 0.25))
}

war_gp_df <- war_gp_df %>%
  group_by(player) %>%
  arrange(player, season) %>%
  mutate(last3_war = paste(war_gp, lag(war_gp), lag(war_gp, 2))) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(weighted_war_gp = weight_war(last3_war)) 

# Re-format war82_df team names
war_gp_df$team[war_gp_df$team == "L.A"] <- "LAK"
war_gp_df$team[war_gp_df$team == "N.J"] <- "NJD"
war_gp_df$team[war_gp_df$team == "T.B"] <- "TBL"

# Re-format war82_df player names before separating into first_name, last_name
war_gp_df$player[war_gp_df$player == "A.J..GREER"] <- "AJ.GREER"
war_gp_df$player[war_gp_df$player == "B.J..CROMBEEN"] <- "BJ.CROMBEEN"
war_gp_df$player[war_gp_df$player == "J.T..COMPHER"] <- "JT.COMPHER"
war_gp_df$player[war_gp_df$player == "J.T..BROWN"] <- "JT.BROWN"
war_gp_df$player[war_gp_df$player == "J.T..MILLER"] <- "JT.MILLER"
war_gp_df$player[war_gp_df$player == "MARTIN.ST. LOUIS"] <- "MARTIN.ST LOUIS"
war_gp_df$player[war_gp_df$player == "MARTIN.ST. PIERRE"] <- "MARTIN.ST PIERRE"
war_gp_df$player[war_gp_df$player == "P.A..PARENTEAU"] <- "PA.PARENTEAU"
war_gp_df$player[war_gp_df$player == "P.J..AXELSSON"] <- "PJ.AXELSSON"
war_gp_df$player[war_gp_df$player == "P.K..SUBBAN"] <- "PK.SUBBAN"
war_gp_df$player[war_gp_df$player == "R.J..UMBERGER"] <- "RJ.UMBERGER"
war_gp_df$player[war_gp_df$player == "T.J..GALIARDI"] <- "TJ.GALIARDI"
war_gp_df$player[war_gp_df$player == "T.J..HENSICK"] <- "TJ.HENSICK"
war_gp_df$player[war_gp_df$player == "T.J..OSHIE"] <- "TJ.OSHIE"

#hyphen_names <- EW_war_gp_df %>%
  #filter(str_detect(player, '-'))

# Separate war82_df player names into first_name, last_name
war_gp_df <- war_gp_df %>%
  separate(player, c("first_name","last_name"), sep = '\\.') %>%
  # Create first_initials var for join w/ player_injury_df
  mutate(first_initials = substr(first_name, 1, 2)) 

# Create a new injury df with the required data
player_injury_df <- injury_df %>%
  select(chip = Chip, cap_hit = `Cap Hit`, games_missed_injury = `Games Missed`, injury_type = `Injury Type`, 
         player = Player, position = Position, season = Season, team = Team, -`Number of Records`)

# Re-format player_injury_df team names
team_names_short <- c("ANA" = "Anaheim", "ARI/PHX" = "Arizona/Phoenix", "BOS" = "Boston", "BUF" = "Buffalo", "CGY" = "Calgary",
                      "CAR" = "Carolina", "CHI" = "Chicago", "COL" = "Colorado", "CBJ" = "Columbus", "DAL" = "Dallas",
                      "DET" = "Detroit", "EDM" = "Edmonton", "FLA" = "Florida", "LAK" = "Los Angeles", "MIN" = "Minnesota",
                      "MTL" = "Montreal", "NSH" = "Nashville", "NJD" = "New Jersey", "NYI" = "NY Islanders", "NYR" = "NY Rangers",
                      "OTT" = "Ottawa", "PHI" = "Philadelphia", "PIT" = "Pittsburgh", "SJS" = "San Jose", "STL" = "St. Louis",
                      "TBL" = "Tampa Bay", "TOR" = "Toronto", "VAN" = "Vancouver", "VGK" = "Vegas", "WSH" = "Washington",
                      "WPG/ATL" = "Winnipeg/Atlanta")

player_injury_df$team <- names(team_names_short)[match(player_injury_df$team, team_names_short)]

# Re-format player_injury_df player names before separating into first_name, last_name
player_injury_df$player[player_injury_df$player == "Crombeen, B.J."] <- "Crombeen, BJ"
player_injury_df$player[player_injury_df$player == "King, D.J."] <- "King, DJ"
player_injury_df$player[player_injury_df$player == "Lipon, J.C."] <- "Lipon, JC"
player_injury_df$player[player_injury_df$player == "Dumont, J.P."] <- "Dumont, JP"
player_injury_df$player[player_injury_df$player == "Brown, J.T."] <- "Brown, JT"
player_injury_df$player[player_injury_df$player == "Compher, J.T."] <- "Compher, JT"
player_injury_df$player[player_injury_df$player == "Miller, J.T."] <- "Miller, JT"
player_injury_df$player[player_injury_df$player == "Wyman, J.T."] <- "Wyman, JT"
player_injury_df$player[player_injury_df$player == "Parenteau, P.A."] <- "Parenteau, PA"
player_injury_df$player[player_injury_df$player == "Subban, P.K."] <- "Subban, PK"
player_injury_df$player[player_injury_df$player == "Umberger, R.J."] <- "Umberger, RJ"
player_injury_df$player[player_injury_df$player == "Brennan, T.J."] <- "Brennan, TJ"
player_injury_df$player[player_injury_df$player == "Brodie, T.J."] <- "Brodie, TJ"
player_injury_df$player[player_injury_df$player == "Galiardi, T.J."] <- "Galiardi, TJ"
player_injury_df$player[player_injury_df$player == "Oshie, T.J."] <- "Oshie, TJ"

player_injury_df <- player_injury_df %>%  
  separate(player, c("last_name","first_name"), sep = ', ') %>%
  mutate(last_name = str_to_upper(last_name)) %>%
  mutate(first_name = str_to_upper(first_name)) %>%
  mutate(first_initials = substr(first_name, 1, 2)) %>%
  # Re-format season
  separate(season, c("start_year","end_year"), sep = '/') %>%
  mutate(season = paste0(start_year, '20', end_year)) %>%
  mutate(season = as.integer(season)) %>%
  # Re-format position; split into position and status; blanks accounts for weird formatting issue where there isn't an NA
  separate(position, c("position_new", "status", "blanks"), sep = '"') %>%
  # Aggregate injuries on a per-season basis 
  group_by(first_name, first_initials, last_name, team, season, status, position_new, cap_hit) %>%
  summarise(total_games_missed_injury = sum(games_missed_injury), total_chip = sum(chip)) %>%
  ungroup()

# Create a new team stats df with the required data 
team_level_statistics_df <- team_statistics_df %>%
  select(team = `X2`, season = `Season`, avg_age = `AvAge`, points = `PTS`, points_pct = `PTS%`) %>%
  # Re-format season
  separate(season, c("start_year","end_year"), sep = '-') %>%
  mutate(season = paste0(start_year, end_year)) %>%
  mutate(season = as.integer(season)) %>%
  select(team, season, avg_age, points, points_pct)

# Re-format team_level_statistics_df team names
team_names_short2 <- c("ANA" = "Anaheim Ducks", "ARI/PHX" = "Arizona Coyotes", "ARI/PHX" = "Phoenix Coyotes", "BOS" = "Boston Bruins", 
                       "BUF" = "Buffalo Sabres", "CGY" = "Calgary Flames", "CAR" = "Carolina Hurricanes", "CHI" = "Chicago Blackhawks", 
                       "COL" = "Colorado Avalanche", "CBJ" = "Columbus Blue Jackets", "DAL" = "Dallas Stars", "DET" = "Detroit Red Wings", 
                       "EDM" = "Edmonton Oilers", "FLA" = "Florida Panthers", "LAK" = "Los Angeles Kings", "MIN" = "Minnesota Wild",
                       "MTL" = "Montreal canadiens", "NSH" = "Nashville Predators", "NJD" = "New Jersey Deviles", "NYI" = "New York Islanders", 
                       "NYR" = "New York Rangers", "OTT" = "Ottawa Senators", "PHI" = "Philadelphia Flyers", "PIT" = "Pittsburgh Penguins", 
                       "SJS" = "San Jose Sharks", "STL" = "St. Louis Blues", "TBL" = "Tampa Bay Lightning", "TOR" = "Toronto Maple Leafs", 
                       "VAN" = "Vancouver Canucks", "VGK" = "Vegas Golden Knights", "WSH" = "Washington Capitals", "WPG/ATL" = "Winnipeg Jets", 
                       "WPG/ATL" = "Atlanta Thrashers")

team_level_statistics_df$team <- names(team_names_short2)[match(team_level_statistics_df$team, team_names_short2)]

team_level_statistics_df <- team_level_statistics_df %>%
  arrange(team, season)

# Filter retired players, create injury value db
player_injury_df$status[is.na(player_injury_df$status)] <- "active"
player_injury_df <- player_injury_df %>% 
  filter(status == "active")

injury_value_df <- player_injury_df %>%
  left_join(war_gp_df, by = c("first_initials", "last_name", "season"))

# Assume for the time being that players without WAR records aren't relevant (didn't meet cut-off)
injury_value_df <- injury_value_df %>% 
  filter(!is.na(war_gp)) %>%
  select(first_name = first_name.x, last_name, position = position_new, season, team = team.x, games_played, games_missed, total_games_missed_injury, 
         cap_hit, total_chip, toi, war_gp, weighted_war_gp) %>%
  mutate(war_lost = total_games_missed_injury * weighted_war_gp)

# Attribute injuries to correct teams
team_injury_value_df <- injury_value_df %>%
  group_by(team, season) %>%
  summarise(team_war_lost = sum(war_lost),
            team_chip = sum(total_chip),
            man_games_lost = sum(total_games_missed_injury))

# Create working df for analysis
team_performance_df <- team_injury_value_df %>%
  left_join(team_level_statistics_df, by = c("team", "season")) 

team_performance_change_df <- team_performance_df %>%
  mutate(team_war_change = team_war_lost - lag(team_war_lost)) %>%
  mutate(team_chip_change = team_chip - lag(team_chip)) %>%
  mutate(team_mgl_change = man_games_lost - lag(man_games_lost)) %>%
  mutate(points_pct_change = points_pct - lag(points_pct)) %>%
  filter(!is.na(points_pct_change))