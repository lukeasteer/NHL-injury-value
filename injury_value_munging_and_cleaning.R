# Install and load packages
install.packages("tidyverse")
library(tidyverse)

# Load WAR data, injury data 
war_df <- read_csv("war_ratings_2019-02-14.csv")
injury_df <- read_csv("NHL_Injury_Database_data.csv")
team_stats_df <- read_csv("NHL_team_data.csv")

# Create a new WAR df with the required data 
war82_df <- war_df %>%
  select(player = Player, season = Season, team = Team, games_played = GP, toi = TOI, war_82 = `WAR/82`) %>%
  # Account for mid-season trades
  separate(team, c("team_one","team_two", "team_three", "team_four"), sep = '/') %>%
  # Create games_missed var, for comparison to games_missed_injury; account for absences for personal reasons, etc.
  mutate(games_missed = 82 - games_played) 

weight_war <- function(last3_war) {
  player_season <- as.numeric(stringr::str_split_fixed(last3_war, " ", 3))
  if (is.na(player_season[2]))
    player_season[1]
  else if (is.na(player_season[3]))
    weighted.mean(player_season[1:2], c(0.542, 0.458))
  else
    weighted.mean(player_season, c(0.417, 0.333, 0.25))
}

# Determine a weighted WAR for each player
war82_df <- war82_df %>%
  group_by(player) %>%
  arrange(player, season) %>%
  mutate(last3_war = paste(war_82, lag(war_82), lag(war_82, 2))) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(weighted_war_82 = weight_war(last3_war)) 

# Re-format war82_db team names
war82_df$team_one[war82_df$team_one == "L.A"] <- "LAK"
war82_df$team_two[war82_df$team_two == "L.A"] <- "LAK"
war82_df$team_three[war82_df$team_three == "L.A"] <- "LAK"
war82_df$team_four[war82_df$team_four == "L.A"] <- "LAK"

war82_df$team_one[war82_df$team_one == "N.J"] <- "NJD"
war82_df$team_two[war82_df$team_two == "N.J"] <- "NJD"
war82_df$team_three[war82_df$team_three == "N.J"] <- "NJD"
war82_df$team_four[war82_df$team_four == "N.J"] <- "NJD"

war82_df$team_one[war82_df$team_one == "T.B"] <- "TBL"
war82_df$team_two[war82_df$team_two == "T.B"] <- "TBL"
war82_df$team_three[war82_df$team_three == "T.B"] <- "TBL"
war82_df$team_four[war82_df$team_four == "T.B"] <- "TBL"

# Re-format player names before separating into first_name, last_name
war82_df$player[war82_df$player == "A.J..GREER"] <- "AJ.GREER"
war82_df$player[war82_df$player == "B.J..CROMBEEN"] <- "BJ.CROMBEEN"
war82_df$player[war82_df$player == "DENIS JR..GAUTHIER"] <- "DENIS JR.GAUTHIER"
war82_df$player[war82_df$player == "J.F..BERUBE"] <- "JF.BERUBE"
war82_df$player[war82_df$player == "J-F.JACQUES"] <- "JS.AUBIN"
war82_df$player[war82_df$player == "J-P.DUMONT"] <- "JP.DUMONT"
war82_df$player[war82_df$player == "J-SEBASTIAN.AUBIN"] <- "JS.AUBIN"
war82_df$player[war82_df$player == "J.T..COMPHER"] <- "JT.COMPHER"
war82_df$player[war82_df$player == "J.T..BROWN"] <- "JT.BROWN"
war82_df$player[war82_df$player == "J.T..MILLER"] <- "JT.MILLER"
war82_df$player[war82_df$player == "MARTIN.ST. LOUIS"] <- "MARTIN.ST LOUIS"
war82_df$player[war82_df$player == "MARTIN.ST. PIERRE"] <- "MARTIN.ST PIERRE"
war82_df$player[war82_df$player == "P.A..PARENTEAU"] <- "PA.PARENTEAU"
war82_df$player[war82_df$player == "PIERRE-ALEX.PARENTEAU"] <- "PA.PARENTEAU"
war82_df$player[war82_df$player == "P.K..SUBBAN"] <- "PK.SUBBAN"
war82_df$player[war82_df$player == "P. J..AXELSSON"] <- "PJ.AXELSSON"
war82_df$player[war82_df$player == "R.J..UMBERGER"] <- "RJ.UMBERGER"
war82_df$player[war82_df$player == "T.J..GALIARDI"] <- "TJ.GALIARDI"
war82_df$player[war82_df$player == "T.J..HENSICK"] <- "TJ.HENSICK"
war82_df$player[war82_df$player == "T.J..OSHIE"] <- "TJ.OSHIE"

# Separate player names into first_name, last_name
war82_df <- war82_df %>%
  separate(player, c("first_name","last_name"), sep = '\\.') %>%
  # Create first_initials var for join w/ clean_injury_db
  mutate(first_initials = substr(first_name, 1, 2)) 

# Create a new injury df with the required data
player_injury_df <- injury_df %>%
  select(chip = Chip, cap_hit = `Cap Hit`, games_missed_injury = `Games Missed`, injury_type = `Injury Type`, 
         player = Player, position = Position, season = Season, team = Team, -`Number of Records`)

# Re-format clean_injury_db team names
team_names_short <- c("ANA" = "Anaheim", "ARI/PHX" = "Arizona/Phoenix", "BOS" = "Boston", "BUF" = "Buffalo", "CGY" = "Calgary",
                      "CAR" = "Carolina", "CHI" = "Chicago", "COL" = "Colorado", "CBJ" = "Columbus", "DAL" = "Dallas",
                      "DET" = "Detroit", "EDM" = "Edmonton", "FLA" = "Florida", "LAK" = "Los Angeles", "MIN" = "Minnesota",
                      "MTL" = "Montreal", "NSH" = "Nashville", "NJD" = "New Jersey", "NYI" = "NY Islanders", "NYR" = "NY Rangers",
                      "OTT" = "Ottawa", "PHI" = "Philadelphia", "PIT" = "Pittsburgh", "SJS" = "San Jose", "STL" = "St. Louis",
                      "TBL" = "Tampa Bay", "TOR" = "Toronto", "VAN" = "Vancouver", "VGK" = "Vegas", "WSH" = "Washington",
                      "WPG/ATL" = "Winnipeg/Atlanta")

player_injury_df$team <- names(team_names_short)[match(player_injury_df$team, team_names_short)]

# Re-format player names before separating into first_name, last_name
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
  mutate(season = paste0(start_year, '-20', end_year)) %>%
  # Re-format position; split into position and status; blanks accounts for weird formatting issue where there isn't an NA
  separate(position, c("position_new", "status", "blanks"), sep = '"') %>%
  # Aggregate injuries on a per-season basis 
  group_by(first_name, first_initials, last_name, team, season, status, position_new, cap_hit) %>%
  summarise(total_games_missed_injury = sum(games_missed_injury), total_chip = sum(chip)) %>%
  ungroup()

# Create a new team stats df with the required data 
clean_team_stats_df <- team_stats_df %>%
  select(team = `X2`, season = `Season`, avg_age = `AvAge`, points = `PTS`, points_pct = `PTS%`) 

# Re-format team_stats_db team names
team_names_short2 <- c("ANA" = "Anaheim Ducks", "ARI/PHX" = "Arizona Coyotes", "ARI/PHX" = "Phoenix Coyotes", "BOS" = "Boston Bruins", 
                      "BUF" = "Buffalo Sabres", "CGY" = "Calgary Flames", "CAR" = "Carolina Hurricanes", "CHI" = "Chicago Blackhawks", 
                      "COL" = "Colorado Avalanche", "CBJ" = "Columbus Blue Jackets", "DAL" = "Dallas Stars", "DET" = "Detroit Red Wings", 
                      "EDM" = "Edmonton Oilers", "FLA" = "Florida Panthers", "LAK" = "Los Angeles Kings", "MIN" = "Minnesota Wild",
                      "MTL" = "Montreal canadiens", "NSH" = "Nashville Predators", "NJD" = "New Jersey Deviles", "NYI" = "New York Islanders", 
                      "NYR" = "New York Rangers", "OTT" = "Ottawa Senators", "PHI" = "Philadelphia Flyers", "PIT" = "Pittsburgh Penguins", 
                      "SJS" = "San Jose Sharks", "STL" = "St. Louis Blues", "TBL" = "Tampa Bay Lightning", "TOR" = "Toronto Maple Leafs", 
                      "VAN" = "Vancouver Canucks", "VGK" = "Vegas Golden Knights", "WSH" = "Washington Capitals", "WPG/ATL" = "Winnipeg Jets", 
                      "WPG/ATL" = "Atlanta Thrashers")

clean_team_stats_df$team <- names(team_names_short2)[match(clean_team_stats_df$team, team_names_short2)]

clean_team_stats_df <- clean_team_stats_df %>%
  arrange(team, season)

# Filter retired players, create injury value db
player_injury_df$status[is.na(player_injury_df$status)] <- "active"
player_injury_df <- player_injury_df %>% 
  filter(status == "active")

injury_value_df <- player_injury_df %>%
  left_join(war82_df, by = c("first_initials", "last_name", "season"))

# Examine players who don't have a WAR record (didn't meet event cut-off)
##event_cutoff_cases <- injury_value_db %>% 
##filter(is.na(war_82))

# Assume for the time being that players without WAR records aren't relevant (didn't meet cut-off)
injury_value_df <- injury_value_df %>% 
  filter(!is.na(war_82)) %>%
  select(first_name = first_name.x, last_name, position_new, season,
           team, team_one, team_two, team_three, team_four,
           games_played, games_missed, total_games_missed_injury, cap_hit, total_chip, toi, war_82, weighted_war_82)

# Manually examine players who were traded to determine if injured games were correctly attributed
# STILL NEED TO DO THIS
injury_value_df$team_two[is.na(injury_value_df$team_two)] <- "none"
injury_value_df$team_three[is.na(injury_value_df$team_three)] <- "none"
injury_value_df$team_four[is.na(injury_value_df$team_four)] <- "none"

traded_players <- injury_value_df %>%
  filter(team_two != "none")

# Filter out goalies, because we won't be including those in the analysis
skater_injury_value_df <- injury_value_df %>%
  filter(position_new != "G")

# Determine WAR lost for each player season
skater_injury_value_df <- skater_injury_value_df %>%
  mutate(war_GP = weighted_war_82 / 82) %>%
  mutate(war_lost= total_games_missed_injury * war_GP)

# Attribute injuries to correct teams
team_injury_value_df <- skater_injury_value_df %>%
  group_by(team, season) %>%
  summarise(team_war_lost = sum(war_lost),
            team_chip = sum(total_chip),
            man_games_lost = sum(total_games_missed_injury))

# Create working df for analysis
final_analysis_df <- team_injury_value_df %>%
  left_join(clean_team_stats_df, by = c("team", "season"))