# Install and load packages
install.packages("tidyverse")
library(tidyverse)

# Load WAR data, injury data 
war_db <- read_csv("war_ratings_2018-04-12.csv")
injury_db <- read_csv("NHL_Injury_Database_data.csv")
team_stats_db <- read_csv("NHL_team_data.csv")

# Create a new WAR df with the required data 
war82_db <- war_db %>%
  select(player = Player, season = Season, team = Team, games_played = GP, toi = TOI, war_82 = `WAR/82`) %>%
  # Account for mid-season trades
  separate(team, c("team_one","team_two", "team_three", "team_four"), sep = '/') %>%
  # Create games_missed var, for comparison to games_missed_injury; account for absences for personal reasons, etc.
  mutate(games_missed = 82 - games_played)

# Re-format war82_db team names
war82_db$team_one[war82_db$team_one == "L.A"] <- "LAK"
war82_db$team_two[war82_db$team_two == "L.A"] <- "LAK"
war82_db$team_three[war82_db$team_three == "L.A"] <- "LAK"
war82_db$team_four[war82_db$team_four == "L.A"] <- "LAK"

war82_db$team_one[war82_db$team_one == "N.J"] <- "NJD"
war82_db$team_two[war82_db$team_two == "N.J"] <- "NJD"
war82_db$team_three[war82_db$team_three == "N.J"] <- "NJD"
war82_db$team_four[war82_db$team_four == "N.J"] <- "NJD"

war82_db$team_one[war82_db$team_one == "T.B"] <- "TBL"
war82_db$team_two[war82_db$team_two == "T.B"] <- "TBL"
war82_db$team_three[war82_db$team_three == "T.B"] <- "TBL"
war82_db$team_four[war82_db$team_four == "T.B"] <- "TBL"

# Re-format player names before separating into first_name, last_name
war82_db$player[war82_db$player == "A.J..GREER"] <- "AJ.GREER"
war82_db$player[war82_db$player == "B.J..CROMBEEN"] <- "BJ.CROMBEEN"
war82_db$player[war82_db$player == "DENIS JR..GAUTHIER"] <- "DENIS JR.GAUTHIER"
war82_db$player[war82_db$player == "J.F..BERUBE"] <- "JF.BERUBE"
war82_db$player[war82_db$player == "J-F.JACQUES"] <- "JS.AUBIN"
war82_db$player[war82_db$player == "J-P.DUMONT"] <- "JP.DUMONT"
war82_db$player[war82_db$player == "J-SEBASTIAN.AUBIN"] <- "JS.AUBIN"
war82_db$player[war82_db$player == "J.T..COMPHER"] <- "JT.COMPHER"
war82_db$player[war82_db$player == "J.T..BROWN"] <- "JT.BROWN"
war82_db$player[war82_db$player == "J.T..MILLER"] <- "JT.MILLER"
war82_db$player[war82_db$player == "MARTIN.ST. LOUIS"] <- "MARTIN.ST LOUIS"
war82_db$player[war82_db$player == "MARTIN.ST. PIERRE"] <- "MARTIN.ST PIERRE"
war82_db$player[war82_db$player == "P.A..PARENTEAU"] <- "PA.PARENTEAU"
war82_db$player[war82_db$player == "PIERRE-ALEX.PARENTEAU"] <- "PA.PARENTEAU"
war82_db$player[war82_db$player == "P.K..SUBBAN"] <- "PK.SUBBAN"
war82_db$player[war82_db$player == "P. J..AXELSSON"] <- "PJ.AXELSSON"
war82_db$player[war82_db$player == "R.J..UMBERGER"] <- "RJ.UMBERGER"
war82_db$player[war82_db$player == "T.J..GALIARDI"] <- "TJ.GALIARDI"
war82_db$player[war82_db$player == "T.J..HENSICK"] <- "TJ.HENSICK"
war82_db$player[war82_db$player == "T.J..OSHIE"] <- "TJ.OSHIE"

# Separate player names into first_name, last_name
war82_db <- war82_db %>%
  separate(player, c("first_name","last_name"), sep = '\\.') %>%
  # Create first_initials var for join w/ clean_injury_db
  mutate(first_initials = substr(first_name, 1, 2)) 

# Create a new injury df with the required data
clean_injury_db <- injury_db %>%
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

clean_injury_db$team <- names(team_names_short)[match(clean_injury_db$team, team_names_short)]

# Re-format player names before separating into first_name, last_name
clean_injury_db$player[clean_injury_db$player == "Crombeen, B.J."] <- "Crombeen, BJ"
clean_injury_db$player[clean_injury_db$player == "King, D.J."] <- "King, DJ"
clean_injury_db$player[clean_injury_db$player == "Lipon, J.C."] <- "Lipon, JC"
clean_injury_db$player[clean_injury_db$player == "Dumont, J.P."] <- "Dumont, JP"
clean_injury_db$player[clean_injury_db$player == "Brown, J.T."] <- "Brown, JT"
clean_injury_db$player[clean_injury_db$player == "Compher, J.T."] <- "Compher, JT"
clean_injury_db$player[clean_injury_db$player == "Miller, J.T."] <- "Miller, JT"
clean_injury_db$player[clean_injury_db$player == "Wyman, J.T."] <- "Wyman, JT"
clean_injury_db$player[clean_injury_db$player == "Parenteau, P.A."] <- "Parenteau, PA"
clean_injury_db$player[clean_injury_db$player == "Subban, P.K."] <- "Subban, PK"
clean_injury_db$player[clean_injury_db$player == "Umberger, R.J."] <- "Umberger, RJ"
clean_injury_db$player[clean_injury_db$player == "Brennan, T.J."] <- "Brennan, TJ"
clean_injury_db$player[clean_injury_db$player == "Brodie, T.J."] <- "Brodie, TJ"
clean_injury_db$player[clean_injury_db$player == "Galiardi, T.J."] <- "Galiardi, TJ"
clean_injury_db$player[clean_injury_db$player == "Oshie, T.J."] <- "Oshie, TJ"

clean_injury_db <- clean_injury_db %>%  
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

# Filter retired players, create injury value db
clean_injury_db$status[is.na(clean_injury_db$status)] <- "active"
clean_injury_db <- clean_injury_db %>% 
  filter(status == "active")

injury_value_db <- clean_injury_db %>%
  left_join(war82_db, by = c("first_initials", "last_name", "season"))

# Examine players who don't have a WAR record (didn't meet event cut-off)
##event_cutoff_cases <- injury_value_db %>% 
##filter(is.na(war_82))

# Assume for the time being that players without WAR records aren't relevant (didn't meet cut-off)
injury_value_db <- injury_value_db %>% 
  filter(!is.na(war_82)) %>%
  select(first_name.x, last_name, position_new, season,
           team, team_one, team_two, team_three, team_four,
           games_played, games_missed, total_games_missed_injury, cap_hit, total_chip, toi, war_82)

# Examine players who were traded to determine if injured games were correctly attributed
# STILL NEED TO DO THIS
injury_value_db$team_two[is.na(injury_value_db$team_two)] <- "none"
injury_value_db$team_three[is.na(injury_value_db$team_three)] <- "none"
injury_value_db$team_four[is.na(injury_value_db$team_four)] <- "none"

#traded_players <- injury_value_db %>%
  #filter(team_two != "none")

# Filter out goalies, because we won't be including those in the analysis
skater_injury_value_db <- injury_value_db %>%
  filter(position_new != "G")

# Determine WAR lost for each player season
skater_injury_value_db <- skater_injury_value_db %>%
  mutate(war_GP = war_82 / 82) %>%
  mutate(war_lost= total_games_missed_injury * war_GP)

# Attribute injuries to correct teams
team_injury_value_db <- skater_injury_value_db %>%
  group_by(team, season) %>%
  summarise(team_war_lost = sum(war_lost),
            team_chip = sum(total_chip),
            man_games_lost = sum(total_games_missed_injury))