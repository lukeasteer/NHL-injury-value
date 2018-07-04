# Install and load packages
install.packages("tidyverse")
library(tidyverse)

# Load WAR data, injury data 
war_db <- read_csv("war_ratings_2018-04-05.csv")
injury_db <- read_csv("NHL_Injury_Database_data.csv")

# Create a new WAR df with the required data 
war82_db <- war_db %>%
  select(player = Player, season = Season, team = Team, games_played = GP, toi = TOI, war_82 = `WAR/82`) %>%
  # Handle players who were traded mid-season
  separate(team, c("team_one","team_two", "team_three"), sep = '/') %>%
  # Create games_missed var, for comparison to games_missed_injury; account for absences for personal reasons, etc.
  mutate(games_missed = 82 - games_played)

# Create a new injury df with the required data
clean_injury_db <- injury_db %>%
  select(chip = Chip, cap_hit = `Cap Hit`, games_missed_injury = `Games Missed`, injury_type = `Injury Type`, 
         player = Player, position = Position, season = Season, team = Team, -`Number of Records`) %>%
  # Re-format player names
  separate(player, c("last_name","first_name"), sep = ',') %>% 
  mutate(player = paste0(first_name, '.', last_name) %>% str_to_upper) %>%
  # Re-format season
  separate(season, c("start_year","end_year"), sep = '/') %>%
  mutate(season = paste0(start_year, '-20', end_year)) %>%
  # Re-format position; split into position and status
  separate(position, c("position_new", "status"), sep = '"') %>%
  # Filter out all retired players
  filter(status != retired) %>%
  # Aggregate injuries on a per-season basis 
  group_by(player, team, season, position_new, cap_hit) %>%
  summarise(total_games_missed_injury = sum(games_missed_injury), total_chip = sum(chip)) 

# Re-arrange columns
#war82_db <- war82_db[, c(1:6, 9, 7, 8)]
#clean_injury_db <- clean_injury_db[, c(1, 2, 5, 3,) ] # Not done

# Create two named lists of NHL teams for re-formatting
team_names_short <- c("ANA" = "Anaheim", "ARI/PHX" = "Arizona/Phoenix", "BOS" = "Boston", "BUF" = "Buffalo", "CGY" = "Calgary",
                      "CAR" = "Carolina", "CHI" = "Chicago", "COL" = "Colorado", "CBJ" = "Columbus", "DAL" = "Dallas",
                      "DET" = "Detroit", "EDM" = "Edmonton", "FLA" = "Florida", "LAK" = "Los Angeles", "MIN" = "Minnesota",
                      "MTL" = "Montreal", "NSH" = "Nashville", "NJD" = "New Jersey", "NYI" = "NY Islanders", "NYR" = "NY Rangers",
                      "OTT" = "Ottawa", "PHI" = "Philadelphia", "PIT" = "Pittsburgh", "SJS" = "San Jose", "STL" = "St. Louis",
                      "TBL" = "Tampa Bay", "TOR" = "Toronto", "VAN" = "Vancouver", "VGK" = "Vegas", "WSH" = "Washington",
                      "WPG/ATL" = "Winnipeg/Atlanta")

team_names_short2 <- c("LAK" = "L.A", "NJD" = "N.J", "TBL" = "T.B")

# Re-format team names
#clean_injury_db$team <- names(team_names_short)[match(clean_injury_db$team, team_names_short)]

# Examine players who were traded to determine if injured games were correctly attributed
#traded_players <- war82_db %>%
#filter(team_two != "NA") %>%
#left_join(clean_injury_db, by = c("player", "season"))

# Properly attribute injured games

# Join WAR df, injury df
#injury_value_db <- clean_injury_db %>%
#left_join(war82_db, by = c("player", "season"))

# Create team injury summaries
#injury_value_db_team <- injury_value_db %>%
#group_by(team, season) %>%
#summarise(total_team_games_missed = sum(total_games_missed), total_team_chip = sum(total_chip)) %>%
#ungroup()