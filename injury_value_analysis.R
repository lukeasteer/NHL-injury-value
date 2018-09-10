# Examine players who don't have a WAR record (didn't meet event cut-off)
##event_cutoff_cases <- injury_value_db %>% 
##filter(is.na(war_82))

# Assume for the time being that players without WAR records aren't relevant (didn't meet cut-off)
injury_value_db <- injury_value_db %>% 
  filter(!is.na(war_82)) %>%
  group_by(first_name.x, last_name, position_new, season,
           team, team_one, team_two, team_three, team_four,
           games_played, games_missed, total_games_missed_injury, cap_hit, total_chip, toi, war_82)

# Examine players who were traded to determine if injured games were correctly attributed
injury_value_db$team_two[is.na(injury_value_db$team_two)] <- "none"
injury_value_db$team_three[is.na(injury_value_db$team_three)] <- "none"
injury_value_db$team_four[is.na(injury_value_db$team_four)] <- "none"

traded_players <- injury_value_db %>%
  filter(team_two != "none")

# Determine value lost to injury for each player
# How to handle goalies? Ignore them for now?
injury_value_db <- injury_value_db %>%
  mutate(war_GP = war_82 / 82) %>%
  mutate(war_lost= total_games_missed_injury * war_GP)

# Attribute injuries to correct teams
team_injury_value_db <- injury_value_db %>%
  filter(position_new != "G") %>%
  group_by(team, season) %>%
  summarise(team_war_lost = sum(war_lost),
            team_chip = sum(total_chip),
            man_games_lost = sum(total_games_missed_injury))

team_injury_total <- team_injury_value_db %>%
  summarise(injury_value_lost = sum(team_war_lost))