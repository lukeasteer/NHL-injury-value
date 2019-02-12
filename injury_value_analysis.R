team_injury_total <- team_injury_value_db %>%
  summarise(injury_value_lost = sum(team_war_lost))