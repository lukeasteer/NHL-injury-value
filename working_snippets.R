sample <- player_injury_value_df %>%
  select(first_name, last_name, position_new, season, team, weighted_games_played, war_lost) %>%
  arrange(desc(war_lost)) %>%
  head(20)

x_test <- player_injury_value_df %>%
  filter(position_new == "G")

summary(x_test$weighted_games_played)  

no_war_record <- injury_value_df %>%
  filter(is.na(war_82))