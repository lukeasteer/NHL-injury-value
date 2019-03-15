# http://www.sthda.com/english/articles/40-regression-analysis/167-simple-linear-regression-in-r/
# https://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html

library(MASS)

model1 <- lm(points_pct_change ~ team_war_change, team_performance_change_df)
model1a <- rlm(points_pct_change ~ team_war_change, team_performance_change_df)

model2 <- lm(points_pct_change ~ team_chip_change, team_performance_change_df)
model3 <- lm(points_pct_change ~ team_mgl_change, team_performance_change_df)

model4 <- lm(team_war_lost ~ avg_age, team_performance_change_df)

model5 <- lm(team_war_lost ~ lag(team_war_lost), team_performance_change_df)

model6 <- lm(points_pct ~ team_war_lost, team_performance_change_df)

test_plot1 <- team_performance_change_df %>%
  ggplot(aes(x = team_war_change, y = points_pct_change)) +
  geom_point() +
  stat_smooth()

test_plot1a <- inclusion_team_performance_change_df %>%
  ggplot(aes(x = team_war_change, y = points_pct_change)) +
  geom_point() +
  stat_smooth()

test_plot2 <- team_performance_change_df %>%
  ggplot(aes(x = team_chip_change, y = points_pct_change)) +
  geom_point() +
  stat_smooth()

test_plot3 <- team_performance_change_df %>%
  ggplot(aes(x = team_mgl_change, y = points_pct_change)) +
  geom_point() +
  stat_smooth()

test_plot4 <- team_performance_change_df %>%
  ggplot(aes(x = avg_age, y = points_pct_change)) +
  geom_point() +
  stat_smooth()

test_plot5 <- team_performance_change_df %>%
  ggplot(aes(x = team_war_lost, y = lag(team_war_lost))) +
  geom_point() +
  stat_smooth()