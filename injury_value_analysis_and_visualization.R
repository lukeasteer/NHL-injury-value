# http://www.sthda.com/english/articles/40-regression-analysis/167-simple-linear-regression-in-r/
# https://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html

model1 <- lm(points_pct_change ~ team_war_change, final_analysis_df_test)
model2 <- lm(points_pct_change ~ team_chip_change, final_analysis_df_test)
model3 <- lm(points_pct_change ~ team_mgl_change, final_analysis_df_test)

model4 <- lm(team_war_lost ~ avg_age, final_analysis_df_test)

model5 <- lm(team_war_lost ~ lag(team_war_lost), final_analysis_df_test)

model6 <- lm(points_pct ~ team_war_lost, final_analysis_df_test)

test_plot1 <- final_analysis_df_test %>%
  ggplot(aes(x = team_war_change, y = points_pct_change)) +
  geom_point() +
  stat_smooth()

test_plot2 <- final_analysis_df_test %>%
  ggplot(aes(x = team_chip_change, y = points_pct_change)) +
  geom_point() +
  stat_smooth()

test_plot3 <- final_analysis_df_test %>%
  ggplot(aes(x = team_mgl_change, y = points_pct_change)) +
  geom_point() +
  stat_smooth()

test_plot5 <- final_analysis_df_test %>%
  ggplot(aes(x = team_war_lost, y = lag(team_war_lost))) +
  geom_point() +
  stat_smooth()