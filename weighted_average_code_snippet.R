install.packages("pracma")
library(pracma)

weight_war <- function(war_82, nseasons) {
  if(nseasons == 1)
    war_82
  else if(nseasons == 2)
    weighted.mean(war_82, c(0.3, 0.7))
  else
    weighted.mean(war_82, c(0.2, 0.3, 0.7))
}

weighted_war82_db <- war82_db %>%
  mutate(season = as.numeric(str_sub(season, 1, 4))) %>%
  group_by(player) %>%
  arrange(player, season) %>%
  slice(1:3) %>%
  summarize(weighted_war = weight_war(war_82, n()))

weighted2_war82_db <- war82_db %>%
  group_by(player, season) %>%
  mutate(weighted_war82 = movavg(war_82, 3, type=c("w")))