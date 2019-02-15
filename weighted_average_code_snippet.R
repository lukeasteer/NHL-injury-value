#install.packages("pracma")
#library(pracma)

#weight_war <- function(war_82, nseasons) {
  #if(nseasons == 1)
    #war_82
  #else if(nseasons == 2)
    #weighted.mean(war_82, c(0.3, 0.7))
  #else
    #weighted.mean(war_82, c(0.2, 0.3, 0.7))
#}

#weighted_war82_db <- war82_db %>%
  #mutate(season = as.numeric(str_sub(season, 1, 4))) %>%
  #group_by(player) %>%
  #arrange(player, season) %>%
  #slice(1:3) %>%
  #summarize(weighted_war = weight_war(war_82, n()))

#weighted2_war82_db <- war82_db %>%
  #group_by(player, season) %>%
  #mutate(weighted_war82 = movavg(war_82, 3, type=c("w")))

weight_war <- function(last3_war) {
  player_season <- as.numeric(stringr::str_split_fixed(last3_war, " ", 3))
  if (is.na(player_season[2]))
    player_season[1]
  else if (is.na(player_season[3]))
    weighted.mean(player_season[1:2], c(0.3, 0.7))
  else
    weighted.mean(player_season, c(0.2, 0.3, 0.5))
}

weighted3_war82_db <- war82_db %>%
  mutate(name = paste(first_name, last_name)) %>%
  group_by(name) %>%
  arrange(name, season) %>%
  mutate(last3_war = paste(war_82, lag(war_82), lag(war_82, 2))) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(weighted_war_82 = weight_war(last3_war)) %>%
  select(name, season, war_82, weighted_war_82)