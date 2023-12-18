library(nflreadr)
library(tidyverse)
library(gt)
library(gtExtras)
library(nflfastR)

#load in participation PBP data & rosters
pbp <- nflreadr::load_participation(2023, include_pbp = TRUE)
schedule <- nflreadr::load_schedules(2023)

#games
games <- schedule %>%
  select(season, week, away_team, home_team, away_score, home_score, result, spread_line, 
         away_moneyline, home_moneyline, div_game)

#team stats and opposing team stats
pos<-pbp %>%
  group_by(season,week,posteam, defteam) %>%
  filter(!is.na(posteam)) %>%
  summarise(
    team_EPAperPlay = mean(epa, na.rm = T),
    team_QB_EPAperPlay = mean(qb_epa, na.rm = T),
    team_PROE = mean(pass_oe, na.rm = T),
    team_TotalPassYards = sum(passing_yards, na.rm = T),
    team_TotalRushYards = sum(rushing_yards, na.rm = T),
    team_PenalyYards = sum(penalty_yards, na.rm = T),
    team_Drives = n_distinct(drive)
  )
opp<-pbp %>%
  group_by(season,week,posteam, defteam) %>%
  filter(!is.na(posteam)) %>%
  summarise(
    opp_EPAperPlay = mean(epa, na.rm = T),
    opp_QB_EPAperPlay = mean(qb_epa, na.rm = T),
    opp_PROE = mean(pass_oe, na.rm = T),
    opp_TotalPassYards = sum(passing_yards, na.rm = T),
    opp_TotalRushYards = sum(rushing_yards, na.rm = T),
    opp_PenalyYards = sum(penalty_yards, na.rm = T),
    opp_Drives = n_distinct(drive)
  )

weekly_stats<-left_join(pos, opp, by=join_by(x$season==y$season,x$week==y$week,x$defteam==y$posteam, x$posteam==y$defteam))
