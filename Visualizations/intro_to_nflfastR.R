library(nflfastR)
library(tidyverse)
library(ggimage)
library(gt)

pbp <- load_pbp(2021:2022)

#play by play: run/pass
pbp_rp <- pbp %>%
  filter(rush == 1 | pass == 1) %>%
  filter(!is.na(epa))

#who was the Denver Broncos best rusher last season?
pbp_rp %>% 
  filter(posteam == "DEN", rush == 1, !is.na(rusher_player_name)) %>% 
  group_by(rusher_player_name) %>% 
  summarize(rushes = n(),
            epa_rush = mean(epa)) %>% 
  filter(rushes >= 10) %>% 
  arrange(-epa_rush)

#who was the SF 49ers best quarterback last season?
pbp_rp %>% 
  filter(posteam == "SF", !is.na(id)) %>% 
  group_by(id) %>% 
  summarize(name = first(name),
            plays = n(),
            epa_per_play = mean(epa),
            pass_attempts = sum(complete_pass + incomplete_pass, na.rm = T)) %>% 
  filter(plays >= 50, pass_attempts >= 10) %>% 
  arrange(-epa_per_play)




#---compare pass efficiency vs. rush efficiency last season---
pass_efficiency_22 <- pbp %>% 
  filter(season == 2022, pass == 1) %>% 
  group_by(posteam) %>% 
  summarize(passes = n(),
            pass_epa = mean(epa))

rush_efficiency_22 <- pbp %>% 
  filter(season == 2022, rush == 1) %>% 
  group_by(posteam) %>% 
  summarize(rushes = n(),
            rush_epa = mean(epa))

#join the two datasets together
total_eff <- pass_efficiency_22 %>% 
  left_join(rush_efficiency_22, by = "posteam")

#join the team logos with total_eff
total_eff <- total_eff %>% 
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

#make the plot
total_eff %>% 
  ggplot(aes(x = pass_epa, y = rush_epa)) +
  geom_hline(yintercept = mean(total_eff$rush_epa), linetype = "dashed") +
  geom_vline(xintercept = mean(total_eff$pass_epa), linetype = "dashed") +
  geom_smooth(method = "lm") +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
  theme_bw() +
  labs(x = "EPA/pass",
       y = "EPA/rush",
       title = "EPA/pass and EPA/Rush in 2022",
       subtitle = "Regular season and playoffs included",
       caption = "By: Craig Shaffer | Data: nflfastR") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) #put the title and subtitle in the middle

#saving the plot
ggsave('pass-rush-epa-22.png', width = 14, height = 10, dpi = "retina")




#---compare how quarterback late down aggressiveness has changed---
#2021 yards past sticks
agg_21 <- pbp %>% 
  mutate(yards_past_sticks = air_yards - ydstogo) %>% 
  filter(season == 2021, down %in% c(3, 4), !is.na(passer_player_id)) %>%
  group_by(passer_player_id) %>% 
  summarize(name = first(name),
            passes_21 = n(),
            agg_21 = mean(yards_past_sticks, na.rm = T))

#2022 yards past sticks
agg_22 <- pbp %>% 
  mutate(yards_past_sticks = air_yards - ydstogo) %>% 
  filter(season == 2022, down %in% c(3, 4), !is.na(passer_player_id)) %>%
  group_by(passer_player_id) %>% 
  summarize(passes_22 = n(),
            team = last(posteam), 
            agg_22 = mean(yards_past_sticks, na.rm = T))

#combine 2021 and 2022 together
agg_21_22 <- agg_21 %>% 
  left_join(agg_22, by = "passer_player_id") %>% 
  left_join(teams_colors_logos, by = c("team" = "team_abbr")) %>% 
  filter(passes_21 >= 100)

#make the gt table
agg_21_22 %>% 
  mutate(agg_21 = round(agg_21, 1),
         agg_22 = round(agg_22, 1),
         diff = agg_22 - agg_21) %>% 
  select(name, team_wordmark, agg_21, agg_22, diff) %>% 
  arrange(-diff) %>% 
  gt() %>%
  cols_align(align = "center") %>% 
  gtExtras::gt_img_rows(team_wordmark) %>% 
  cols_label(name = "Quarterback",
             team_wordmark = "",
             agg_21 = "Late-Down YPS, 2021",
             agg_22 = "Late-Down YPS, 2022",
             diff = "Difference") %>% 
  gtExtras::gt_theme_espn()





