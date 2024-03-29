library(tidyverse)
library(ranger)
library(nflfastR)
library(ggthemes)
library(vip)

#make custom package
theme_reach <- function() {
  theme_fivethirtyeight() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 18, hjust = 0.5),
      plot.caption = element_text(size = 16),
      axis.title.x = element_text(size=18),
      axis.title.y = element_text(size=18),
      axis.text = element_text(size = 14),
      strip.text = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 14)
    )
}

#load play-by-play data
pbp <- load_pbp(2020:2022)

#getting just field goals
field_goals <- pbp %>%
  filter(field_goal_attempt == 1) %>%
  mutate(made_fg = ifelse(field_goal_result == "made", 1, 0))

#making the logistic regression model
log_fg <- glm(made_fg ~ kick_distance,
              data = field_goals,
              family = "binomial")

summary(log_fg)

#checking the prediction probabilities
field_goals %>%
  mutate(pred_prob = log_fg$fitted.values) %>%
  ggplot(aes(x = kick_distance)) +
  geom_line(aes(y = pred_prob), color = "black", size = 2) +
  geom_point(aes(y = made_fg, color = ifelse(made_fg == 1, "darkgreen", "darkred")), 
             alpha = 0.3) +
  scale_color_identity() +
  theme_reach() +
  labs(x = "Kick Distance",
       y = "Chance to Make Field Goal (0-1)")

#getting field goals over expected
field_goals <- field_goals %>%
  mutate(pred_prob = log_fg$fitted.values) %>%
  mutate(fg_oe = made_fg - pred_prob)

fg_oe_stats <- field_goals %>%
  group_by(kicker_player_name) %>%
  summarize(kicks = n(),
            exp_fg_perc = mean(pred_prob),
            actual_fg_perc = mean(made_fg),
            fg_oe = 100*mean(fg_oe)) %>%
  filter(kicks >= 50) %>%
  arrange(-fg_oe)

#plotting the kickers
fg_oe_stats %>%
  mutate(`Field Goal % Over Expected` = fg_oe) %>%
  ggplot(aes(x = exp_fg_perc, y = actual_fg_perc)) +
  geom_point(aes(size = kicks, fill = `Field Goal % Over Expected`), shape = 21, color = "black") +
  scale_fill_viridis_c() +
  ggrepel::geom_text_repel(aes(label = kicker_player_name), size = 5) +
  geom_smooth(method = "lm", se = FALSE, color = "gray", size = 1.5) +
  theme_reach() +
  labs(x = "Expected Field Goal Percentage",
       y = "Actual Field Goal Percentage",
       title = "Actual and Expected Field Goal Percentage, 2020-2022",
       subtitle = "Minimum of 50 kicks") +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  guides(size = FALSE)
