library(tidyverse)
wnba <- read_csv("http://www.stat.cmu.edu/cmsac/sure/materials/data/eda_projects/wnba_championship_game_five.csv")

install.packages("patchwork")

# Shots further from the hoop are more likely to be assisted

shot_map_con <- wnba %>%
  filter(event == "field_goal_attempt", team == "con") %>%
  mutate(shot_x = x_loc/10, shot_y = y_loc/10) %>%
  filter(shot_y <= 40) %>%
  ggplot(aes(x = shot_x, y = shot_y, color = assisted)) +
  geom_point() +
  theme_bw() +
  labs (title = "Map of Connecticut Sun Shots",
        x = "Horizontal Location",
        y = "Vertical Location")

shot_map_was <- wnba %>%
  filter(event == "field_goal_attempt", team == "was") %>%
  mutate(shot_x = x_loc/10, shot_y = y_loc/10) %>%
  filter(shot_y <= 40) %>%
  ggplot(aes(x = shot_x, y = shot_y,  color = assisted)) +
  geom_point() +
  theme_bw() +
  labs (title = "Map of Washington Mystics Shots",
        x = "Horizontal Location",
        y = "Vertical Location")

library(patchwork)
shot_map_con / shot_map_was


