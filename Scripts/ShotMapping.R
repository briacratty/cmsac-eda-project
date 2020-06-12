library(tidyverse)
wnba <- read_csv("http://www.stat.cmu.edu/cmsac/sure/materials/data/eda_projects/wnba_championship_game_five.csv")

install.packages("patchwork")

# Shots closer to the hoop are more likely to be assisted

shot_map_con <- wnba %>%
  filter(event == "field_goal_attempt", team == "con") %>%
  mutate(shot_x = x_loc/10, shot_y = y_loc/10) %>%
  filter(shot_y <= 40) %>%
  ggplot(aes(x = shot_x, y = shot_y, color = as.factor(assisted))) +
  geom_point() +
  theme_bw() +
  xlim(-20,20) +
  coord_fixed() +
  labs (title = "Map of Connecticut Sun Shots",
        x = "Horizontal Location",
        y = "Vertical Location") +
  guides(color = guide_legend(title="Assisted?")) +
  scale_color_discrete(labels = c("No", "Yes"))

shot_map_was <- wnba %>%
  filter(event == "field_goal_attempt", team == "was") %>%
  mutate(shot_x = x_loc/10, shot_y = y_loc/10) %>%
  filter(shot_y <= 40) %>%
  ggplot(aes(x = shot_x, y = shot_y,  color = as.factor(assisted))) +
  geom_point() +
  theme_bw() +
  xlim(-20,20) +
  coord_fixed() +
  labs (title = "Map of Washington Mystics Shots",
        x = "Horizontal Location",
        y = "Vertical Location") +
  guides(color = guide_legend(title="Assisted?")) +
  scale_color_discrete(labels = c("No", "Yes"))


library(patchwork)
shot_map_con / shot_map_was 

# rebound mapping
rebound_map_con <- wnba %>%
  filter(event == "rebound", team == "con") %>%
  mutate(rebound_x = x_loc/10, rebound_y = y_loc/10) %>%
  filter(rebound_y <= 40) %>%
  ggplot(aes(x = rebound_x, y = rebound_y)) +
  geom_point() +
  theme_bw() +
  xlim(-20,20) +
  ylim(0,30) +
  coord_fixed() +
  labs (title = "Map of Connecticut Sun Rebounds",
        x = "Horizontal Location",
        y = "Vertical Location") 


rebound_map_was <- wnba %>%
  filter(event == "rebound", team == "was") %>%
  mutate(rebound_x = x_loc/10, rebound_y = y_loc/10) %>%
  filter(rebound_y <= 40) %>%
  ggplot(aes(x = rebound_x, y = rebound_y)) +
  geom_point() +
  theme_bw() +
  xlim(-20,20) +
  ylim(0,30) +
  coord_fixed() +
  labs (title = "Map of Washington Sun Rebounds",
        x = "Horizontal Location",
        y = "Vertical Location") 

library(patchwork)
rebound_map_con / rebound_map_was 

