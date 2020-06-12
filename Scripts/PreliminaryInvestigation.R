library(tidyverse)
wnba <- read_csv("http://www.stat.cmu.edu/cmsac/sure/materials/data/eda_projects/wnba_championship_game_five.csv")

str(wnba)
as_tibble(wnba)

#Assisted layups are the most likely shot to be made 

wnba %>%
  filter((shot_type == "layup"), !is.na(shot_type), assisted == 1) %>%
  as_tibble(wnba) %>%
  head(n=10)

wnba %>%
  filter(event == "field_goal_attempt") %>%
  ggplot(aes(x = shot_type, 
             fill = assisted)) +
  geom_bar() +
  theme_bw()


  
  

install.packages("patchwork")

# X, y location on the court is a determinate of the probability of 
# a shot being made

shot_map_con <- wnba %>%
  filter(event == "field_goal_attempt", team == "con") %>%
  mutate(shot_x = x_loc/10, shot_y = y_loc/10) %>%
  filter(shot_y <= 40) %>%
  ggplot(aes(x = shot_x, y = shot_y, color = shot_made)) +
  geom_point() +
  theme_bw() +
  labs (title = "Map of Connecticut Sun Shots",
        x = "Horizontal Location",
        y = "Vertical Location")

shot_map_was <- wnba %>%
  filter(event == "field_goal_attempt", team == "was") %>%
  mutate(shot_x = x_loc/10, shot_y = y_loc/10) %>%
  filter(shot_y <= 40) %>%
  ggplot(aes(x = shot_x, y = shot_y,  color = shot_made)) +
  geom_point() +
  theme_bw() +
  labs (title = "Map of Washington Mystics Shots",
        x = "X Coordinate",
        y = "Y Coordinate")

library(patchwork)
shot_map_con / shot_map_was



#closer the score the less likely a shot is to be made (possible stress/anxiety)

wnba %>%
  mutate(score_difference = was_score - con_score) %>%
  filter (event == "field_goal_attempt") %>%
  ggplot(aes(x = score_difference, color = shot_made)) +
  geom_bar()
  


wnba %>%
  filter (event == "field_goal_attempt") %>%
  ggplot(aes(x = period,
             fill = shot_type)) +
  geom_bar(position = "dodge") + 
  theme_bw()



shot_map_by_period <- wnba %>%
  filter(event == "field_goal_attempt") %>%
  mutate(shot_x = x_loc/10, shot_y = y_loc/10) %>%
  filter(shot_y <= 40) %>%
  ggplot(aes(x = shot_x, y = shot_y, color = shot_made)) +
  geom_point() +
  facet_wrap(~period, ncol = 2) +
  theme_bw() +
  coord_fixed() +
  labs (title = "Map of Shots by Period",
        x = "X Location",
        y = "Y Location")
shot_map_by_period

# fouls are more likely to occur closer to the hoop
wnba %>%
  filter(event == "foul") %>%
  ggplot(aes(x = distance_from_hoop)) +
  stat_ecdf() +
  geom_vline(xintercept = 15) +
  geom_rug(alpha = 0.7) +
  labs(title = "Fouls Likely to Occur Closer to the Hoop",
       subtitle = "Game 5 WNBA Championship 2019",
       x = "Distance from the Hoop (feet)",
       y = "ECDF") +
  theme_bw()


  
  

