library(tidyverse)
wnba <- read_csv("http://www.stat.cmu.edu/cmsac/sure/materials/data/eda_projects/wnba_championship_game_five.csv")

as_tibble(wnba)

wnba %>%
  filter((shot_angle >= 41 & shot_angle <= 49), !is.na(shot_type)) %>%
  as_tibble(wnba) %>%
  head()

wnba %>%
  filter((shot_angle >= 50 & shot_angle <= 56), !is.na(shot_type)) %>%
  as_tibble(wnba) %>%
  head()

wnba %>%
  filter((field_goal_type == "three_pointer"), !is.na(shot_type)) %>%
  as_tibble(wnba) %>%
  head(n=10)

wnba %>%
  filter((shot_type == "layup"), !is.na(shot_type), assisted == 1) %>%
  as_tibble(wnba) %>%
  head(n=10)

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
        x = "X Coordinate",
        y = "Y Coordinate")

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


new_wnba <- wnba %>% 
  mutate (time_elapsed_in_period = parse_time("10:00", format="%M:%S") - 
            parse_time(strftime(strptime(wnba$clock, format="%M:%S"), 
                                format="%M:%S"), format="%M:%S")) %>%
  mutate(time_elapsed_seconds = time_elapsed_in_period + 600*(period-1)) %>%
  mutate(time_elapsed_minutes = time_elapsed_seconds / 60) %>%
  filter(event == "field_goal_attempt") %>%
  ggplot(aes(x = time_elapsed_in_minutes, 
         y = shot_type, 
         color = shot_made)) +
  geom_line()

str(new_wnba$time_elapsed_in_minutes)
    
  

  

