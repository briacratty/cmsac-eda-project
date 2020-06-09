library(tidyverse)
wnba <- read_csv("http://www.stat.cmu.edu/cmsac/sure/materials/data/eda_projects/wnba_championship_game_five.csv")

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


#As the periods progress, players get less accurate with shooting 

new_wnba <- wnba %>% 
  mutate (time_elapsed_in_period = parse_time("10:00", format="%M:%S") - 
            parse_time(strftime(strptime(wnba$clock, format="%M:%S"), 
                                format="%M:%S"), format="%M:%S"), 
          time_elapsed_seconds = time_elapsed_in_period + 600*(period-1),
          time_elapsed_minutes = as.numeric(time_elapsed_seconds) / 60) %>%
  filter(event == "field_goal_attempt") %>%
  ggplot(aes(x = time_elapsed_in_minutes, 
         y = shot_type, 
         color = shot_made)) +
  geom_line()


str(new_wnba$time_elapsed_in_period)
#showing up as null variable so can't be plotted


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
  

