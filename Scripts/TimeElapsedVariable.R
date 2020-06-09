library(tidyverse)
wnba <- read_csv("http://www.stat.cmu.edu/cmsac/sure/materials/data/eda_projects/wnba_championship_game_five.csv")

#As the periods progress, players get less accurate with shooting 


new_wnba <- wnba %>% 
  mutate(clock = as.character(clock)) %>%
  separate(clock, c("minutes", "seconds", "blank"), 
           sep = ":", remove = FALSE) %>%
  mutate(minutes = as.numeric(minutes),
         seconds = as.numeric(seconds)) %>%
  dplyr::select(-blank) %>%
  mutate(total_time = (9 - minutes) * 60 + seconds, 
         time_elapsed = ((total_time + 600 * (period-1)) / 60),
         score_difference = was_score - con_score) 
  
new_wnba %>%
  filter(event == "field_goal_attempt") %>%
  ggplot(aes(x = time_elapsed,
             y = score_difference,
             color = shot_made)) +
  geom_point() +
  theme_bw() +
  labs (title = "Score Difference as Time Elapsed",
        subtitle = "Game 5 of 2019 WNBA Championship",
        x = "Time Elapsed in Game",
        y = "Score Difference") +
  scale_fill_discrete(name = "Shot Made?")

