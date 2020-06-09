#Assisted layups are the most likely shot to be made 
library(tidyverse)
wnba <- read_csv("http://www.stat.cmu.edu/cmsac/sure/materials/data/eda_projects/wnba_championship_game_five.csv")

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