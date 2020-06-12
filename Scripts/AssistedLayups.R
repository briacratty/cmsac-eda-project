# Assisted layups are the most likely shot to be made 
library(tidyverse)
wnba <- read_csv("http://www.stat.cmu.edu/cmsac/sure/materials/data/eda_projects/wnba_championship_game_five.csv")

wnba %>%
  filter((shot_type == "layup"), !is.na(shot_type), assisted == 1) %>%
  as_tibble(wnba) %>%
  head(n=10)

# shows which shot types are more/less likely to be assisted
wnba %>%
  filter(event == "field_goal_attempt") %>%
  ggplot(aes(x = as.factor(assisted), 
             fill = shot_type)) +
  geom_bar(position = "dodge") +
  theme_bw() +
  labs(title = "Likelihood of Shot Types Being Assisted",
       subtitle = "Game 5 WNBA Championship 2019",
       x = "Unasssisted (0) or Assisted (1)",
       y = "Count") +
  guides(fill = guide_legend(title="Shot Type")) 
  

# show dispersion of field goal types in each period
wnba %>%
  filter(event == "field_goal_attempt") %>%
  ggplot(aes(x = as.factor(period), 
             fill = field_goal_type)) +
  geom_bar(position = "dodge") +
  theme_bw() +
  labs(x = "Period",
       y = "Count")


