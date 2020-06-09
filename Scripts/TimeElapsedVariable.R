
#As the periods progress, players get less accurate with shooting 

library(tidyverse)
new_wnba <- wnba %>% 
  mutate(clock = as.character(clock)) %>%
  separate(clock, c("minutes", "seconds", "blank"), 
           sep = ":", remove = FALSE) %>%
  mutate(minutes = as.numeric(minutes),
         seconds = as.numeric(seconds)) %>%
  dplyr::select(-blank) %>%
  mutate(total_time = (9 - minutes) * 60 + seconds, 
         time_elapsed = ((total_time + 600 * (period-1)) / 60)) %>%
  filter(event == "field_goal_attempt") %>%
  ggplot(aes(x = time_elapsed,
             color = shot_made)) +
  geom_point() +
  theme_bw()