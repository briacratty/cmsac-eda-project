install.packages("protoclust")

library(protoclust)
library(tidyverse)
wnba_minimax_clust <- protoclust(dist(
  dplyr::select(wnba, distance_from_hoop, shot_angle))) 
plot(wnba_minimax_clust)

minimax_shot_clusters <- 
  protocut(wnba_minimax_clust, k=3)

wnba_clusters <- wnba %>%
  mutate(minimax_clusters = 
           as.factor(minimax_shot_clusters$cl)) 

wnba_clusters %>%
  ggplot(aes(x = shot_angle, y = distance_from_hoop,
             color = minimax_clusters)) +
  geom_point() +
  theme_bw() +
  labs(title = "Cluster of Shots",
       x = "Shot Angle",
       y = "Distance from Hoop") +
  scale_fill_discrete(name = "Clusters")


# shot location cluster
library(protoclust)
library(tidyverse)

wnba_clust <- wnba %>%
  filter(event == "field_goal_attempt") %>%
  mutate(shot_x = x_loc/10, shot_y = y_loc/10) %>%
  filter(shot_y <= 40)  

  
wnba_minimax_clust <- protoclust(dist(
  dplyr::select(wnba_clust, shot_x, shot_y))) 
plot(wnba_minimax_clust)

minimax_shot_clusters <- 
  protocut(wnba_minimax_clust, k=4)

wnba_clusters <- wnba_clust %>%
  mutate(minimax_clusters = 
           as.factor(minimax_shot_clusters$cl)) 

wnba_clusters %>%
  ggplot(aes(x = shot_x, y = shot_y,
             color = minimax_clusters)) +
  geom_point() +
  theme_bw() +
  labs(title = "Clusters of Shots Based On Court Location",
       subtitle = "Game 5 WNBA Championship 2019",
       x = "Horizontal Location",
       y = "Vertical Location") +
  coord_fixed() +
  scale_color_discrete(name = "Clusters")

table("Team" = wnba_clust$team,
      "Clusters" = minimax_shot_clusters$cl)






