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


