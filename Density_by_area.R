library(tidyverse)
data <- read.csv("world_population_df.csv")

data %>% 
  group_by(Country) %>% 
  summarize(Area_in_km = sum(Area), Density_in_km = mean(Density)) %>%
  arrange(desc(Area_in_km)) %>% 
  head(10)

