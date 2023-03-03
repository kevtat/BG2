

library(ggplot2)
library(tidyverse)
library(dplyr)
df <- read_delim("world_population.csv")
#highest
df %>%
  group_by(Continent) %>%
  arrange(desc(`Growth Rate`)) %>%
  slice_head(n = 1) %>%
  select(Continent, `Country/Territory`)


#lowest
df %>%
  group_by(Continent) %>%
  arrange(desc(`Growth Rate`)) %>%
  slice_tail(n = 1) %>%
  select(Continent, `Country/Territory`)

df %>% 
  group_by(Continent) %>% 
  summarise(Averagegrowthrate = mean(`Growth Rate`))

