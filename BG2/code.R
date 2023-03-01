

library(ggplot2)
library(tidyverse)
library(dplyr)
df <- read_delim("world_population.csv")

df %>%
  group_by(Continent) %>%
  arrange(`Growth Rate`) %>%
  slice_head(n = 10) %>%
  select(Continent, `Country/Territory`)

