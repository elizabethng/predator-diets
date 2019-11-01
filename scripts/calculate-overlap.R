# Collecting info to do overlap calculations

library(readr)
library(here)
library(tidyverse)

# Locate the formatted trawl data
dat <- read_rds(here("output", "data_formatted", "dat_trawl.rds"))

# Species need to use the same mesh for overlap calculations. 
# How many trawl locations are there per predator species?
# In this data set they should all have the same number. 

dat %>% 
  mutate(pres = ifelse(catch_kg > 0, 1, 0)) %>%
  group_by(species, year, season) %>%
  summarize(n = n(),
            n_pres = sum(pres, na.rm = TRUE),
            tot_kg = sum(catch_kg, na.rm = TRUE)
            ) %>%
  mutate(p_pres = 100*(round(n_pres/n, 2))) %>% View()
  