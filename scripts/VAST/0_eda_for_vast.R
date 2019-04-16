# Data exploration for draft analysis

library(tidyverse)
library(here)


# Get Data
dat = read_rds(here("output", "data_formatted", "dat_preds.rds")) %>%
  pluck("ATLANTIC COD")

# Observations by season
dat %>% group_by(season) %>% summarise(n())

# Observations by year (pooled by season)
yrdat = dat%>% group_by(year) %>% 
  summarise(nprey = sum(pypres),
            wt_prey = sum(pyamtw), 
            npred = n())

yrdat
yrdat %>% filter(nprey < 1)

with(yrdat, plot(npred, nprey))

# Observations by area -- drop MAB
dat %>% group_by(geoarea) %>% summarise(n())

# Observations by area and year
dat %>% group_by(year, geoarea) %>% 
  summarise(n = n()) %>%
  xtabs(n ~., .)

# Maybe also drop ScS and SNE (grant says GB and GoM anyway)
