# Script to run models for multiple species at a time

library(tidyverse)
library(here)

source("C:/Users/Elizabeth Ng/Documents/GitHub/predator-diets/scripts/run_VAST.R")

Species = "THORNY SKATE"
DateFile = paste("output", "VAST", "SS_mods", gsub(" ", "_", Species), sep = "/")
Data_Set = paste(gsub(" ", "_", Species), "071019", sep = "_")

all_dat = read_rds(here("output", "data_formatted", "dat_preds_all.rds")) %>%
  mutate(myseason = ifelse(season == "FALL"|season == "SUMMER", "FALL", "SPRING"))

dat = all_dat %>%
  filter(pdcomnam == Species) %>%
  group_by(year, myseason) %>%
  summarize(prey = sum(pypres))
xtabs(prey ~., dat)
xtabs(prey ~., dat) %>% colSums

# filter(season == "FALL" | season == "SUMMER") %>%
# filter(year %in% 1987:2015)

hake_mod = run_VAST(DateFile = DateFile, 
                dat = dat, 
                Data_Set = Data_Set)



# Top 5 predators (n is stomachs with prey)
# spiny dogfish, more in spring 1979:2015 with RW (n = 777)
# atlantic cod, more in spring 1979:2015 with RW (n = 295)
# goosefish, more in spring 1988:2015 (n = 138)
# silver hake, more in fall 1986:2015 (n = 388)

# Then (probably too few to run though)
# bluefish, fall (n = 61)
# pollock, fall (n = 71)
# winter skate, spring (n = 57)
# thorny skate, fall (n = 31)
