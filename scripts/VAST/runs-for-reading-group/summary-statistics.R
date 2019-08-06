# Summary statistics for VAST reading group presentation

library(tidyverse)
library(here)
library(VAST)
library(TMB)


Species = c("SILVER HAKE", "RED HAKE", "FOURSPOT FLOUNDER", "ATLANTIC COD", 
            "POLLOCK", "WHITE HAKE", "WINTER SKATE", "SPINY DOGFISH", "SUMMER FLOUNDER", 
            "GOOSEFISH", "THORNY SKATE", "SEA RAVEN", "BLUEFISH", "WEAKFISH")[8]


dat = read_rds(here("output", "data_formatted", "dat_preds_all.rds")) %>%
  filter(pdcomnam == Species) %>%
  mutate(myseason = ifelse(season == "FALL"|season == "SUMMER", "FALL", "SPRING"))

annual = dat %>% group_by(year, myseason)%>% 
  summarize(positive = sum(pypres),
            total = n()) %>%
  gather("encounter_type", "number", -year, -myseason)

ggplot(annual, aes(year, number, fill = encounter_type)) + geom_col()
ggplot(annual, aes(year, number, fill = encounter_type)) + geom_col() + facet_grid(~myseason)
