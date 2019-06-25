library(tidyverse)
library(here)

dat = read_rds(here("output", "data_formatted", "dat_preds_all.rds"))

# Find number of tows with more than one predator stomach sampled per prey
# So looking by towid (and species?)

# All species in the same tow
dat %>% 
  group_by(towid) %>%
  summarize(n = n()) %>%
  pull(n) %>%
  hist()

# By species in the same tow
specsum = dat %>% 
  group_by(towid, pdcomnam) %>%
  summarize(n = n()) 

specsum %>%
  pull(n) %>%
  hist()

ggplot(specsum, aes(n, group = pdcomnam, color = pdcomnam)) + geom_freqpoly() #+ facet_wrap(~pdcomnam)


# What percent of tows have more than one stomach per species per tow?
specsum %>% 
  mutate(mult_pred = n > 1) %>%
  group_by(pdcomnam) %>%
  summarize(perc_mult_pred = 100*sum(mult_pred)/length(unique(specsum$towid)))

# What percent of tows have more than one stomach across all predator stomachs?
dat %>% 
  group_by(towid) %>%
  summarize(n = n()) %>%
  mutate(mult_pred = n > 1) %>%
  pull(mult_pred) %>%
  mean()

# Check for just Atlantic cod and spiny dogfish...
specsum %>% 
  mutate(mult_pred = n > 1) %>%
  filter(pdcomnam == "ATLANTIC COD"|pdcomnam == "SPINY DOGFISH") %>%
  group_by(pdcomnam) %>%
  summarize(perc_mult_pred = 100*sum(mult_pred)/length(unique(specsum$towid)))

# By species is probably the most relevant, I won't average across species within tows
