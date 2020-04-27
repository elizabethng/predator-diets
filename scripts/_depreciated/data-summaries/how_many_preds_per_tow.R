library("tidyverse")
library("here")

dat <- read_rds(here("data", "processed", "dat_preds_all.rds"))

# Find number of tows with more than one predator stomach sampled per prey
# So looking by towid (and species?)

# All species in the same tow
dat %>% 
  group_by(towid) %>%
  summarize(n = n()) %>%
  pull(n) %>%
  hist()

# By species in the same tow
specsum <- dat %>% 
  group_by(towid, predator) %>%
  summarize(n = n()) 

specsum %>%
  pull(n) %>%
  hist()

ggplot(specsum, aes(n, group = predator, color = predator)) + geom_freqpoly() #+ facet_wrap(~predator)


# What percent of tows have more than one stomach per species per tow?
specsum_sum <- specsum %>% 
  mutate(mult_pred = n > 1) %>%
  group_by(predator) %>%
  summarize(perc_mult_pred = 100*sum(mult_pred)/length(unique(specsum$towid)))
specsum_sum
mean(specsum_sum$perc_mult_pred)

# Check for predators used in this study, only
filter(specsum_sum, 
       predator %in% c("atlantic cod", "silver hake", "spiny dogfish", "goosefish", "white hake")) %>%
  summarize(mean(perc_mult_pred))


# Brian thinks it is closer to 70% check antoher way
dat %>% 
  group_by(towid, predator) %>%
  summarize(n_stomachs = n()) %>%
  mutate(multiple_stomachs = n_stomachs > 1) %>%
  group_by(predator) %>%
  summarize(n_multiple_stomachs = sum(multiple_stomachs),
            n_tows = n()) %>%
  mutate(percent_multiple_stomachs = 100*n_multiple_stomachs/n_tows) %>%
  filter(predator %in% c("atlantic cod", "silver hake", "spiny dogfish", "goosefish", "white hake")) %>%
  summarize(mean(percent_multiple_stomachs))

# Ok so it is higher, 68%
# Check, what is the average number of stomach samples per tow?
dat %>% 
  group_by(towid, predator) %>%
  summarize(n_stomachs = n()) %>%
  group_by(predator) %>%
  summarize(mean_n_stomachs = mean(n_stomachs)) %>%
  filter(predator %in% c("atlantic cod", "silver hake", "spiny dogfish", "goosefish", "white hake")) %>%
  summarize(mean(mean_n_stomachs))



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
  filter(predator == "atlantic cod"|predator == "spiny dogfish") %>%
  group_by(predator) %>%
  summarize(perc_mult_pred = 100*sum(mult_pred)/length(unique(specsum$towid)))

# By species is probably the most relevant, I won't average across species within tows
# On average by species, 15% of tows have more than one stomach per predator species
