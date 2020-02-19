# Script to check sample sizes for all the species I plan to use in my models

library(here)
library(tidyverse)

raw_data <- readr::read_rds(here::here("output", "data_formatted", "dat_preds_all.rds")) 


# Check raw data ----------------------------------------------------------
# Check for years missing observations and years without positive prey presence
raw_data %>%
  filter(pdcomnam %in% c("SPINY DOGFISH", "ATLANTIC COD", "WHITE HAKE", "GOOSEFISH", "SILVER HAKE")) %>%
  group_by(pdcomnam, myseason, year) %>%
  summarize(
    pypres = sum(pyamtw),
    n = n()
    ) %>%
  pivot_longer(-c(pdcomnam, myseason, year), names_to = "type", values_to = "value") %>%
  ggplot(aes(year, value, group = type, fill = type)) +
  geom_col(position = "dodge") +
  facet_grid(myseason ~ pdcomnam)


raw_data %>%
  filter(pdcomnam %in% c("SPINY DOGFISH", "ATLANTIC COD", "WHITE HAKE", "GOOSEFISH", "SILVER HAKE")) %>%
  group_by(pdcomnam, myseason, year) %>%
  summarize(
    pypres = sum(pyamtw) > 0,
    n = n()
  ) %>%
  ggplot(aes(year, n, fill = pypres)) +
  geom_col() +
  facet_grid(myseason ~ pdcomnam)

# Based on this, I'll have to truncate some years off of goosefish, silver hake, and white hake
# I definitely won't be able to a spring model for white hake. 

# However, if I take Jim's suggestion and average by tows, then I think that will
# fix a lot of things.


# Check tow-level averages ------------------------------------------------

# towid should be unique across seasons, but double check (evaluates to 0 if ok)
raw_data %>% 
  group_by(towid) %>% 
  summarize(n_seasons = length(levels(as.factor(myseason)))) %>%
  mutate(too_many = ifelse(n_seasons > 1, TRUE, FALSE)) %>%
  summarize(n_too_many = sum(too_many))
  
# Check the unique number of of predator x tow combinations
raw_data %>%
  group_by(pdcomnam, towid) %>%
  summarize(n = n()) %>%
  nrow()

# Get average per tow values (of prey amount and covariates...)
tow_avg_dat <- raw_data %>%
  mutate(size_cat = ifelse(sizecat == "S", -1, 
                           ifelse(sizecat == "M", 0, 1))) %>%
  group_by(pdcomnam, towid, year, myseason, declat, declon) %>%
  summarize(
    pyamtw_mean = mean(pyamtw, na.rm = TRUE),
    pdlen_mean = mean(pdlen, na.rm = TRUE),
    # pdwgt_mean = mean(pdwgt, na.rm = TRUE),
    sizecat_median = median(size_cat,  na.rm = TRUE))

nrow(tow_avg_dat) # good, matches above

# Check missingness
tow_avg_dat$pyamtw_mean %>% is.na() %>% sum()
tow_avg_dat$pdlen_mean %>% is.na() %>% sum()
# tow_avg_dat$pdwgt_mean %>% is.na() %>% sum() # that's why I left these out
tow_avg_dat$sizecat_median %>% is.na() %>% sum()

tow_avg_dat %>%
  filter(pdcomnam %in% c("SPINY DOGFISH", "ATLANTIC COD", "WHITE HAKE", "GOOSEFISH", "SILVER HAKE")) %>%
  group_by(pdcomnam, myseason, year) %>%
  summarize(
    pypres = sum(pyamtw_mean) > 0,
    n = n()
  ) %>%
  ggplot(aes(year, n, fill = pypres)) +
  geom_col() +
  facet_grid(myseason ~ pdcomnam)

# Now that I can visualize this, how should I deal with the missing data?
# Especially for the seasonal models, where things ger very sparse?
# It's also tough, becuase the early years are interesting becasue herring
# abundance was low. 

# A good option could be to fix those years to zero, need to check how to do that.

tow_avg_dat %>%
  filter(pdcomnam %in% c("SPINY DOGFISH", "ATLANTIC COD", "WHITE HAKE", "GOOSEFISH", "SILVER HAKE")) %>%
  group_by(pdcomnam, myseason, year) %>%
  summarize(
    pypres = sum(pyamtw_mean) > 0,
    n = n()
  ) %>%
  ggplot(aes(year, n, fill = pypres)) +
  geom_col() +
  facet_grid(~ pdcomnam)


# Make a table to reference
tow_avg_dat %>%
  # filter(pdcomnam %in% c("SPINY DOGFISH", "ATLANTIC COD", "WHITE HAKE", "GOOSEFISH", "SILVER HAKE")) %>%
  group_by(pdcomnam, myseason, year) %>%
  summarize(
    pypres = sum(pyamtw_mean) > 0,
    n = n()
  ) %>%
  ungroup() %>%
  pivot_wider(names_from = myseason, values_from = c(pypres, n)) %>%
  write_csv(path = here("output", "tables", "summary_of_herring_presence_in_preds.csv"))
  

# Check for 0%, 100%, and low sample size among tows within a year
check <- tow_avg_dat %>%
  filter(pdcomnam %in% c("SPINY DOGFISH", "ATLANTIC COD", "WHITE HAKE", "GOOSEFISH", "SILVER HAKE")) %>%
  group_by(pdcomnam, myseason, year) %>%
  mutate(pypres = as.numeric(pyamtw_mean > 0)) %>%
  summarize(
    npos = sum(pypres),
    ntot = n()
  ) %>%
  ungroup() %>%
  mutate(
    perc_pres = 100*(npos/ntot),
    obs100 = perc_pres == 100,
    obs0 = perc_pres== 0,
    low_samps = ntot < 10)


  mutate(flag = any(c(obs0, obs100, low_samps)))
    
    
    
Data_Geostat %>% mutate(pypres = as.numeric(Catch_KG>0))%>% group_by(Year) %>%  summarize(n_pos = sum(pypres), ntot = n()) %>% mutate(perc_obs = 100*round(n_pos/ntot, 2)) %>% View()
