# Check and plot predator covariates
# Did not keep size category coefficeints 
# !! [ ] could change that
# So just plot lengths for all

library("tidyverse")
library("here")


# Raw data for length information
# rawdiet <- readr::read_rds(here("data", "processed", "dat_preds_all.rds")) %>%
#   dplyr::filter(pdcomnam %in% c("ATLANTIC COD", "SILVER HAKE", "SPINY DOGFISH", "GOOSEFISH"))

topdiet <- readr::read_rds(here("output", "top_diet.rds"))

# Get length covariates
lencoefs <- topdiet %>%
  mutate(estimates = purrr::map(output, "estimates")) %>%
  select(season, species, estimates) %>%
  unnest(cols = c(estimates)) %>% 
  filter(covariate %in% c("pdlenz", "pdlenz2")) %>%
  pivot_wider(names_from = c(covariate, predictor), values_from = estimate)
  
# Get length data
lendat <- topdiet %>%
  select(data) %>%
  unnest()

# Join in covariates for calculation
preddat <- lendat %>%
  select(species, season, year, pdlen, declon, declat) %>%
  left_join(lencoefs, by = c("season", "species")) %>%
  mutate(
    mean_len = mean(pdlen),
    sd_len = sd(pdlen),
    z_calc = (pdlen - mean_len)/sd_len,
    z_check = scale(pdlen)[,1]
  )





# Check: looks good
filter(lendat, species == "silver hake", season == "spring") %>%
  pull(pdlen) %>%
  scale()
  
filter(preddat, species == "silver hake", season == "spring") %>%
  select(mean_len, sd_len)
  
  
  

# Quick map of length distribution
  
  
  
  
  
  
  




  


# ISSUE--come back to this with more time
#      --problem was myspecies vs species
topdiet %>%
  select(data) %>%
  unnest(cols = c(data))

topdiet %>%
  select(data) %>%
  unnest()

topdiet %>%
  select(species, season, data) %>%
  unnest(cols = c(species, season, data))

topdiet %>%
  rename(myseason = season) %>%
  unnest(cols = c(data))
