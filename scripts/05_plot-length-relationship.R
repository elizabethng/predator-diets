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
  select(season, species, covars, estimates) %>%
  unnest(cols = c(estimates)) %>% 
  filter(covariate )
  

  
