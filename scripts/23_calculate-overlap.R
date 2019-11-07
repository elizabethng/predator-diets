# Collecting info to do overlap calculations

# library(readr)
# library(here)
# library(tidyverse)
# 
# # Locate the formatted trawl data
# dat <- read_rds(here("output", "data_formatted", "dat_trawl.rds"))
# 
# # Species need to use the same mesh for overlap calculations. 
# # How many trawl locations are there per predator species?
# # In this data set they should all have the same number. 
# 
# dat %>% 
#   mutate(pres = ifelse(catch_kg > 0, 1, 0)) %>%
#   group_by(species, year, season) %>%
#   summarize(n = n(),
#             n_pres = sum(pres, na.rm = TRUE),
#             tot_kg = sum(catch_kg, na.rm = TRUE)
#             ) %>%
#   mutate(p_pres = 100*(round(n_pres/n, 2))) %>% View()



# Check whether I can use run_mod function and process_data
# with the process_data function

library(tidyverse)
library(VAST)
library(TMB)


Version <- FishStatsUtils::get_latest_version()

gitdir <- c("C:/Users/Elizabeth Ng/Documents/GitHub/predator-diets")
source(file.path(gitdir, "functions", "process_data.R"))
source(file.path(gitdir, "functions", "run_mod.R"))

strata_file_loc <- file.path(gitdir, "configuration-files", "strata_limits_subset.R")
rawdat_file_loc <- here::here("output", "data_formatted", "dat_preds_all.rds")
output_file_loc <- here::here("density_test")


safe_run_mod <- purrr::safely(run_mod)


config_file_loc <- c(file.path(gitdir, "configuration-files", "lognm-pl-independent-years-no2spatial.R"))


check <- safe_run_mod(species = "SPINY DOGFISH",
                      season = "spring",
                      covar_columns = NA,
                      config_file_loc = config_file_loc,
                      strata_file_loc = strata_file_loc,
                      rawdat_file_loc = rawdat_file_loc,
                      output_file_loc = output_file_loc)

  