# Using the selected random effect structure from last script,
# fit all the different covariate effects for covariate selection. 
# Only need for diet data since trawl mods don't have covs. 

library("tidyverse")
library("here")
library("VAST")
library("TMB")



# Setup -------------------------------------------------------------------
# Load helper functions
source(here("functions", "run_mod.R"))
source(here("functions", "make_run_name.R"))

Version <- FishStatsUtils::get_latest_version()
safe_run_mod <- purrr::safely(run_mod)

# Set VAST output location
diagnostic_folder <- file.path("D:", "Dropbox", "Predator_Diets", "output", "VAST")


# Diet Data ---------------------------------------------------------------

# Load top models
dietsetup <- read_rds(here("output", "top_st_diet.rds")) %>%
  select(predator, season, data, processed_data, config_file_loc)

covar_columns <- c(NA,
                   "int sizecat",
                   "int pdlenz",
                   "int pdlenz pdlenz2")

dietrun <- tidyr::expand_grid(dietsetup, covar_columns)

# Make file run name and output file location
dietrun <- dietrun %>%
  dplyr::mutate(run_name = purrr::pmap_chr(
    list("diet",
         predator,
         season, 
         covar_columns, 
         config_file_loc),
    make_run_name
  )) %>%
  dplyr::mutate(output_file_loc = map2_chr(diagnostic_folder, run_name, file.path))

# Run the model
dietrun <- dietrun %>%
  dplyr::mutate(output = purrr::pmap(
  list(covar_columns, 
       config_file_loc, 
       strata_file_loc = here("configuration-files", "strata_limits_subset.R"), 
       processed_data, 
       output_file_loc,
       check_identifiable = FALSE,
       use_REML = FALSE,
       run_fast = TRUE),
  safe_run_mod))

readr::write_rds(dietrun, path = here("output", "select_cov_diet.rds"))



