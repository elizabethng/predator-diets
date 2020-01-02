# Script to check models that were found to have failed to converge 
# (or threw errors) in 02_pick-ranef-structure.R

library("tidyverse")
library("here")


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
badmod_data <- read_rds(here("output", "bad_st_diet.rds"))

# Run the model
checkrun <- badmod_data 

for(i in 1:nrow(checkrun)){
  checkrun$output[[i]] <- safe_run_mod(
    checkrun$covar_columns[[i]], 
    checkrun$config_file_loc[[i]], 
    strata_file_loc = here("configuration-files", "strata_limits_subset.R"), 
    checkrun$processed_data[[i]], 
    checkrun$output_file_loc[[i]],
    check_identifiable = TRUE,
    use_REML = TRUE,
    run_fast = TRUE
  )

}


# readr::write_rds(dietrun, path = here("output", "select_cov_diet.rds"))