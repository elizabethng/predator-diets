# Final step of model selection process, refit the top models
# for both diet data and trawl data using
#  * REML
#  * Doing bias correction
#  * Doing fine.scale = TRUE (??)
#  * Getting output
#    - Index and SEs
#    - Parameter estimates
#    - vcov for fixed effects
#    - knot-level means
#    - knot-level SEs
#    - mesh
# [ ] What else?


library("tidyverse")
library("here")
library("VAST")
library("TMB")



# Setup -------------------------------------------------------------------
# Load helper functions
functions <- list.files(here("functions"), full.names = TRUE)
invisible(sapply(functions, source))

Version <- FishStatsUtils::get_latest_version()
safe_run_mod <- purrr::safely(run_mod_fast)

# Set VAST output location
diagnostic_folder <- file.path("D:", "Dropbox", "Predator_Diets", "output", "VAST")


# Diet Data ---------------------------------------------------------------

# Load top models
dietrun <- read_rds(here("output", "top_cov_diet.rds")) %>%
  rename(pdcomnam = species, myseason = season) %>%
  select(pdcomnam, 
         myseason, 
         data, 
         processed_data, 
         config_file_loc, 
         covar_columns, 
         run_name,
         output_file_loc) # may want to modify in future?

# Run the model
dietrun <- dietrun %>%
  dplyr::mutate(output = purrr::pmap(
    list(covar_columns, 
         config_file_loc, 
         strata_file_loc = here("configuration-files", "strata_limits_subset.R"), 
         processed_data, 
         output_file_loc,
         check_identifiable = TRUE,
         use_REML = TRUE),
    safe_run_mod))

readr::write_rds(dietrun, path = here("output", "top_final_diet.rds"))

# Trawl Data --------------------------------------------------------------
# [ ] Need to run through

trawlrun <- read_rds(here("output", "top_st_trawl.rds")) %>%
  rename(pdcomnam = species, myseason = season) %>%
  select(pdcomnam, 
         myseason, 
         data, 
         processed_data, 
         config_file_loc, 
         covar_columns, 
         run_name,
         output_file_loc) # may want to modify in future?

# Run the model
trawlrun <- trawlrun %>%
  dplyr::mutate(output = purrr::pmap(
    list(covar_columns, 
         config_file_loc, 
         strata_file_loc = here("configuration-files", "strata_limits_subset.R"), 
         processed_data, 
         output_file_loc,
         check_identifiable = TRUE,
         use_REML = TRUE),
    safe_run_mod))

readr::write_rds(dietrun, path = here("output", "top_final_trawl.rds"))
