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
#    - Diagnostic plot
#    - Final parameter estimates

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


# debug(run_mod)

# Diet Data ---------------------------------------------------------------

# Load top models and change output file location
dietrun <- read_rds(here("output", "top_cov_diet.rds")) %>%
  select(-output, -covars) %>%
  mutate(output_file_loc = gsub(" ", "-", predator),
         output_file_loc = paste0("diet_", season, "_", output_file_loc),
         output_file_loc = here("output", "diagnostics", output_file_loc))
 

# Run the model
dietrun <- dietrun %>%
  dplyr::mutate(output = purrr::pmap(
    list(covar_columns,
         use_aniso,
         config_file_loc, 
         strata_file_loc = here("configuration-files", "strata_limits_subset.R"), 
         processed_data, 
         output_file_loc,
         check_identifiable = FALSE,
         use_REML = TRUE,
         run_fast = FALSE),
    safe_run_mod))

readr::write_rds(dietrun, path = here("output", "top_final_diet.rds"))

# Trawl Data ---------------------------------------------------------------
# [ ] vessel change here will likely go away once I re-run pipeline
# Load top models, change output file location, and add vessel
trawlrun <- read_rds(here("output", "top_st_trawl.rds")) %>%
  select(-output, -model) %>%
  mutate(output_file_loc = gsub(" ", "-", species),
         output_file_loc = paste0("trawl_", season, "_", output_file_loc),
         output_file_loc = here("output", "diagnostics", output_file_loc)) %>%
  mutate(covar_columns = "vessel")


# Run the model
trawlrun <- trawlrun %>%
  dplyr::mutate(output = purrr::pmap(
    list(covar_columns, 
         use_aniso,
         config_file_loc, 
         strata_file_loc = here("configuration-files", "strata_limits_subset.R"), 
         processed_data, 
         output_file_loc,
         check_identifiable = FALSE,
         use_REML = TRUE,
         run_fast = FALSEs),
    safe_run_mod))

readr::write_rds(trawlrun, path = here("output", "top_final_trawl.rds"))
