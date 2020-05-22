### Run models to get SE!


library("tidyverse")
library("here")
library("VAST")
library("TMB")



# Setup -------------------------------------------------------------------
# Load helper functions
source(here("functions", "run_mod_SE.R"))
source(here("functions", "make_run_name.R"))

Version <- FishStatsUtils::get_latest_version()
safe_run_mod <- purrr::safely(run_mod_SE)

# debug(run_mod_SE)

# Set VAST output location
diagnostic_folder <- file.path("D:", "Dropbox", "Predator_Diets", "output", "VAST-SE")


# Diet Data ---------------------------------------------------------------

# Load top models
dietrun <- read_rds(here("output", "top_cov_diet.rds")) %>%
  select(-output, -covars) # create separate output folder?


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

readr::write_rds(dietrun, path = here("output", "top_se_diet.rds"))
