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
source(here("functions", "run_mod.R"))
source(here("functions", "make_run_name.R"))

Version <- FishStatsUtils::get_latest_version()
safe_run_mod <- purrr::safely(run_mod)

# Set VAST output location
diagnostic_folder <- file.path("D:", "Dropbox", "Predator_Diets", "output", "VAST-update")


# Trawl Data ---------------------------------------------------------------

# Load top models
trawlrun <- read_rds(here("output", "top_st_trawl.rds")) %>%
  select(-output, -model) %>%
  mutate(output_file_loc = gsub(pattern = "D:/Dropbox/Predator_Diets/output/VAST",  
                               replacement = diagnostic_folder, 
                               x = output_file_loc)) %>%
  filter(species == "atlantic cod", season == "fall")

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
         run_fast = FALSE),
    safe_run_mod))

readr::write_rds(trawlrun, path = here("output", "top_final_trawl.rds"))
