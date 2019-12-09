# Using the selected random effect structure from last script,
# fit all the different covariate effects for covariate selection. 
# Only need for diet data since trawl mods don't have covs. 

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


# dietsetup <- readr::read_rds(here("data", "processed", "dat_preds_all.rds")) %>%
#   # dplyr::filter(year %in% 1990:1995) %>%
#   # dplyr::filter(pdcomnam == "SPINY DOGFISH", myseason == "SPRING") %>%
#   dplyr::filter(pdcomnam %in% c("ATLANTIC COD", "SILVER HAKE", "SPINY DOGFISH", "GOOSEFISH")) %>%
#   group_by(pdcomnam, myseason) %>%
#   nest() %>%
#   mutate(processed_data = purrr::map(data, process_diet_data))


# Load top models (don't need to repeat data processing step)
dietsetup <- read_rds(here("output", "top_st_diet.rds")) %>%
  rename(pdcomnam = species, myseason = season) %>%
  select(pdcomnam, myseason, data, processed_data, config_file_loc)

covar_columns <- c(NA,
                   "int sizecat",
                   "int pdlenz",
                   "int pdlenz pdlenz2")

dietrun <- tidyr::expand_grid(dietsetup, covar_columns)

# Make file run name and output file location
dietrun <- dietrun %>%
  dplyr::mutate(run_name = purrr::pmap_chr(
    list("diet",
         pdcomnam,
         myseason, 
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
       use_REML = FALSE),
  safe_run_mod))

readr::write_rds(dietrun, path = here("output", "cov_sel_diet.rds")) # was raw_diet.rds



