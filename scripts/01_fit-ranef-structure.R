# Fit models for spatio-temporal random effect model selection
# using over-saturated covariates and REML.

library("tidyverse")
library("here")
library("VAST")
library("TMB")


# Setup -------------------------------------------------------------------
 test <- FALSE

# Load helper functions
source(here("functions", "process_trawl_data.R"))
source(here("functions", "process_diet_data.R"))
source(here("functions", "make_run_name.R"))
source(here("functions", "run_mod.R"))

Version <- FishStatsUtils::get_latest_version() # [ ] move into config_file ??
safe_run_mod <- purrr::safely(run_mod)     # [ ] move into run_mod function?

# Set VAST output location
diagnostic_folder <- file.path("D:", "Dropbox", "Predator_Diets", "output", "VAST")

debug(run_mod)

# Diet Data ---------------------------------------------------------------
# Filter and process data
if(test == TRUE){
  dietsetup <- readr::read_rds(here("data", "processed", "dat_preds_all.rds")) %>%
    dplyr::filter(year %in% 1990:1995) %>%
    dplyr::filter(predator %in% c("spiny dogfish", "goosefish")) %>%
        group_by(predator, season) %>%
    nest() %>%
    mutate(processed_data = purrr::map(data, process_diet_data))
}else if(test == FALSE){
  dietsetup <- readr::read_rds(here("data", "processed", "dat_preds_all.rds")) %>%
    dplyr::filter(predator %in% c("atlantic cod", "silver hake", "spiny dogfish", "goosefish", "white hake")) %>%
    group_by(predator, season) %>%
    nest() %>%
    mutate(processed_data = purrr::map(data, process_diet_data))
}


# Add model options (covariates, config_files)
use_aniso <- c(TRUE, FALSE)
covar_columns <- c("int sizecat pdlenz pdlenz2")
config_file_loc <- map2_chr("configuration-files", 
                            c("gamma_ind-yrs_st-full.R", 
                              "gamma_ind-yrs_st-pres.R",
                              "gamma_ind-yrs_s0-pres.R",
                              "gamma_ind-yrs_st-none.R"), 
                            here)

dietrun <- tidyr::expand_grid(dietsetup, covar_columns, use_aniso, config_file_loc)

# Make file run name and save file location
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
       use_aniso,
       config_file_loc, 
       strata_file_loc = here("configuration-files", "strata_limits_subset.R"), 
       processed_data, 
       output_file_loc,
       check_identifiable = FALSE,
       use_REML = TRUE,
       run_fast = TRUE),
  safe_run_mod))

readr::write_rds(dietrun, path = here("output", "select_st_diet.rds"))


# Trawl Data --------------------------------------------------------------
# Filter and process data

if(test == TRUE){
  trawlsetup <- readr::read_rds(here("data", "processed", "dat_trawl.rds")) %>%
    dplyr::filter(year %in% 1990:1995) %>%
    group_by(species, season) %>%
    nest() %>%
    mutate(processed_data = purrr::map(data, process_trawl_data))
}else if(test == FALSE){
  trawlsetup <- readr::read_rds(here("data", "processed", "dat_trawl.rds")) %>%
    group_by(species, season) %>%
    nest() %>%
    mutate(processed_data = purrr::map(data, process_trawl_data))
}


# Add in model options (covariates, config_files)
use_aniso <- c(TRUE, FALSE)
covar_columns <- "vessel_change"
config_file_loc <- map2_chr("configuration-files", 
                            c("gamma_ind-yrs_st-none.R",
                              "gamma_ind-yrs_s0-pres.R",
                              "gamma_ind-yrs_st-pres.R", 
                              "gamma_ind-yrs_st-full.R"
                              ),
                            here)

trawlrun <- tidyr::expand_grid(trawlsetup, covar_columns, use_aniso, config_file_loc)

# Make file run name and save file location
trawlrun <- trawlrun %>%
  dplyr::mutate(run_name = purrr::pmap_chr(
    list("trawl", 
         species, 
         season, 
         covar_columns, 
         config_file_loc),
    make_run_name
  )) %>%
  dplyr::mutate(output_file_loc = map2_chr(diagnostic_folder, run_name, file.path))


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
         run_fast = TRUE),
    safe_run_mod))
readr::write_rds(trawlrun, path = here::here("output", "select_st_trawl.rds"))
