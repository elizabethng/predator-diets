# Fit models for spatio-temporal random effect model selection
# [ ] REML may be hardcoded in run_mod right now

library("tidyverse")
library("here")
library("VAST")
library("TMB")



# Setup -------------------------------------------------------------------
# Load helper functions
functions <- list.files(here("functions"), full.names = TRUE)
invisible(sapply(functions, source))

Version <- FishStatsUtils::get_latest_version() # [ ] move into config_file ??
safe_run_mod <- purrr::safely(run_mod_fast)     # [ ] move into run_mod function?

# Set VAST output location
diagnostic_folder <- file.path("D:", "Dropbox", "Predator_Diets", "output", "VAST")


# Diet Data ---------------------------------------------------------------
# Filter and process data
dietsetup <- readr::read_rds(here("data", "processed", "dat_preds_all.rds")) %>%
  # dplyr::filter(year %in% 1990:1995) %>%
  # dplyr::filter(pdcomnam == "SPINY DOGFISH", myseason == "SPRING") %>%
  dplyr::select(-season) %>%
  dplyr::filter(pdcomnam %in% c("ATLANTIC COD", "SILVER HAKE", "SPINY DOGFISH", "GOOSEFISH")) %>%
  rename(species = pdcomnam, season = myseason) %>% # should push this change all the way to processign stage
  group_by(species, season) %>%
  nest() %>%
  mutate(processed_data = purrr::map(data, process_diet_data))

# Add model options (covariates, config_files)
covar_columns <- c("int sizecat pdlenz pdlenz2")
config_file_loc <- map2_chr("configuration-files", 
                            c("gamma_ind-yrs_st-full.R", 
                              "gamma_ind-yrs_st-pres.R",
                              "gamma_ind-yrs_s0-pres.R",
                              "gamma_ind-yrs_st-none.R"), 
                            here)

dietrun <- tidyr::expand_grid(dietsetup, covar_columns, config_file_loc)

# Make file run name and save file location
dietrun <- dietrun %>%
  dplyr::mutate(run_name = purrr::pmap_chr(
    list("diet",
         species,
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
       use_REML = TRUE),
  safe_run_mod))

readr::write_rds(dietrun, path = here("output", "st_sel_diet.rds"))




# Trawl Data --------------------------------------------------------------
# Filter and process data
trawlsetup <- readr::read_rds(here("data", "processed", "dat_trawl.rds")) %>%
  dplyr::filter(year %in% 1990:1995) %>%
  filter(pdcomnam == "SPINY DOGFISH", myseason == "SPRING") %>%
  group_by(pdcomnam, myseason) %>%
  nest() %>%
  mutate(processed_data = purrr::map(data, process_trawl_data))

# Add in model options (covariates, config_files)
covar_columns <- NA
config_file_loc <- map2_chr("configuration-files", 
                            c("gamma_ind-yrs_st-full.R", 
                              "gamma_ind-yrs_st-pres.R", 
                              "gamma_ind-yrs_st-none.R", 
                              "lognm_ind-yrs_st-full.R", 
                              "lognm_ind-yrs_st-pres.R", 
                              "lognm_ind-yrs_st-none.R"), 
                            here)

trawlrun <- tidyr::expand_grid(trawlsetup, covar_columns, config_file_loc)[1, ]

# Make file run name and save file location
trawlrun <- trawlrun %>%
  dplyr::mutate(run_name = purrr::pmap_chr(
    list("trawl", 
         pdcomnam, 
         myseason, 
         covar_columns, 
         config_file_loc),
    make_run_name
  )) %>%
  dplyr::mutate(output_file_loc = map2_chr(diagnostic_folder, run_name, file.path))


# Run the model
trawlrun <- trawlrun %>%
  dplyr::mutate(output = purrr::pmap(
    list(covar_columns, 
         config_file_loc, 
         strata_file_loc = here("configuration-files", "strata_limits_subset.R"), 
         processed_data, 
         output_file_loc,
         check_identifiable = FALSE,
         use_REML = TRUE),
    safe_run_mod))


readr::write_rds(trawlrun, path = here::here("output", "st_sel_trawl.rds")) # previously raw_trawl.rds
