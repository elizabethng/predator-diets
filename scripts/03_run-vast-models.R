library("tidyverse")
library("here")
library("VAST")
library("TMB")



# Setup -------------------------------------------------------------------
# Load helper functions
functions <- list.files(here("functions"), full.names = TRUE)
sapply(functions, source)

Version <- FishStatsUtils::get_latest_version() # [ ] move into config_file ??
safe_run_mod <- purrr::safely(run_mod)          # [ ] move into run_mod function?

# Set VAST output location
diagnostic_folder <- file.path("D:", "Dropbox", "Predator_Diets", "output", "VAST")


# Diet Data ---------------------------------------------------------------
# Filter and process data
dietsetup <- readr::read_rds(here("data", "processed", "dat_preds_all.rds")) %>%
  dplyr::filter(year %in% 1990:2000) %>%
  dplyr::filter(pdcomnam == "SPINY DOGFISH", myseason == "SPRING") %>%
  group_by(pdcomnam, myseason) %>%
  nest() %>%
  mutate(processed_data = purrr::map(data, process_diet_data))

# Add model options (covariates, config_files)
covar_columns <- c(NA,
                   "int sizecat",
                   "int pdlenz",
                   "int pdlenz pdlenz2")[2]
config_file_loc <- c(here("configuration-files", "lognm-pl-independent-years-no2spatial.R"), 
                     here("configuration-files", "gamma-pl-independent-years-no2spatial.R"))[[1]]

dietrun <- tidyr::expand_grid(dietsetup, covar_columns, config_file_loc)

# Make file run name and save file location
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
       check_identifiable = TRUE),
  safe_run_mod))

readr::write_rds(dietrun, path = here("output", "dietraw.rds"))




# Trawl Data --------------------------------------------------------------
# Filter and process data
setup <- readr::read_rds(here::here("output", "data_formatted", "dat_trawl.rds")) %>%
  # dplyr::filter(year %in% 1990:1995) %>%
  # filter(pdcomnam == "SPINY DOGFISH") %>%
  group_by(pdcomnam, myseason) %>%
  nest() %>%
  mutate(processed_data = purrr::map(data, process_trawl_data))

# 0. Add in model options (covariates, config_files)
covar_columns <- NA
config_file_loc <- c(file.path(gitdir, "configuration-files", "gamma-pl-independent-years.R"), 
                     file.path(gitdir, "configuration-files", "lognm-pl-independent-years.R"))

mytest <- setup %>% 
  tidyr::expand_grid(
    covar_columns,
    config_file_loc)

# 3. Make file run name and save file location
mytest <- mytest %>%
  dplyr::mutate(run_name = purrr::pmap_chr(
    list("trawl", pdcomnam, myseason, covar_columns, config_file_loc),
    make_run_name
  )) %>%
  dplyr::mutate(output_file_loc = map2_chr(
    folder_name,
    run_name,
    here::here
  ))

# 4. Run the model
mytest <- mytest %>%
  dplyr::mutate(output = purrr::pmap(
    list(covar_columns, 
         config_file_loc, 
         strata_file_loc = file.path(gitdir, "configuration-files", "strata_limits_subset.R"), 
         processed_data, 
         output_file_loc,
         check_identifiable = TRUE),
    safe_run_mod))

readr::write_rds(mytest, path = here::here("TRAWL", "modruns.rds"))
