# Run models to compare spatial and non-spatial specifications. 

# Approach:
#   1. filter species and season externally
#   2. process data using appropriate function
#   3. pass settings to make name for output folder
#   4. pass data, name, and other setting to process data to generate output

library(tidyverse)
library(VAST)
library(TMB)


# Load functions ----------------------------------------------------------
gitdir <- "C:/Users/Elizabeth Ng/Documents/GitHub/predator-diets"
functions <- list.files(file.path(gitdir, "functions"), full.names = TRUE)
sapply(functions, source)

Version <- FishStatsUtils::get_latest_version() # [ ] move into config_file ??
safe_run_mod <- purrr::safely(run_mod)          # [ ] move into run_mod function?




# Functional programming approach for diet data -----------------------------------------
# 1. Filter and 2. process data
setup <- readr::read_rds(here::here("output", "data_formatted", "dat_preds_all.rds")) %>%
  # dplyr::filter(year %in% 1990:2000) %>%
  filter(pdcomnam == "ATLANTIC COD") %>%
  group_by(pdcomnam, myseason) %>%
  nest() %>%
  mutate(processed_data = purrr::map(data, process_diet_data))

# 0. Add in model options (covariates, config_files)
covar_columns <- c(
  # NA,
  # "int sizecat",
  # "int pdlenz",
  "int pdlenz pdlenz2")

config_file_loc <- c(file.path(gitdir, "configuration-files", "gamma-pl-independent-years-nospatial.R"), 
                     file.path(gitdir, "configuration-files", "gamma-pl-independent-years-no2spatial.R"))

mytest <- setup %>% 
  tidyr::expand_grid(
    covar_columns,
    config_file_loc)

# 3. Make file run name and save file location
mytest <- mytest %>%
  dplyr::mutate(run_name = purrr::pmap_chr(
    list("diet", pdcomnam, myseason, covar_columns, config_file_loc),
    make_run_name
  )) %>%
  dplyr::mutate(output_file_loc = map2_chr(
    "spatial-comparison",
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

readr::write_rds(mytest, path = here::here("spatial-comparison", "modruns.rds"))
