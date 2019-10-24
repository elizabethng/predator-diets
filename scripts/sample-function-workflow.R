# Sample workflow with new data-processing functions. 
# Source functions from github
# Ouput VAST Save file, diagnostics etc to Dropbox

# library(here)
# library(tidyverse)
library(VAST)
library(TMB)
library(magrittr)


# For transparency:
#   1. set species and season externally
#   2. pass species, config_file, season, covars to make output folder
#   3. pipe read data directly to process data to generate output


Version <- FishStatsUtils::get_latest_version()

gitdir <- c("C:/Users/Elizabeth Ng/Documents/GitHub/predator-diets") # Alas, things get wonky now with my project structure. 
source(file.path(gitdir, "functions", "process_data.R"))
source(file.path(gitdir, "functions", "run_mod.R"))

config_file_loc <- file.path(gitdir, "configuration-files", "config-file-example.R")
strata_file_loc <- file.path(gitdir, "configuration-files", "strata_limits_subset.R")
rawdat_file_loc <- here::here("output", "data_formatted", "dat_preds_all.rds")
output_file_loc <- here::here("TEST")


safe_run_mod <- purrr::safely(run_mod)

check <- safe_run_mod(species = "SILVER HAKE",
                      season = "fall",
                      covar_columns = NA,
                      config_file_loc = config_file_loc,
                      strata_file_loc = strata_file_loc,
                      rawdat_file_loc = rawdat_file_loc,
                      output_file_loc = output_file_loc)

# With covariates
check <- safe_run_mod(species = "SILVER HAKE",
                      season = "fall",
                      covar_columns = c("int", "sizecat"),
                      config_file_loc = config_file_loc,
                      strata_file_loc = strata_file_loc,
                      rawdat_file_loc = rawdat_file_loc,
                      output_file_loc = output_file_loc)

# Make a tibble of models to run (species, season, covariates)
# and then use map_at or whatever to run this function with those
# arguments.
# Keep that in mind when figuring out how to pass "covar_columns"
# to run mod. It would be easiest to pass it as a character vector.

species <- c("ATLANTIC COD")
season <- c("spring", "fall")
covars <- list(NA) # ,
               # c("int", "sizecat"),
               # c("int", "pdlenz"),
               # c("int", "pdlenz", "pdlenz2"))

# modruns <- purrr::cross(
#   list(species, 
#        season, 
#        covars,
#        config_file_loc,
#        strata_file_loc,
#        rawdat_file_loc,
#        output_file_loc)) %>%
#   purrr::map(set_names, c("species", "season", "covars", "config", "strata", "rawdat", "outloc")) %>%
#   rlang::set_names(paste0("run", 1:length(.)))

modruns <- tidyr::expand_grid(species, season, covars,
                              config_file_loc,
                              strata_file_loc,
                              rawdat_file_loc,
                              output_file_loc)  %>%
  tibble::rownames_to_column(var = "id")

modruns <- modruns %>%
        dplyr::mutate(output = purrr::pmap(
          list(species, 
               season, 
               covars, 
               config_file_loc, 
               strata_file_loc, 
               rawdat_file_loc, 
               output_file_loc),
          safe_run_mod))


withres <- modruns %>% 
  dplyr::mutate(errors = purrr::map(output,"error")) %>% 
  dplyr::mutate(worked = purrr::map_lgl(errors, is.null)) %>% 
  dplyr::filter(worked) %>% 
  dplyr::mutate(output = purrr::map(output, "result"))

# Pull out aic for making a table
withres %>%
  dplyr::mutate(aic = purrr::map_dbl(output, "aic")) %>%
  dplyr::select(-contains("_"))

# I think it needs a list...
badruns <- modruns %>%
  tibble::rownames_to_column(var = "id") %>%
  dplyr::group_by(id) %>%
  tidyr::nest()
  


  # dplyr::select(output) %>%
  # as.list() %>%
  purrr::map_lgl(~is.null(.x$error) == F) 




