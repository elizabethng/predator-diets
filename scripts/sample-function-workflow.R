# Sample workflow with new data-processing functions. 
# Source functions from github
# Ouput VAST Save file, diagnostics etc to Dropbox

library(tidyverse)
library(VAST)
library(TMB)

# For transparency:
#   1. set species and season externally
#   2. pass species, config_file, season, covars to make output folder
#   3. pipe read data directly to process data to generate output

Version <- FishStatsUtils::get_latest_version()

gitdir <- c("C:/Users/Elizabeth Ng/Documents/GitHub/predator-diets") # Alas, things get wonky now with my project structure. 
source(file.path(gitdir, "functions", "process_data.R"))
source(file.path(gitdir, "functions", "run_mod.R"))

# config_file_loc <- file.path(gitdir, "configuration-files", "config-file-example.R")
config_file_loc <- file.path(gitdir, "configuration-files", "config-file-independent-years.R")
strata_file_loc <- file.path(gitdir, "configuration-files", "strata_limits_subset.R")
rawdat_file_loc <- here::here("output", "data_formatted", "dat_preds_all.rds")
output_file_loc <- here::here("TEST")


safe_run_mod <- purrr::safely(run_mod)

# check <- safe_run_mod(species = "SILVER HAKE",
#                       season = "fall",
#                       covar_columns = NA,
#                       config_file_loc = config_file_loc,
#                       strata_file_loc = strata_file_loc,
#                       rawdat_file_loc = rawdat_file_loc,
#                       output_file_loc = output_file_loc)
# 
# # With covariates
# check <- safe_run_mod(species = "ATLANTIC COD",
#                       season = "fall",
#                       covar_columns = c("int", "sizecat"),
#                       config_file_loc = config_file_loc,
#                       strata_file_loc = strata_file_loc,
#                       rawdat_file_loc = rawdat_file_loc,
#                       output_file_loc = output_file_loc)


species <- c("ATLANTIC COD", "SILVER HAKE", "RED HAKE", "FOURSPOT FLOUNDER", "ATLANTIC COD", "POLLOCK", "WHITE HAKE", "WINTER SKATE", "SPINY DOGFISH", "SUMMER FLOUNDER", "GOOSEFISH","THORNY SKATE", "SEA RAVEN", "BLUEFISH", "WEAKFISH")
season <- c("spring", "fall")
covars <- list(NA,
               c("int", "sizecat"),
               c("int", "pdlenz"),
               c("int", "pdlenz", "pdlenz2"))


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



