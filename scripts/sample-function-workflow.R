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
                      covar_columns = "none",
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

