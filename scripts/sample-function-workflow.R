# Sample workflow with new data-processing functions. 
# Read formatted data and config from Dropbox
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


# raw_data <- readr::read_rds(here::here("output", "data_formatted", "dat_preds_all.rds")) 
# mydat <- process_data(raw_data, species = "SPINY DOGFISH", season = "both") 

config_file_loc <- file.path(gitdir, "configuration-files", "config-file-example.R")
strata_file_loc <- file.path(gitdir, "configuration-files", "strata_limits_subset.R")
rawdat_file_loc <- here::here("output", "data_formatted", "dat_preds_all.rds")
output_file_loc <- here::here("TEST") # overall folder where data are stored, output file name generated in run_mod


run_mod(species = "ATLANTIC COD",
        season = "both",
        covar_columns = NA,
        config_file_loc = config_file_loc,
        strata_file_loc = strata_file_loc,
        rawdat_file_loc = rawdat_file_loc,
        output_file_loc = output_file_loc)







