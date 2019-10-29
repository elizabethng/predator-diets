library(tidyverse)
library(VAST)
library(TMB)


Version <- FishStatsUtils::get_latest_version()

gitdir <- c("C:/Users/Elizabeth Ng/Documents/GitHub/predator-diets") # Alas, things get wonky now with my project structure. 
source(file.path(gitdir, "functions", "process_data.R"))
source(file.path(gitdir, "functions", "run_mod.R"))

strata_file_loc <- file.path(gitdir, "configuration-files", "strata_limits_subset.R")
rawdat_file_loc <- here::here("output", "data_formatted", "dat_preds_all.rds")
output_file_loc <- here::here("TEST")


safe_run_mod <- purrr::safely(run_mod)

# config_file_loc <- c(file.path(gitdir, "configuration-files", "lognm-pl-independent-years.R")) 
# config_file_loc <- c(file.path(gitdir, "configuration-files", "gamma-pl-independent-years.R"))
config_file_loc <- file.path(gitdir, "configuration-files", "config-file-example.R")

species = "SPINY DOGFISH"
season = "spring"
covar_columns = NA

check <- safe_run_mod(species = "SPINY DOGFISH",
                      season = "spring",
                      covar_columns = NA,
                      config_file_loc = config_file_loc,
                      strata_file_loc = strata_file_loc,
                      rawdat_file_loc = rawdat_file_loc,
                      output_file_loc = output_file_loc)



# Old stuff from saple-function-workflow ----------------------------------

# species <- c("BLUEFISH")
# season <- c("fall")
# covars <- list(NA)
# 
# modruns <- tidyr::expand_grid(species, season, covars,
#                               config_file_loc,
#                               strata_file_loc,
#                               rawdat_file_loc,
#                               output_file_loc)  %>%
#   tibble::rownames_to_column(var = "id")
# 
# 
# modruns <- modruns %>%
#   dplyr::mutate(output = purrr::pmap(
#     list(species, 
#          season, 
#          covars, 
#          config_file_loc, 
#          strata_file_loc, 
#          rawdat_file_loc, 
#          output_file_loc),
#     safe_run_mod))



# Single run checks -------------------------------------------------------

# config_file_loc <- file.path(gitdir, "configuration-files", "config-file-example.R")
# config_file_loc <- file.path(gitdir, "configuration-files", "config-file-independent-years.R")

# check <- safe_run_mod(species = "WHITE HAKE",
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
#
check <- safe_run_mod(species = "SPINY DOGFISH",
                      season = "spring",
                      covar_columns = NA,
                      config_file_loc = config_file_loc,
                      strata_file_loc = strata_file_loc,
                      rawdat_file_loc = rawdat_file_loc,
                      output_file_loc = output_file_loc)

