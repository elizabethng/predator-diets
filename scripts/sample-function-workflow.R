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

gitdir <- c("C:/Users/Elizabeth Ng/Documents/GitHub/predator-diets")
source(file.path(gitdir, "functions", "process_data.R"))
source(file.path(gitdir, "functions", "run_mod.R"))

strata_file_loc <- file.path(gitdir, "configuration-files", "strata_limits_subset.R")
rawdat_file_loc <- here::here("output", "data_formatted", "dat_preds_all.rds")
output_file_loc <- here::here("new_test")


safe_run_mod <- purrr::safely(run_mod)


config_file_loc <- c(file.path(gitdir, "configuration-files", "lognm-pl-independent-years-no2spatial.R"), 
                     file.path(gitdir, "configuration-files", "gamma-pl-independent-years-no2spatial.R"))




# Functional programming approach -----------------------------------------


species <- c("SPINY DOGFISH", "ATLANTIC COD", "GOOSEFISH", "WHITE HAKE")
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
         output_file_loc,
         check_identifiable = TRUE),
    safe_run_mod))

readr::write_rds(modruns, file = here::here("new_test", "modruns.rds"))
modruns <- readr::read_rds(here::here("new_test", "modruns.rds"))

# Which models failed?
failed <- modruns %>% 
  dplyr::mutate(errors = purrr::map(output,"error")) %>% 
  dplyr::mutate(worked = purrr::map_lgl(errors, is.null)) %>% 
  dplyr::filter(!worked) %>%
  dplyr::mutate(errors = purrr::map_chr(errors, "message")) %>%
  dplyr::mutate(config_file_loc = basename(config_file_loc)) %>%
  dplyr::mutate(model = str_extract(config_file_loc, "^(.{8})")) %>%
  dplyr::mutate(covars = purrr::map_chr(covars, ~paste0(.x, collapse = ", "))) %>% # neaten up covariates
  dplyr::select(-contains("_")) %>%
  dplyr::select(-output)

# All the white hake spring, but that makes sense because there was very little data for those runs



# Format the models that ran
worked <- modruns %>% 
  dplyr::mutate(errors = purrr::map(output,"error")) %>% 
  dplyr::mutate(worked = purrr::map_lgl(errors, is.null)) %>% 
  dplyr::filter(worked) %>% 
  dplyr::mutate(output = purrr::map(output, "result")) %>%
  dplyr::mutate(config_file_loc = basename(config_file_loc)) %>%
  dplyr::mutate(model = str_extract(config_file_loc, "^(.{8})")) %>%
  dplyr::mutate(covars = purrr::map_chr(covars, ~paste0(.x, collapse = ", "))) %>%
  dplyr::select(-contains("_")) %>%
  dplyr::select(-c(errors, worked)) %>%
  dplyr::mutate(aic = purrr::map_dbl(output, "aic"))


# Create aic tables for species
aictabs <- worked %>%
  dplyr::select(-output) %>%
  dplyr::mutate(species = gsub(" ", "-", tolower(species))) %>%
  dplyr::group_by(species, season) %>%
  dplyr::arrange(aic) %>%
  dplyr::mutate(delta_aic = round(aic - min(aic), 0))

# Save output
for(species_ in unique(aictabs$species)){
  for(season_ in unique(aictabs$season)){
    aictabs %>%
      dplyr::filter(species == species_ & season == season_) %>%
      readr::write_csv(here::here("output", "tables", paste(species_, season_, "aic.csv", sep = "_")))
  }
}



# Markdown display options  
pander::pandoc.table()
kableExtra::kable() %>%
  kableExtra::kable_styling()

  # tidyr::nest() %>%
  # dplyr::mutate(data = purrr::map(data, dplyr::arrange(desc("aic"))))

  


# Pull out top models for for making index plots
topmods <- worked %>%
  group_by(species, season) %>%
  arrange(aic) %>%
  top_n(n = 1)

# dplyr::select(-contains("_"))
# mutate(modname = str_replace_all(news_id, "\\d$", ""))  # some regex from research derby to try and do aic tables

# Make a plot of the indices
# Get runs that match top AIC and then extract the timeseries



