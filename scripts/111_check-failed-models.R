# Script to check models that were found to have failed to converge 
# (or threw errors) in 02_pick-ranef-structure.R and 04_pick-covariates.R

library("tidyverse")
library("here")


# Setup -------------------------------------------------------------------
# Load helper functions
source(here("functions", "run_mod.R"))
source(here("functions", "make_run_name.R"))

Version <- FishStatsUtils::get_latest_version()
safe_run_mod <- purrr::safely(run_mod)

# Set VAST output location
diagnostic_folder_name <- "VAST-test-version"


# Diet Data ---------------------------------------------------------------

# Load bad models
badmod_data <- read_rds(here("output", "bad_st_diet.rds"))
# badmod_data <- read_rds(here("output", "bad_cov_diet.rds"))

# for checking only
# badmod_data <- read_rds(here("output", "top_cov_diet.rds"))

# Switch diagnostic folder location
checkrun <- badmod_data %>%
  mutate(output_file_loc = gsub("VAST", diagnostic_folder_name, output_file_loc))


debug(run_mod)

for(i in 1:nrow(checkrun)){
  checkrun$output[[i]] <- safe_run_mod(
    covar_columns = checkrun$covar_columns[[i]], 
    use_aniso = checkrun$use_aniso[[i]],
    config_file_loc = checkrun$config_file_loc[[i]], 
    strata_file_loc = here("configuration-files", "strata_limits_subset.R"), 
    processed_data = checkrun$processed_data[[i]], 
    output_file_loc = checkrun$output_file_loc[[i]],
    check_identifiable = TRUE,
    use_REML = TRUE,
    run_fast = TRUE
  )
}


allruns <- checkrun %>% 
  dplyr::mutate(
    errors = purrr::map(output,"error"), 
    worked = purrr::map_lgl(errors, is.null)
  ) %>% 
  dplyr::filter(worked) %>% 
  dplyr::mutate(
    output = purrr::map(output, "result"),
    model = basename(config_file_loc), 
    model = gsub(".R", "", model),
    covars = purrr::map_chr(covar_columns, ~ gsub(" ", ", ", .x))
  ) %>%
  dplyr::select(-c(errors, 
                   worked,
                   processed_data,
                   covar_columns, 
                   config_file_loc,
                   run_name,
                   output_file_loc)) %>%
  dplyr::mutate(
    converged = purrr::map_chr(output, "converged")) %>%
  dplyr::mutate(
    aic = purrr::map(output, "aic"),
    aic = na_if(aic, "NULL")
  ) %>%
  unnest(cols = c("aic")) 

# Two models still failed to converge, NA covars and sizecat
# Oddly seems like the opposite of what I did manually
# Interestingly, the aic values are the same for the converged and
# non-converged versions of the model

# readr::write_rds(allruns, path = here("output", "select_cov_diet_fix.rds"))
# readr::write_rds(allruns, path = here("output", "select_st_diet_fix.rds"))
