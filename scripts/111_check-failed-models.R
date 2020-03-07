# Script to check models that were found to have failed to converge 
# (or threw errors) in 02_pick-ranef-structure.R and 04_pick-covariates.R

library("tidyverse")
library("here")


# Options -----------------------------------------------------------------

ranef_mods <- TRUE # if false, check covariate models
interactive <- FALSE


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
if(ranef_mods == TRUE){
  badmod_data <- read_rds(here("output", "bad_st_diet.rds"))
}else{
  badmod_data <- read_rds(here("output", "bad_cov_diet.rds"))
}

# Switch diagnostic folder location
checkrun <- badmod_data %>%
  mutate(output_file_loc = gsub("VAST", diagnostic_folder_name, output_file_loc))


if(interactive == TRUE){debug(run_mod)}

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



# Process results ---------------------------------------------------------
# Code for random effect checks
if(ranef_mods == TRUE){
  firstcheck <- checkrun %>%
    select(predator, season, use_aniso, run_name, model, covars, converged, output) %>%
    dplyr::mutate(
      errors = purrr::map(output,"error"),
      worked = purrr::map_lgl(errors, is.null)
    ) 
  
  # Additional optimization steps for ones that fail on second attempt
  # May need additional code to help with anisotropy parameters
  if(any(!firstcheck$worked)){
    badrows <- which(firstcheck$worked == FALSE)
    
    debug(run_mod)
    
    for(i in badrows){
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
  }
  
  secondcheck <- checkrun %>%
    select(predator, season, use_aniso, run_name, model, covars, converged, output) %>%
    dplyr::mutate(
      errors = purrr::map(output,"error"),
      worked = purrr::map_lgl(errors, is.null)
    ) 
  
  # Separate out models that are not identifiable
  ar0 <- secondcheck %>% 
    select(-errors) %>%
    dplyr::filter(worked) %>% 
    dplyr::mutate(output = purrr::map(output, "result"),
                  element = purrr::map(output, 1))
  
  # ar1 <- mutate(ar0, identifialbe = ifelse(is.null(dim(element)), TRUE, FALSE))
  ar0$identifiable <- sapply(ar0$element, function(x) is.null(dim((x))))
  
  res_ident <- filter(ar0, identifiable == TRUE)
  res_nonid <- filter(ar0, identifiable == FALSE)
  

  nonid_params <- res_nonid %>%
    rowid_to_column("mod_num") %>%
    dplyr::mutate(
      bad_params = purrr::map_int(output, "WhichBad"),
      param_list = purrr::map(output, "BadParams"),
      param_list = purrr::map(param_list, ~ rowid_to_column(.x, "param")) # add an id for parameter number
    ) %>%
    select(-element, -output) %>%
    unnest(param_list) %>%
    filter(param == bad_params | Param_check == "Bad") %>%
    select(-run_name, -covars, -converged, -worked, -identifiable)
  
  readr::write_rds(nonid_params, path = here("output", "select_st_diet_bad_mods.rds"))
  
  
  fix_params <- res_ident %>%
    select(-element) %>%
    dplyr::mutate(
      aic = purrr::map(output, "aic"),
      aic = na_if(aic, "NULL")
    ) %>%
    unnest(cols = c("aic")) 
  
  tmp <- checkrun %>% 
    dplyr::mutate(output = purrr::map(output, "result"),
                  element = purrr::map(output, 1))
  tmp$identifiable <- sapply(myres$element, function(x) is.null(dim((x))))
  
  allruns <- tmp %>%
    # filter(identifiable) %>%
  select(-output.x, -model, -data.y, -output.y, -covars,
         -converged, -element) %>%
    rename(data = data.x) 
}


# Code for covariate checks
if(ranef_mods == FALSE){
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
}



# Save output -------------------------------------------------------------

if(ranef_mods == TRUE){
  readr::write_rds(allruns, path = here("output", "select_st_diet_fix.rds"))
}else{
  readr::write_rds(allruns, path = here("output", "select_cov_diet_fix.rds"))
}


