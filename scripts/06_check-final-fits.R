# Check fits of final models

# Script to check convergence and random effects magnitude for 
# final models fit with REML. 

# Save results for use by next script to do model 
# selection for covariates.

library("tidyverse")
library("here")


# Diet Models ---------------------------------------------------------------
dietrun <- read_rds(here("output", "top_final_diet.rds"))

allruns <- dietrun %>% 
  dplyr::mutate(
    errors = purrr::map(output,"error"), 
    worked = purrr::map_lgl(errors, is.null)
  ) %>% 
  # dplyr::filter(worked) %>% 
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
    converged = purrr::map_chr(output, "converged")) # Some errors were passed (non numerical argument)
# converged = ifelse(converged %in% c("TRUE", "FALSE"), converged, NA) 
# )  

# Error messages get caught before going to output$error
failed <- allruns %>%
  filter(converged != "TRUE")
# filter(is.na(converged) | converged == FALSE)

# Process models without errors
worked <- allruns %>%
  dplyr::mutate(
    aic = purrr::map(output, "aic"),
    aic = na_if(aic, "NULL")
  ) %>%
  unnest(cols = c("aic")) %>%
  filter(converged == TRUE) %>%
  filter(!is.na(converged)) %>%
  mutate(hatval = purrr::map(output, "estimates")) %>%
  select(-output, -data) %>%
  dplyr::group_by(predator, season) %>%
  dplyr::mutate(delta_aic = aic - min(aic))


# check if any hatvals for top models are small
modchecks <- worked %>%
  unnest(cols = c(hatval)) %>%
  ungroup() %>%
  mutate(
    ranef = ifelse(test = (covariate %in% c("epsilon", "omega") & estimate != 1),
                   yes = TRUE,
                   no = FALSE),
    ranef_ok = ifelse(test = ranef == TRUE, 
                      yes = abs(estimate) > 0.001,
                      no = NA)
  ) %>%
  group_by(predator, season, model, covars, use_aniso, aic, delta_aic, converged) %>%
  summarize(
    ranef_ok = all(ranef_ok, na.rm = TRUE),
    ranef_n = sum(ranef)
  ) %>%
  ungroup() 

# Write output for next phase of model selection
topmods <- modchecks %>%
  filter(delta_aic < 2,
         ranef_ok == TRUE) %>%
  group_by(predator, season) %>%
  top_n(-1, wt = ranef_n) %>%
  top_n(-1, wt = use_aniso) %>%
  ungroup()

# Get data, config file etc.
topmod_data <- dietrun %>%
  dplyr::mutate(
    model = basename(config_file_loc), 
    model = gsub(".R", "", model)
  ) %>%
  semi_join(topmods, by = c("predator", "season", "model", "use_aniso"))


badmod_data <- dietrun %>%
  dplyr::mutate(
    model = basename(config_file_loc), 
    model = gsub(".R", "", model)
  ) %>%
  inner_join(failed, by = c("predator", "season", "model", "use_aniso"))
write_rds(badmod_data, here("output", "bad_st_diet.rds"))



# Trawl Models --------------------------------------------------------------
trawlrun <- read_rds(here("output", "top_final_trawl.rds"))

allruns <- trawlrun %>% 
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
    converged = purrr::map_chr(output, "converged"), # Some errors were passed (non numerical argument)
    converged = ifelse(converged %in% c("TRUE", "FALSE"), converged, NA) 
  ) %>%
  dplyr::mutate(
    aic = purrr::map(output, "aic"),
    aic = na_if(aic, "NULL")
  ) %>%
  unnest(cols = c("aic")) 

# Error messages get caught before going to output$error
failed <- allruns %>%
  filter(is.na(converged) | converged == FALSE)

# Process models without errors
worked <- allruns %>%
  # filter(converged == TRUE) %>%
  mutate(hatval = purrr::map(output, "estimates")) %>%
  select(-output, -data) %>%
  dplyr::group_by(species, season) %>%
  dplyr::mutate(delta_aic = aic - min(aic))


# check if any hatvals for top models are small
modchecks <- worked %>%
  unnest(cols = c(hatval)) %>%
  ungroup() %>%
  mutate(
    ranef = ifelse(test = (covariate %in% c("epsilon", "omega") & estimate != 1),
                   yes = TRUE,
                   no = FALSE),
    ranef_ok = ifelse(test = ranef == TRUE, 
                      yes = abs(estimate) > 0.001,
                      no = NA)
  ) %>%
  group_by(species, season, model, covars, use_aniso, aic, delta_aic) %>%
  summarize(
    ranef_ok = all(ranef_ok, na.rm = TRUE),
    ranef_n = sum(ranef)
  ) %>%
  ungroup() 

# Write output for next phase of model selection
topmods <- modchecks %>%
  filter(delta_aic < 2,
         ranef_ok == TRUE) %>%
  group_by(species, season) %>%
  top_n(-1, wt = ranef_n) %>%
  top_n(-1, wt = use_aniso) %>%
  ungroup()

# Get data, config file etc.
topmod_data <- trawlrun %>%
  dplyr::mutate(
    model = basename(config_file_loc), 
    model = gsub(".R", "", model)
  ) %>%
  semi_join(topmods, by = c("species", "season", "model", "use_aniso"))


