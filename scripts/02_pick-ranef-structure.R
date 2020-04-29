# Script to check aic and random effects magnitude for 
# random effect structure model selection fit with REML. 

# Save results for use by next script to do model 
# selction for covaraites.

library("tidyverse")
library("here")

add_identifiability_check = FALSE # Add model results for mannually-checked convergence

# Diet Models ---------------------------------------------------------------

dietrun <- read_rds(here("output", "select_st_diet.rds"))

allruns <- dietrun %>% 
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
  select(-output, -data) 

if(add_identifiability_check == TRUE){
  # Load convergence-checked models
  checkrun <- read_rds(here("output", "select_st_diet_fix.rds")) %>%
    filter(identifiable) %>%
    select(-identifiable) %>% 
    dplyr::mutate(
      model = basename(config_file_loc), 
      model = gsub(".R", "", model),
      covars = purrr::map_chr(covar_columns, ~ gsub(" ", ", ", .x))
    ) %>%
    dplyr::select(-c(processed_data,
                     covar_columns, 
                     config_file_loc,
                     run_name,
                     output_file_loc)) %>%
    dplyr::mutate(
      aic = purrr::map(output, "aic"),
      aic = na_if(aic, "NULL")
    ) %>%
    unnest(cols = c("aic")) %>%
    mutate(hatval = purrr::map(output, "estimates")) %>%
    select(-output, -data) 
  
  worked <- select(worked, -converged) %>%
    bind_rows(checkrun)
}


# check if any hatvals for top models are small
modchecks <- worked %>%
  dplyr::group_by(predator, season) %>%
  dplyr::mutate(delta_aic = aic - min(aic)) %>%
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
  group_by(predator, season, model, covars, use_aniso, aic, delta_aic) %>% # converged
  summarize(
    ranef_ok = all(ranef_ok, na.rm = TRUE),
    ranef_n = sum(ranef)
  ) %>%
  ungroup() 

# Add in non-identifiable models and save AIC table
if(add_identifiability_check == TRUE){
  read_rds(here("output", "select_st_diet_fix.rds")) %>%
    filter(!identifiable) %>%
    dplyr::mutate(
      model = basename(config_file_loc), 
      model = gsub(".R", "", model),
      covars = purrr::map_chr(covar_columns, ~ gsub(" ", ", ", .x))
    ) %>%
    select(predator, season, model, covars, use_aniso, identifiable) %>%
    bind_rows(modchecks) %>%
    mutate(identifiable = ifelse(is.na(identifiable), TRUE, identifiable)) %>%
    write_csv(path = here("output", "aic_st_diet.csv"))
}

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
write_rds(topmod_data, here("output", "top_st_diet.rds"))


# Final AIC table
# topmod_data %>% 
#   mutate(n_tows = map_int(processed_data, nrow),
#          perc_tows_w_herring = map(processed_data, "Catch_KG"),
#          perc_tows_w_herring = map_int(perc_tows_w_herring,
#                                        ~ sum(.x > 0, na.rm = TRUE)),
#          perc_tows_w_herring = 100*perc_tows_w_herring/n_tows) %>%
#   select(predator, season, model, use_aniso, perc_tows_w_herring, n_tows) %>%
#   write_csv(here("output", "aic_top_st_diet.csv"))

badmod_data <- dietrun %>%
  dplyr::mutate(
    model = basename(config_file_loc), 
    model = gsub(".R", "", model)
  ) %>%
  inner_join(failed, by = c("predator", "season", "model", "use_aniso"))
write_rds(badmod_data, here("output", "bad_st_diet.rds"))


# Full AIC table
full_diet_aic <- allruns %>%
  dplyr::mutate(
    aic = purrr::map(output, "aic"),
    aic = na_if(aic, "NULL")
  ) %>%
  unnest(cols = c("aic")) %>%
  mutate(hatval = purrr::map(output, "estimates")) %>%
  select(-output, -data, -hatval) %>%
  dplyr::group_by(predator, season) %>%
  dplyr::mutate(delta_aic = aic - min(aic, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(predator = str_to_sentence(predator),
         season = str_to_sentence(season)) %>%
  rename_all(str_to_title)

write_csv(full_diet_aic, here("output", "aic_top_st_diet.csv"))

# Trawl Models --------------------------------------------------------------

trawlrun <- read_rds(here("output", "select_st_trawl.rds"))

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
  filter(converged == TRUE) %>%
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
write_rds(topmod_data, here("output", "top_st_trawl.rds"))

# Full AIC table
full_trawl_aic <- allruns %>%
  dplyr::mutate(
    aic = purrr::map(output, "aic"),
    aic = na_if(aic, "NULL")
  ) %>%
  unnest(cols = c("aic")) %>%
  mutate(hatval = purrr::map(output, "estimates")) %>%
  select(-output, -data, -hatval) %>%
  dplyr::group_by(species, season) %>%
  dplyr::mutate(delta_aic = aic - min(aic, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(species = str_to_sentence(species),
         season = str_to_sentence(season)) %>%
  rename_all(str_to_title)

write_csv(full_trawl_aic, here("output", "aic_top_st_trawl.csv"))

  