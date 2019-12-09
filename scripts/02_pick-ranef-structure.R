# Script to check aic and random effects magnitude for 
# random effect structure model selection fit with REML. 
# Not yet implemented for trawl output. 

# Save results for use by next script to do model 
# selction for covaraites.

library("tidyverse")
library("here")


# Diet Models ---------------------------------------------------------------

dietrun <- read_rds(here("output", "st_sel_diet.rds"))

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
  dplyr::select(
    -contains("_"),
    -c(errors, worked)) %>%
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

# Check magnitude of effects
worked <- allruns %>%
  filter(converged == TRUE) %>%
  mutate(hatval = purrr::map(output, "estimates")) %>%
  select(-output, -data)


# Create AIC table with a check that variances are ok
aictabs <- worked %>%
  dplyr::group_by(species, season) %>%
  dplyr::arrange(aic) %>%
  dplyr::mutate(delta_aic = round(aic - min(aic), 1))


# check if any hatvals are less than 0.001
modchecks <- aictabs %>%
  filter(delta_aic < 2) %>%
  unnest(cols = c(hatval)) %>%
  ungroup() %>%
  filter(
    covariate %in% c("epsilon", "omega"),
    estimate != 1  # default value = 1 when no ranef is estimated
  ) %>%
  mutate(
    estimate = abs(estimate),
    ranef_ok = estimate > 0.001) %>%
  group_by(species, season, model) %>%
  mutate(ranef_ok = all(ranef_ok)) %>%
  nest(hatval = c(covariate, predictor, estimate))
  
# Write output for next phase of model selection
topmods <- modchecks %>%
  filter(ranef_ok == TRUE) %>%
  mutate(nparm = map_int(hatval, nrow)) %>%
  ungroup() %>%
  group_by(species, season) %>%
  top_n(-1, wt = nparm) %>%
  ungroup()

# Get data, config file etc.
topmod_data <- dietrun %>%
  dplyr::mutate(
    model = basename(config_file_loc), 
    model = gsub(".R", "", model)
  ) %>%
  semi_join(topmods, by = c("species", "season", "model"))
write_rds(topmod_data, here("output", "top_st_diet.rds"))



# Trawl data --------------------------------------------------------------

trawlrun <- read_rds(here("output", "st_sel_trawl.rds"))

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
  dplyr::select(
    -contains("_"),
    -c(errors, worked)) %>%
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

# Check magnitude of effects
worked <- allruns %>%
  filter(converged == TRUE) %>%
  mutate(hatval = purrr::map(output, "estimates")) %>%
  select(-output, -data)


# Create AIC table with a check that variances are ok
aictabs <- worked %>%
  dplyr::group_by(species, season) %>%
  dplyr::arrange(aic) %>%
  dplyr::mutate(delta_aic = round(aic - min(aic), 1))


# check if any hatvals are less than 0.001
modchecks <- aictabs %>%
  filter(delta_aic < 2) %>%
  unnest(cols = c(hatval)) %>%
  ungroup() %>%
  filter(
    covariate %in% c("epsilon", "omega"),
    estimate != 1  # default value = 1 when no ranef is estimated
  ) %>%
  mutate(
    estimate = abs(estimate),
    ranef_ok = estimate > 0.001) %>%
  group_by(species, season, model) %>%
  mutate(ranef_ok = all(ranef_ok)) %>%
  nest(hatval = c(covariate, predictor, estimate))

# Write output for next phase of model selection
topmods <- modchecks %>%
  filter(ranef_ok == TRUE) %>%
  mutate(nparm = map_int(hatval, nrow)) %>%
  ungroup() %>%
  group_by(species, season) %>%
  top_n(-1, wt = nparm) %>%
  ungroup()

# Get data, config file etc.
topmod_data <- trawlrun %>%
  dplyr::mutate(
    model = basename(config_file_loc), 
    model = gsub(".R", "", model)
  ) %>%
  semi_join(topmods, by = c("species", "season", "model"))
write_rds(topmod_data, here("output", "top_st_trawl.rds"))

  