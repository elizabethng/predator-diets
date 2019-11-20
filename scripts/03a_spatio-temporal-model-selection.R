# Check aic and random effects magnitude and compare REML vs non REML
# Code cobbled together from process-vast-output and check-st-effects.

library("tidyverse")
library("here")


# REML models
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

failed <- allruns %>%
  filter(is.na(converged) | converged == FALSE) # error messages get caught before going to output$erro
select(failed, -data, -output) # weird that these were all the full st, especially for a few that I thought would fit

# Check magnitude of effects
worked <- allruns %>%
  filter(converged == TRUE) %>%
  mutate(hatval = purrr::map(output, "estimates")) %>%
  select(-output, -data)


covcheck <- worked %>%
  unnest(cols = c(hatval)) %>%
  ungroup() %>%
  filter(
    covariate %in% c("epsilon", "omega"),
    estimate != 1  # default value = 1 when no ranef is estimated
    ) %>%
  mutate(
    estimate = abs(estimate),
    ranef_check = estimate > 0.001)

filter(covcheck, !ranef_check)

# Create AIC tab with a check that variances are ok
# [sample code]
aictabs <- worked %>%
  dplyr::group_by(species, season) %>%
  dplyr::arrange(aic) %>%
  dplyr::mutate(delta_aic = round(aic - min(aic), 1))

filter(aictabs, delta_aic < 2) %>% View("topmods")

# Top models are st in presence only, except for
# - spiny dogfish both seasons support for st in both components (also has the most data)
# - goosefish spring, spatial in presence only, no temporal (1.9 delAIC, so choose simpler)

# check if any hatvals are less than 0.001
topmods <- aictabs %>%
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
  

  
  
  