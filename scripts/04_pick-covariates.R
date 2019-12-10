# Process and select top models for diet data to get
# final covariate structure to use. Save output to use in next
# step of the analysis.

# Basically a repeat of 02_pick-ranef-structure.R, could eventually wrap function

library("tidyverse")
library("here")


dietrun <- read_rds(here("output", "cov_sel_diet.rds")) %>%
  rename(species = pdcomnam, season = myseason)

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
  filter(!is.na(converged)) %>%
  # filter(converged == TRUE) %>% # None of the spring spiny dogfish converged
  mutate(hatval = purrr::map(output, "estimates")) %>%
  select(-output, -data)


# Create AIC table with a check that variances are ok
aictabs <- worked %>%
  dplyr::group_by(species, season) %>%
  dplyr::mutate(delta_aic = aic - min(aic))


# check if any hatvals are less than 0.001
modchecks <- aictabs %>%
  unnest(cols = c(hatval)) %>%
  ungroup() %>%
  filter(
    covariate %in% c("epsilon", "omega"),
    estimate != 1  # default value = 1 when no ranef is estimated
  ) %>%
  mutate(
    estimate = abs(estimate),
    ranef_ok = estimate > 0.001
    ) %>%
  group_by(species, season, covars) %>%
  mutate(ranef_ok = all(ranef_ok)) %>%
  nest(hatval = c(covariate, predictor, estimate)) %>%
  ungroup()

# Write output for next phase of model selection
topmods <- modchecks %>%
  # filter(ranef_ok == TRUE) %>%
  filter(delta_aic < 2) %>%
  mutate(
    nparm = str_count(covars, ","),
    nparm = replace_na(nparm, 0)
    ) %>% 
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
write_rds(topmod_data, here("output", "top_cov_diet.rds"))
