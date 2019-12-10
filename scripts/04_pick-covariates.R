# Process and select top models for diet data to get
# final covariate structure to use. Save output to use in next
# step of the analysis.

# Basically a repeat of 02_pick-ranef-structure.R, could eventually wrap function

library("tidyverse")
library("here")


dietrun <- read_rds(here("output", "select_cov_diet.rds"))

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

# Process models without errors
worked <- allruns %>%
  filter(converged == TRUE) %>%
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
  group_by(predator, season, model, covars, aic, delta_aic) %>%
  summarize(
    ranef_ok = all(ranef_ok, na.rm = TRUE),
    ranef_n = sum(ranef)
  ) %>%
  ungroup() 

# Write output for next phase of model selection
topmods <- modchecks %>%
  filter(delta_aic < 2,
         ranef_ok == TRUE) %>%
  mutate(
    fixef_n = str_count(covars, ","),
    fixef_n = replace_na(fixef_n, 0)
  ) %>% 
  group_by(predator, season) %>%
  top_n(-1, wt = ranef_n) %>%
  ungroup()

# Get data, config file etc.
topmod_data <- dietrun %>%
  mutate(covars = purrr::map_chr(covar_columns, ~ gsub(" ", ", ", .x))) %>%
  semi_join(topmods, by = c("predator", "season", "covars"))
write_rds(topmod_data, here("output", "top_cov_diet.rds"))
