# Check aic and random effects magnitude and compare REML vs non REML
# Code cobbled together from process-vast-output and check-st-effects.

# REML models
worked <- dietrun %>% 
  dplyr::mutate(
    errors = purrr::map(output,"error"), 
    worked = purrr::map_lgl(errors, is.null)
  ) %>% 
  dplyr::filter(worked) %>% 
  dplyr::mutate(
    output = purrr::map(output, "result"),
    model = basename(config_file_loc), 
    model = gsub(".R", "", model),
    covars = purrr::map_chr(covar_columns, ~sub(" ", ", ", .x))) %>%
  dplyr::select(
    -contains("_"),
    -c(errors, worked)) %>%
  dplyr::mutate(aic = purrr::map_dbl(output, "aic"))

# Check convergence
worked %>%
  select(season, species, output) %>%
  transmute(converged = purrr::map_lgl(output, "converged"))


# Check magnitude of effects
worked %>%
  select(season, species, model, output, aic) %>%
  mutate(hatval = purrr::map(output, "estimates")) %>%
  select(-output) %>%
  unnest(cols = c(hatval)) %>%
  ungroup() %>%
  filter(
    covariate %in% c("epsilon", "omega"),
    estimate != 1  # default value = 1 when no ranef is estimated
    ) %>%
  mutate(
    estimate = abs(estimate),
    check = estimate > 0.001)

# Create AIC tab with a check that variances are ok
# [sample code]
aictabs <- worked %>%
  dplyr::select(
    -output,
    -data
  ) %>%
  dplyr::group_by(species, season) %>%
  dplyr::arrange(aic) %>%
  dplyr::mutate(delta_aic = round(aic - min(aic), 1))
