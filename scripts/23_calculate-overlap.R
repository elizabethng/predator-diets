# Collecting info to do overlap calculations

library(tidyverse)

trawlmods <- readr::read_rds(path = here::here("TRAWL", "modruns.rds"))

# Filter out any errors and get top models
results <- trawlmods %>% 
  dplyr::rename(
    season = myseason,
    species = pdcomnam
  )  %>%
  dplyr::mutate(
    species = gsub(" ", "-", tolower(species)),
    season = tolower(season)
  ) %>%
  dplyr::mutate(errors = purrr::map(output,"error")) %>% 
  dplyr::mutate(worked = purrr::map_lgl(errors, is.null)) %>% 
  dplyr::filter(worked) %>% 
  dplyr::mutate(output = purrr::map(output, "result")) %>%
  dplyr::mutate(model = basename(config_file_loc)) %>%
  dplyr::mutate(model = str_extract(model, "^(.{8})")) %>%
  dplyr::select(-contains("_")) %>%
  dplyr::select(-c(errors, worked)) %>%
  dplyr::mutate(aic = purrr::map_dbl(output, "aic")) %>%
  dplyr::group_by(season, species) %>%
  dplyr::mutate(delta_aic = round(aic - min(aic), 0)) %>%
  dplyr::filter(delta_aic == 0)
