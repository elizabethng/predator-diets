# Script to process VAST output
# 1. Save failed model run info as .csv
# 2. Make AIC tables for models that ran
# 3. Save top model output as .rds

library(tidyverse)

# Load the results
modruns <- readr::read_rds(here::here("output", "raw_diet.rds"))

dietruns <- modruns %>%
  rename(species = pdcomnam, 
         season = myseason) %>%
  mutate(
    species = tolower(species),
    season = tolower(season)
  )
  
# Which models failed?
failed <- dietruns %>% 
  dplyr::mutate(errors = purrr::map(output,"error")) %>% 
  dplyr::mutate(worked = purrr::map_lgl(errors, is.null)) %>% 
  dplyr::filter(!worked) %>%
  dplyr::mutate(errors = purrr::map_chr(errors, "message")) %>%
  dplyr::mutate(
    model = basename(config_file_loc),
    model = gsub(".R", "", model)
  ) %>%
  dplyr::mutate(covars = purrr::map_chr(covar_columns, ~sub(" ", ", ", .x))) %>% # neaten up covariates
  dplyr::select(
    -contains("_"),
    -output,
    -data
  )
write_csv(failed, here::here("output", "failed_diet.csv"))
# All the white hake spring, but that makes sense because there was very little data for those runs



# Format the models that ran
worked <- dietruns %>% 
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


# Create aic tables for species
aictabs <- worked %>%
  dplyr::select(
    -output,
    -data
  ) %>%
  dplyr::group_by(species, season) %>%
  dplyr::arrange(aic) %>%
  dplyr::mutate(delta_aic = round(aic - min(aic), 1))

# Save output
for(species_ in unique(aictabs$species)){
  for(season_ in unique(aictabs$season)){
    aictabs %>%
      dplyr::filter(species == species_ & season == season_) %>%
      readr::write_csv(here::here("output", "aic", paste(species_, season_, "aic.csv", sep = "_")))
  }
}


# Pull out top models for for making index plots
topmods <- worked %>%
  dplyr::group_by(species, season) %>%
  dplyr::top_n(n = -1, wt = aic)
readr::write_rds(topmods, here::here("output", "top_diet.rds"))


