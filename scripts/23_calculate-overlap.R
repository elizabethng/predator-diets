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
    species = gsub(" ", "_", tolower(species)),
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

# Extract and consolidate knot-level data
knotdat <- results %>%
  dplyr::select(season, species, output) %>%
  dplyr::transmute(knotdat = purrr::map(output, "knot_density")) %>%
  tidyr::unnest(cols = c(knotdat)) %>%
  dplyr::ungroup() %>%
  dplyr::select(season, species, year, knot, density) %>%
  dplyr::distinct()
  

# Bhattacharyya's Coefficient
# sum_{i=1}^{n} sqrt{p_pred_i*p_prey_i}, 
# where p_pred_i and p_pred_i are 
# the proportion of the total number of predator and prey
# in a given area A

# Might be easier to do with table joins


prey <- knotdat %>%
  filter(species == "atlantic_herring")

pred <- knotdat %>%
  filter(species != "atlantic_herring")

bhatdat <- left_join(pred, prey, by = c("season", "year", "knot")) %>%
  rename(
    pred = species.x,
    pred_dens = density.x,
    prey = species.y,
    prey_dens = density.y
  ) %>%
  mutate(
    p_pred = pred_dens/(pred_dens + prey_dens),
    p_prey = prey_dens/(pred_dens + prey_dens)
  ) %>%
  mutate(
    bhat = sqrt(p_pred*p_prey)
  )
  
# How correlated is bhat with p_pred and p_prey?  
bhatdat %>%
  filter(season == "spring" & pred == "atlantic_cod") %>%
  select(prey_dens, pred_dens, bhat) %>%
  GGally::ggpairs()
  
  
  
  select(-pred_dens, -prey, -prey_dens, -)
  group_by(season, pred, year)


poop <- knotdat %>%
  mutate(type = ifelse(species == "atlantic_herring", "prey", "predator")) %>%
  pivot_wider(id_cols = c(season, year, knot, species),
              names_from = type,
              values_from = density)



# Only need one density value per knot per species and year
specdat <- knotdat %>%
  pivot_wider(id_cols = c(season, year, knot), names_from = species, values_from = density)


  
  


