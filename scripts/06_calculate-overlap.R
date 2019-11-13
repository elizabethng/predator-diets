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
  group_by(season, pred, year) %>%
  mutate(
    # p_pred = pred_dens/(pred_dens + prey_dens),
    # p_prey = prey_dens/(pred_dens + prey_dens)
    p_pred = pred_dens/sum(pred_dens),
    p_prey = prey_dens/sum(prey_dens)
  ) %>%
  mutate(
    bhat = sqrt(p_pred*p_prey)
  )
  
# How correlated is bhat with p_pred and p_prey?  
# bhatdat %>%
#   filter(season == "spring" & pred == "atlantic_cod") %>%
#   select(prey_dens, pred_dens, bhat) %>%
#   GGally::ggpairs()
  
  
# Aggregate by year  
overlapindex <- bhatdat %>%
  group_by(season, pred, year) %>%
  summarize(
    bhat = sum(bhat)
  ) %>%
  ungroup() %>%
  mutate(pred = gsub("_", " ", pred)) %>%
  mutate(name = paste0(pred, ", ", season))
  
# Plot the indices
ggplot(overlapindex, aes(x = year, y = bhat, group = name, color = season)) +
  geom_line() +
  facet_wrap(~pred) + 
  theme_bw()
ggsave(here::here("output", "plots", "overlap-comparison.pdf"),
       width = 9, height = 5, units = "in")

gitdir <- "C:/Users/Elizabeth Ng/Documents/GitHub/predator-diets"
write_rds(overlapindex, path = file.path(gitdir, "output", "overlap_index.rds"))
