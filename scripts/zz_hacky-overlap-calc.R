# Get a rough estiamte of overlap by re-aligning knot locations
# using post hoc knn

library("tidyverse")
library("here")

trawlmods <- readr::read_rds(here::here("output", "top_trawl.rds"))

# Extract and consolidate knot-level data (only need for each knot)
knotdat <- trawlmods %>%
  dplyr::select(season, species, output) %>%
  dplyr::transmute(knotdat = purrr::map(output, "knot_density")) %>%
  tidyr::unnest(cols = c(knotdat)) %>%
  dplyr::ungroup()

# Small example
smalldat <- trawlmods %>%
  filter(season == "spring") %>%
  filter(species %in% c("atlantic herring", "spiny dogfish")) %>%
  dplyr::select(season, species, output) %>%
  dplyr::transmute(knotdat = purrr::map(output, "knot_density")) %>%
  tidyr::unnest(cols = c(knotdat)) %>%
  dplyr::ungroup()
  
points <- select(smalldat, E_km, N_km) %>%
  stats::kmeans(centers = 100)

# Check correspondence
plot(points$centers)
sample_n(smalldat, 50) %>% 
  select(E_km, N_km) %>%
  points(col = "red")

# Add in new knots
smalldat$cluster <- points$cluster

# Bhattacharyya's Coefficient
# sum_{i=1}^{n} sqrt{p_pred_i*p_prey_i}, 
# where p_pred_i and p_pred_i are 
# the proportion of the total number of predator and prey
# in a given area A

smallerdat <- smalldat %>%
  select(species, season, year, Lat, Lon, density, cluster) %>%
  group_by(species, season, year, cluster) %>%
  mutate(
    new_density = mean(density),
    density_scaled = new_density/sum(new_density)
    ) %>%
  select(-density, -new_density)
  

prey <- filter(smallerdat, species == "atlantic herring") %>%
  select(-Lat, -Lon) %>%
  distinct()
pred <- filter(smallerdat, species != "atlantic herring")%>%
  select(-Lat, -Lon) %>%
  distinct()

bhatdat <- left_join(pred, prey, by = c("season", "year", "cluster")) %>%
  rename(
    pred = species.x,
    pred_dens = density_scaled.x,
    prey = species.y,
    prey_dens = density_scaled.y
  ) %>%
  mutate(bhat = sqrt(pred_dens*prey_dens))


