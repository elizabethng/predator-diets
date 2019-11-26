# Get a rough estiamte of overlap by re-aligning knot locations
# using post hoc knn
# Steps
# 1. Generate a grid for aggregated
# 2. Assign each observation to a grid cell
# 3. Using grid cell index only,
#    a. Average density within cells (for each year/season/species)
#    b. Normalize density (fro each year/season/species)
#    c. Filter out prey data
#    d. Join prey data to predator data by cell/year/season/species
#    e. Calculate overlap metric
# 4. Join back in spatial references for plotting


library("tidyverse")
library("here")
library("sf")


# 0. Load data ------------------------------------------------------------


trawlmods <- readr::read_rds(here::here("output", "top_trawl.rds"))

# Extract and consolidate knot-level data (only need for each knot)
knotdat <- trawlmods %>%
  dplyr::select(season, species, output) %>%
  dplyr::transmute(knotdat = purrr::map(output, "knot_density")) %>%
  tidyr::unnest(cols = c(knotdat)) %>%
  dplyr::ungroup()



# 1. Generate grid ----------------------------------
# Small example
smalldat <- trawlmods %>%
  filter(season == "spring") %>%
  filter(species %in% c("atlantic herring", "spiny dogfish")) %>%
  dplyr::select(season, species, output) %>%
  dplyr::transmute(knotdat = purrr::map(output, "knot_density")) %>%
  tidyr::unnest(cols = c(knotdat)) %>%
  dplyr::ungroup()


# Associate all locations to Atalntic herring locations
# Should do for each species separately?


prey <- filter(smalldat, species == "atlantic herring") %>%
  filter(
    !is.na(Lat),
    !is.na(Lon),
    year == 2000
  ) %>%
  select(-c(knot, E_km, N_km, Include, density_log, exclude_reason)) %>%
  st_as_sf(coords = c("Lat", "Lon"), crs = 4326)


grid <- prey %>%
  st_combine() %>%
  st_convex_hull() %>%
  st_make_grid(n = c(30, 30), square = FALSE)

# plot(grid)
# ggplot() +
#   geom_sf(data = grid) +
#   geom_sf(data = prey)

grid <- st_sf(id = 1:length(grid), geometry = grid)

# Get the prey values that correspond to new polygons 
test2 <- st_join(grid, prey, join = st_contains) 

# Missing values are ones that I don't care about, I can drop them
mutate(test2, missing = is.na(density)) %>%
  ggplot() +
  geom_sf(aes(fill = missing))

test2 %>%
  filter(!is.na(density)) %>%
  group_by(id) %>%
  summarize(mean_density = mean(density)) %>%
  ggplot() +
  geom_sf(aes(fill = mean_density))

# Can drop the unneded hexagons
empty_cells <- test2 %>%
  filter(is.na(density)) %>%
  pull(id)

grid <- filter(grid, !(id %in% empty_cells))
# plot(st_geometry(grid))
  
# so now `grid` is the df with only the polygons that I want. 



# 2. Assign each observation to grid cell --------------------------------------------------------
# To make calculations faster
# randomly take a subsample of knot values
alldat <- knotdat %>%
  filter(!is.na(Lat)) %>%
  select(-c(E_km, N_km, Include, density_log, exclude_reason)) %>%
  group_by(species, season, year, knot) %>%
  sample_n(3)

# Make the data spatial
alldat <- alldat %>% 
  ungroup() %>%
  select(-knot) %>%
  st_as_sf(coords = c("Lat", "Lon"), crs = 4326)

# Join to grid cells
grid_dat <- st_join(grid, alldat, join = st_contains)



# 3a. Average density within cells ----------------------------------------
# Average density within cells (for each year/season/species)

# Using grid cell data only
nonspat <- as.data.frame(grid_dat) %>%
  select(-geometry) %>%
  group_by(id, species, season, year) %>%
  summarize(mean_density = mean(density))



# 3b. Normalize density ---------------------------------------------------
# Normalize density (for each year/season/species)

scaledat <- ungroup(nonspat) %>%
  group_by(species, season, year) %>%
  mutate(scale_density = mean_density/sum(mean_density)) %>%
  select(-mean_density)


# 3c. Split out prey data ------------------------------------------------

preydat <- filter(scaledat, species == "atlantic herring") %>%
  rename(prey = species,
         prey_dens = scale_density)
preddat <- filter(scaledat, species != "atlantic herring") %>%
  rename(pred = species,
         pred_dens = scale_density)


# 3d. Join prey data to predator data -------------------------------------
# Join prey data to predator data by cell/year/season/species

predpreydat <- inner_join(preydat, preddat, by = c("id", "season", "year"))


# 3e. Calculate overlap metric --------------------------------------------
# Should just be able to go row wise

bhat <- ungroup(predpreydat) %>%
  mutate(bhat = sqrt(pred_dens*prey_dens))



# 4. Calculate annual index -----------------------------------------------
# Aggregate over years
annualindex <- bhat %>%
  group_by(pred, season, year) %>%
  summarize(tot_bhat = sum(bhat))

ggplot(annualindex, aes(x = year, y = tot_bhat, color = season)) +
  geom_point() +
  geom_line() +
  facet_wrap(~pred)



# 5. Join back to spatial -------------------------------------------------

bhat_spat <- left_join(grid, bhat, by = "id")

bhat_spat %>%
  filter(
    pred == "atlantic cod",
    year == 1978
    ) %>%
  ggplot() +
  geom_sf(aes(fill = bhat)) +
  facet_wrap(~season)
