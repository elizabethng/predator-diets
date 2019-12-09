# Get a rough estiamte of overlap by re-aligning knot locations
# using post hoc spatial grid

# Steps
# 1. Generate a grid for aggregated
# 2. Assign each observation to a grid cell
# 3. Using grid cell index only,
#    a. Average density within cells (for each year/season/species)
#    b. Normalize density (fro each year/season/species)
#    c. Filter out prey data
#    d. Join prey data to predator data by cell/year/season/species
#    e. Calculate overlap metric
# 4. Calculate annual index of overlap (average accross space)
# 5. Calcualte annualy averaged spatial index (average accross years)
# 6. Join back in spatial references for plotting (get raw space-time plots)


library("rnaturalearth")
library("tidyverse")
library("here")
library("sf")


# 0. Load data ------------------------------------------------------------


trawlmods <- readr::read_rds(here::here("output", "top_trawl.rds"))

# Extract knot-level data
knotdat <- trawlmods %>%
  dplyr::select(season, species, output) %>%
  dplyr::transmute(knotdat = purrr::map(output, "knot_density")) %>%
  tidyr::unnest(cols = c(knotdat)) %>%
  dplyr::ungroup()



# 1. Generate grid ----------------------------------
spatialdat <- knotdat %>%
  filter(
    !is.na(Lat),
    !is.na(Lon)
  ) %>%
  select(-c(knot, E_km, N_km, Include, density_log, exclude_reason)) %>%
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326) 

grid <- spatialdat %>%
  st_combine() %>%
  st_convex_hull() %>%
  st_make_grid(n = c(30, 30), square = FALSE)

grid <- st_sf(id = 1:length(grid), geometry = grid)



# 2. Assign each observation to grid cell --------------------------------------------------------

grid_dat <- st_join(grid, spatialdat, join = st_contains)


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
  summarize(tot_bhat = sum(bhat)) %>%
  rename("overlap metric" = tot_bhat)

ggplot(annualindex, aes(x = year, y = `overlap metric`, color = season)) +
  geom_point() +
  geom_line() +
  facet_wrap(~pred) +
  theme_bw()
ggsave(here("output", "plots", "overlap-index-ts.pdf"),
       width = 9, height = 5, units = "in")
write_rds(annualindex, path = here::here("output", "index_overlap.rds"))


# 5. Calculate anually averaged spatial -----------------------------------

northamerica <- ne_countries(continent = "north america",
                      scale = "medium",
                      returnclass = "sf")
# states <- ne_states(country = "united states of america", returnclass = "sf")

annualavg <- bhat %>%
  group_by(id, pred, season) %>%
  summarize(tot_bhat = mean(bhat))

overlapmap <- left_join(grid, annualavg, by = "id") %>% 
  drop_na() 


ggplot() +
  geom_sf(data = overlapmap, aes(fill = tot_bhat, color = tot_bhat), lwd = 0) +
  facet_grid(season ~ pred) +
  scale_fill_viridis_c(
    option = "inferno",
    name = "overlap metric"
  ) +
  scale_color_viridis_c(
    option = "inferno",
    name = "overlap metric"
  ) +
  geom_sf(data = northamerica, color = "white", fill = "grey", inherit.aes = FALSE) +
  coord_sf(xlim = c(-79.5, -65.5), ylim = c(32.5, 45.5)) +
  theme(panel.grid.major = element_line(color = "white"),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
ggsave(here("output", "plots", "overlap-map-avg.pdf"), width = 9, height = 5, units = "in")

# 6. Join back to spatial -------------------------------------------------

bhat_spat <- left_join(grid, bhat, by = "id")
label_dat <- as.data.frame(bhat_spat) %>%
  select(season, year, pred) %>%
  distinct() %>%
  na.omit()

bhat_spat %>%
  filter(
    pred == "goosefish",
    season == "spring") %>%
  ggplot() +
  geom_sf(aes(fill = bhat, color = bhat)) +
  scale_fill_viridis_c(
    option = "inferno",
    name = "overlap metric"
  ) +
  scale_color_viridis_c(
    option = "inferno",
    name = "overlap metric"
  ) +
  facet_wrap(~year) +
  geom_text(data = label_dat, aes(label = year), x = -69.5, y = 33, color = "grey", inherit.aes = FALSE) + 
  geom_sf(data = northamerica, color = "white", fill = "grey", inherit.aes = FALSE) +
  coord_sf(xlim = c(-79.5, -65.5), ylim = c(32.5, 45.5)) +
  theme(panel.grid.major = element_line(color = "white"),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text.x = element_blank(), # controls facets
        strip.text.y = element_blank(),
        strip.background = element_blank())
ggsave(here("output", "plots", "overlap-map-ts-goosefish-spring.pdf"), width = 12, height = 10, units = "in")
