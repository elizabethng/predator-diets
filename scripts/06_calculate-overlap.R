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

trawlmods <- readr::read_rds(here::here("output", "top_final_trawl.rds"))

# Extract years to exclude (shouldn't be many in trawl data)
exclude_years <- trawlmods %>%
  dplyr::select(season, species, output) %>%
  dplyr::mutate(output = purrr::map(output, "result")) %>%
  dplyr::mutate(output = purrr::map(output, "index")) %>%
  unnest(cols = c(output)) %>%
  select(season, species, Year, exclude_reason) %>%
  rename(year = Year)

# Extract location level densities
locdat <- trawlmods %>%
  dplyr::select(season, species, output) %>%
  dplyr::mutate(output = purrr::map(output, "result")) %>% 
  dplyr::mutate(output = purrr::map(output, "knot_density"))

# Check that locations are the same
compdata <- locdat %>%
  mutate(locations = purrr::map(output, `[`, c("Lat", "Lon", "x2i"))) %>%
  select(locations)
for(i in 1:9){
  print(all_equal(compdata[i,][[1]], compdata[i+1,][[1]]))
}


# 1. Calculate overlap metric ---------------------------------------------


# Get just the density data
densdat <- locdat %>%
  mutate(density = purrr::map(output, ~ select(.x, starts_with("density_")))) %>%
  select(-output)
  # unnest(cols = c(density)) %>%
  # group_by(season, species) %>%
  # nest()

# Normalize the densities in each year
normalize <- function(x){x/sum(x, na.rm = TRUE)}

normdat <- densdat %>%
  mutate(density = purrr::map(density, ~ map_df(.x, normalize)))

# Pull out atlantic herring data
preydat <- filter(normdat, species == "atlantic herring")
preddat <- filter(normdat, species != "atlantic herring")

widedat <- left_join(preddat, preydat, by = "season") %>%
  rename(predator = species.x,
         preddens = density.x,
         preydens = density.y) %>%
  select(-species.y) %>%
  mutate(bhat = pmap(list(preddens, preydens), `*`)) %>%
  mutate(bhat = pmap(list(bhat), sqrt)) %>%
  select(-preddens, -preydens)

# Quick check 
t1 = filter(preydat, season == "spring")$density[[1]]
t2 = filter(preddat, season == "spring", species == "atlantic cod")$density[[1]]
tres = sqrt(t1*t2)
all(tres == filter(widedat, season == "spring", predator == "atlantic cod")$bhat[[1]])

# Annual Index: column sums
results <- widedat %>%
  mutate(annual_index = pmap(list(bhat), colSums)) %>%
  mutate(average_spatial = pmap(list(bhat), rowSums))



# 2. Spatially-explicit time series -----------------------------------------------
locations <- locdat$output[[1]][, c("Lon", "Lat")] %>%
  rename(lon = Lon, lat = Lat)

stoverlap <- results %>%
  select(season, predator, bhat) %>% 
  mutate(bhat = pmap(list(bhat), function(x) bind_cols(locations, x))) %>%
  unnest(cols = c(bhat)) %>%
  pivot_longer(cols = starts_with("density_"), names_to = "year", values_to = "bhat") %>%
  mutate(year = gsub("density_", "", year),
         year = as.numeric(year))

stoverlap_spatial <- st_as_sf(stoverlap, coords = c("lon", "lat"), crs = 4326)

label_dat <- stoverlap %>%
  select(year) %>%
  distinct() %>%
  na.omit()

northamerica <- ne_countries(continent = "north america",
                             scale = "medium",
                             returnclass = "sf")

if(TRUE){
  # Make for all species for reference
  predators <- unique(stoverlap$predator)
  seasons <- unique(stoverlap$season)
  
  
  for(preds in predators){
    for(seas in seasons){
      
      p <- stoverlap_spatial %>%
        filter(
          predator == preds,
          season == seas) %>%
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
      ggsave(plot = p, 
             filename = here("output", "plots", "overlap-ts",
                             paste0(gsub(" ", "-", paste(preds, seas)), ".pdf")),
             width = 12, height = 10, units = "in")
    }
  }
  
}

# 3. Annual Index ---------------------------------------------------------
annualindex <- results %>%
  select(season, predator, annual_index) %>%
  mutate(annual_index = pmap(list(annual_index), enframe)) %>%
  unnest(cols = c(annual_index)) %>%
  rename(year = name, bhat = value) %>%
  mutate(year = gsub("density_", "", year),
         year = as.numeric(year)) %>%
  rename("overlap metric" = bhat)

ggplot(annualindex, aes(x = year, y = `overlap metric`, color = season)) +
  geom_point() +
  geom_line() +
  facet_wrap(~predator) +
  theme_bw()
ggsave(here("output", "plots", "overlap-index-ts.pdf"),
       width = 9, height = 5, units = "in")
write_rds(annualindex, path = here::here("output", "index_overlap.rds"))



# 4. Annually-averaged overlap ----------------------------------------------

averagespatial <- results %>%
  select(season, predator, average_spatial) %>% 
  mutate(average_spatial = pmap(list(average_spatial), enframe)) %>%
  mutate(average_spatial = pmap(list(average_spatial), function(x) bind_cols(locations, x))) %>%
  unnest(cols = c(average_spatial)) %>%
  rename(bhat = value) %>%
  select(-name)

# ggplot(averagespatial, aes(x = lon, y = lat, color = bhat)) +
#   geom_point() +
#   scale_color_viridis_c(option = "inferno") +
#   facet_grid(season ~ predator)



spatial_spatial <- st_as_sf(averagespatial, coords = c("lon", "lat"), crs = 4326)

ggplot() +
  geom_sf(data = spatial_spatial, aes(fill = bhat, color = bhat)) +
  facet_grid(season ~ predator) +
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
  select(year) %>%
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

# Make for all species for reference
predators <- c("atlantic cod", "goosefish", "silver hake", "spiny dogfish")
seasons <- c("spring", "fall")


for(preds in predators){
  for(seas in seasons){
    
    p <- bhat_spat %>%
      filter(
        pred == preds,
        season == seas) %>%
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
    ggsave(plot = p, 
           filename = here("output", "plots", "overlap-ts",
                           paste0(gsub(" ", "-", paste(preds, seas)), ".pdf")),
           width = 12, height = 10, units = "in")
  }
}
