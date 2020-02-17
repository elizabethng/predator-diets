# Make map of diet index using discretized space and quantile color scale
# Based on hex grid calculation for overlap

# Outline
# 0. Load diet index, north america map, and average across years
# 1. Generate hex grid to use for discretization
# 2. Assign observations at fine-scale level to grid cells
# 3. Find average density for each cell, normalize within species, join back to grid
# 4. Find pretty breaks for relative index scale and make map

library("rnaturalearth")
library("tidyverse")
library("here")
library("sf")


# 0. Load data ------------------------------------------------------------
topdiets <- readr::read_rds(here::here("output", "top_final_diet.rds"))

plot_dietindex <- topdiets %>%
  dplyr::select(season, predator, output) %>%
  dplyr::mutate(output = purrr::map(output, "result")) %>%
  dplyr::mutate(output = purrr::map(output, "index")) %>%
  unnest(cols = c(output)) %>%
  select(-Unit, -Fleet, -SD_log) %>%
  rename(year = Year,
         density = Estimate_metric_tons,
         density_se = SD_mt) %>%
  mutate(density = ifelse(is.na(exclude_reason), density, NA),
         density_se = ifelse(is.na(exclude_reason), density_se, NA)) %>%
  select(-exclude_reason) %>%
  rename(
    Year = year,
    Season = season,
    Density = density
  ) %>%
  mutate(
    predator = str_to_sentence(predator), 
    Season = str_to_sentence(Season)
  )

northamerica <- ne_countries(continent = "north america",
                             scale = "medium",
                             returnclass = "sf")

locations <- topdiets$output[[1]]$result$knot_density[, c("Lon", "Lat")] %>%
  rename(lon = Lon, lat = Lat)

knot_diets <- topdiets %>%
  dplyr::select(season, predator, output) %>%
  dplyr::mutate(output = purrr::map(output, "result")) %>%
  dplyr::mutate(output = purrr::map(output, "knot_density"))

# Average over year
average_diet <- knot_diets %>%
  mutate(density = purrr::map(output, ~ select(.x, starts_with("density_")))) %>%
  select(-output) %>%
  mutate(average_spatial = pmap(list(density), rowSums)) %>% 
  mutate(average_spatial = pmap(list(average_spatial), enframe)) %>%
  mutate(average_spatial = pmap(list(average_spatial), function(x) bind_cols(locations, x))) %>%
  unnest(cols = c(average_spatial)) %>%
  rename(density_avg = value) %>%
  select(-name, -density) %>%
  group_by(season, predator) %>%
  # mutate(density_avg_z = scale(density_avg)[,1]) %>%
  ungroup()

finescale_locs <- st_as_sf(average_diet, coords = c("lon", "lat"), crs = 4326) %>%
  mutate(predator = str_to_sentence(predator),
         season = str_to_sentence(season))

# 1. Generate grid ----------------------------------
spatialdat <- st_as_sf(locations, coords = c("lon", "lat"), crs = 4326) 

grid <- spatialdat %>%
  st_combine() %>%
  st_convex_hull() %>%
  st_make_grid(n = c(50, 50), square = FALSE)

grid <- st_sf(id = 1:length(grid), geometry = grid)



# 2. Assign each observation to grid cell --------------------------------------------------------

grid_dat <- st_join(grid, finescale_locs, join = st_contains)


# 3a. Average density within cells ----------------------------------------
# Average density within cells (for each year/season/species)

# Using grid cell data only
nonspat <- as.data.frame(grid_dat) %>%
  select(-geometry) %>%
  group_by(id, predator, season) %>%
  summarize(mean_density = mean(density_avg))



# 3b. Normalize density ---------------------------------------------------
# Normalize density (for each year/season/species)
nonspat <- ungroup(nonspat) %>%
  group_by(predator, season) %>%
  mutate(scaled_density = scale(mean_density)[,1])

densitymap <- left_join(grid, nonspat, by = "id") %>% 
  drop_na() 



# 4. Map with pretty breaks ------------------------------------------------------
# Maps based on https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/#more-intuitive-legend
# Make pretty breaks for (normalized) relative density based roughly on quantiles

pretty_breaks <- c(-1, 0, 1, 2, 5)

# Justification: about 85% of the data are below 1,
# about 95% are below 2
# and about 99% are below 5
sum(densitymap$scaled_density<1)/length(densitymap$scaled_density)
sum(densitymap$scaled_density<2)/length(densitymap$scaled_density)
sum(densitymap$scaled_density<5)/length(densitymap$scaled_density)

# find the extremes
minVal <- min(densitymap$scaled_density, na.rm = T)
maxVal <- max(densitymap$scaled_density, na.rm = T)
# compute labels
labels <- c()
brks <- c(minVal, pretty_breaks, maxVal)
# round the labels (actually, only the extremes)
for(idx in 1:length(brks)){
  labels <- c(labels,round(brks[idx + 1], 2))
}

labels <- labels[1:length(labels)-1] # get rid of Na
# define a new variable on the data set just as above
densitymap$brks <- cut(densitymap$scaled_density, 
                     breaks = brks, 
                     include.lowest = TRUE, 
                     labels = labels)

brks_scale <- levels(densitymap$brks)
# labels_scale <- rev(brks_scale)

q <- ggplot() +
  geom_sf(data = densitymap, aes(fill = brks, color = brks), lwd = 0) +
  facet_grid(season ~ predator) +
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
        strip.background = element_blank()) +
  scale_fill_manual(
    values = viridis::viridis(6),
    breaks = brks_scale,
    name = "Diet index",
    drop = FALSE,
    guide = guide_legend(
      reverse = TRUE,
      keyheight = unit(70*rev(diff(brks))/sum(diff(brks)), units = "mm"),
      keywidth = unit(2, units = "mm"),
      label.vjust = 1
    )
  ) +
  scale_color_manual(
    values = viridis::viridis(6),
    breaks = brks_scale,
    name = "Diet index",
    drop = FALSE,
    guide = guide_legend(
      reverse = TRUE,
      keyheight = unit(70*rev(diff(brks))/sum(diff(brks)), units = "mm"), # key height prop to distance between values
      keywidth = unit(2, units = "mm"),
      label.vjust = 1
    )
  )
q

ggsave(plot = q, 
       filename = here("output", "plots", "diet-map-quantile.pdf"), 
       width = 9, height = 5, units = "in")
