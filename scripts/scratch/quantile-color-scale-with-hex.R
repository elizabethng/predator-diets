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
  mutate(density_avg_z = scale(density_avg)[,1]) %>%
  ungroup()

plot_average_diet <- st_as_sf(average_diet, coords = c("lon", "lat"), crs = 4326) %>%
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

grid_dat <- st_join(grid, plot_average_diet, join = st_contains)


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


# Plot raw densities ------------------------------------------------------

ggplot() +
  geom_sf(data = densitymap, aes(fill = mean_density, color = mean_density), lwd = 0) +
  facet_grid(season ~ predator) +
  scale_fill_viridis_c(
    option = "inferno",
    name = "diet index"
  ) +
  scale_color_viridis_c(
    option = "inferno",
    name = "diet index"
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




# Plot scaled densities ---------------------------------------------------

ggplot() +
  geom_sf(data = densitymap, aes(fill = scaled_density, color = scaled_density), lwd = 0) +
  facet_grid(season ~ predator) +
  scale_fill_viridis_c(
    option = "viridis",
    name = "relative diet index"
  ) +
  scale_color_viridis_c(
    option = "viridis",
    name = "relative diet index"
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


# Quantile scale with mean density -------------------------------------------------------
no_classes <- 6
labels <- c()

quantiles <- quantile(densitymap$mean_density, 
                      probs = seq(0, 1, length.out = no_classes + 1))

# here I define custom labels (the default ones would be ugly)
labels <- c()
for(idx in 1:length(quantiles)){
  labels <- c(labels, paste0(round(quantiles[idx], 2), 
                             " – ", 
                             round(quantiles[idx + 1], 2)))
}
# I need to remove the last label 
# because that would be something like "66.62 - NA"
labels <- labels[1:length(labels)-1]

# here I actually create a new 
# variable on the dataset with the quantiles
densitymap$density_quantiles <- cut(densitymap$mean_density, 
                                     breaks = quantiles, 
                                     labels = labels, 
                                     include.lowest = T)

ggplot() +
  geom_sf(data = densitymap, aes(fill = density_quantiles, color = density_quantiles), lwd = 0) +
  facet_grid(season ~ predator) +
  viridis::scale_fill_viridis(
    option = "viridis",
    name = "diet index",
    discrete = TRUE
  ) +
  viridis::scale_color_viridis(
    option = "viridis",
    name = "diet index",
    discrete = TRUE
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


# Quantile scale with scaled density -------------------------------------------------------
no_classes <- 6
labels <- c()

quantiles <- quantile(densitymap$scaled_density, 
                      probs = seq(0, 1, length.out = no_classes + 1))

# here I define custom labels (the default ones would be ugly)
labels <- c()
for(idx in 1:length(quantiles)){
  labels <- c(labels, paste0(round(quantiles[idx], 2), 
                             " – ", 
                             round(quantiles[idx + 1], 2)))
}
# I need to remove the last label 
# because that would be something like "66.62 - NA"
labels <- labels[1:length(labels)-1]

# here I actually create a new 
# variable on the dataset with the quantiles
densitymap$density_quantiles <- cut(densitymap$scaled_density, 
                                    breaks = quantiles, 
                                    labels = labels, 
                                    include.lowest = T)

ggplot() +
  geom_sf(data = densitymap, aes(fill = density_quantiles, color = density_quantiles), lwd = 0) +
  facet_grid(season ~ predator) +
  viridis::scale_fill_viridis(
    option = "viridis",
    name = "relative diet index",
    discrete = TRUE
  ) +
  viridis::scale_color_viridis(
    option = "viridis",
    name = "relative diet index",
    discrete = TRUE
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



# Make breaks pretty ------------------------------------------------------
# Make pretty breaks for quantile-scaled relative density??
# quantiles

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
        axis.ticks.y = element_blank()) +
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
