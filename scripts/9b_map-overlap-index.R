# Make map of overlap index using discretized space and quantile color scale
# Based on hex grid calculation for overlap

# Outline
# 0. Load overlap index, north america map, and average across years
# 1. Generate hex grid to use for discretization
# 2. Assign observations at fine-scale level to grid cells
# 3. Average density for each cell (per pred/season/year)
# 4. Plot annually averaged diet index
#    - Average accross years
#    - Find pretty breaks for relative index scale and make map
# 5. Plot time-series for each predator

library("rnaturalearth")
library("tidyverse")
library("here")
library("sf")


# 0. Load data ------------------------------------------------------------
overlap <- readr::read_rds(here("output", "finescale_overlap.rds"))

northamerica <- ne_countries(continent = "north america",
                             scale = "medium",
                             returnclass = "sf")

locations <- overlap$bhat[[1]][, c("Lon", "Lat")] %>%
  rename(lon = Lon, lat = Lat)

knot_diets <- overlap %>%
  dplyr::select(season, predator, bhat)

finescale_data <- knot_diets %>%
  unnest(bhat) %>%
  rename(knot = x2i,
         lat = Lat,
         lon = Lon) %>%
  pivot_longer(cols = starts_with("density_"), names_to = "year", values_to = "density") %>%
  mutate(year = gsub("density_", "", year),
         year = as.numeric(year))

finescale_locs <- finescale_data %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
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


# 3. Average density within cells ----------------------------------------
# Calculate average density within cells (for each year/season/species)
# Using grid cell data only
nonspat <- as.data.frame(grid_dat) %>%
  select(-geometry) %>%
  group_by(id, predator, season, year) %>%
  summarize(density_grid = mean(density))


# 4. Make annually-averaged overlap index map --------------------------------

# 4.1. Get relative average index ---------------------------------------------------
# Average across years then normalize between predators

annual_dat <- nonspat %>%
  ungroup() %>%
  group_by(id, predator, season) %>%
  summarize(density_annual_avg = mean(density_grid, na.rm = TRUE)) %>% # avg by year
  ungroup() %>% 
  group_by(predator, season) %>%
  mutate(scaled_density = scale(density_annual_avg)[,1]) # normalize within species/season

densitymap <- left_join(grid, annual_dat, by = "id") %>% 
  drop_na() 



# 4.2. Map with pretty breaks ------------------------------------------------------
# Maps based on https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/#more-intuitive-legend

# Do a check for new break values
quantile(densitymap$density_annual_avg)
quantile(densitymap$scaled_density, prob = 0.85)

# Make pretty breaks for (normalized) relative density based roughly on quantiles
pretty_breaks <- c(-1, 0, 1, 2, 5)

# Justification: upper values represent quantiles
sum(densitymap$scaled_density<1)/length(densitymap$scaled_density) # 90%
sum(densitymap$scaled_density<2)/length(densitymap$scaled_density) # 95%
sum(densitymap$scaled_density<5)/length(densitymap$scaled_density) # 99%

# find the extremes
minVal <- min(densitymap$scaled_density, na.rm = T)
maxVal <- max(densitymap$scaled_density, na.rm = T)

# compute labels
labels <- c()
brks <- c(minVal, pretty_breaks, maxVal)
for(idx in 1:length(brks)){
  labels <- c(labels,round(brks[idx + 1], 2)) # round the labels (actually, only the extremes)
}
labels <- labels[1:length(labels)-1] # get rid of Na

# define a new variable on the data set just as above
densitymap$brks <- cut(densitymap$scaled_density, 
                       breaks = brks, 
                       include.lowest = TRUE, 
                       labels = labels)

brks_scale <- levels(densitymap$brks)

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
    values = viridis::magma(6),
    breaks = brks_scale,
    name = "Overlap index",
    drop = FALSE,
    guide = guide_legend(
      reverse = TRUE,
      keyheight = unit(70*rev(abs(diff(brks)))/(maxVal - minVal), units = "mm"),
      keywidth = unit(2, units = "mm"),
      label.vjust = 1
    )
  ) +
  scale_color_manual(
    values = viridis::magma(6),
    breaks = brks_scale,
    name = "Overlap index",
    drop = FALSE,
    guide = guide_legend(
      reverse = TRUE,
      keyheight = unit(70*rev(abs(diff(brks)))/(maxVal - minVal), units = "mm"), # key height prop to distance between values
      keywidth = unit(2, units = "mm"),
      label.vjust = 1
    )
  )
q

ggsave(plot = q, 
       filename = here("output", "plots", "overlap-map-quantile.pdf"), 
       width = 9, height = 5, units = "in")


# 5. Plot time series for each predator -----------------------------------
# Make with mostly default plot specs and save as .png or similar for ease of viewing
# Get the spatial data
speciesmap <- left_join(grid, nonspat, by = "id") %>% 
  drop_na() 

for(pred in unique(speciesmap$predator)){
  for(seas in unique(speciesmap$season)){

    tmpdat <- filter(speciesmap, predator == pred & season == seas)
    
    # Make discrete scale based on quantiles
    no_classes <- 6
    
    quantiles <- quantile(tmpdat$density_grid, 
                          probs = seq(0, 1, length.out = no_classes + 1))
    
    # Define custom labels
    labels <- c()
    for(idx in 1:length(quantiles)){
      labels <- c(labels, paste0(signif(quantiles[idx], 2), " – ", 
                                 signif(quantiles[idx + 1], 2)))
    }
    labels <- labels[1:length(labels)-1] # remove last label
    tmpdat$quantiles <- cut(tmpdat$density_grid, 
                            breaks = quantiles, 
                            labels = labels, 
                            include.lowest = T)
    

        # Make one plot for each year
    for(yr in unique(tmpdat$year)){
      tmpplot <- filter(tmpdat, year == yr) %>%
        ggplot() +
        geom_sf(aes(fill = quantiles, color = quantiles), lwd = 0) +
        ggtitle(paste(pred, "-", seas, "-", yr)) +
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
              strip.background = element_blank())  +
        scale_fill_manual(
          values = viridis::magma(no_classes),
          name = "Overlap index",
          drop = FALSE,
          guide = guide_legend(
            reverse = TRUE)
        ) +
        scale_color_manual(
          values = viridis::magma(no_classes),
          name = "Overlap index",
          drop = FALSE,
          guide = guide_legend(
            reverse = TRUE)
        )
      ggsave(plot = tmpplot, 
             filename = here("output", "plots", "overlap-map-ts", 
                             paste0(pred, "-", seas, "-", yr, ".png")), 
             width = 3, height = 3, units = "in")
    }
    
  }
}

