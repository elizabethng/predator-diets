# Make map of diet index (DENSITY not area expanded)
# using discretized space and quantile color scale
# Based on hex grid calculation for overlap

# Outline
# 0. Load diet index, north america map, and average across years
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
topdiets <- readr::read_rds(here::here("output", "top_final_diet.rds"))

northamerica <- ne_countries(continent = "north america",
                             scale = "large",
                             returnclass = "sf")

locations <- topdiets$output[[1]]$result$knot_density[, c("Lon", "Lat")] %>%
  rename(lon = Lon, lat = Lat)

knot_diets <- topdiets %>%
  dplyr::select(season, predator, output) %>%
  dplyr::mutate(output = purrr::map(output, "result")) %>%
  dplyr::mutate(output = purrr::map(output, "knot_density"))

finescale_data <- knot_diets %>%
  unnest(output) %>%
  select(-Include) %>%
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


# 4. Make annually-averaged diet index map --------------------------------

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
  facet_grid(season ~ predator, switch = "y") +
  geom_sf(data = northamerica, color = "white", fill = "grey",
          inherit.aes = FALSE, lwd = 0.1) +
  coord_sf(xlim = c(-79.5, -65.5), ylim = c(32.5, 45.5)) +
  theme(panel.grid.major = element_line(color = "white"),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_blank(),
        legend.position = "bottom") +
  scale_fill_manual(
    values = viridis::viridis(6),
    breaks = brks_scale,
    name = "Relative mass of Atlantic herring",
    drop = FALSE,
    guide = guide_legend(
      direction = "horizontal",
      reverse = FALSE,
      keywidth = unit(70*abs(diff(brks))/(maxVal - minVal), units = "mm"), # key height prop to distance between values
      keyheight = unit(2, units = "mm"),
      title.position = "top",
      title.hjust = 0.5,
      label.position = "bottom",
      label.hjust = 1,
      nrow = 1,
      byrow = TRUE
    )
  ) +
  scale_color_manual(
    values = viridis::viridis(6),
    breaks = brks_scale,
    name = "Relative mass of Atlantic herring",
    drop = FALSE,
    guide = guide_legend(
      direction = "horizontal",
      reverse = FALSE,
      keywidth = unit(70*abs(diff(brks))/(maxVal - minVal), units = "mm"), # key height prop to distance between values
      keyheight = unit(2, units = "mm"),
      title.position = "top",
      title.hjust = 0.5,
      label.position = "bottom",
      label.hjust = 1,
      nrow = 1,
      byrow = TRUE
    )
  )
q

ggsave(plot = q, 
       filename = here("output", "plots", "diet-map-quantile.pdf"), 
       width = 170, 
       height = 95, 
       units = "mm")


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
      labels <- c(labels, paste0(round(quantiles[idx], 2), " – ", 
                                 round(quantiles[idx + 1], 2)))
    }
    labels <- labels[1:length(labels)-1] # remove last label
    tmpdat$quantiles <- cut(tmpdat$density_grid, 
                             breaks = quantiles, 
                             labels = labels, 
                             include.lowest = T)
    
    # get year list
    yearlist <- filter(topdiets, predator == tolower(pred) & season == tolower(seas))
    yearlist <- yearlist$output[[1]]$result$index %>% filter(is.na(exclude_reason)) %>% pull(Year)
    
    # Make one plot for each year
    for(yr in yearlist){
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
          values = viridis::viridis(no_classes),
          # breaks = quantiles,
          name = "Diet index",
          drop = FALSE,
          guide = guide_legend(
            reverse = TRUE)
        ) +
        scale_color_manual(
          values = viridis::viridis(no_classes),
          # breaks = quantiles,
          name = "Diet index",
          drop = FALSE,
          guide = guide_legend(
            reverse = TRUE)
        )
      ggsave(plot = tmpplot, 
             filename = here("output", "plots", "diet-map-ts", 
                             paste0(pred, "-", seas, "-", yr, ".png")), 
             width = 3, height = 3, units = "in")
    }
      
  }
}









