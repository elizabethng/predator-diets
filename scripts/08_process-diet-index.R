# Process diet index data
# 1. Extract and save index
# 2. Make annually-averaged map
# 3. Make predator time-series maps

library("rnaturalearth")
library("tidyverse")
library("here")
library("sf")


# 0. Load data ------------------------------------------------------------
topdiets <- readr::read_rds(here::here("output", "top_final_diet.rds"))


# 1. Extract and save annual index ----------------------------------------
# Note that exclude_reason is in here

dietindex <- topdiets %>%
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
  select(-exclude_reason)
  # mutate(density_cv = ) # hmm I need n, but what is that here??
write_rds(dietindex, path = here("output", "index_diet.rds"))


plot_dietindex <- dietindex %>%
  rename(
    Year = year,
    Season = season,
    Density = density
  ) %>%
  mutate(
    predator = str_to_sentence(predator), 
    Season = str_to_sentence(Season)
  )

p <- ggplot(plot_dietindex, aes(x = Year, y = Density, color = Season)) +
  geom_point() +
  geom_errorbar(aes(ymin = (Density - density_se), 
                    ymax = (Density + density_se), 
                    color = Season),
                width = 0) +
  facet_wrap(~ predator, scales = "free_y") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())
ggsave(plot = p, filename = here("output", "plots", "diet-index-ts.pdf"), width = 10, height = 6, units = "in")

# Compare with outliers removed
plot_dietindex %>%
  filter(!is.na(Density)) %>%
  filter(density_se < 900) %>%
  ggplot(aes(x = Year, y = Density, color = Season)) +
  geom_point() +
  geom_errorbar(aes(ymin = (Density - density_se), 
                    ymax = (Density + density_se), 
                    color = Season),
                width = 0) +
  facet_wrap(~ predator, scales = "free_y") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())


# Annually-averaged map ---------------------------------------------------
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

p <- ggplot() +
  geom_sf(data = plot_average_diet, aes(fill = density_avg, color = density_avg)) +
  facet_grid(season ~ predator) +
  scale_fill_viridis_c(
    option = "inferno",
    name = "Diet index"
  ) +
  scale_color_viridis_c(
    option = "inferno",
    name = "Diet index"
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
        axis.ticks.y = element_blank(),
        strip.background = element_blank())
ggsave(plot = p, filename = here("output", "plots", "diet-map-avg.pdf"), width = 9, height = 5, units = "in")

p2 <- plot_average_diet %>%
  mutate(density_avg = log(density_avg)) %>%
  ggplot() +
  geom_sf(data = plot_average_diet, aes(fill = density_avg, color = density_avg)) +
  facet_grid(season ~ predator) +
  scale_fill_viridis_c(
    option = "inferno",
    name = "log(Density)"
  ) +
  scale_color_viridis_c(
    option = "inferno",
    name = "log(Density)"
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
        axis.ticks.y = element_blank(),
        strip.background = element_blank())
ggsave(plot = p2, filename = here("output", "plots", "diet-map-avg-log.pdf"), width = 9, height = 5, units = "in")

p3 <- ggplot() +
  geom_sf(data = northamerica, color = "white", fill = "grey", inherit.aes = FALSE) +
  geom_sf(data = filter(plot_average_diet, predator == "Goosefish", season == "Fall"),
                        aes(fill = density_avg_z, color = density_avg_z)) +
  facet_grid(season ~ predator) +
  scale_fill_viridis_c(
    # option = "inferno",
    name = "Scaled density"
  ) +
  scale_color_viridis_c(
    # option = "inferno",
    name = "Scaled density"
  ) +
  coord_sf(xlim = c(-79.5, -65.5), ylim = c(32.5, 45.5)) +
  theme(panel.grid.major = element_line(color = "white"),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_blank())
ggsave(plot = p2, filename = here("output", "plots", "diet-map-avg-z.pdf"), width = 9, height = 5, units = "in")

# Diet index timeseries plots ---------------------------------------------

alldietdat <- knot_diets %>%
  unnest(cols = c(output)) %>%
  pivot_longer(cols = starts_with("density_"), names_to = "year", values_to = "density") %>%
  select(-x2i, -Include) %>%
  mutate(year = gsub("density_", "", year),
         year = as.numeric(year))

# Not finished:

if(TRUE){
  # Make for all species for reference
  predators <- unique(alldietdat$predator)
  seasons <- unique(alldietdat$season)
  
  
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
          name = "Overlap metric"
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

