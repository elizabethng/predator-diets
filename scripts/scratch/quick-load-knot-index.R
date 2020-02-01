library("rnaturalearth")
library("tidyverse")
library("here")
library("sf")
library("stars")


# Data --------------------------------------------------------------------

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

# Plots -------------------------------------------------------------------

myraster <- select(plot_average_diet, -density_avg_z) %>%
  stars::st_rasterize()
ggplot() +
  stars::geom_stars(data = myraster) +
  viridis::scale_fill_viridis()

ggplot() +
  stars::geom_stars(data = myraster) +
  viridis::scale_fill_viridis(na.value = "white") +
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


jj <- st_as_sf(average_diet, coords = c("lon", "lat"), crs = 4326, as_points=FALSE, merge=TRUE, connect8=TRUE) 
jj <- st_as_sfc(average_diet, coords = c("lon", "lat"), crs = 4326)

st_as_sf(as_points=FALSE, merge=TRUE, connect8=TRUE)


myraster <- stars::st_as_stars(plot_average_diet)
poop <- stars::st_set_dimensions(myraster, "predator")

ggplot() +
  stars::geom_stars(data = myraster) +
  facet_wrap(~ predator)
