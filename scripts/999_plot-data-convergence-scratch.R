# Wonky data exploration
exdat <- filter(poop, mod_num == 2) %>%
  slice(1) %>%
  semi_join(badmod_data, ., by = c("predator", "season", "use_aniso", "model")) %>%
  select(data.x) %>%
  unnest(data.x)


northamerica <- rnaturalearth::ne_countries(continent = "north america",
                             scale = "medium",
                             returnclass = "sf")

mapdat <- exdat %>%
  filter(pypres == 1) %>%
  sf::st_as_sf(coords = c("declon", "declat"), crs = 4326)

ggplot() +
  geom_sf(data = mapdat, aes(color = year)) +
  geom_sf(data = northamerica, color = "white", fill = "grey", inherit.aes = FALSE) +
  coord_sf(xlim = c(-79.5, -65.5), ylim = c(32.5, 45.5))


group_by(exdat, year) %>%
  summarise(n = sum(pypres)) %>%
  print(n = Inf)


# Look at raw data
library(tidyverse)

alldiet <- readr::read_rds(here::here("data", "processed", "dat_tows_all.rds"))

mydiet <- alldiet %>%
  # filter(mean_pyamtw > 0) %>%
  sf::st_as_sf(coords = c("declon", "declat"), crs = 4326) %>%
  filter(predator %in% c("silver hake", "atlantic cod", "goosefish", "spiny dogfish"))

ggplot() +
  geom_sf(data = mydiet, aes(color = mean_pyamtw)) +
  scale_color_viridis_c() +
  geom_sf(data = northamerica, color = "white", fill = "grey", inherit.aes = FALSE) +
  coord_sf(xlim = c(-79.5, -65.5), ylim = c(32.5, 45.5)) +
  facet_grid(season ~ predator)


# Zero observations
ggplot() +
  geom_sf(data = filter(mydiet, mean_pyamtw == 0),
          aes(color = mean_pyamtw), alpha = 0.1) +
  scale_color_viridis_c() +
  geom_sf(data = northamerica, color = "white", fill = "grey", inherit.aes = FALSE) +
  coord_sf(xlim = c(-79.5, -65.5), ylim = c(32.5, 45.5)) +
  facet_grid(season ~ predator)

# Positve observations
ggplot() +
  geom_sf(data = filter(mydiet, mean_pyamtw > 0),
          aes(color = mean_pyamtw), alpha = 0.1) +
  scale_color_viridis_c() +
  geom_sf(data = northamerica, color = "white", fill = "grey", inherit.aes = FALSE) +
  coord_sf(xlim = c(-79.5, -65.5), ylim = c(32.5, 45.5)) +
  facet_grid(season ~ predator)


# Single species over time
ggplot() +
  geom_sf(data = filter(mydiet, 
                        mean_pyamtw > 0,
                        predator == "silver hake"),
          aes(color = season, alpha = mean_pyamtw/max(mean_pyamtw))) + # , alpha = 0.1
  # scale_color_viridis_c() +
  geom_sf(data = northamerica, color = "white", fill = "grey", inherit.aes = FALSE) +
  coord_sf(xlim = c(-79.5, -65.5), ylim = c(32.5, 45.5)) +
  facet_wrap(~year)

