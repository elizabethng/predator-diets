# Script to make summary plots of:
#  * trawl locations
#  * non-empty stomach locations (colored by presence/absence)
#  * knot locations
# Also potentially include
#  * sample sizes for each?

# How do I plan to use this as a function?
# Give it a folder with config-file name
#  * extract the season from the config-file name
# 

library(here)
library(tidyverse)
library(sf)

output_folder <- "new_test"
config_file_name <- "spiny-dogfish_season-spring_covar-NA_lognm-pl-independent-years-no2spatial"


map_theme <- function(...){
  theme(panel.grid.major = element_line(color = "white"),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        # legend.position = c(0.9, 0.1), # c(0.3, 0.12),
        # legend.direction = "horizontal", 
        strip.background = element_blank(), 
        text = element_text(size = 13),
        ...)
}


# Get all trawl locations
all_trawls <- readr::read_rds(here::here("output", "data_formatted", "dat_preds_all.rds")) %>%
  select(towid, declat, declon, year, myseason) %>%
  distinct() %>%
  rename(
    Season = myseason,
    Year = year
  ) %>%
  mutate(
    Season = tolower(Season)
  ) %>%
  st_as_sf(coords = c("declon", "declat"), crs = 4326)

# utms <- all_trawls %>%
#   st_transform(crs = 32619) %>%
#   st_coordinates()


# Get non-empty stomach locations (i.e., from Data_Geostat)
load(here("new_test", 
          "spiny-dogfish_season-spring_covar-NA_lognm-pl-independent-years-no2spatial", 
          "model-settings.RData"))

datgeo <- Data_Geostat %>%
  rename(Season = season) %>%
  filter(!is.na(Catch_KG)) %>%
  mutate(Season = tolower(Season)) %>%
  mutate(Herring = ifelse(Catch_KG > 0, "present", "absent")) %>%
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326)

datknots <- as_tibble(Spatial_List$loc_x) %>%
  mutate(
    E_m = E_km*1000,
    N_m = N_km*1000,
  ) %>%
  st_as_sf(coords = c("E_m", "N_m"), crs = 32619) %>%
  st_transform(crs = 4326)


# Make a map
p <- ggplot() +
  borders("world", fill = "grey", colour = "white") +
  borders("state", fill = "grey", colour = "white") +
  geom_sf(data = filter(all_trawls, Year >= min(datgeo$Year) & Season == "spring"), 
          mapping = aes(), 
          color = "grey", 
          alpha = 0.1, 
          pch = 20,
          size = 1) +
  geom_sf(data = datknots, 
          color = "black", 
          pch = 3, 
          size = 1) +
  geom_sf(data = datgeo, 
          aes(color = Herring, fill = Herring), 
          alpha = 0.7, 
          size = 1,
          show.legend = "point") +
  scale_color_manual(values = c("absent" = "grey40", "present" = "mediumblue")) +
  scale_fill_manual(values = c("absent" = "grey40", "present" = "mediumblue")) +
  coord_sf(xlim = c(-77, -65), ylim = c(35, 46)) + 
  facet_wrap(~Year, ncol = 9) +
  map_theme()
ggsave(paste0(config_file_name, ".pdf"), 
       plot = p,
       path = here::here("output", "map-tows-stomachs-and-knots"),
       width = 13.2, height = 10.2, units = "in")

# Check a subset of years
ggplot() +
  borders("world", fill = "grey", colour = "white") +
  borders("state", fill = "grey", colour = "white") +
  geom_sf(data = filter(all_trawls, Year %in% c(1990:1995)),
          mapping = aes(), color = "grey", alpha = 0.1, pch = 20
  ) +
  geom_sf(data = datknots, color = "black", pch = 4, size = 1.5) +
  geom_sf(data = filter(datgeo, Year %in% c(1990:1995)),
          aes(color = Herring, fill = Herring), 
          alpha = 0.7, show.legend = "point"
  ) +
  scale_color_manual(values = c("absent" = "grey40", "present" = "mediumblue")) +
  scale_fill_manual(values = c("absent" = "grey40", "present" = "mediumblue")) +
  coord_sf(xlim = c(-77, -65), ylim = c(35, 46)) + 
  facet_wrap(~Year) +
  map_theme()
