# Script to make summary plots of:
#  * trawl locations
#  * non-empty stomach locations (colored by presence/absence)
#  * knot locations
# Also potentially include
#  * sample sizes for each?

library(here)
library(tidyverse)
library(sf)

map_theme <- function(...){
  theme(panel.grid.major = element_line(color = "white"),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = c(0.3, 0.12),
        legend.direction = "horizontal", 
        strip.background = element_blank(), 
        text = element_text(size = 13),
        ...)
}

# new_test/spiny-dogfish_season-spring_covar-NA_lognm-pl-independent-years-no2spatial

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
  )

# utms <- all_trawls %>%
#   st_as_sf(coords = c("declon", "declat"), crs = 4326) %>%
#   st_transform(crs = 32619) %>%
#   st_coordinates()
# 
# all_trawls <- all_trawls %>%
#   mutate(
#     "Easting (km)" = utms[, 1],
#     "Northing (km)" = utms[, 2]
#   )

# Get non-empty stomach locations (i.e., from Data_Geostat)
load(here("new_test", 
          "spiny-dogfish_season-spring_covar-NA_lognm-pl-independent-years-no2spatial", 
          "model-settings.RData"))

# Make a map
ggplot(all_trawls, aes(x = declon, y = declat)) +
  geom_point(color = "grey", alpha = 0.1, pch = 20) +
  borders("world", fill = "grey", colour = "white") +
  borders("state", fill = "grey", colour = "white") +
  coord_quickmap(xlim = c(-77, -65), ylim = c(35, 46)) + 
  map_theme()




scale_color_viridis_c(
  option = "inferno", 
  name = "log(Consumption)") +
  


# Check the plot of the knot locations and the data
plot(Extrapolation_List$Data_Extrap$E_km, Extrapolation_List$Data_Extrap$N_km, 
     pch = ".", xlab = "Easting (km)", ylab = "Northing (km)")
points(Spatial_List$loc_i[,1], Spatial_List$loc_i[,2],
       col = "blue", pch = 20)
points(Spatial_List$loc_x[,1], Spatial_List$loc_x[,2], 
       col = "yellow", pch = 19)

# Check variability in the second componenet of the model
Data_Geostat %>%
  mutate(prey_pres = ifelse(Catch_KG > 0, "present", "absent")) %>%
  # filter(Catch_KG > 0) %>%
  # ggplot(aes(Lon, Lat, color = Catch_KG)) +
  ggplot(aes(Lon, Lat, color = prey_pres)) +
  geom_point() +
  facet_wrap(~Year)
