# Create extrapolation grid for GB and GOM

library(tidyverse)
library(sf)
library(here)

SaveOutput = TRUE
n_points = c(155, 136)

# 0. Load data
data(northwest_atlantic_grid, package = "FishStatsUtils")

# 1. Get area perimeter
nw_perimeter = northwest_atlantic_grid %>%
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326) %>%
  concaveman::concaveman()

# diff(st_bbox(nw_perimeter)[c(1, 3)])/0.09
# diff(st_bbox(nw_perimeter)[c(2, 4)])/0.09

# 2. Create grid cells
starttime = Sys.time()
nw_grid = nw_perimeter %>%
  st_make_grid(what = "polygons", n = n_points) %>%
  st_intersection(nw_perimeter)
plot(nw_grid)
Sys.time() - starttime

# 3. Get grid cell centroids
tmp_nw_points = nw_grid %>%
  st_centroid()
plot(tmp_nw_points, add = TRUE, col = "red", pch = ".")


# 4. Get coordinates for centroids
#    Convert Lat/Lon to UTM (might come in handy for later?)
#    Just build a new (non-spatial) data frame
nw_points = tibble(
  Lon = st_coordinates(tmp_nw_points)[,1],
  Lat = st_coordinates(tmp_nw_points)[,2])

tmp_utm = tmp_nw_points %>% 
  st_transform(crs = 32610) %>%
  st_coordinates()

nw_points$UTM_E = tmp_utm[,1]
nw_points$UTM_N = tmp_utm[,2]


# 5. Calculate polygon areas
#    May want to do this for UTM stuff instead?
nw_points$area_km2 = nw_grid %>% 
  st_transform(crs = 32610) %>%
  st_area() %>%
  units::set_units(., "km^2")


# 6. Bundle output and save
nw_atlantic = list(
  nw_grid = nw_grid,
  nw_points = nw_points
)

if(SaveOutput == TRUE){
  saveRDS(nw_atlantic, here("output", "data_formatted", "nw_atlantic_grid.rds"))
}
