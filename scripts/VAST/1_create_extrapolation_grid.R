# Create extrapolation grid for GB and GOM

library(tidyverse)
library(sf)
library(here)
library(USAboundaries) 

SaveOutput = TRUE
if(SaveOutput == TRUE){}

# Get sampling region
dat = read_rds(here("output", "data_formatted", "dat_preds.rds")) %>%
  pluck("ATLANTIC COD") %>%
  filter(geoarea == "GB" | geoarea == "GoM") %>%
  st_as_sf(coords = c("declon", "declat"), crs = 4326) 
plot(st_geometry(dat))

samp_area = dat %>% 
  st_union() %>%
  st_convex_hull() # st_buffer(0.2) takes longer
plot(samp_area)


# Check overlap with land
myregion = us_states(states = c("CT", "RI", "MA", "NH", "VT","ME"))

ggplot() + 
  geom_sf(data = samp_area) +
  geom_sf(data = myregion) +
  # geom_sf(data = dat) +
  coord_sf(xlim = c(-73, -65), 
           ylim = c(40, 46)) +
  theme(panel.grid.major = element_line(color = "grey"),
        panel.background = element_blank())

# Make grid
# st_make_grid

# Compare to what Jim used
strata.limits = list('Georges_Bank'= c(1130, 1140, 1150, 1160, 1170, 1180, 1190, 1200, 1210, 1220, 1230, 1240, 1250, 1290, 1300))
Region = "Northwest_Atlantic"
Extrapolation_List = make_extrapolation_info(Region=Region, strata.limits=strata.limits)


# Depth data
# data downloaded from https://topex.ucsd.edu/cgi-bin/get_data.cgi
dat = read_table(here("data", "depth_data.txt"), col_names = FALSE) %>%
  rename(lon = X1, lat = X2, depth = X3) %>%
  mutate(lon = lon - 365)

ggplot(dat, aes(lon, lat, fill = depth)) + geom_raster()
# hmm looks wonky. come back to this


# Closer look at stuff inside VAST
data(northwest_atlantic_grid, package = "FishStatsUtils")

with(northwest_atlantic_grid, plot(Lon, Lat, pch = "."))
ggplot(northwest_atlantic_grid, aes(Lon, Lat, color = factor(stratum_number))) + geom_point() + theme(legend.title = element_blank())
ggplot(northwest_atlantic_grid, aes(Lon, Lat, color = Area_in_survey_km2)) + geom_point() + theme(legend.position = "none") + facet_wrap(~EPU)

# Looks like the small areas are weird boundaries? Maybe left over from a shapefile...
ggplot(filter(northwest_atlantic_grid, Area_in_survey_km2 < 13), aes(Lon, Lat, color = Area_in_survey_km2)) + 
  geom_point() + theme(legend.position = "none")

ggplot(filter(northwest_atlantic_grid, Area_in_survey_km2 > 13), aes(Lon, Lat, color = Area_in_survey_km2)) + 
  geom_point() + theme(legend.position = "none")

ggplot(filter(northwest_atlantic_grid, Area_in_survey_km2 > 13), aes(Lon, Lat)) + 
  geom_point(pch = ".") + theme(legend.position = "none")

# Then I end up with weird shadows. 
# I wonder if I can just use this to get an outer boundary, and then just use sf to get a regular grid

nw_perimeter = northwest_atlantic_grid %>%
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326) %>%
  concaveman::concaveman()

# Default is 10 x 10 grid (or ~1.3 x 1.2 degrees)
nw_grid = nw_perimeter %>%
  st_make_grid(n = c(40, 40)) %>%      # 0.09^2 # takes about 10 min
  st_intersection(nw_perimeter)

plot(nw_grid) # takes about 
plot(nw_perimeter, add = TRUE)

# calculat areas of polygons
# may want to do this for UTM stuff instead?
nw_grid %>% mutate(areast_area()) # how to track units automatically? need to set units for object?

# Get centroids for VAST stuff
mutate(centroid = st_centroid())     # probably need a pull here
plot(st_centroid(nw_grid)) # good enough to do after clipping? yes, since those are the areas I'm looking at

plot(st_make_grid(what = "centers"), axes = TRUE)
plot(st_make_grid(what = "corners"), add = TRUE, col = 'green', pch=3)
sfc = st_sfc(st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,0)))))
plot(st_make_grid(sfc, cellsize = .1, square = FALSE))
plot(sfc, add = TRUE)
# non-default offset:
plot(st_make_grid(sfc, cellsize = .1, square = FALSE, offset = c(0, .05 / (sqrt(3)/2))))
plot(sfc, add = TRUE)
