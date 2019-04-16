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