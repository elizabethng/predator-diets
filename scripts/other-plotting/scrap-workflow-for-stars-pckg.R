# Try using Stars package to handle spatial output

library("tidyverse")
library("sf")
library("stars")

# Example wide dataset
# Process output
obs <- matrix(NA, nrow = dim(Report$D_gcy)[1], ncol = dim(Report$D_gcy)[3])
for(i in 1:length(Year_Set)){
  obs[, i] <- Report$D_gcy[ , 1, i]
}
colnames(obs) <- paste0("y_", Year_Set)
obs_dat <- as_tibble(obs)

# Locations
locs <- as_tibble(MapDetails_List$PlotDF)

# Wide data
widedat <- bind_cols(locs, obs_dat)

# Make the data spatial
spatdat <- st_as_sf(widedat, coords = c("Lon", "Lat"), crs = 4326) %>%
  select(y_1990)
datst <- st_rasterize(spatdat) # only keeps the first var?


ggplot() +
  geom_sf(data = spatdat, aes(color = y_1990))


ggplot() +
  geom_sf(data = sample_n(spatdat, 20), aes(color = y_1990))
# Just have to adjust the point size for output size (prevent overlap)