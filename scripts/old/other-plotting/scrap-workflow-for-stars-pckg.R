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

# Plot SE effects when estimated (in check-VAST-non-fxn output)
poop <- select(map_dat, E_km, N_km, starts_with("stderror")) %>%
  pivot_longer(cols = starts_with("stderror"), names_to = "year", values_to = "density_se") %>%
  mutate(year = gsub("stderror_", "", year)) %>%
  mutate(year = as.numeric(year))

filter(poop, year == 1990) %>%
  ggplot(aes(E_km, N_km, color = density_se)) +
  geom_point(size = 3) +
  scale_color_viridis_c() +
  theme_bw()


## Approach if I have SD report for D_gcy
denstiy_table <- summary(Opt$SD) %>%
  data.frame() %>%
  rownames_to_column() %>%
  rename(parameter = rowname, estimate = Estimate, std_error = `Std..Error`) %>%
  as_tibble() %>%
  filter(str_starts(parameter, "D_gcy"))

density_dat <- matrix(denstiy_table$estimate, nrow = dim(Report$D_gcy)[1], ncol = dim(Report$D_gcy)[3], byrow = FALSE)
density_dat_se <- matrix(denstiy_table$std_error, nrow = dim(Report$D_gcy)[1], ncol = dim(Report$D_gcy)[3], byrow = FALSE)

colnames(density_dat) <- paste0("density_", Year_Set)
colnames(density_dat_se) <- paste0("stderror_", Year_Set)

density <- as_tibble(density_dat)
density_se <- as_tibble(density_dat_se)

# Locations
# (but will probably only output this at the end when finescale = TRUE)
if(use_fine_scale == TRUE){
  locs <- as_tibble(MapDetails_List$PlotDF) # this is always every point loc  
}else{
  locs <- as_tibble(Spatial_List$MeshList$loc_x) # note these are UTM (zone 19 I think)
}

# Wide data
# Sacrifice tidiness for efficiency with obs
map_dat <- bind_cols(locs, density, density_se)
readr::write_csv(map_dat, file.path(DateFile, "my_map_dat.csv"))

