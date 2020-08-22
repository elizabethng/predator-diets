# Learn how to use simulation functionality in VAST

library("tidyverse")
library("VAST")

# Test will small example


working_dir <- paste0(here::here("prep-for-simulation"), "/")

# Load data set
example <- load_example(data_set = "EBS_pollock") 
# Reduce data set to run faster
example$sampling_data <- filter(example$sampling_data, Year %in% 1995:2000)

# Make settings (turning off bias.correct to save time for example)
settings <- make_settings(n_x = 100, 
                          Region = example$Region, 
                          purpose = "index2", 
                          strata.limits = example$strata.limits, 
                          bias.correct = FALSE)

# Check whether random effects are re-simulated during simulation
# NOTE: This may also be changed by simulate_data function
#       [ ] Check what Type actually does)
settings$Options
# If `simulate_random_effects=TRUE` is present in Options vector,
# It will re-simulate ranefs (at least as of Aug 2019 when Meaghan Bryan presented about it)

# Run model
fit_orig <- fit_model("settings" = settings, 
                      "Lat_i" = example$sampling_data[,'Lat'],
                      "Lon_i" = example$sampling_data[,'Lon'], 
                      "t_i" = example$sampling_data[,'Year'],
                      "c_i" = rep(0,nrow(example$sampling_data)), 
                      "b_i" = example$sampling_data[,'Catch_KG'],
                      "a_i" = example$sampling_data[,'AreaSwept_km2'], 
                      "v_i" = example$sampling_data[,'Vessel'],
                      "getJointPrecision" = TRUE, # Needed for simulating data
                      "working_dir" = working_dir )

# Plot results
plot(fit_orig) # some wonky-ness

# Save initial fit
# save(fit_orig, file=paste0(working_dir,"fit_orig.RData"))



# Simulator example -------------------------------------------------------

# Some settings
Type <- 3  # What type of simulator --> need to figure out the different between these

# Simulate one iteration
simdat <- simulate_data(fit_orig, type = Type)
names(simdat)

# Reconstruct the data frame
names(example$sampling_data)
simdat_df <- data.frame(
  Catch_kg = simdat$b_i,
  Year = simdat$t_i,
  Vessel = simdat$v_i,
  AreaSwept_km2 = simdat$a_i,
  Lon_i = example$sampling_data[,'Lon'], # in example, Jim just used values from data set...
  Lat_i = example$sampling_data[,'Lat']
)

# Or use estimated density for each grid cell g, category c, and year y
simdat$D_gcy # Null
simdat$D_gct # what is this??

dim(simdat$D_gct)
length(simdat$D_i)  # might be an average?
as.vector(simdat$D_gct) %>% length()

MapDetails_List <- FishStatsUtils::make_map_info(
  Region = example$Region,
  Extrapolation_List = fit_orig$extrapolation_list,
  spatial_list = fit_orig$spatial_list,
  NN_Extrap = fit_orig$spatial_list$PolygonList$NN_Extrap,
  fine_scale = fit_orig$spatial_list$fine_scale,
  Include = (fit_orig$extrapolation_list[["Area_km2_x"]] > 0 &
               fit_orig$extrapolation_list[["a_el"]][, 1] > 0))

# Get locations and standard errors for spatial density
# Will automatically account for different dimensions of D_gcy depending on whether finescale is true
Year_Set <- seq(min(example$sampling_data[,'Year']), max(example$sampling_data[,'Year']))
density_dat <- matrix(as.vector(simdat$D_gct), 
                      nrow = dim(simdat$D_gct)[1], 
                      ncol = dim(simdat$D_gct)[3], 
                      byrow = FALSE)
colnames(density_dat) <- paste0("density_", Year_Set)
density <- as_tibble(density_dat)

# Locations
# locs <- as_tibble(fit_orig$spatial_list$MeshList$loc_x ) # ended up with 2k knot locations instead of 100?
locs <- as_tibble(MapDetails_List$PlotDF) # because finescale is on (?)

# Wide data
# Sacrifice tidiness for efficiency with obs
map_dat <- bind_cols(locs, density)

long_map <- map_dat %>%
  select(-Include) %>%
  rename(knot = x2i,
         lat = Lat,
         lon = Lon) %>%
  pivot_longer(cols = starts_with("density_"), names_to = "year", values_to = "density") %>%
  mutate(year = gsub("density_", "", year),
         year = as.numeric(year))
filter(long_map, year == 1995) %>%
  ggplot(aes(x = lon, y = lat, color = density)) +
  geom_point()

# For comparison
filter(example$sampling_data, Year == 1995) %>%
  ggplot(aes(x = Lon, y = Lat, color = Catch_KG)) +
  geom_point()

hist(long_map$density)
hist(example$sampling_data$Catch_KG)


summary(long_map$density) # Don't know why so much higher, but will scale anyway probably for sampling
summary(example$sampling_data$Catch_KG)
