# Try with wrapper functions

library("tidyverse")
library("VAST")

# NOTES
# Need to be able to re-fit model to link dll before simulating. 

# Next steps
# [x] Get simple example running for fall Atlantic Cod
# [x] Find coordinates for simulated data
# [x] Bundle and save components needed to re-run model outside of this project
#     [-] Use save? Bundle into a list and write_rds?
# [x] Figure out why it's not detecting existing kmeans output --> recalculates in make_extrapolation_info because of small nubmer of data points
# [-] Give informative start values from previously fitted model (how to pass those?) -- could probably just pass them with start.par-type argument, but not worth it right now, pretty fast and only need to run twice
# [x] Run simple example for fall Atlantic herring
# [x] Bundle output to run outside of project

# later could work this to output both and push choice further down stream
my_species <- c("atlantic cod", "atlantic herring")[1]
save_output <- FALSE

# Load data ---------------------------------------------------------------

# Set output folder
working_dir <- here::here("prep-for-simulation", "output")

# Load strata limits
source(here::here("configuration-files", "strata_limits_subset.R"))

# Load data and model to run
trawlrun <- read_rds(here::here("output", "top_st_trawl.rds")) %>%
  select(-output, -model) %>%
  filter(species == my_species, season == "fall")



# Model settings ----------------------------------------------------------

# Simplified from original to avoid throwing errors. Does not match
# top models from predator-diets paper. Need to evaluate in future.
# epsilon value in first predictor was going to 0 for Atlantic cod.

settings <- make_settings(n_x = 100, 
                          Region = "northwest_atlantic", 
                          purpose = "index2", 
                          fine_scale = TRUE,
                          strata.limits = strata.limits,  
                          FieldConfig = c(
                            "Omega1"   = 1,   # number of spatial variation factors (0, 1, AR1)
                            "Epsilon1" = 0,   # number of spatio-temporal factors
                            "Omega2"   = 1,
                            "Epsilon2" = 0),
                          bias.correct = FALSE,
                          use_anisotropy = FALSE)

# Run model ---------------------------------------------------------------

fit_call <- expression(fit_model("settings" = settings, 
                      "Lat_i" = trawlrun$processed_data[[1]]$Lat,
                      "Lon_i" = trawlrun$processed_data[[1]]$Lon, 
                      "t_i" = trawlrun$processed_data[[1]]$Year,
                      "c_i" = rep(0,nrow(trawlrun$processed_data[[1]])), 
                      "b_i" = trawlrun$processed_data[[1]]$Catch_KG,
                      "a_i" = trawlrun$processed_data[[1]]$AreaSwept_km2, 
                      "v_i" = trawlrun$processed_data[[1]]$Vessel,
                      "getJointPrecision" = TRUE, # Needed for simulating data
                      "working_dir" = working_dir,
                      "DirPath" = paste0(working_dir, "/"))) # this may cause errors outside this repo?
fit_orig <- eval(fit_call)

# Gather and save elements needed to run outside of this folder
run_components <- list(
  trawlrun = trawlrun,
  strata_limits = strata.limits,
  settings = settings,
  fit_call = fit_call
)

if(save_output){
  write_rds(run_components, path = here::here("prep-for-simulation", "output", 
                                              paste0("run_fall_", gsub("atlantic ", "", my_species), ".rds")))
}


# Simulate data -----------------------------------------------------------
# Simulate new data
simdat <- simulate_data(fit_orig, type = 3)
names(simdat)

# Reconstruct the data frame
simdat_df <- data.frame(
  Catch_kg = simdat$b_i,
  Year = simdat$t_i,
  Vessel = simdat$v_i,
  AreaSwept_km2 = simdat$a_i,
  Lon_i = example$sampling_data[,'Lon'], # in example, Jim just used values from data set...
  Lat_i = example$sampling_data[,'Lat']
)

# More output
# Or use estimated density for each grid cell g, category c, and year y
simdat$D_gcy # Null
simdat$D_gct # what is this??

dim(simdat$D_gct)
length(simdat$D_i)  # might be an average?
as.vector(simdat$D_gct) %>% length()

MapDetails_List <- FishStatsUtils::make_map_info(
  Region = settings$Region,
  Extrapolation_List = fit_orig$extrapolation_list,
  spatial_list = fit_orig$spatial_list,
  NN_Extrap = fit_orig$spatial_list$PolygonList$NN_Extrap,
  fine_scale = fit_orig$spatial_list$fine_scale,
  Include = (fit_orig$extrapolation_list[["Area_km2_x"]] > 0 &
               fit_orig$extrapolation_list[["a_el"]][, 1] > 0))

# Get locations of finescale predictions
density_dat <- matrix(as.vector(simdat$D_gct), 
                      nrow = dim(simdat$D_gct)[1], 
                      ncol = dim(simdat$D_gct)[3], 
                      byrow = FALSE)
colnames(density_dat) <- paste0("density_", fit_orig$year_labels)
density <- as_tibble(density_dat)

# Locations for finescale points
# locs <- as_tibble(fit_orig$spatial_list$MeshList$loc_x ) # locations if finescale == FALSE
locs <- as_tibble(MapDetails_List$PlotDF)

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
filter(trawlrun$processed_data[[1]], Year == 1995) %>%
  ggplot(aes(x = Lon, y = Lat, color = Catch_KG)) +
  geom_point()
