# Try with wrapper functions

library("tidyverse")
library("VAST")

# NOTES
# Need to be able to re-fit model to link dll before simulating. 

# Next steps
# [ ] Get simple example running for fall Atlantic Cod
# [ ] Bundle and save components needed to re-run model outside of this project
#     [ ] Use save? Bundle into a list and write_rds?
# [ ] Run simple example for fall Atlantic herring
# [ ] Bundle output to run outside of project


# Load data ---------------------------------------------------------------

# Set output folder
working_dir <- here::here("prep-for-simulation", "output")

# Load data and model to run
trawlrun <- read_rds(here::here("output", "top_st_trawl.rds")) %>%
  select(-output, -model) %>%
  filter(species == "atlantic cod", season == "fall")

# Load strata limits
source(here::here("configuration-files", "strata_limits_subset.R"))


# Model settings ----------------------------------------------------------

# ORIGINAL OPTIONS
# Top Atlantic Cod fall model
# Poisson-link gamma model with independent years and 
# spatiotemporal random effects in both predictors

# use_REML <- TRUE
# use_fine_scale <- TRUE
# use_bias_correct <- FALSE
# Method = "Mesh"
# grid_size_km = 50
# n_x = 100
# Kmeans_Config = list( "randomseed" = 1, "nstart" = 100, "iter.max" = 1e3 )

settings <- make_settings(n_x = 100, 
                          Region = "northwest_atlantic", 
                          purpose = "index2", 
                          fine_scale = TRUE,
                          strata.limits = strata.limits,  
                          FieldConfig = c(
                            "Omega1"   = 1,   # number of spatial variation factors (0, 1, AR1)
                            "Epsilon1" = 0,   # number of spatio-temporal factors
                            "Omega2"   = 1,
                            "Epsilon2" = 0
                          ),
                          # RhoConfig = c(
                          #   "Beta1" = 0,      # temporal structure on years (intercepts) 
                          #   "Beta2" = 0, 
                          #   "Epsilon1" = 0,   # temporal structure on spatio-temporal variation
                          #   "Epsilon2" = 0
                          # ),
                          # OverdispersionConfig = c(
                          #   "Eta1" = 0,       # used for vessel effects
                          #   "Eta2" = 0
                          # ),
                          # ObsModel = c(       # c(2,1) poisson-link gamma
                          #   "PosDist" = 2,   
                          #   "Link"    = 1
                          # ),
                          bias.correct = FALSE,
                          use_anisotropy = FALSE)

# Run model ---------------------------------------------------------------

fit_orig <- fit_model("settings" = settings, 
                      "Lat_i" = trawlrun$processed_data[[1]]$Lat,
                      "Lon_i" = trawlrun$processed_data[[1]]$Lon, 
                      "t_i" = trawlrun$processed_data[[1]]$Year,
                      "c_i" = rep(0,nrow(trawlrun$processed_data[[1]])), 
                      "b_i" = trawlrun$processed_data[[1]]$Catch_KG,
                      "a_i" = trawlrun$processed_data[[1]]$AreaSwept_km2, 
                      "v_i" = trawlrun$processed_data[[1]]$Vessel,
                      "getJointPrecision" = TRUE, # Needed for simulating data
                      "working_dir" = working_dir)

write_rds(fit_orig, path = here::here("prep-for-simulation", "output", "fit_orig.rds"))



# Simulate data -----------------------------------------------------------
fit_orig <- read_rds(path = here::here("prep-for-simulation", "output", "fit_orig.rds"))

# Simulate new data
simdat <- simulate_data(fit_orig, type = 3)
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

# More output
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
