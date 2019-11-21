# finding shared locations for trawl data
# easier option--post hoc knn

library("tidyverse")
library("here")
library("VAST")
library("TMB")


trawlsetup <- readr::read_rds(here("data", "processed", "dat_trawl.rds"))

survey_locs <- trawlsetup %>%
  select()
  filter(unique(towid_unique))

# requires hard-coding defaults
  Method = c("Grid", "Mesh", "Spherical_mesh")[2]
  grid_size_km = 50
  n_x = c(50, 100, 250, 500, 1000, 2000)[2] # Number of stations
  Kmeans_Config = list( "randomseed" = 1, "nstart" = 100, "iter.max" = 1e3 )
  
  
  Extrapolation_List <- FishStatsUtils::make_extrapolation_info(
    Region = "northwest_atlantic",
    strata.limits = strata.limits
  )
  
  Spatial_List <- FishStatsUtils::make_spatial_info(
    grid_size_km = grid_size_km,
    n_x = n_x,
    Method = Method,
    Lon = Data_Geostat$Lon,
    Lat = Data_Geostat$Lat,
    Extrapolation_List = Extrapolation_List,
    fine_scale = FALSE,
    DirPath = DateFile,
    Save_Results = TRUE
  )