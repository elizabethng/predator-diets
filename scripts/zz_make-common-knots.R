# Use a common grid based on the entire trawl region to make the knot locations
# for analysis.

trawldat <- readr::read_rds(here::here("output", "data_formatted", "dat_trawl.rds")) 

onecopy <- trawldat %>%
  filter(pdcomnam == "ATLANTIC COD") %>%
  select(-towid, -vessel, -pdcomnam, -catch_kg)
  
onecopy %>%
  group_by(myseason) %>%
  summarize(
    unique_locs = length(unique(declat)),
    locs = length(declat),
    tows = length(unique(towid_unique)),
    n = n()
  )
  
spring <- filter(onecopy, myseason == "SPRING")
fall <- filter(onecopy, myseason == "FALL")


# Spatial_List <- FishStatsUtils::make_spatial_info(
#   grid_size_km = grid_size_km,
#   n_x = n_x,
#   Method = Method,
#   Lon = Data_Geostat$Lon,
#   Lat = Data_Geostat$Lat,
#   Extrapolation_List = Extrapolation_List,
#   fine_scale = FALSE,
#   DirPath = DateFile,
#   Save_Results = TRUE
# )

test_spring <- make_spatial_info(
  n_x = 100,
  Lon_i = spring$declon,
  Lat_i = spring$declat,
  
)

test_fall <- 