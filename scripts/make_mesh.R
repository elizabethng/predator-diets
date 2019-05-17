# Script to make mesh that follows good INLA principles, but also works for VAST

# Need the same things as for SpatialList object
# Here are the components need for fit 
# (more are needed for plots, but I'll do my own)

# "a_xl" = Spatial_List$a_xl            A data frame with areas for each knot and each strattum
# "MeshList" = Spatial_List$MeshList    A tagged list with inputs related to the SPDE mesh
# "GridList" = Spatial_List$GridList    A tagged list with inputs related to the 2D AR1 grid
# "Method" = Spatial_List$Method,       The Method input (for archival purposes)
# "loc_x" = Spatial_List$loc_x,         The UTM location for each knot

path_name = paste("output", "VAST", "_runs_for_popdy_meeting", sep = "/")
results_files = "cod_density_plg_rw"
load(here(path_name, results_files, "Record.RData"))

Record$Spatial_List %>% names

my_dat = read_rds(here("output", "data_formatted", "dat_preds_all.rds"))

tmp = my_dat %>% 
  sf::st_as_sf(coords = c("declon", "declat"), crs = 4326) %>%
  sf::st_transform(crs = 32619) %>% # Transform to wgs 84 19N (could also try 18N crs = 32618)
  sf::st_coordinates()

dat = my_dat %>%
  mutate(utm_E = tmp[,1], utm_N = tmp[,2])
