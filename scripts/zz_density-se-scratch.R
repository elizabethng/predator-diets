# Scratch work for extracting density point-estimate standard errors

poop <- summary(Opt$SD) %>% 
  as_tibble(rownames = "parameter")

unique(poop$parameter)

filter(poop, parameter == "D_gcy")

# Ok what is this structure? How does it match up with density?
# How can I ensure that they match up between predators and prey?
# Perhaps running the two-species model will be easier for the
# ammount of work I'll have to do.

Year_Set <- seq(min(Data_Geostat[,'Year']),max(Data_Geostat[,'Year']))
density_dat <- matrix(as.vector(Report$D_gcy), nrow = dim(Report$D_gcy)[1], ncol = dim(Report$D_gcy)[3], byrow = FALSE)
colnames(density_dat) <- paste0("density_", Year_Set)
density <- as_tibble(density_dat)
# I think values with 1 are years without observations....

# Check that dimensions/values are the same
length(as.vector(Report$D_gcy))
nrow(filter(poop, parameter == "D_gcy"))

filter(poop, parameter == "D_gcy") %>%
  pull(Estimate) %>%
  identical(as.vector(Report$D_gcy))

# Do similar approach to get density SEs
dens_se_vec <- filter(poop, parameter == "D_gcy") %>%
  pull(`Std. Error`)

density_SE <- matrix(dens_se_vec, nrow = dim(Report$D_gcy)[1], ncol = dim(Report$D_gcy)[3], byrow = FALSE)
colnames(density_SE) <- paste0("density_SE_", Year_Set)
dens_SE <- as_tibble(density_SE)

# Ok, so I can pull out denstiy standard errors, 
# but do they represent the same locations between predators?
# I think I got around that issue previously be re-setting the grid
# using sf package to calculate the average values within my own
# cells

# How to get the locations of the knots?
# Already have the code written
# Locations
# (but will probably only output this at the end when finescale = TRUE)
if(use_fine_scale == TRUE){
  locs <- as_tibble(MapDetails_List$PlotDF) # this is always every point loc  
}else{
  locs <- as_tibble(Spatial_List$MeshList$loc_x) # note these are UTM (zone 19 I think)
}

# Wide data
# Sacrifice tidiness for efficiency with obs
map_dat <- bind_cols(locs, density)
map_dat_se <- bind_cols(locs, dens_SE)

# Now check with another species and compare
write_rds(list(map_dat, map_dat_se), paste0(diagnostic_folder, "/output.rds"))


# Compare atlantic herring and atlantic cod
cod <- read_rds("D:/Dropbox/Predator_Diets/output/VAST-test-version/atlantic-cod-spring/output.rds")
cod_locs <- cod[[2]][, 1:2]


plot(locs$E_km,locs$N_km)
points(cod_locs$E_km, cod_locs$N_km, col = "red")

# So they are close but the mesh is not the same. 