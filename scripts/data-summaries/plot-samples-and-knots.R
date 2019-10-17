# Check the distribution of sampels in space for the extrapolation grid

library(tidyverse)
library(here)
library(VAST)



# 1. Get data -------------------------------------------------------------


Species = c("SILVER HAKE", "RED HAKE", "FOURSPOT FLOUNDER", "ATLANTIC COD", 
            "POLLOCK", "WHITE HAKE", "WINTER SKATE", "SPINY DOGFISH", "SUMMER FLOUNDER", 
            "GOOSEFISH", "THORNY SKATE", "SEA RAVEN", "BLUEFISH", "WEAKFISH")[8]

dat = read_rds(here("output", "data_formatted", "dat_tows_all.rds")) %>%
  filter(pdcomnam == Species)

Data_Geostat = data.frame(
  Catch_KG = dat$mean_pyamtw,
  Year = dat$year,
  Vessel = "missing",
  AreaSwept_km2 = 1,
  Lat = dat$declat,
  Lon = dat$declon) %>%
  na.omit()

# 2. Set up extrapolation -------------------------------------------------
Data_Set = paste(Species, "all seasons")
Region = "Northwest_Atlantic"

Method = c("Grid", "Mesh", "Spherical_mesh")[2]
grid_size_km = 25
n_x = 100          # number of knots
Fine_scale = FALSE

subset.strata.limits = data.frame("STRATA" = 
                                    c(7510L, 7500L, 3440L, 3430L, 3420L, 1610L, 1620L, 1630L, 1640L, 
                                      3390L, 3400L, 3410L, 1650L, 1660L, 1670L, 1680L, 3360L, 3370L, 
                                      3380L, 3330L, 3340L, 3350L, 1720L, 3310L, 3320L, 1700L, 1710L, 
                                      3300L, 1690L, 3270L, 3280L, 3290L, 1750L, 1760L, 1740L, 3240L, 
                                      3250L, 3260L, 1730L, 3230L, 3220L, 1040L, 1030L, 3180L, 3190L, 
                                      3200L, 1020L, 1010L, 3150L, 3160L, 3170L, 1080L, 1070L, 1060L, 
                                      1120L, 1110L, 1100L, 1150L, 3120L, 3130L, 3140L, 1140L, 3110L, 
                                      1130L, 3100L, 3080L, 1050L, 3090L, 1090L, 3070L, 3060L, 1190L, 
                                      3050L, 3460L, 3520L, 3550L, 3030L, 3040L, 1180L, 1170L, 3020L, 
                                      1160L, 3010L, 1250L, 1230L, 3450L, 1200L, 3540L, 3480L, 1240L, 
                                      3470L, 3510L, 3530L, 3920L, 3500L, 3560L, 3570L, 1220L, 1210L, 
                                      3580L, 3590L, 1280L, 1290L, 3600L, 3610L, 1260L, 1270L, 3630L, 
                                      3620L, 3640L, 3650L, 3660L, 1310L, 1300L, 1320L, 1370L, 1400L, 
                                      1360L, 3680L, 3670L, 3690L, 1340L, 3700L, 3710L, 3720L, 3908L, 
                                      1380L, 1330L, 3730L, 3740L, 3750L, 3780L, 3770L, 1390L, 3760L, 
                                      3810L, 3800L, 3790L, 3830L, 3820L, 3850L, 3840L, 1351L, 3870L, 
                                      3860L, 1352L, 3900L, 3890L, 3880L))

Extrapolation_List_2 = make_extrapolation_info(Region = Region, strata.limits = subset.strata.limits)

Spatial_List_2 = make_spatial_info(
  grid_size_km = grid_size_km,
  n_x = n_x,
  Method = Method,
  Lon = Data_Geostat$Lon,
  Lat = Data_Geostat$Lat,
  Extrapolation_List = Extrapolation_List_2,
  fine_scale = Fine_scale,
  Save_Results = FALSE)

# Add knots to Data_Geostat
Data_Geostat$knot_i_2 = Spatial_List_2$knot_i

# Check the knot locations (sparse southern region)
plot(Extrapolation_List_2$Data_Extrap$E_km, Extrapolation_List_2$Data_Extrap$N_km, 
     pch = ".", xlab = "Easting (km)", ylab = "Northing (km)")

extrap_info <- Extrapolation_List_2$Data_Extrap %>%
  select(stratum_number, Lon, Lat, E_km, N_km) %>%
  filter(stratum_number %in% subset.strata.limits$STRATA)

plot(extrap_info$E_km, spatial_info$N_km, pch = ".")

knot_info <- Spatial_List_2$loc_x %>%
  as.data.frame() %>%
  sf::st_as_sf(coords = c("E_km", "N_km"), cros = 4326)
  
points(Spatial_List_2$loc_x[,1], Spatial_List_2$loc_x[,2], 
       col = "yellow", pch = 19)


# Make plots --------------------------------------------------------------

ggplot(spatial_info, aes(x = Lon, y = Lat)) +
  geom_point(pch = ".", color = "grey") +
  geom_point(data = Data_Geostat, aes(x = Lon, y = Lat, color = Catch_KG)) +
  borders("world", fill = "grey", colour = "white") +
  borders("state", fill = "grey", colour = "white") +
  coord_quickmap(xlim = c(-77, -65), ylim = c(35, 46)) +
  theme(panel.grid.major = element_line(color = "white"),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + 
  theme(legend.position = "bottom")


ggplot(Data_Geostat, aes(x = Lon, y = Lat, color = Catch_KG)) +
  scale_color_continuous(low = "white", high = "black") +
  borders("world", fill = "grey", colour = "white") +
  borders("state", fill = "grey", colour = "white") +
  coord_quickmap(xlim = c(-77, -65), ylim = c(35, 46)) +
  theme(panel.grid.major = element_line(color = "white"),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + 
  theme(legend.position = "bottom")
