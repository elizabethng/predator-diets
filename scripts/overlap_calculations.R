# Calculate Hurlbert's Index

# i: index for knot
# p_pred_i proportion of total predators in location i
# p_prey_i proportion of total prey in location i
# A_i area of location i
# A_occupied total area 

# Q: A_occupied is that for both at the same time?  

library(tidyverse)
library(here)

results_files = c("herring_density_plg_rw", 
                  "cod_density_plg_rw", 
                  "dogfish_density_plg_rw")


# Load data ---------------------------------------------------------------
path_name = paste("output", "VAST", "_runs_for_popdy_meeting", sep = "/")

total_density = density = vector("list", length(results_files))
names(total_density) = names(density) = c("herring", "cod", "dogfish")


for(i in seq(results_files)){
  load(here(path_name, results_files[i], "Save.RData"))
  density[[i]] = Save$Report$D_xcy # as an array
  
  total_density[[i]] = read_csv(here(path_name, results_files[i], "Table_for_SS3.csv"))
}

load(here(path_name, results_files[i], "Record.RData"))
knot_areas = Record$Spatial_List$PolygonList$a_xl
knot_utms = Record$Spatial_List$loc_x

rm(Save)
rm(Record)


# Normalize areas ---------------------------------------------------------
knot_denom = knot_areas/sum(knot_areas)



# Normalize densities -----------------------------------------------------
# Want to divide density in each year by total abundance??
# Try with raw densities, since that seems to match the Hurlbert 1978 paper best

cod_index = matrix(0, nrow = 100, ncol = 43)
dogfish_index = matrix(0, nrow = 100, ncol = 43)


# multiply pred and prey densities
# multiply by knot areas
# store in index vector

# what to do about 0 areas?? what does that even mean??
# just get rid of them afterwards...

for(year in 1:43){
  cod_index[,year] = (density$herring[,,year]*density$cod[,,year])/knot_denom
  dogfish_index[,year] = (density$herring[,,year]*density$dogfish[,,year])/knot_denom
}

cod_index = na_if(cod_index, Inf)
dogfish_index = na_if(dogfish_index, Inf)



# Calculate and plot annual index -----------------------------------------

plot(1973:2015, colSums(cod_index, na.rm = TRUE))
plot(1973:2015, colSums(dogfish_index, na.rm = TRUE))


all_results = tibble(
  year = 1973:2015,
  tot_herring = total_density$herring$Estimate_metric_tons,
  tot_cod= total_density$cod$Estimate_metric_tons,
  tot_dogfish = total_density$dogfish$Estimate_metric_tons,
  overlap_cod = colSums(cod_index, na.rm = TRUE),
  overlap_dogfish = colSums(dogfish_index, na.rm = TRUE)
)


all_results_l = gather(all_results, index, value, -year)

ggplot(all_results_l, aes(year, value, group = index, color = index)) + 
  geom_line() + 
  facet_wrap(~index, scales = "free")


# Make a map --------------------------------------------------------------

knots_cod = data.frame(cod_index)
names(knots_cod) = 1973:2015
knots_cod$E_km = knot_utms[,1]
knots_cod$N_km = knot_utms[,2]
knots_cod_l = gather(knots_cod, year, index, -c(E_km, N_km)) %>%
  mutate(index_log = log(index))

tmp = knots_cod_l %>%
  sf::st_as_sf(coords = c("E_km", "N_km"), crs = 32610) %>%
  sf::st_transform(crs = 4326) %>%
  sf::st_coordinates()

knots_cod_l$lon = tmp[,1]
knots_cod_l$lat = tmp[,2]

ggplot(knots_cod_l, aes(E_km, N_km, color = index_log)) +
  geom_point() +
  scale_color_viridis_c() +
  facet_wrap(~year)


knots_dogfish = data.frame(dogfish_index)
names(knots_dogfish) = 1973:2015
knots_dogfish$E_km = knot_utms[,1]
knots_dogfish$N_km = knot_utms[,2]
knots_dogfish_l = gather(knots_dogfish, year, index, -c(E_km, N_km)) %>%
  mutate(index_log = log(index))

tmp = knots_dogfish_l %>%
  # mutate(E_km = -E_km) %>%
  sf::st_as_sf(coords = c("E_km", "N_km"), crs = 32619) %>%
  sf::st_transform(crs = 4326) %>%
  sf::st_coordinates()
  
knots_dogfish_l$lon = tmp[,1]
knots_dogfish_l$lat = tmp[,2]


ggplot(
  filter(knots_dogfish_l, year == 1973)
       , aes(lon, lat, color = index)) +
  borders("world", fill = "grey", colour = "white") +
  # coord_quickmap(xlim = c(-81, -63), ylim = c(32, 47.46)) +
  geom_point() +
  scale_color_viridis_c() +
  facet_wrap(~year) +
  theme(panel.grid.major = element_line(color = "white"),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + 
  theme(legend.position = "bottom") 

# + guides(fill = guide_legend(override.aes = list(shape = 21)))


# Simple maps
ggplot(knots_cod_l, aes(E_km, N_km, color = index_log)) +
  geom_point() +
  scale_color_viridis_c() +
  facet_wrap(~year)

ggplot(knots_dogfish_l, aes(E_km, N_km, color = index_log)) +
  geom_point() +
  scale_color_viridis_c() +
  facet_wrap(~year)




# Random other plots ------------------------------------------------------

ss_info = readxl::read_excel(here("data", "TimeSeries.xlsx"))

# ss_info$biomass_norm = ss_info$`Jan.1 Biomass (mt)`/sum(ss_info$`Jan.1 Biomass (mt)`)
# ss_info$recruit_norm = ss_info$`Age-1 Recruitment (000s)`/sum(ss_info$`Age-1 Recruitment (000s)`)

# 
# other_plots = tibble(
#   year = 1973:2015,
#   tot_herring = total_density$herring$Estimate_metric_tons/(sum(total_density$herring$Estimate_metric_tons)) ,
#   tot_cod = total_density$cod$Estimate_metric_tons/(total_density$cod$Estimate_metric_tons),
#   tot_dogfish = total_density$dogfish$Estimate_metric_tons/(total_density$dogfish$Estimate_metric_tons),
#   overlap_cod = colSums(cod_index, na.rm = TRUE),
#   overlap_dogfish = colSums(dogfish_index, na.rm = TRUE),
#   biomass_norm = ss_info$biomass_norm,
#   recruit_norm = ss_info$recruit_norm
# )
# 
# 
# all_results_l = gather(all_results, index, value, -year)
# 


ss_info_l = ss_info %>%
  gather(index, value, -Year)

ggplot(ss_info_l, aes(Year, value, group = index, color = index)) + 
  geom_line() + 
  facet_wrap(~index, scales = "free")
