# Calculate Hurlbert's Index

# i: index for knot
# p_pred_i proportion of total predators in location i
# p_prey_i proportion of total prey in location i
# A_i area of location i
# A_occupied total area 

# Q: A_occupied is that for both at the same time?  

library(tidyverse)
library(here)

save_output = TRUE

results_files = c("herring_density_plg_rw", 
                  "cod_density_plg_rw", 
                  "dogfish_density_plg_rw")


# Load data ---------------------------------------------------------------
path_name = paste("output", "VAST", "_runs_post_popdy", sep = "/")
pred = c("herring", "cod", "dogfish")

total_density = density = vector("list", length(results_files))
names(total_density) = names(density) = c("herring", "cod", "dogfish")


for(i in seq(results_files)){
  # knot-level density
  load(here(path_name, results_files[i], "Save.RData"))
  density[[i]] = drop(Save$Report$D_xcy)
  
  colnames(density[[i]]) = 1973:2015
  density[[i]] = as_tibble(density[[i]]) %>%
    mutate(species = pred[i]) %>%
    mutate(knot = 1:100) %>%
    gather("year", "density", -knot, -species)
  
  # knot info
  load(here(path_name, results_files[i], "Record.RData"))
  density[[i]] = as_tibble(Record$Spatial_List$loc_x) %>%
    mutate(area = c(Record$Spatial_List$PolygonList$a_xl),
           knot = 1:100) %>%
    right_join(density[[i]])
    
  tmp = read_csv(here(path_name, results_files[i], "Table_for_SS3.csv"))
  
  total_density[[i]] = tmp %>%
    mutate(species = pred[i]) %>%
    select(-Unit, -Fleet)

  rm(Save)
  rm(Record)
}

# Check that knot locations are the same
sum(density[[1]][,1:3] != density[[2]][,1:3])
sum(density[[2]][,1:3] != density[[3]][,1:3])

# Condense lists
density = bind_rows(density) %>%
  mutate(area_norm = area/mean(area)) # normalize areas
total_density = bind_rows(total_density)


# Normalize densities -----------------------------------------------------

index_values = density %>%
  spread(species, density)

index_values = index_values %>%
  mutate(
    cod_overlap = cod*herring/area_norm,
    dogfish_overlap = dogfish*herring/area_norm) %>%
  gather("index", "value", -E_km, -N_km, -area, -knot, -year, -area_norm)




# Want to divide density in each year by total abundance??
# Try with raw densities, since that seems to match the Hurlbert 1978 paper best

cod_index = matrix(0, nrow = 100, ncol = 43)
dogfish_index = matrix(0, nrow = 100, ncol = 43)


# multiply pred and prey densities
# multiply by knot areas
# store in index vector

# what to do about 0 areas?? what does that even mean??
# just get rid of them afterwards...
# Does this mean I actually have fewer than 100 knots?? (because of data scarcity?)

for(year in 1:43){
  cod_index[,year] = (density$herring[,,year]*density$cod[,,year])/knot_denom
  dogfish_index[,year] = (density$herring[,,year]*density$dogfish[,,year])/knot_denom
}




# Fix problem cell
prob_cell = which(cod_index == max(cod_index, na.rm = TRUE), arr.ind = TRUE)
# Method 1: set to NA for that year
# cod_index[prob_cell] = dogfish_index[prob_cell] = NA

# Method 2: replace with average density for that year
new_herring_density = mean(density$herring[-66, , 21])
cod_index[prob_cell] = (new_herring_density*density$cod[66, , 21])/knot_denom[66]
dogfish_index[prob_cell] = (new_herring_density*density$dogfish[66, , 21])/knot_denom[66]


cod_index = na_if(cod_index, Inf)
dogfish_index = na_if(dogfish_index, Inf)

# Save knot-level results
# rows are knot locations, columns are years
# reshape the output for easier manipulation

colnames(cod_index) = colnames(dogfish_index) = 1973:2015

cod_index_tidy = as_tibble(cod_index) %>%
  mutate(knot = 1:100) %>%
  gather("year", "overlap", -knot)

dogfish_index_tidy = as_tibble(dogfish_index) %>%
  mutate(knot = 1:100) %>%
  gather("year", "overlap", -knot)

if(save_output){
  saveRDS(cod_index_tidy, 
          here("output", "data_formatted", 
               "cod_overlap_knots.rds"))
  saveRDS(dogfish_index_tidy, 
          here("output", "data_formatted", 
               "dogfish_overlap_knots.rds"))
}

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
  facet_wrap(~index, nrow = 3, scales = "free")

if(save_output) saveRDS(all_results_l, here("output", "data_formatted", "overlap_indices.rds"))

# Plot overlap in each year

p_cod = ggplot(
  filter(all_results_l, index == "overlap_cod"),
  aes(year, value/10000)) + 
  geom_line(size = 1, color = "#000060") +
  theme_bw() + 
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black")) +
  xlab("Year") +
  ylab("Overlap index (cod and herring)") + 
  theme(text = element_text(size = 16)) +
  theme(legend.position = "none")
if(save_output) ggsave(here("output", "plots", "cod_herring_overlap.pdf"), 
       p_cod,
       width = 7,
       height = 4)


p_dog = ggplot(
  filter(all_results_l, index == "overlap_dogfish" & year > 1972),
  aes(year, value/10000)) + 
  geom_line(size = 1, color = "#FA813B") +
  theme_bw() + 
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black")) +
  xlab("Year") +
  ylab("Overlap index (dogfish and herring)") + 
  theme(text = element_text(size = 16)) +
  theme(legend.position = "none")
if(save_output) ggsave(here("output", "plots", "dogfish_herring_overlap.pdf"), 
       p_dog,
       width = 7,
       height = 4)




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
