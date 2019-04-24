# Make my own plots of density that are easier to visualize

library(tidyverse)
library(here)
library(gganimate)


results_files = c("herring_density_plg_rw", 
                  "cod_density_plg_rw", 
                  "dogfish_density_plg_rw")[2]

path_name = paste("output", "VAST", "_runs_for_popdy_meeting", sep = "/")



# Knot locations
all_locations = read.csv(here("PlotDF_output.csv")) %>%
  select(-X)


# Estimated densities at knots
load(here(path_name, results_files, "Save.RData"))
est_dens = Save$Report$D_xcy 

# make estimated densities easier to manipulate
tmp = numeric()
for(i in 1:43){
  tmp = c(tmp, est_dens[,,i])
}

all_dens = tibble(
  year = sort(rep(1973:2015, 100)),
  x2i = rep(1:100, 43),
  density = tmp)


# Join each year of data with the knot locations for plotting
map_dat = left_join(all_dens, all_locations) %>%
  mutate(density_log = log(density))

# missing 34....
map_dat[!complete.cases(map_dat),]
map_dat = map_dat[complete.cases(map_dat),]

# Try mapping
p = ggplot(filter(map_dat, Include == TRUE), 
       aes(x = Lon, y = Lat, color = density_log)) +
  geom_point() +
  scale_color_viridis_c(
    option = "inferno", 
    name = "log(Density)") +
  borders("world", fill = "grey", colour = "white") +
  coord_quickmap(xlim = c(-77, -63), ylim = c(37, 47)) +
  theme(panel.grid.major = element_line(color = "white"),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + 
  theme(legend.position = "bottom") +
  labs(title = 'Year: {closest_state}') +
  transition_states(year,
                    transition_length = 2,
                    state_length = 5)
  # facet_wrap(~year)

anim_save(animation = p, filename = here("test_anim_cod_cons.gif"))




