# Make my own plots of density that are easier to visualize

library(tidyverse)
library(here)
library(gganimate)


results_files = c("herring_density_plg_rw", 
                  "cod_density_plg_rw", 
                  "dogfish_density_plg_rw",
                  "cod_plg_rw",
                  "dogfish_plg_rw")[5]

path_name = paste("output", "VAST", "_runs_for_popdy_meeting", sep = "/")



# Knot locations
all_locations = read.csv(here("PlotDF_output.csv")) %>%
  select(-X)


# Estimated densities at knots
load(here(path_name, results_files, "Save.RData"))
est_dens = Save$Report$D_xcy 

# make estimated densities easier to manipulate
# Might want to change this to load from results rather than assuming...
n_years = dim(est_dens)[3]

tmp = numeric()
for(i in 1:n_years){
  tmp = c(tmp, est_dens[,,i])
}

all_dens = tibble(
  year = sort(rep((2016-n_years):2015, 100)),
  x2i = rep(1:100, n_years),
  density = tmp)


# Join each year of data with the knot locations for plotting
map_dat = left_join(all_dens, all_locations) %>%
  mutate(density_log = log(density))

# missing 34....
map_dat[!complete.cases(map_dat),]
map_dat = map_dat[complete.cases(map_dat),]

# Try mapping animation
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

anim_save(animation = p, filename = here("animated_dogfish_consumption.gif"))


# Map comparison of year, all areas
my_year = c(1977, 2015)
p = ggplot(filter(map_dat, year %in% my_year), 
           aes(x = Lon, y = Lat, color = density_log)) +
  geom_point() +
  scale_color_viridis_c(
    option = "inferno", 
    name = "log(Consumption)") +
  borders("world", fill = "grey", colour = "white") +
  coord_quickmap(xlim = c(-79, -63), ylim = c(32, 47)) +
  facet_wrap(~year) +
  theme(panel.grid.major = element_line(color = "white"),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + 
  theme(legend.position = c(0.3, 0.12),
        legend.direction = "horizontal") + 
  theme(strip.background = element_blank()) + 
  theme(text = element_text(size = 13))
plot(p)
ggsave(here("output", "plots", paste0(results_files, "_comparison", ".pdf")),
       plot = p, width = 12, height = 8)



