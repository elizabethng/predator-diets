# Make my own plots of density that are easier to visualize

library(tidyverse)
library(here)


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


# Try mapping
ggplot(filter(map_dat, year == 1973),
       aes(x = Lon, y = Lat, color = density_log)) +
  geom_point()
  


