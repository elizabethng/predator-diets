# Check for patterns in residuals to see if other things are important in model
# Overlap
# Predator size


library(tidyverse)
library(here)


# Overlap -----------------------------------------------------------------
# How important is overlap? I compared the annual trends, but how much does
# it matter in space? 

# Read in overlap data at knot scale. 

cod_index = read_rds(here("output", "data_formatted", "cod_overlap_knots.rds")) %>%
  mutate(species = "cod")

dogfish_index = read_rds(here("output", "data_formatted", "dogfish_overlap_knots.rds")) %>%
  mutate(species = "dogfish")

# Big years
filter(cod_index, overlap == max(overlap))
filter(dogfish_index, overlap == max(overlap))

# Make into one data set
overlaps = rbind(cod_index, dogfish_index)

# Knot locations
all_locations = read_csv(here("PlotDF_output.csv")) %>%
  select(-X1, -Include) %>%
  rename(knot = x2i)

# Join knot locations and density
map_dat = left_join(overlaps, all_locations)

dim(map_dat)
filter(map_dat, knot != 34) %>% dim # missing 34
# map_dat = map_dat[complete.cases(map_dat),]
map_dat = filter(map_dat, knot != 34)

# Make a map of overlap
# p = ggplot(filter(map_dat, species == "cod"), 
#            aes(x = Lon, y = Lat, color = log(overlap))) +
#   geom_point() +
#   scale_color_viridis_c(
#     option = "inferno", 
#     name = "log(Overlap)") +
#   borders("world", fill = "grey", colour = "white") +
#   coord_quickmap(xlim = c(-79, -63), ylim = c(31, 47)) +
#   theme(panel.grid.major = element_line(color = "white"),
#         panel.background = element_blank(),
#         axis.title.x = element_blank(),
#         axis.text.x = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.title.y = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank()) + 
#   theme(legend.position = "bottom") +
#   # labs(title = 'Year: {closest_state}') +
#   # transition_states(year,
#   #                   transition_length = 2,
#   #                   state_length = 5)
#   facet_wrap(~year)
# plot(p)



for(pred in unique(map_dat$species)){
  for(yr in unique(map_dat$year)){
    p = ggplot(filter(map_dat, species == pred & year == yr), 
               aes(x = Lon, y = Lat, color = log(overlap))) +
      geom_point() +
      scale_color_viridis_c(
        option = "inferno", 
        name = "log(Overlap)") +
      borders("world", fill = "grey", colour = "white") +
      coord_quickmap(xlim = c(-79, -63), ylim = c(31, 47)) +
      theme(panel.grid.major = element_line(color = "white"),
            panel.background = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) + 
      theme(legend.position = "bottom") +
      ggtitle(paste(pred, yr))
    ggsave(here("output", "plots", "annual_overlaps", paste0(pred, "_", yr, ".jpeg")),
           width = 3,
           height = 4)
  }
}



ggplot(filter(all_locations, knot == 66), 
       aes(x = Lon, y = Lat, color = knot)) +
  geom_point() +
  borders("world", fill = "grey", colour = "white") +
  coord_quickmap(xlim = c(-79, -63), ylim = c(31, 47)) +
  theme(panel.grid.major = element_line(color = "white"),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + 
  theme(legend.position = "none")

ggplot(filter(map_dat, log_overlap > 11), 
       aes(x = Lon, y = Lat, color = knot)) +
  geom_point() +
  borders("world", fill = "grey", colour = "white") +
  coord_quickmap(xlim = c(-79, -63), ylim = c(31, 47)) +
  theme(panel.grid.major = element_line(color = "white"),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + 
  theme(legend.position = "none")

# Large edge overlap ones may be largest over time due to the large size of the knot??
simple_locs = all_locations %>%
  group_by(knot) %>%
  summarize(n = n(),
            lat = mean(Lat),
            lon = mean(Lon))

ggplot(simple_locs, aes(lat, lon, label = knot)) +
  geom_text(check_overlap = TRUE)

