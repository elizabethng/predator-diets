# Make my own plots of density that are easier to visualize
# Should now (7/11/19) be able to skip data processing part and just read_csv(my_map_dat.csv) from the appropriate results folder

library("tidyverse")
library("gganimate")
library("here")

topmods <- readr::read_rds(here::here("output", "top_diet.rds"))

map_dat <- topmods %>%
  transmute(knot_density = purrr::map(output, "knot_density")) %>%
  ungroup() %>%
  tidyr::unnest(knot_density) %>%
  mutate(ifelse(is.na(exclude_reason), density, NA)) %>%
  filter(Include == TRUE) %>%
  mutate(Year = year)

# Check one plot
map_dat %>%
  filter(species == "atlantic cod",
         season == "spring",
         year == 2015) %>%
  ggplot(aes(x = Lon, y = Lat, color = density_log)) +
  geom_point() +
  scale_color_viridis_c(
    option = "inferno", 
    name = "log(Density)") +
  borders("world", fill = "grey", colour = "white") +
  coord_quickmap(xlim = c(-77, -63), ylim = c(30, 47)) +
  theme(panel.grid.major = element_line(color = "white"),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  facet_wrap(~Year)

map_fun <- function(dat){
  p <- ggplot(dat, aes(x = Lon, y = Lat, color = density_log)) +
    geom_point() +
    scale_color_viridis_c(
      option = "inferno", 
      name = "log(Density)" # ,limits = c(-10.060097, 5.052691) # range(dat$density_log)
      ) + 
    borders("world", fill = "grey", colour = "white") +
    coord_quickmap(xlim = c(-77, -63), ylim = c(30, 47)) +
    theme(panel.grid.major = element_line(color = "white"),
          panel.background = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    facet_wrap(~Year, ncol = 9)
  return(p)
}

map_dat %>%
  filter(species == "atlantic cod",
         season == "spring",
         year == 2015) %>%
  map_fun()

mymaps <- map_dat %>%
  # filter(year %in% 1999:2000) %>%
  group_by(species, season) %>% 
  nest() %>%
  mutate(plot = purrr::map(data, ~ map_fun(.x))) %>%
  ungroup() %>%
  mutate(species = gsub(" ", "-", species)) %>%
  mutate(name = paste0(species, "-", season, ".pdf"))

walk2(mymaps$name, mymaps$plot, ~ ggsave(
    plot = .y,
    filename = .x,
    device = "pdf",
    path = here::here("output", "plots", "diet-ts"),
    width = 13.2, height = 10.2, units = "in")
  )


# iwalk(mymaps, ~ cat(..1, ..2))

# mymaps %>%
#   iwalk( ~ ggsave(
#     filename = paste0(paste0(c(..1, ..2), collapse = "-"), ".pdf"),
#     plot = ..4,
#     path = here::here("output", "plots", "diet-ts"),
#     width = 13.2, height = 10.2, units = "in")
#     )


# for(species_ in unique(mymaps$species)){
#   for(season_ in unique(mymaps$season)){
#     pdf(here("output", "plots", "diet-ts", 
#              paste0(paste0(c(species_, season_), collapse = "-"), ".pdf")))
#     mymaps %>%
#       dplyr::filter(species == species_ & season == season_) %>%
#       select(plot) %>%
#       pwalk(~ print(.x))
#       
#     dev.off()
#   }
# }



# Probably a way to map this using a custom function
  # ungroup() %>%
  # group_by(species, season)
  




# Try mapping animation
# p <- ggplot(filter(map_dat, Include == TRUE), 
#        aes(x = Lon, y = Lat, color = density_log)) +
#   geom_point() +
#   scale_color_viridis_c(
#     option = "inferno", 
#     name = "log(Density)") +
#   borders("world", fill = "grey", colour = "white") +
#   coord_quickmap(xlim = c(-77, -63), ylim = c(37, 47)) +
#   theme(panel.grid.major = element_line(color = "white"),
#         panel.background = element_blank(),
#         axis.title.x = element_blank(),
#         axis.text.x = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.title.y = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank()) + 
#   theme(legend.position = "bottom") +
#   labs(title = 'Year: {closest_state}') +
#   transition_states(year,
#                     transition_length = 2,
#                     state_length = 5)
#   # facet_wrap(~year)
# anim_save(animation = p, filename = here("animated_dogfish_consumption.gif"))
# 
# 
# # Map comparison of year, all areas
# my_year = c(1977, 2015)
# p = ggplot(filter(map_dat, year %in% my_year), 
#            aes(x = Lon, y = Lat, color = density_log)) +
#   geom_point() +
#   scale_color_viridis_c(
#     option = "inferno", 
#     name = "log(Consumption)") +
#   borders("world", fill = "grey", colour = "white") +
#   coord_quickmap(xlim = c(-79, -63), ylim = c(32, 47)) +
#   facet_wrap(~year) +
#   theme(panel.grid.major = element_line(color = "white"),
#         panel.background = element_blank(),
#         axis.title.x = element_blank(),
#         axis.text.x = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.title.y = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank()) + 
#   theme(legend.position = c(0.3, 0.12),
#         legend.direction = "horizontal") + 
#   theme(strip.background = element_blank()) + 
#   theme(text = element_text(size = 13))
# plot(p)
# ggsave(here("output", "plots", paste0(results_files, "_comparison", ".pdf")),
#        plot = p, width = 12, height = 8)
# 
# 

