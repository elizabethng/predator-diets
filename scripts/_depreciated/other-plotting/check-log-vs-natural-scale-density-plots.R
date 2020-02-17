# Compare log and natural reposne scales for spatial density

plotdat <- as_tibble(MapDetails_List$PlotDF) %>%
  mutate(
    natural_scale = Report$D_gcy[ , 1, 1],
    log_scale = my_plots[,1]  
  ) %>%
  pivot_longer(cols = c(natural_scale, log_scale), names_to = "type", values_to = "value")
  
ggplot(plotdat, aes(x = Lon, y = Lat, color = value)) +
  geom_point() +
  scale_color_viridis_c() +
  facet_grid(~type)

filter(plotdat, type == "log_scale") %>%
  ggplot(aes(x = Lon, y = Lat, color = value)) +
  geom_point() +
  scale_color_viridis_c() +
  facet_grid(~type)

filter(plotdat, type == "natural_scale") %>%
  ggplot(aes(x = Lon, y = Lat, color = value)) +
  geom_point() +
  scale_color_viridis_c() +
  facet_grid(~type)
