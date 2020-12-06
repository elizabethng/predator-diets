# Plot individual years of range overlap, predator presence, and prey presence


overlapdat_finescale<- read_rds(here("scripts", "new_overlap-calculation", "overlap-data_finescale.rds"))

ac_overlap <- overlapdat_finescale %>%
  filter(species_pred == "white hake", season == "spring") %>%
  filter(year %in% c(1975, 2015))

ggplot(ac_overlap, aes(x = Lon_pred, y = Lat_pred, z = prob_pred)) +
  stat_summary_hex() +
  scale_fill_viridis_c() +
  facet_wrap(~year) +
  theme_classic()

ggplot(ac_overlap, aes(x = Lon_pred, y = Lat_pred, z = prob_prey)) +
  stat_summary_hex() +
  scale_fill_viridis_c() +
  facet_wrap(~year) +
  theme_classic()

ggplot(ac_overlap, aes(x = Lon_pred, y = Lat_pred, z = prob_pred*prob_prey)) +
  stat_summary_hex() +
  scale_fill_viridis_c() +
  facet_wrap(~year) +
  theme_classic()



# Code from plot SE -------------------------------------------------------
plot_data <- all_data %>%
  rename(estimate = est) %>%
  mutate(estimate = ifelse(estimate == "fine_index", "log_index", "log_index_se"))

# Plot time series for each predator -----------------------------------
plot_specs <- select(plot_data, season, predator, estimate) %>% distinct()

for(pred in unique(plot_specs$predator)){
  for(seas in unique(plot_specs$season)){
    for(est in unique(plot_specs$estimate)){
      tmpdat <- filter(plot_data, predator == pred & season == seas & estimate == est)
      
      tmpplot <- ggplot(tmpdat, aes(x = Lon, y = Lat, z = value)) +
        stat_summary_hex() +
        # geom_point() +
        scale_fill_viridis_c() +
        facet_wrap(~year) +
        labs(fill = est) +
        ggtitle(label = paste(str_to_sentence(pred), seas, sep = "-")) +
        theme_classic()
      
      ggsave(plot = tmpplot, 
             filename = here("output", "plots", "index-ts", 
                             paste0(est, "-", pred, "-", seas, ".png")), 
             width = 8, height = 11, units = "in")
    }
  }
}