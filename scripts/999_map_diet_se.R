# Make map of diet INDEX and SE using discretized space and quantile color scale
# Based on hex grid calculation for overlap

library("tidyverse")
library("here")


# 0. Load data ------------------------------------------------------------
dietse <- readr::read_rds(path = here("output", "top_se_diet.rds"))

# Do it for the list table
all_data <- dietse %>%
  dplyr::select(season, predator, output) %>%
  dplyr::mutate(output = purrr::map(output, "result")) %>%
  unnest_wider(output) %>%
  mutate(map_log_index_gcy_est = map(map_log_index_gcy_est, ~ rownames_to_column(.x, "x2i")),
         map_log_index_gcy_se = map(map_log_index_gcy_se, ~ rownames_to_column(.x, "x2i"))) %>%
  mutate(map_log_index_gcy_est = map(map_log_index_gcy_est, ~ mutate(.x, x2i = as.integer(x2i))),
         map_log_index_gcy_se = map(map_log_index_gcy_se, ~ mutate(.x, x2i = as.integer(x2i)))) %>%
  mutate(fine_index = map2(fine_scale_locs, map_log_index_gcy_est, ~ left_join(.x, .y, by = "x2i")),
         fine_index_se = map2(fine_scale_locs, map_log_index_gcy_se, ~ left_join(.x, .y, by = "x2i"))) %>%
  select(season, predator, fine_index, fine_index_se) %>%
  pivot_longer(cols = c(fine_index, fine_index_se), names_to = "est", values_to = "value") %>% # also could move this up in chain and reduce the doubling up on stuff
  mutate(value = map(value, ~ pivot_longer(.x,
                                           cols = starts_with("density_"), 
                                           names_to = "year", 
                                           values_to = "value")),
         value = map(value, ~ mutate(.x,
                                     year = gsub("density_", "", year),
                                     year = as.numeric(year)))) %>%
  unnest(value)



# Format for plotting -----------------------------------------------------
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









