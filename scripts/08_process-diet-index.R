# Process diet index data
# 1. Extract and save index
# 2. Plot diet index

library("tidyverse")
library("here")


# 0. Load data ------------------------------------------------------------
topdiets <- readr::read_rds(here::here("output", "top_final_diet.rds"))


# 1. Extract and save annual index ----------------------------------------
# Note that exclude_reason is in here

dietindex <- topdiets %>%
  dplyr::select(season, predator, output) %>%
  dplyr::mutate(output = purrr::map(output, "result")) %>%
  dplyr::mutate(output = purrr::map(output, "index")) %>%
  unnest(cols = c(output)) %>%
  select(-Unit, -Fleet, -SD_log) %>%
  rename(year = Year,
         density = Estimate_metric_tons,
         density_se = SD_mt) %>%
  mutate(density = ifelse(is.na(exclude_reason), density, NA),
         density_se = ifelse(is.na(exclude_reason), density_se, NA)) %>%
  select(-exclude_reason)
  # mutate(density_cv = ) # hmm I need n, but what is that here??
write_rds(dietindex, path = here("output", "index_diet.rds"))



# Plot diet-based abundance index -----------------------------------------

plot_dietindex <- dietindex %>%
  rename(
    Year = year,
    Season = season,
    Density = density
  ) %>%
  mutate(
    predator = str_to_sentence(predator), 
    Season = str_to_sentence(Season)
  )

p <- ggplot(plot_dietindex, aes(x = Year, y = Density, color = Season)) +
  geom_point() +
  scale_color_manual(values = c(scales::muted("blue", l = 50, c = 100), scales::muted("red", l = 50, c = 100))) +
  geom_errorbar(aes(ymin = (Density - density_se), 
                    ymax = (Density + density_se), 
                    color = Season),
                width = 0) +
  facet_wrap(~ predator, scales = "free_y") +
  labs(y = "Mass of Atlantic herring in predator stomachs") +
  theme_bw() +
  theme(legend.position = c(0.8, 0.2),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())
ggsave(plot = p, filename = here("output", "plots", "diet-index-ts.pdf"), width = 9, height = 5, units = "in")



# Simpler BW option -------------------------------------------------------
# 
# ggplot(plot_dietindex, aes(x = Year, y = Density)) +
#   geom_point() +
#   geom_line() +
#   geom_errorbar(aes(ymin = (Density - density_se), 
#                     ymax = (Density + density_se)),
#                 width = 0) +
#   facet_grid(rows = c(vars(predator), vars(Season)), scale = "free_y") +
#   labs(y = "Diet index") +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         strip.background = element_blank())
# 
# 
# # Check error bars --------------------------------------------------------
# filter(plot_dietindex, predator == "Goosefish") %>%
#   drop_na() %>%
#   mutate(ymin = Density - density_se,
#          ymax = Density + density_se) %>%
# ggplot(aes(x = Year, y = Density, color = Season)) +
#   # geom_point() +
#   geom_errorbar(aes(ymin = ymin, 
#                     ymax = ymax, 
#                     color = Season),
#                 width = 0) +
#   labs(y = "Diet index") +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         strip.background = element_blank())
