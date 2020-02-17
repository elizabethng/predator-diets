# Process diet index data
# 1. Extract and save index
# 2. Make annually-averaged map
# 3. Make predator time-series maps

library("rnaturalearth")
library("tidyverse")
library("here")
library("sf")


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
  geom_errorbar(aes(ymin = (Density - density_se), 
                    ymax = (Density + density_se), 
                    color = Season),
                width = 0) +
  facet_wrap(~ predator, scales = "free_y") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())
ggsave(plot = p, filename = here("output", "plots", "diet-index-ts.pdf"), width = 10, height = 6, units = "in")

# Compare with outliers removed
plot_dietindex %>%
  filter(!is.na(Density)) %>%
  filter(density_se < 900) %>%
  ggplot(aes(x = Year, y = Density, color = Season)) +
  geom_point() +
  geom_errorbar(aes(ymin = (Density - density_se), 
                    ymax = (Density + density_se), 
                    color = Season),
                width = 0) +
  facet_wrap(~ predator, scales = "free_y") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())


# Annually-averaged map ---------------------------------------------------




ggsave(plot = p, 
       filename = here("output", "plots", "overlap-ts",
                       paste0(gsub(" ", "-", paste(preds, seas)), ".pdf")),
       width = 12, height = 10, units = "in")