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
write_rds(dietindex, path = here("output", "index_diet.rds"))


# Format index for plotting -----------------------------------------------
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


# Format assessment data --------------------------------------------------
assessdatr <- readxl::read_xlsx(here("data", "raw", "TimeSeries.xlsx"))
pred_seas <- select(dietindex, predator, season) %>% 
  distinct()

stock_index <- assessdatr %>%
  select(Year, `SSB (mt)`) %>%
  mutate(stock_index = scale(`SSB (mt)`)[,1]) %>%
  select(-`SSB (mt)`) %>%
  rename(year = Year) %>%
  expand_grid(pred_seas, .) %>%
  ungroup() %>%
  mutate(
    predator = str_to_sentence(predator),
    season = str_to_sentence(season)
  ) %>% 
  rename(
    Year = year,
    Season = season
  )

# Scale assessment index to match scale of diet data
mean_dietindex <- plot_dietindex %>%
  drop_na() %>%
  group_by(predator) %>%
  summarize(
    mean = mean(Density),
    sd = sd(Density)
  )

# Re-scale assessment index by mean and sd of diet index
stock_scaled <- left_join(
  stock_index, mean_dietindex, by = c("predator")
) %>%
  mutate(stock_scaled = stock_index*sd + mean)


# Plot diet-based biomass index -----------------------------------------
# Include stock assessment index for comparison
pp <- ggplot(plot_dietindex, aes(x = Year, y = Density, color = Season)) +
  geom_line(
    data = stock_scaled,
    aes(x = Year, y = stock_scaled),
    color = "grey60"
  ) +
  geom_point() +
  scale_color_manual(values = c(scales::muted("blue", l = 50, c = 100), scales::muted("red", l = 50, c = 100))) +
  geom_errorbar(aes(ymin = (Density - density_se), 
                    ymax = (Density + density_se), 
                    color = Season),
                width = 0) +
  facet_wrap(~ predator, scales = "free_y") +
  labs(y = "Diet index") +
  theme_bw() +
  theme(legend.position = c(0.8, 0.2),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())
pp
ggsave(plot = pp, 
       filename = here("output", "plots", "diet-index-ts_w-SSB.pdf"), 
       width = 9, height = 5, units = "in")

