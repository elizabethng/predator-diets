# Script to plot index comparisons
# 1. plot time series comparison of diet index and overlap index
# 2. plot one to one comparison of diet index and overlap index
# 3. plot time series comparison of diet index and assessment biomass index
# 4. plot one to one comparison of diet index and assessment biomass index


library("tidyverse")
library("here")

# Load data and format for combining
dietindexr <- read_rds(here::here("output", "index_diet.rds"))
overlapindexr <- read_rds(here::here("output", "index_overlap.rds"))
assessdatr <- readxl::read_xlsx(here("data", "raw", "TimeSeries.xlsx"))


# Format for combining
# 1. scale time series
# 2. rename and formate to shared index, value columns

dietindex <- dietindexr %>%
  mutate(density = ifelse(is.na(exclude_reason), density, NA)) %>%
  select(-SD_log, -SD_mt, -exclude_reason, -name) %>%
  group_by(species, season) %>%
  mutate(density = scale(density)[,1]) %>%
  ungroup() %>%
  rename(diet = density, pred = species) %>%
  pivot_longer(cols = diet, names_to = "index", values_to = "value")

overlapindex <- overlapindexr %>%
  group_by(pred, season) %>%
  # rename(overlap = `overlap metric`) %>%
  mutate(overlap = scale(`overlap metric`)[,1]) %>% # try without scaling first
  select(-`overlap metric`) %>%
  ungroup() %>%
  pivot_longer(cols = overlap, names_to = "index", values_to = "value")


# Combine diet data and overlap data
overlap_diet_comp <- bind_rows(dietindex, overlapindex) %>%
  rename(predator = pred) %>%
  mutate(full_name = paste0(predator, ", ", season, ", ", index, " index"))
# %>%
#   dplyr::filter(species != "white hake", species != "silver hake")


ggplot(overlap_diet_comp, 
       aes(x = year, y = value, group = full_name, color = index)) +
  geom_line(size = 1) +
  geom_point() +
  facet_grid(predator ~ season) +
  theme_bw()

# overlap_diet_comp %>%
#   ggplot(aes(x = year, y = value, group = full_name, color = index)) +
#   geom_line(size = 1) +
#   scale_color_manual(
#     values = c(
#       "diet index" = "black",
#       "overlap index" = viridisLite::viridis(n = 200, option = "plasma")[70]
#       )
#     ) +
#   geom_point() +
#   facet_grid(predator ~ season) +
#   theme_bw()
ggsave(here("output", "plots", "overlap-diet-comp-ts.pdf"), width = 10, height = 6, units = "in")

# One to one plot
overlap_diet_comp %>% 
  pivot_wider(id_cols = c(year, predator, season), names_from = index, values_from = value) %>%
  ggplot(aes(x = overlap, y = diet)) +
  geom_point() +
  # scale_color_viridis_c(option = "plasma") +
  geom_abline(color = "lightgrey") +
  scale_x_continuous(limits = c(-5.13, 5.13)) +
  scale_y_continuous(limits = c(-5.13, 5.13)) +
  coord_fixed() +
  facet_grid(predator ~ season) +
  theme_bw()
ggsave(here("output", "plots", "overlap-diet-comp-1to1.pdf"), width = 6, height = 8, units = "in")


# Format the assessment data
assessdat <- assessdatr %>%
  dplyr::filter(Year > 1972) %>%
  pivot_longer(cols = -Year, names_to = "index", values_to = "value") %>%
  group_by(index) %>%
  mutate(value = scale(value)[,1]) %>%
  ungroup() %>%
  rename(year = Year)

ssbdat <- assessdat %>%
  dplyr::filter(index == "SSB (mt)")

ggplot(data = dietindex, aes(x = year, y = value, group = name)) +
  geom_line() +
  geom_point() + 
  geom_point(data = dietindex, aes(x = year, y = value)) +
  facet_grid(pred ~ season) +
  theme_bw()

# assessdat %>%
#   dplyr::filter(index == "Jan.1 Biomass (mt)") %>%
#   ggplot(aes(x = year, y = value)) +
#   geom_line(
#     color =  "firebrick", 
#     size = 1
#   ) +
#   geom_line(
#     data = dietindex,
#     aes(x = year, y = value, group = name),
#     inherit.aes = FALSE,
#     color = "black",
#     size = 1
#   ) +
#   geom_point(
#     data = dietindex, 
#     aes(x = year, y = value), 
#     inherit.aes = FALSE,
#     color = "black",
#     size = 1.5) +
#   facet_grid(predator ~ season) +
#   theme_bw()
ggsave(file.path(gitdir, "output", "assess-diet-comp-ts.pdf"), width = 10, height = 6, units = "in")



# One to one plot
assess_diet_comp <- assessdat %>%
  dplyr::filter(index == "Jan.1 Biomass (mt)") %>%
  right_join(dietindex, by = "year") %>%
  rename(
    `Jan.1 Biomass (mt)` = value.x,
    `diet index` = value.y
  ) %>%
  select(-index.x, -index.y)

assess_diet_comp %>% 
  ggplot(aes(x = `Jan.1 Biomass (mt)`, y = `diet index`, color = year)) +
  geom_point() +
  scale_color_viridis_c(option = "plasma") +
  geom_abline(color = "lightgrey") +
  scale_x_continuous(limits = c(-5.13, 5.13)) +
  scale_y_continuous(limits = c(-5.13, 5.13)) +
  coord_fixed() +
  facet_grid(predator ~ season) +
  theme_bw()
ggsave(file.path(gitdir, "output", "assess-diet-comp-1to1.pdf"), width = 6, height = 8, units = "in")




