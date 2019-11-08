# Script to plot all of the indices together

library(tidyverse)

# Load data and format for combining
gitdir <- "C:/Users/Elizabeth Ng/Documents/GitHub/predator-diets"
dietindexr <- readr::read_rds(file.path(gitdir, "output", "diet_index.rds"))
overlapindexr <- readr::read_rds(file.path(gitdir, "output", "overlap_index.rds"))
assessdatr <- readxl::read_xlsx(here::here("data", "TimeSeries.xlsx"))

# Format for combining
dietindex <- dietindexr %>%
  # dplyr::filter(is.na(reason)) %>%
  mutate(density = ifelse(is.na(reason), density, NA)) %>%
  select(-SD_log, -SD_mt, -reason) %>%
  mutate(density = scale(density)[,1]) %>%
  rename(`diet index` = density) %>%
  pivot_longer(cols = `diet index`, names_to = "index", values_to = "value")

overlapindex <- overlapindexr %>%
  group_by(season) %>%
  mutate(bhat = scale(bhat)[,1]) %>%
  ungroup() %>%
  rename(species = pred, 
         `overlap index` = bhat) %>%
  pivot_longer(cols = `overlap index`, names_to = "index", values_to = "value")


overlap_diet_comp <- bind_rows(dietindex, overlapindex) %>%
  mutate(full_name = paste0(name, ", ", index)) %>%
  dplyr::filter(species != "white hake", species != "silver hake")
  
overlap_diet_comp %>%
  ggplot(aes(x = year, y = value, group = full_name, color = index)) +
  geom_line() +
  facet_grid(species ~ season) +
  theme_bw()
ggsave(file.path(gitdir, "output", "overlap-diet-comparison.pdf"), width = 10, height = 6, units = "in")




assessdat <- assessdatr %>%
  dplyr::filter(Year > 1972) %>%
  pivot_longer(cols = -Year, names_to = "index", values_to = "value") %>%
  group_by(index) %>%
  mutate(value = scale(value)[,1]) %>%
  ungroup() %>%
  rename(year = Year) %>%
  mutate(name = index)





indexdat <- bind_rows(dietindex, overlapindex, assessdat) %>%
  mutate(composite_name = paste(name, index, ))

# Make a plot with everything
# (will ggplot know to include the NA stuff in all the plots??)

ggplot(indexdat, aes(x = year, y = value, group = name, color = name)) +
  geom_line() +
  facet_wrap(~name)






