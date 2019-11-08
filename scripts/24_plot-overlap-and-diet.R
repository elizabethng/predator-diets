# Script to plot all of the indices together

library(tidyverse)

# Load data and format for combining
gitdir <- "C:/Users/Elizabeth Ng/Documents/GitHub/predator-diets"
dietindexr <- readr::read_rds(file.path(gitdir, "output", "diet_index.rds"))
overlapindexr <- readr::read_rds(file.path(gitdir, "output", "overlap_index.rds"))
assessdatr <- readxl::read_xlsx(here::here("data", "TimeSeries.xlsx"))

# Format for combining
dietindex <- dietindexr %>%
  dplyr::filter(is.na(reason)) %>%
  select(-SD_log, -SD_mt, -reason) %>%
  mutate(density = scale(density)[,1]) %>%
  rename(`diet index` = density) %>%
  pivot_longer(cols = `diet index`, names_to = "index", values_to = "value")

overlapindex <- overlapindexr %>%
  mutate(bhat = scale(bhat)[,1]) %>%
  rename(species = pred, 
         `overlap index` = bhat) %>%
  pivot_longer(cols = `overlap index`, names_to = "index", values_to = "value")

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






