# Plot observations by Julian Day to check for seasonal distribution

library(here)
library(tidyverse)

save_output = FALSE

dat = read_rds(here("output", "data_formatted", "dat_preds.rds")) %>%
  pluck("ATLANTIC COD")

dates = select(dat, season, year, month, day, pyamtw)
dates$date = with(dates, as.Date(paste(year, month, day, sep='-')))
dates = dates %>%
  mutate(julian_day = format(dates$date, "%j"),
         julian_day = as.numeric(julian_day))


ggplot(dates, aes(julian_day, fill = factor(year))) + 
  geom_histogram(bins = 365) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_vline(xintercept =  60, color = "blue") +      # Start of Spring (Mar 1)
  geom_vline(xintercept = 152, color = "green") +     # Start of Summer (Jun 1)
  geom_vline(xintercept = 244, color = "orange") +    # Start of Fall (Sep 1)
  geom_vline(xintercept = 335, color = "red")         # Start of Winter (Dec 1)
  
  
  
ggplot(dates, aes(julian_day, fill = factor(season))) + 
  geom_histogram(bins = 365) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_vline(xintercept =  60, color = "blue") +      # Start of Spring (Mar 1)
  geom_vline(xintercept = 152, color = "green") +     # Start of Summer (Jun 1)
  geom_vline(xintercept = 244, color = "orange") +    # Start of Fall (Sep 1)
  geom_vline(xintercept = 335, color = "red")         # Start of Winter (Dec 1)

if(save_output) ggsave(here("output", "eda", "samples_by_season.jpg"))

# Maybe try both groupings
# Spring + Summer and Fall + Winter seems more natural, but based on when sampling occurs
# Winter + Spring and Summer + Fall actually seems more natural


# Look at year by season interaction (missing lots of positive observations)
ggplot(dates, aes(julian_day)) + 
  geom_histogram(bins = 365) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_vline(xintercept = 152, color = "red") +     # Start of Summer (Jun 1)
  facet_wrap(~year) +
  theme_bw()

ggplot(dates, aes(x = julian_day)) + 
  geom_bar(aes(weight = pyamtw)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_vline(xintercept = 152, color = "red") +     # Start of Summer (Jun 1)
  facet_wrap(~year) +
  theme_bw()

# Make a table of values
obs_summary = dat %>%
  mutate(season_group = ifelse(season == "WINTER" | season == "SPRING", "SPRING", "FALL")) %>%
  # mutate(season_group = ifelse(season == "WINTER" | season == "FALL", "FALL", "SPRING")) %>%
  group_by(year, season_group) %>%
  summarize(
    n_obs = n(),
    n_prey = sum(pypres),
    g_prey = sum(pyamtw))

xtabs(n_obs ~ year + season_group, data = obs_summary)

# Fall has 5 zeros in a row (1980 - 1984)
# Spring has 2 zeros in a row (1977 to 1978)


