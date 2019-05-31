# Look at seasonality

library(here)
library(tidyverse)

dat = read_rds(here("output", "data_formatted", "dat_preds.rds")) %>%
  pluck(1)

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

ggsave(here("output", "eda", "samples_by_season.jpg"))

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
