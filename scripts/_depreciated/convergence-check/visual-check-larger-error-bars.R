# Check whether data quantity is causing big error bounds in estimation

library("tidyverse")
library("here")
library("sf")

dietindex <- readr::read_rds(here::here("output", "index_diet.rds"))

poop <- dietindex %>%
  mutate(ratio = density_se/density) 

poop %>%
  pull(ratio) %>%
  hist(breaks = 30)

filter(poop, ratio > 0.65)

ggplot(poop, aes(x = year, y = density, color = density)) +
  scale_color_viridis_c() +
  geom_point() +
  geom_errorbar(aes(ymin = (density - density_se), 
                    ymax = (density + density_se)),
                width = 0) +
  facet_wrap(~ predator, scales = "free_y") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())

# years to check
# atlantic cod spring 1984
top_n(dietindex, n = 20, wt = density_se)

# Just match them with the data...
topdiets <- readr::read_rds(here::here("output", "top_final_diet.rds")) 

sumdat <- topdiets %>%
  select(predator, season, processed_data) %>%
  unnest(processed_data) %>%
  mutate(positive_catch = Catch_KG > 0) %>%
  group_by(predator, season, Year) %>%
  summarize(pos_obs = sum(positive_catch),
            tot_obs = n()) %>%
  rename(year = Year)

combdat <- left_join(dietindex, sumdat, by = c("predator", "season", "year")) %>%
  mutate(color = pos_obs < 2)

ggplot(combdat, aes(x = pos_obs, y = density_se, color= color)) + 
  geom_point()

# current number of trawls used
filter(combdat, !is.na(pos_obs)) %>% nrow()

# reduce to at least 3 observations
# reduces it by 100!
filter(combdat, !is.na(pos_obs) & pos_obs > 2) %>% nrow()

# what about just having at least 2 obs?
filter(combdat, !is.na(pos_obs) & pos_obs > 1) %>% nrow()

# Look at the plot of that
filter(combdat, !is.na(pos_obs) & pos_obs > 1) %>%
  ggplot(aes(x = year, y = density, color = season)) +
  geom_point() +
  geom_errorbar(aes(ymin = (density - density_se), 
                    ymax = (density + density_se),
                    color = season),
                width = 0) +
  facet_wrap(~ predator, scales = "free_y") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())

# One last check, color them by number of observations
coldat <- combdat %>%
  mutate(color = ifelse(pos_obs == 1, "1", 
                        ifelse(pos_obs >= 2 & pos_obs <= 5 , "2 to 5",
                               "> 5"))) %>%
  mutate(color = factor(color, levels = c("1", "2 to 5", "> 5")))

ggplot(coldat, aes(x = year, y = density, color = color)) +
  geom_point() +
  geom_errorbar(aes(ymin = (density - density_se), 
                    ymax = (density + density_se),
                    color = color),
                width = 0) +
  facet_wrap(~ predator, scales = "free_y") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())

