# Plot time series results for annual range overlap

library("tidyverse")
library("here")


# Functions ---------------------------------------------------------------
# Function to read rds and extract year effect only (don't need locations for year)
get_year_effects <- function(path){
  res <- read_rds(path)
  out <- res %>%
    select(-data) %>%
    mutate(results = map(results, "annual")) %>%
    unnest(results)
  return(out)
}
# jj <- get_year_effects(here("scripts", "new_overlap-calculation", "output", "atlantic cod_fall.rds"))


# Load results --------------------------------------------------------------
rawres <- here("scripts", "new_overlap-calculation", "output") %>% 
  dir() %>%
  tibble() %>%
  rename(filenames = ".") %>%
  rowwise() %>%
  mutate(
    results = list(get_year_effects(here("scripts", "new_overlap-calculation", "output", filenames)))
  )
  

# Format and plot ---------------------------------------------------------
plotdat <- rawres %>%
  unnest(results) %>%
  select(-filenames) %>%
  mutate(
    season = str_to_sentence(season),
    predator = str_to_sentence(predator)
  ) %>%
  rename(
    Season = season
  )
write_rds(plotdat, here::here("output", "index_range-overlap.rds"))
  
ggplot(plotdat, aes(x = year, y = range_overlap, color = Season, fill = Season)) +
  geom_point() +
  geom_line() +
  geom_ribbon(
    aes(ymin = lcb, ymax = ucb, fill = Season), alpha = 0.3, color = NA
    ) +
  scale_color_manual(
    aesthetics = c("color", "fill"),
    values = c(scales::muted("blue", l = 50, c = 100), 
               scales::muted("red", l = 50, c = 100))
    ) +
  ylab("Range overlap") +
  facet_wrap(~predator) +
  theme_bw() +
  theme(legend.position = c(0.8, 0.2),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())
ggsave(here("output", "plots", "overlap-index-ts_range-overlap.pdf"),
       width = 9, height = 5, units = "in")


# Binomial CIs instead of bootstrap ---------------------------------------

binomdat <- plotdat %>%
  mutate(
    ro_sd = sqrt(range_overlap*(1 - range_overlap)/100), # for 100 bootstrap samples
    lcb_b = range_overlap - 1.96*ro_sd,
    ucb_b = range_overlap + 1.96*ro_sd
  )
ggplot(binomdat, aes(x = year, y = range_overlap, color = Season, fill = Season)) +
  geom_point() +
  geom_line() +
  geom_ribbon(
    aes(ymin = lcb_b, ymax = ucb_b, fill = Season), alpha = 0.3, color = NA
  ) +
  scale_color_manual(
    aesthetics = c("color", "fill"),
    values = c(scales::muted("blue", l = 50, c = 100), 
               scales::muted("red", l = 50, c = 100))
  ) +
  ylab("Range overlap") +
  facet_wrap(~predator) +
  theme_bw() +
  theme(legend.position = c(0.8, 0.2),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())
ggsave(here("output", "plots", "overlap-index-ts_range-overlap.pdf"),
       width = 9, height = 5, units = "in")
