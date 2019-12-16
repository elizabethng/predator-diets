# Check and plot predator covariates
# Did not keep size category coefficeints 
# !! [ ] could change that
# So just plot lengths for all

library("tidyverse")
library("here")


# Raw data for length information
topdiet <- readr::read_rds(here("output", "top_cov_diet.rds"))

# Get length covariates
lencoefs <- topdiet %>%
  mutate(estimates = purrr::map(output, "estimates")) %>%
  select(season, species, estimates) %>%
  unnest(cols = c(estimates)) %>% 
  filter(covariate %in% c("pdlenz", "pdlenz2")) %>%
  pivot_wider(names_from = c(covariate), values_from = estimate)
  
# Get length data
lendat <- topdiet %>%
  select(data) %>%
  unnest() %>%
  select(species, season, pdlen) %>%
  summarize(
    mean = mean(pdlen),
    sd = sd(pdlen)
  )
  
alldat <- left_join(lencoefs, lendat, by = c("season", "species")) %>%
  expand_grid(z_score = seq(-2, 2, length.out = 30)) %>%
  mutate(
    length = sd*z_score + mean,
    effect = pdlenz*z_score + pdlenz2*(z_score^2)
  ) %>%
  mutate(predictor = ifelse(predictor == "pred1", "presence", "amount"))



ggplot(alldat, aes(x = length, y = effect, group = paste(season, species, predictor), color = season)) +
  geom_line() +
  facet_grid(predictor ~ species, scales = "free_x") +
  theme_bw()
ggsave(here("output", "plots", "length-effects.pdf"), width = 6, height = 3.5, units = "in")

  

# Try to plot on probability scale
alldat <- alldat %>%
  mutate(probability = 1-exp(-exp(effect)))
ggplot(alldat, aes(x = length, y = probability, group = paste(season, species, predictor), color = season)) +
  geom_line() +
  facet_grid(predictor ~ species, scales = "free_x") +
  theme_bw()


# ISSUE--come back to this with more time
# - problem was myspecies vs species
# - renaming one will probably fix this and let me use the 
#   standard syntax (unnest(cols = c(data)))
topdiet %>%
  select(data) %>%
  unnest(cols = c(data))

topdiet %>%
  select(data) %>%
  unnest()

topdiet %>%
  select(species, season, data) %>%
  unnest(cols = c(species, season, data))

topdiet %>%
  rename(myseason = season) %>%
  unnest(cols = c(data))
