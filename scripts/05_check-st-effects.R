# Check magnitude of ST effects for top models

library("tidyverse")
library("here")

topdiet <- readr::read_rds(here("output", "top_diet.rds"))
toptrawl <- readr::read_rds(here("output", "top_trawl.rds"))

# Check convergence
topdiet %>%
  select(season, species, output) %>%
  transmute(converged = purrr::map_lgl(output, "converged"))

toptrawl %>%
  select(season, species, output) %>%
  transmute(converged = purrr::map_lgl(output, "converged"))


# Check magnitude of random effects
topdiet$model

topdiet %>%
  select(season, species, output) %>%
  transmute(hatval = purrr::map(output, "estimates")) %>%
  unnest(cols = c(hatval)) %>%
  ungroup() %>%
  filter(covariate == "epsilon") %>%
  mutate(
    estimate = abs(estimate),
    check = estimate > 0.001) %>%
  filter(estimate != 1)  # default value = 1, only dogfish should have pred2


# Likely related to the amount of data
topdiet %>%
  select(-output, -aic) %>%
  mutate(
    n_tows = purrr::map_int(data, nrow),
    p_pos = purrr::map(data, "pypres"),
    p_pos = purrr::map_dbl(p_pos, mean)) %>%
  select(-data) %>%
  arrange(desc(n_tows)) %>%
  write_csv(here("output","aic", "diet-table.csv"))
  


# Check magnitude of random effects
toptrawl$model

toptrawl %>%
  select(season, species, output) %>%
  transmute(hatval = purrr::map(output, "estimates")) %>%
  unnest(cols = c(hatval)) %>%
  ungroup() %>%
  filter(covariate == "epsilon") %>%
  mutate(
    estimate = abs(estimate),
    check = estimate > 0.001)
