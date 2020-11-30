# Script to make table of parameter estimates for top diet models
# Or I guess I could do them all

library("tidyverse")
library("here")


# Load data --------------------------------------------------------------
# Read in Report results and extract first predictor
reportdat <- tibble(
  locs = here("output", "diagnostics") %>%
    dir()
) %>%
  slice(1) %>%
  # filter(str_starts(locs, "trawl_")) %>%
  mutate(path = here("output", "diagnostics", locs, "Report.rds")) %>%
  rowwise() %>%
  mutate(data = list(read_rds(path))) %>%
  ungroup()

optdat <- tibble(
  locs = here("output", "diagnostics") %>%
    dir()
) %>%
  mutate(path = here("output", "diagnostics", locs, "opt.rds")) %>%
  rowwise() %>%
  mutate(data = list(read_rds(path))) %>%
  ungroup() %>%
  mutate(data = map(data, "SD")) %>%
  mutate(
    par.fixed = map(data, ~pluck(.x, "par.fixed")),
    cov.fixed = map(data, ~pluck(.x, "cov.fixed")),
    gradient.fixed = map(data, ~pluck(.x, "gradient.fixed"))
    ) %>%
  separate(locs, c(NA, "season", "species"), sep = "_") %>%
  mutate(species = gsub("-", " ", species)) %>%
  select(-path, -data)

