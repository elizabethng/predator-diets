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


# Write function to get output from each SD report ------------------------
res <- tibble(
  locs = here("output", "diagnostics") %>%
    dir()
) %>%
  slice(1) %>%
  mutate(path = here("output", "diagnostics", locs, "opt.rds")) %>%
  rowwise() %>%
  mutate(data = list(read_rds(path))) %>%
  pull(data)

jj <- res[[1]]$SD

est <- jj$par.fixed
ses <- jj$cov.fixed %>% diag() %>% sqrt()
grd <- jj$gradient.fixed

tibble(
  parameter = names(est),
  estimate = est,
  est_se = ses,
  gradient = grd)

format_sd_report <- function(SD){
  est <- SD$par.fixed
  ses <- SD$cov.fixed %>% diag() %>% sqrt()
  grd <- SD$gradient.fixed
  
  out <- tibble(
    parameter = names(est),
    estimate = est,
    est_se = ses,
    gradient = grd
    )
  
  return(out)
}
pp <- format_sd_report(jj)
