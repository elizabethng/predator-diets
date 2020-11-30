# Script to make table of parameter estimates for top diet models
# Or I guess I could do them all

library("tidyverse")
library("here")


# Functions ---------------------------------------------------------------
# Extract and format SD report fixed effects results
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

# Load data --------------------------------------------------------------
# Read in all opt output, which contain SD reports
optdat <- tibble(
  locs = here("output", "diagnostics") %>%
    dir()
) %>%
  mutate(path = here("output", "diagnostics", locs, "opt.rds")) %>%
  rowwise() %>%
  mutate(data = list(read_rds(path))) %>%
  ungroup() %>%
  mutate(
    data = map(data, "SD"),
    data = map(data, ~format_sd_report(.x))
  ) %>%
  separate(locs, c("model", "season", "species"), sep = "_") %>%
  mutate(species = gsub("-", " ", species)) %>%
  select(-path) %>%
  unnest(data)


# Format table for output -------------------------------------------------

