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
# Remove anisotropy parameters becasue I don't discuss them in the paper
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
  unnest(data) %>%
  filter(str_detect(parameter, "input", negate = TRUE))


# Format table for output -------------------------------------------------
# [ ] order is predator name, then spring/fall
# [-] add symbol column --> might just do this manually
# [ ] update column names

param_order <- tribble(
    ~ parameter,    ~ order,
    "L_epsilon1_z", 6,
    "L_epsilon2_z", 7,
    "L_omega1_z",   4,
    "L_omega2_z",   5,
    "logkappa1",    2,
    "logkappa2",    3,
    "logSigmaM",    1
    ) %>%
  mutate(new_name = paste(order, parameter, sep = "_")) %>%
  arrange(new_name) %>%
  select(-order)

formdat <- optdat %>%
  left_join(param_order, by = "parameter") %>%
  select(model, 
         Species = species, 
         Season = season, 
         Parameter = new_name, 
         Estimate = estimate, 
         SE = est_se, 
         Gradient = gradient) %>%
  arrange(Species, desc(Season), Parameter) %>%
  mutate(
    Species = str_to_sentence(Species),
    Season = str_to_sentence(Season)
  )
  
# Split tables and save ---------------------------------------------------
# Diet table
diettab <- formdat %>%
  filter(model == "diet") %>%
  select(-model)

trawltab <- formdat %>%
  filter(model == "trawl") %>%
  select(-model)

write_csv(diettab, here("output", "tables", "param_ests_diet.csv"))
write_csv(trawltab, here("output", "tables", "param_ests_trawl.csv"))

