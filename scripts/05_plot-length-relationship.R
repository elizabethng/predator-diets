# Check and plot predator covariates
# Did not keep size category coefficeints 
# !! [ ] could change that
# So just plot lengths for all

library("tidyverse")
library("here")


# Raw data for length information
topdiet <- readr::read_rds(here("output", "top_diet.rds"))

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
  unnest()

# Join in covariates for calculation
preddat <- lendat %>%
  select(species, season, year, pdlen, declon, declat) %>%
  left_join(lencoefs, by = c("season", "species")) %>%
  mutate(
    mean_len = mean(pdlen),
    sd_len = sd(pdlen),
    z_calc = (pdlen - mean_len)/sd_len,
    pred_val = pdlenz*z_calc,
    pred_val2 = pdlenz2*(z_calc^2)
  ) %>%
  mutate(
    len_effect = pred_val*sd_len + mean_len,
    len_effect2 = pred_val2*sd_len + mean_len
  ) %>%
  na.omit() %>%
  select(species, season, year, pdlen, len_effect, len_effect2, predictor) %>%
  pivot_longer(cols = c(len_effect, len_effect2), names_to = "length", values_to = "predicted_value")

ggplot(preddat, aes(x = pdlen, y = predicted_value, color = length)) +
  geom_point() +
  facet_grid(species~predictor)

ggplot(preddat, aes(x = pdlen, y = predicted_value, color = length)) +
  geom_point() +
  facet_grid(species~predictor)

  

preddat %>% 
  sample_n(100) %>%
  ggplot(aes(x = z_calc, y = pred_val)) +
  geom_point()

preddat %>% 
  sample_n(100) %>%
  ggplot(aes(x = z_calc, y = pred_val2)) +
  geom_point()



preddat %>% 
  sample_n(100) %>%
  ggplot(aes(x = pdlen, y = len_effect)) +
  geom_point()

preddat %>% 
  sample_n(100) %>%
  ggplot(aes(x = pdlen, y = len_effect2)) +
  geom_point()





# Quick map of length distribution
  
  
  
  
  
  
  




  


# ISSUE--come back to this with more time
#      --problem was myspecies vs species
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
