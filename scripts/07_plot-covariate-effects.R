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
  mutate(output = purrr::map(output, "result")) %>%
  mutate(estimates = purrr::map(output, "estimates")) %>%
  select(season, predator, estimates) %>%
  unnest(cols = c(estimates)) %>%    
  filter(covariate %in% c("pdlenz", "pdlenz2")) %>% # did I not keep sizecat?
  pivot_wider(names_from = c(covariate), values_from = estimate)

# Do I want to pivot wider again and then append the vcov as another column, simulate data...

# Get length data for back-transforming scaled lengths
obslen <- topdiet %>%
  select(predator, season, data) %>%
  unnest(cols = c(data)) %>%
  select(predator, season, year, pdlen)
# ggplot(obslen, aes(x = pdlen)) + geom_histogram(bins = 30) + facet_grid(season ~ predator)

lendat <- obslen %>%
  group_by(predator, season) %>%
  summarize(
    mean = mean(pdlen),
    sd = sd(pdlen)
  )

sedat <- topdiet %>%
  mutate(output = purrr::map(output, "result")) %>%
  mutate(vcov = purrr::map(output, "covar_vcov")) %>%
  select(predator, season, vcov) %>%
  mutate(vcov = map(vcov, diag)) %>%
  mutate(vcov = map(vcov, enframe)) %>%
  unnest(cols = c(vcov)) %>%
  mutate(predictor = ifelse(str_starts(name, "lambda1"), "pred1", "pred2")) %>%
  mutate(covariate = rep(c("pdlenz_se", "pdlenz2_se"), 16)) %>% # !! hardcoded here!
  rename(estimate = value) %>%
  select(-name) %>%
  pivot_wider(names_from = c(covariate), values_from = estimate) %>%
  mutate(pdlenz_se = sqrt(pdlenz_se), pdlenz2_se = sqrt(pdlenz2_se))
  
# Join data for plotting
alldat <- left_join(lencoefs, sedat, by = c("season", "predator", "predictor")) %>%
  left_join(lendat, by = c("season", "predator")) %>%
  expand_grid(z_score = seq(-5, 5, length.out = 50)) %>%
  mutate(
    length = sd*z_score + mean,
    effect = pdlenz*z_score + pdlenz2*(z_score^2),
    ucb =  effect + 1.96*(pdlenz_se + pdlenz2_se),
    lcb = effect - 1.96*(pdlenz_se + pdlenz2_se)
  ) %>%
  mutate(predictor = ifelse(predictor == "pred1", "presence", "amount"))

lengthlimits <- obslen %>% 
  group_by(predator, season) %>% 
  summarize(min = min(pdlen), max = max(pdlen))

plotdat <- left_join(alldat, lengthlimits, by = c("predator", "season")) %>%
  filter(length > min & length < max)

# ggplot(obslen, aes(x = pdlen)) + 
#   geom_histogram(binwidth = 1) + 
#   facet_grid(season ~ predator) +
#   geom_rug()

ggplot(plotdat, aes(x = length, y = effect, group = paste(season, predator, predictor), color = season)) +
  geom_line() +
  geom_ribbon(aes(ymin = lcb, ymax = ucb, fill = season), alpha = 0.1, color = NA) +
  geom_rug(data = obslen, aes(x = pdlen), inherit.aes = FALSE) +
  facet_grid(predictor ~ predator, scales = "free_x") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())
        # panel.border = element_blank())
        # axis.line = element_line())
ggsave(here("output", "plots", "length-effects.pdf"), width = 6, height = 3.5, units = "in")

  

# Try to plot on probability scale
responsedat <- alldat %>%
  mutate(probability = 1-exp(-exp(effect)),
         prob_ucb = 1-exp(-exp(ucb)),
         prob_lcb = 1-exp(-exp(lcb)))
ggplot(responsedat, aes(x = length, y = probability, group = paste(season, predator, predictor), color = season)) +
  geom_line() +
  geom_ribbon(aes(ymin = prob_lcb, ymax = prob_ucb, fill = season), alpha = 0.1, color = NA) +
  facet_grid(predictor ~ predator, scales = "free_x") +
  theme_bw()

