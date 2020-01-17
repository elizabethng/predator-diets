# Plot covariate effects on linear predictors, including 95% CI a
# and rug plot showing data density

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
  filter(length > min & length < max) %>%
  rename(
    Season = season,
    Effect = effect, 
    "Length (cm)" = length
  ) %>%
  mutate(
    predator = str_to_sentence(predator), 
    predictor = ifelse(predictor == "presence", 1, 2)
  )

obslenplot <- obslen %>%
  rename(Season = season) %>%
  mutate(predator = str_to_sentence(predator))

ggplot(obslen, aes(x = pdlen)) + 
    geom_histogram(bins = 30) +
    facet_grid(season ~ predator) +
    geom_rug(size = 0.01, color = "#D3D3D399")

p <- ggplot(plotdat, aes(x = `Length (cm)`, y = Effect, group = paste(Season, predator, predictor), color = Season)) +
  geom_line() +
  geom_ribbon(aes(ymin = lcb, ymax = ucb, fill = Season), alpha = 0.1, color = NA) +
  geom_rug(data = obslenplot, 
           aes(x = pdlen), 
           inherit.aes = FALSE,
           size = 0.1,
           color = "#D3D3D388") +
  facet_grid(predictor ~ predator, 
             scales = "free_x",
             labeller = label_bquote(italic(p[.(predictor)]))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.spacing.x = unit(0.8, "lines")) # handle label overlap
ggsave(plot = p, filename = here("output", "plots", "length-effects.pdf"), width = 10, height = 4, units = "in")


# Monte Carlo prediction bounds -------------------------------------------

# Ideally want to generate coefficients from vcov of pdlenz_se and pdlenz2_se
# Then use all of those to transform a set of zscores back to the correct scale


# 1. Get estimated varaince-covariance matrices and mean vectors
vcovs <- topdiet %>%
  mutate(output = purrr::map(output, "result")) %>%
  mutate(vcov = purrr::map(output, "covar_vcov")) %>%
  select(predator, season, vcov)

means <- lencoefs %>%
  pivot_wider(names_from = predictor, values_from = c(pdlenz, pdlenz2))
  
# 2. For first pass, isolate silver hake in spring
hakevcov <- vcovs$vcov[[1]]
hakemean <- filter(means, season == "spring", predator == "silver hake") %>%
  select(-season, -predator) %>%
  slice(1) %>%
  unlist()

# 3. Simulate new parameter values
simcovs <- MASS::mvrnorm(n = 1, mu = hakemean, Sigma = hakevcov)

# 4. Get predicted values for zscores
hakelen <- filter(lendat, season == "spring", predator == "silver hake")
simcovs

poop <- expand_grid(hakelen, z_score = seq(-5, 5, length.out = 50)) %>%
  mutate(length = sd*z_score + mean,
         pred1 = simcovs["pdlenz_pred1"]*z_score + simcovs["pdlenz2_pred1"]*(z_score^2),
         pred2 = simcovs["pdlenz_pred2"]*z_score + simcovs["pdlenz2_pred2"]*(z_score^2))

ggplot(poop, aes(x = length, y = pred1)) + geom_line()
ggplot(poop, aes(x = length, y = pred2)) + geom_line()




left_join(lencoefs, sedat, by = c("season", "predator", "predictor")) %>%
  left_join(lendat, by = c("season", "predator")) %>%
  expand_grid(z_score = seq(-5, 5, length.out = 50)) %>%
  mutate(
    length = sd*z_score + mean,
    effect = pdlenz*z_score + pdlenz2*(z_score^2),
    ucb =  effect + 1.96*(pdlenz_se + pdlenz2_se),
    lcb = effect - 1.96*(pdlenz_se + pdlenz2_se)
  ) %>%
  mutate(predictor = ifelse(predictor == "pred1", "presence", "amount"))



