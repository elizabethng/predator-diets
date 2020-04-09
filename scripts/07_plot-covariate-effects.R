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

lengthlimits <- obslen %>% 
  group_by(predator) %>% 
  summarize(min_obs = min(pdlen), max_obs = max(pdlen)) 

obslenplot <- obslen %>%
  rename(Season = season) %>%
  mutate(predator = str_to_sentence(predator))


# Monte Carlo prediction bounds -------------------------------------------

# 1. Get estimated varaince-covariance matrices and mean vectors
vcovs <- topdiet %>%
  mutate(output = purrr::map(output, "result")) %>%
  mutate(vcov = purrr::map(output, "covar_vcov")) %>%
  select(predator, season, vcov)

means <- lencoefs %>%
  pivot_wider(names_from = predictor, values_from = c(pdlenz, pdlenz2)) %>%
  nest(mean_vec = starts_with("pdlenz")) %>%
  mutate(mean_vec = map(mean_vec, unlist)) %>%
  mutate(mean_vec = map(mean_vec, ~ .x[!is.na(.x)]))

mean_vcov <- left_join(vcovs, means, by = c("season", "predator")) %>%
  left_join(lendat, by = c("season", "predator"))

# 2. Generate simulations from parameter values
simpreds <- mean_vcov %>%
  mutate(simcov = pmap(list(
    n = 1000,
    mu = mean_vec,
    Sigma = vcov),
    MASS::mvrnorm)) %>%
  mutate(simcov = map(simcov, as_tibble)) %>%
  mutate(simcov = map(simcov, ~ mutate(.x, sim_id = row_number()))) %>%
  mutate(simcov = map(simcov, ~ pivot_longer(.x, cols = -sim_id, names_to = "type", values_to = "value"))) %>%
  mutate(simcov = map(simcov, ~ separate(.x, type, c("covariate", "predictor")))) %>%
  mutate(simcov = map(simcov, ~ pivot_wider(.x, names_from = c(covariate, predictor), values_from = value))) %>%
  unnest(cols = simcov) %>%
  mutate_all(~ replace_na(.x, 0))


# 3. Get predicted values for zscores
plotdat <- expand_grid(simpreds, z_score = seq(-5, 5, length.out = 50)) %>%
  mutate(length = sd*z_score + mean,
         pred1 = pdlenz_pred1*z_score + pdlenz2_pred1*(z_score^2),
         pred2 = pdlenz_pred2*z_score + pdlenz2_pred2*(z_score^2)) %>%
  select(predator, season, sim_id, length, pred1, pred2) %>%
  pivot_longer(cols = c(pred1, pred2), names_to = "predictor", values_to = "effect") %>%
  left_join(lengthlimits, by = c("predator")) %>%
  filter(length > min_obs & length < max_obs) %>%
  group_by(predator, season, length, predictor) %>%
  summarize(log_effect = mean(effect),
            lcb = quantile(effect, prob = 0.025),
            ucb = quantile(effect, prob = 0.975)) %>%
  ungroup() %>%
  rename(
    Season = season,
    "log Effect" = log_effect, 
    "Length (cm)" = length
  ) %>%
  mutate(
    predator = str_to_sentence(predator), 
    predictor = ifelse(predictor == "pred1", "n", "w"),
    Season = str_to_sentence(Season)
  )

ggplot(plotdat, aes(x = `Length (cm)`, y = `log Effect`, color = Season)) +
  geom_ribbon(aes(ymin = lcb, ymax = ucb, fill = Season), alpha = 0.3, color = NA) +
  geom_line() +
  scale_color_manual(aesthetics = c("color", "fill"),
               values = c(scales::muted("blue", l = 50, c = 100), scales::muted("red", l = 50, c = 100))) +
  facet_grid(predictor ~ predator,
             scales = "free_x",
             labeller = label_bquote(italic(.(predictor)))) + 
  geom_rug(data = obslenplot,
           aes(x = pdlen),
           inherit.aes = FALSE,
           size = 0.1,
           color = "#D3D3D388") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.spacing.x = unit(0.8, "lines"))


# 4. Plot results
p <- ggplot(plotci, aes(x = `Length (cm)`, 
                        y = Effect, 
                        group = paste(Season, predator, predictor), 
                        color = Season)) +
  geom_line(alpha = 0.01) +
  scale_color_manual(values = c(scales::muted("blue", l = 50, c = 100), scales::muted("red", l = 50, c = 100))) +
  geom_rug(data = obslenplot,
           aes(x = pdlen),
           inherit.aes = FALSE,
           size = 0.1,
           color = "#D3D3D388") +
  facet_grid(predictor ~ predator, 
             scales = "free_x",
             labeller = label_bquote(italic(predictor))) +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.spacing.x = unit(0.8, "lines"))

ggsave(plot = p, filename = here("output", "plots", "length-effects.pdf"), width = 9, height = 4, units = "in")
