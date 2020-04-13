# Script to do a simple combination of diet indices

# From JT 
# Use a statistical (e.g linear) model with 
# response of abundance index and 
# predictors each diet index, 
# estimate coefficients, 
# and extract prediction as multispecies diet index.

# [ ] What scale should the responses be on? Scaled? Raw?

library("tidyverse")
library("here")


# 0. Load data ------------------------------------------------------------
# Load data and extract indices
# Somehow use weights for observation uncertainties??

dietindex <- read_rds(path = here("output", "index_diet.rds"))

assessdat <- readxl::read_xlsx(here("data", "raw", "TimeSeries.xlsx")) %>%
  select(Year, `SSB (mt)`) %>%
  mutate(stock_index = scale(`SSB (mt)`)[,1]) %>%
  select(-`SSB (mt)`) %>%
  rename(year = Year)


# 1. Combine data sets ----------------------------------------------------
# match year with diet data

moddat <- left_join(dietindex, assessdat, by = "year") %>%
  mutate(year = as.factor(year)) %>%
  drop_na()

ggplot(moddat, aes(stock_index, density, color = season)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  facet_wrap(season ~ predator, nrow = 2, scales = "free_y")


# 2. Fit linear model -----------------------------------------------------

mod <- lm(stock_index ~ predator + season + density, data = moddat)
summary(mod)

# Try a bunch
options(na.action = "na.fail")
m0 <- lm(stock_index ~ predator*season*density, data = moddat) #, na.action = "na.omit")
dd <- MuMIn::dredge(m0, rank = "AIC", fixed = c("density"))

top_mod <- MuMIn::get.models(dd, subset = 1)[[1]]
summary(top_mod)
broom::tidy(top_mod)

# multipred_model <- lm(stock_index ~ predator + diet_index)

# 3. Extract covariates and make predictions ------------------------------
# Do this by leaving out predator effects? Not entirely clear how to do this. 

predmod <- moddat %>%
  mutate(predicted = predict(top_mod))

plot(predmod$year, predmod$predicted)

ggplot(predmod, aes(x = year, y = predicted)) +
  geom_violin()


# Try random effect approach ----------------------------------------------
# Set predator as a random effect? Then integrate out?

# Try normalizing first
moddat_z <- moddat %>%
  group_by(season, predator) %>%
  mutate(density_z = scale(density)[,1],
         index_z = scale(stock_index)[,1])

ran_mod <- lme4::lmer(stock_index ~ density_z + (density_z | predator), moddat_z)
summary(ran_mod)

# But I think slopes are random
ran_mod <- lme4::lmer(stock_index ~ density_z + (1 + density_z | predator), moddat_z)
summary(ran_mod)


