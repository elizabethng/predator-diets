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

ggplot(moddat, aes(stock_index, density, color = predator)) +
  geom_point()


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




