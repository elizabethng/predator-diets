# Example plot for covariate effects
# In cpp file, they are defined as
# vector<Type> zeta1_i = Q_ik * lambda1_k.matrix();
# vector<Type> zeta2_i = Q_ik * lambda2_k.matrix();
# parameter estimates from D:\Dropbox\Predator_Diets\2019-10-29-new_test\atlantic-cod_season-spring_covar-int-pdlenz-pdlenz2_gamma-pl-independent-years-no2spatial

library(tidyverse)

# Effect on probability of presence
lambda1_1 <- 7.876052e-01 
lambda1_2 <- -3.066623e-01

# Effect on positive model component
lambda2_1 <- 9.790713e-01
lambda2_2 <- -5.540185e-02

# predator lengths are z-scaled from cm
covdat <- tibble(pdlen_z = seq(-2, 3, 0.1)) %>%
  mutate(pres_effect = (lambda1_1*pdlen_z + lambda1_2*pdlen_z^2)) %>%
  mutate(pos_effect = (lambda2_1*pdlen_z + lambda2_2*pdlen_z^2)) %>%
  pivot_longer(cols = contains("_effect"), names_to = "type", values_to = "effect") %>%
  mutate(exp_effect = exp(effect))

ggplot(covdat, aes(x = pdlen_z, y = effect, color = type)) +
  geom_line() +
  theme_bw()
gitdir <- "C:/Users/Elizabeth Ng/Documents/GitHub/predator-diets"
ggsave(file.path(gitdir, "output", "cov-effect-cod-spring.pdf"), width = 6, height = 4, units = "in")

ggplot(covdat, aes(x = pdlen_z, y = exp_effect, color = type)) +
  geom_line() +
  theme_bw()

