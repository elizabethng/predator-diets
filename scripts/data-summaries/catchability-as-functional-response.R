# A litte script to see if I could use catchability directly as a functional response. 
# Unfortunately it doesn't seem that they have the same shapes. 


library(tidyverse)

calc_q = function(N, beta, a = 1){
  q = a*N^(beta-1)
  return(q)
}

# plot q for a variety of beta values
N = seq(1, 20, length.out = 20)
beta = c(1, 1.1, seq(-0.5, 1.5, length.out = 20))
alpha = c(0.5, 0.8, 1, 1.1)

test_results = expand.grid(
  N = N, 
  beta = beta,
  alpha = alpha)

test_results = test_results %>%
  mutate(q = calc_q(N = N, beta = beta, a = alpha)) %>%
  mutate(cpue = q*N)

test_results %>%
  filter(beta == 1) %>%
  ggplot(aes(x = N, y = q, group = beta, color = beta)) +
  geom_line() +
  facet_grid(~alpha)

test_results %>%
  filter(alpha == 1) %>%
  filter(beta %in% c(1, -0.5, 1.1)) %>%
  ggplot(aes(x = N, y = cpue, group = beta, color = factor(beta))) +
  geom_line()


calc_fr = function(a, R, h){
  fr = a*R/(1 + a*h*R)
  return(fr)
}

test_results = test_results %>%
  mutate(fr = calc_fr(a = alpha, R = N, h = beta))

test_results %>%
  filter(beta == 1) %>%
  ggplot(aes(x = N, y = fr, group = beta, color = beta)) +
  geom_line() +
  facet_grid(~alpha)

test_results %>%
  filter(alpha == 1) %>%
  filter(beta >= 0) %>%
  ggplot(aes(x = N, y = fr, group = beta, color = factor(beta))) +
  geom_line()
