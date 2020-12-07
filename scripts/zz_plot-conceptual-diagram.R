# Conceptual diagram of index standardization

# Need 1:1, hyper stable, not correlated

library("tidyverse")
library("here")

set.seed(333)

# Make data ---------------------------------------------------------------
a <- rnorm(100)
b <- rnorm(100)

# Correlated
dat1 <- tibble(
  x = 1:100, # seq(20, 80, length.out = 100),
  y = x + rnorm(n = 100, mean = 0, sd = 20)
)

ggplot(dat1, aes(x, y)) +
  geom_point()
  # stat_ellipse(geom = "polygon")

# Hyperstable
dat2 <- tibble(
  x = 1:100,
  y = 50 + 10*a
)
ggplot(dat2, aes(x, y)) +
  geom_point()
  # stat_ellipse(geom = "polygon")

# Uncorrelated
dat3 <- tibble(
  x = 20*a + 50, 
  y = 20*b + 50
)
ggplot(dat3, aes(x, y)) +
  geom_point()
  # stat_ellipse(geom = "polygon")

cor(dat1)
cor(dat2)
cor(dat3)

# Combine and plot --------------------------------------------------------
alldat <- bind_rows(list(
  "correlated" = dat1, 
  "hyperstable" = dat2,
  "uncorrelated" = dat3),
  .id = "type"
  )

p <- ggplot(alldat, aes(x, y)) +
  # geom_point() +
  stat_ellipse(geom = "polygon", alpha = 0.3) +
  geom_abline(slope = 1) +
  xlab("Biomass") +
  ylab("Diet index") +
  facet_grid(~type) +
  coord_fixed() +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank())
p
ggsave(plot = p, 
       here("output", "plots", "conceptual-diagram.pdf"), 
       width = 170, 
       height = 60, 
       units = "mm")
