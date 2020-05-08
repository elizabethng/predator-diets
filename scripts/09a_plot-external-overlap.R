# Plot overlap index calculated externally using Schoener's D

library("tidyverse")
library("here")


# Get results -------------------------------------------------------------
plot_annualindex <- read_rds(here("output", "EXTERNAL_schoeners_D.rds"))

# Make plot ---------------------------------------------------------------

ggplot(plot_annualindex, aes(x = Year, y = Estimate, color = Season, fill = Season)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = lcb, ymax = ucb, fill = Season), alpha = 0.3, color = NA) +
  scale_color_manual(aesthetics = c("color", "fill"),
                     values = c(scales::muted("blue", l = 50, c = 100), scales::muted("red", l = 50, c = 100))) +
  facet_wrap(~predator) +
  theme_bw() +
  theme(legend.position = c(0.8, 0.2),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())
ggsave(here("output", "plots", "overlap-index-ts_schoeners-D.pdf"),
       width = 9, height = 5, units = "in")

