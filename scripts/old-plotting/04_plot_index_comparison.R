# Simple line plots of multiple VAST indices
# Plot VAST herring index from cod and dogfish consumption with Jon's index

library(tidyverse)
library(here)
library(gganimate)


results_files = c("cod_plg_rw",
                  "dogfish_plg_rw")

path_name = paste("output", "VAST", "_runs_for_popdy_meeting", sep = "/")

z_score = function(dat){
  c = dat - mean(dat)
  z = c/sd(dat)
  return(z)
}

cod_index = read_csv(here(path_name, results_files[1], "Table_for_SS3.csv")) %>%
  mutate(herring_z = z_score(Estimate_metric_tons))

dogfish_index = read_csv(here(path_name, results_files[2], "Table_for_SS3.csv")) %>%
  mutate(herring_z = z_score(Estimate_metric_tons))

jon_data = readxl::read_xlsx(here("data", "TimeSeries.xlsx")) %>%
  mutate(herring_z = z_score(`Jan.1 Biomass (mt)`)) %>%
  filter(Year > 1972)


index_combo = tibble(
  Year = c(cod_index$Year, 
           dogfish_index$Year, 
           jon_data$Year),
  index = c(cod_index$herring_z, 
            dogfish_index$herring_z, 
            jon_data$herring_z),
  type = c(rep("cod", nrow(cod_index)), 
           rep("dogfish", nrow(dogfish_index)), 
           rep("assessment", nrow(jon_data))))

my_colors = c("cod" = "#000060",
              "dogfish" = "#FA813B",
              "assessment" = "black")

p = ggplot(index_combo, aes(Year, index, group = type, color = type)) +
  geom_line(size = 1) + 
  scale_color_manual(values = my_colors) + 
  theme_bw() + 
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black")) +
  ylab("Herring index") + 
  theme(text = element_text(size = 16)) +
  theme(legend.position = "none")
ggsave(here("output", "plots", "herring_index_comparison.pdf"), 
       p,
       width = 7,
       height = 4)

