# Compare seasonal and all-data indices

library(tidyverse)
library(here)
library(gganimate)

species_name = c("dogfish", "cod")[1]

results_files = paste0(species_name,
                       c("_consumption_plg_rw",
                         "_consumption_plg_rw_FALL_A",
                         "_consumption_plg_rw_SPRING_A"))
# results_files = paste0(c("_consumption_plg_rw",
#   "_consumption_plg_rw_trunc_FALL",
#   "_consumption_plg_rw_trunc_SPRING_B"))

path_name = paste("output", "VAST", "_runs_post_popdy", sep = "/")

z_score = function(dat){
  c = dat - mean(dat)
  z = c/sd(dat)
  return(z)
}

index = read_csv(here(path_name, results_files[1], "Table_for_SS3.csv")) %>%
  mutate(herring_z = z_score(Estimate_metric_tons))

fall_index = read_csv(here(path_name, results_files[2], "Table_for_SS3.csv")) %>%
  mutate(herring_z = z_score(Estimate_metric_tons))

spring_index = read_csv(here(path_name, results_files[3], "Table_for_SS3.csv")) %>%
  mutate(herring_z = z_score(Estimate_metric_tons))


index_combo = tibble(
  Year = c(index$Year, 
           fall_index$Year, 
           spring_index$Year),
  est_g = c(index$Estimate_metric_tons, 
            fall_index$Estimate_metric_tons, 
            spring_index$Estimate_metric_tons),
  est_z = c(index$herring_z, 
            fall_index$herring_z, 
            spring_index$herring_z),
  type = c(rep("both", nrow(index)), 
           rep("fall", nrow(fall_index)), 
           rep("spring", nrow(spring_index))))


p = ggplot(index_combo, aes(Year, est_g, group = type, color = type)) +
  geom_line(size = 1) + 
  theme_bw() + 
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black")) +
  ylab("Herring index") + 
  theme(text = element_text(size = 16))
plot(p)

p2 = ggplot(index_combo, aes(Year, est_z, group = type, color = type)) +
  geom_line(size = 1) + 
  theme_bw() + 
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black")) +
  ylab("Herring index") + 
  theme(text = element_text(size = 16))
plot(p2)


test = rbind(index_combo,
             data.frame(Year = index$Year, 
                  est_g = rowMeans(cbind(fall_index$Estimate_metric_tons, spring_index$Estimate_metric_tons)), 
                  est_z = NA,
                  type = "mean"))

ggplot(test, aes(Year, est_g, group = type, color = type)) +
  geom_line(size = 1) + 
  theme_bw() + 
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black")) +
  ylab("Herring index") + 
  theme(text = element_text(size = 16))

