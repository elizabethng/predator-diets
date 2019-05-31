# Plot index of consumption

library(tidyverse)
library(here)
library(gganimate)

save_output = FALSE

results_files = c("cod_plg_rw",
                  "dogfish_plg_rw")

path_name = paste("output", "VAST", "_runs_for_popdy_meeting", sep = "/")

# Compare to Overlap Metric -----------------------------------------------
overlap = read_rds(here("output", "data_formatted", "overlap_indices.rds"))

# Dogfish data don't start until 1977
overlap = filter(overlap, !(index == "overlap_dogfish" & year < 1977))

overlap_std = overlap %>%
  group_by(index) %>%
  mutate(val_z = scale(value))

# Cod ---------------------------------------------------------------------
index_res = read_csv(here(path_name, results_files[1], "Table_for_SS3.csv")) %>%
  mutate(g_herring = Estimate_metric_tons*10^-3)

p_cod = ggplot(index_res, aes(Year, g_herring)) +
  geom_line(color = "#000060") + 
  geom_ribbon(aes(ymin = g_herring - SD_mt*10^-3,
                    ymax = g_herring + SD_mt*10^-3),
              alpha = 0.5, fill = "#000060") +
  theme_bw() + 
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black")) +
  ylab("Herring in stomachs (kg)") + 
  theme(text = element_text(size = 16))

if(save_output) ggsave(here("output", "plots", "cod_herring_index.pdf"), 
       p_cod,
       width = 7,
       height = 4)


o_cod = ggplot(index_res, aes(Year, scale(g_herring))) +
  geom_line(color = "#000060",
            size = 2.5,
            alpha = 0.4) + 
  geom_line(data = filter(overlap_std, index == "overlap_cod"),
            aes(year, val_z),
            size = 1,
            color = "#000060") +
  theme_bw() + 
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black")) +
  ylab("Index") + 
  theme(text = element_text(size = 16))

if(save_output) ggsave(here("output", "plots", "cod_herring_overlap_w_index.pdf"), 
       o_cod,
       width = 7,
       height = 4)


# Dogfish -----------------------------------------------------------------

index_res = read_csv(here(path_name, results_files[2], "Table_for_SS3.csv")) %>%
  mutate(g_herring = Estimate_metric_tons*10^-3)

p_dog = ggplot(index_res, aes(Year, g_herring)) +
  geom_line(color = "#FA813B") +
  geom_ribbon(aes(ymin = g_herring - SD_mt*10^-3,
                  ymax = g_herring + SD_mt*10^-3),
              alpha = 0.5, fill = "#FA813B") +
  theme_bw() + 
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black")) +
  ylab("Herring in stomachs (kg)") + 
  theme(text = element_text(size = 16))

if(save_output) ggsave(here("output", "plots", "dogfish_herring_index.pdf"), 
         p_dog,
         width = 7,
         height = 4)

o_dog = ggplot(index_res, aes(Year, scale(g_herring))) +
  geom_line(color = "#FA813B",
            size = 2.5,
            alpha = 0.4) + 
  geom_line(data = filter(overlap_std, index == "overlap_dogfish"),
            aes(year, val_z),
            size = 1,
            color = "#FA813B") +
  theme_bw() + 
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black")) +
  ylab("Index") + 
  theme(text = element_text(size = 16))

if(save_output) ggsave(here("output", "plots", "dog_herring_overlap_w_index.pdf"), 
       o_dog,
       width = 7,
       height = 4)


