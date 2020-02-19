# Use dat_preds_all from 1_process_consumption_data.Rmd

library(here)

dat_preds_all = read_rds(here("output", "data_formatted", "dat_preds_all.rds"))


# Summarize herring consumption by species --------------------------------
my_summary = dat_preds_all %>% 
  group_by(pdcomnam) %>%
  summarize(
    total_kg = sum(pyamtw)/1000,
    n = n()) %>%
  arrange(desc(total_kg)) %>%
  mutate(pdcomnam = factor(pdcomnam))

spec_order = as.character(my_summary$pdcomnam)

my_summary = my_summary %>%
  mutate(pdcomnam = factor(pdcomnam, levels = spec_order))

barplot(my_summary$total_kg, names.arg = my_summary$pdcomnam)


ggplot(my_summary, aes(x = pdcomnam, y = total_kg)) + 
  geom_bar(stat="identity") +
  theme_bw() + 
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab(NULL) +
  ylab("Herring consumption (kg)") + 
  theme(text = element_text(size = 16))


# Summarize herring consumption by size -----------------------------------

size_summary = dat_preds_all %>% 
  filter(pdcomnam == "ATLANTIC COD" | pdcomnam == "SPINY DOGFISH") %>%
  group_by(sizecat, pdcomnam) %>%
  summarize(
    average_g = mean(pyamtw),
    n_pred = n()) %>%
  ungroup(sizecat) %>%
  # mutate(sizecat = factor(sizecat))
  mutate(sizecat = factor(sizecat, levels = c("S", "M", "L", "XL")))
  

ggplot(size_summary, aes(x = sizecat, y = average_g)) +
  geom_bar(stat="identity") +
  facet_wrap(~pdcomnam) +
  theme_bw() + 
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab(NULL) +
  ylab("Mean per capita herring consumption (g)") + 
  theme(text = element_text(size = 16))

