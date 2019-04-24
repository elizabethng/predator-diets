# Use dat_preds_all from 1_process_consumption_data.Rmd

dat_preds_all

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


