# Wonky data exploration

exdat <- filter(poop, mod_num == 2) %>%
  slice(1) %>%
  semi_join(badmod_data, ., by = c("predator", "season", "use_aniso", "model")) %>%
  select(data.x) %>%
  unnest(data.x)

  

exdat %>%
  filter(pypres == 1) %>%
ggplot(aes(x = declat, y = declon, color = factor(pypres))) +
  geom_point() +
  facet_wrap(~year)

group_by(exdat, year) %>%
  summarise(n = sum(pypres)) %>%
  print(n = Inf)
