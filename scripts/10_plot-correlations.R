# Script to plot index comparisons
# 1. plot one to one comparison of diet index and overlap index
# 2. plot one to one comparison of diet index and assessment biomass index
# 3. plot one to one of overlap and assessment

# Add herring, then add error bars
# for plotting attemps, can just add arbitraty SE to overlap index?

library("tidyverse")
library("here")

use_assessment <- TRUE

# Load data and format for combining
dietindexr <- read_rds(here::here("output", "index_diet.rds"))
overlapindexr <- read_rds(here::here("output", "index_overlap.rds"))
assessdatr <- readxl::read_xlsx(here("data", "raw", "TimeSeries.xlsx"))

# add my Atlantic herring index
herringdatr <- readr::read_rds(here::here("output", "top_final_trawl.rds")) %>%
  filter(species == "atlantic herring") %>%
  select(season, species, output) %>%
  mutate(output = map(output, "result")) %>%
  mutate(output = map(output, "index")) %>%
  unnest(output)
  


# Plot indices as time-series ---------------------------------------------
abundance_indices <- pivot_longer(assessdatr, cols = -Year) %>%
  filter(name %in% c("SSB (mt)",
                     "Jan.1 Biomass (mt)",
                     "Catch (mt)")) %>%
  mutate(source = "assessment") %>%
  bind_rows(herringdatr %>%
              mutate(source = "st index") %>%
              select(Year, 
                     source, 
                     name = season, 
                     value = Estimate_metric_tons)) %>%
  bind_rows(dietindexr %>%
              mutate(name = paste(predator, season),
                     source = "diet index") %>%
              select(Year = year,
                     source,
                     name,
                     value = density)) %>%
  group_by(name) %>%
  mutate(value_z = scale(value)[,1])

ggplot(abundance_indices, aes(x = Year, y = value_z, group = name, color = source)) +
  geom_point() +
  geom_line(size = 1, alpha = 0.5) +
  scale_color_manual(values = c(
    scales::muted("blue", l = 50, c = 100), 
    "grey",
    scales::muted("red", l = 50, c = 100))) +
  theme_bw()

mean_diets <- filter(abundance_indices, source == "diet index") %>%
  group_by(Year, source) %>%
  summarize(value_z = mean(value_z, na.rm = TRUE)) %>%
  mutate(name = "average diets") %>%
  bind_rows(abundance_indices %>%
              filter(source != "diet index"))

ggplot(mean_diets, aes(x = Year, y = value_z, group = name, color = source)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c(
    scales::muted("blue", l = 50, c = 100), 
    "grey",
    scales::muted("red", l = 50, c = 100))) +
  theme_bw()
ggsave(here("output", "plots", "index-ts.pdf"), width = 8.5, height = 4)

# 0. Scale and rename -----------------------------------------------------
diet_index <- dietindexr %>%
  filter(!is.na(density)) %>%
  group_by(season, predator) %>%
  mutate(diet_index = scale(density)[,1]) %>%
  select(-density, -density_se) %>%
  ungroup()

overlap_index <- overlapindexr %>%
  group_by(season, predator) %>%
  mutate(overlap_index = scale(`Overlap index`)[,1]) %>%
  select(-`Overlap index`) %>%
  ungroup()

if(use_assessment == TRUE){
  pred_seas <- select(diet_index, predator, season) %>% distinct()
  
  stock_index <- assessdatr %>%
    select(Year, `SSB (mt)`) %>%
    mutate(stock_index = scale(`SSB (mt)`)[,1]) %>%
    select(-`SSB (mt)`) %>%
    rename(year = Year) %>%
    expand_grid(pred_seas, .) %>%
    ungroup()
}else{
  stock_index <- herringdatr %>%
    select(season, 
           year = Year,
           stock_index = Estimate_metric_tons) %>%
    group_by(season) %>%
    mutate(stock_index = scale(stock_index)[,1]) %>%
    ungroup()
}


plim <- max(c(diet_index$diet_index, overlap_index$overlap_index, stock_index$stock_index))
nlim <- min(c(diet_index$diet_index, overlap_index$overlap_index, stock_index$stock_index))


# 1. Format data for plotting ---------------------------------------------

# Diet index vs overlap index
diet_overlap_dat <- inner_join(diet_index, overlap_index, by = c("season", "predator", "year")) %>%
  rename("Diet index" = diet_index,
         "Overlap index" = overlap_index) %>%
  mutate(predator = str_to_sentence(predator),
         season = str_to_sentence(season)) %>%
  group_by(season, predator) %>%
  mutate(Correlation = cor(`Diet index`, `Overlap index`))

if(use_assessment == TRUE){
  # Diet index vs assessment index
  diet_stock_dat <- inner_join(diet_index, stock_index, by = c("season", "predator", "year")) %>%
    rename("Diet index" = diet_index,
           "Assessment index" = stock_index) %>%
    mutate(predator = str_to_sentence(predator),
           season = str_to_sentence(season)) %>%
    group_by(season, predator) %>%
    mutate(Correlation = cor(`Diet index`, `Assessment index`))
  
  # Overlap index vs assessment index
  overlap_stock_dat <- inner_join(overlap_index, stock_index, by = c("season", "predator", "year")) %>%
    rename("Overlap index" = overlap_index,
           "Assessment index" = stock_index) %>%
    mutate(predator = str_to_sentence(predator),
           season = str_to_sentence(season)) %>%
    group_by(season, predator) %>%
    mutate(Correlation = cor(`Overlap index`, `Assessment index`))
}else{
  # Diet index vs herring index
  diet_stock_dat <- inner_join(diet_index, stock_index, by = c("season", "year")) %>%
    rename("Diet index" = diet_index,
           "Assessment index" = stock_index) %>%
    mutate(predator = str_to_sentence(predator),
           season = str_to_sentence(season)) %>%
    group_by(season, predator) %>%
    mutate(Correlation = cor(`Diet index`, `Assessment index`))
  
  # Overlap index vs herring index
  overlap_stock_dat <- inner_join(overlap_index, stock_index, by = c("season", "year")) %>%
    rename("Overlap index" = overlap_index,
           "Assessment index" = stock_index) %>%
    mutate(predator = str_to_sentence(predator),
           season = str_to_sentence(season)) %>%
    group_by(season, predator) %>%
    mutate(Correlation = cor(`Overlap index`, `Assessment index`))
}


# 2. Calculate correlations --------------------------------------------------

docor <- diet_overlap_dat %>%
  group_by(predator, season) %>%
  summarize(correlation = cor(`Diet index`, `Overlap index`)) %>%
  mutate(comparison = "diet, overlap",
         correlation = signif(correlation, 2))

dacor <- diet_stock_dat %>%
  group_by(predator, season) %>%
  summarize(correlation = cor(`Diet index`, `Assessment index`)) %>%
  mutate(comparison = "diet, assessment",
         correlation = signif(correlation, 2))

oacor <- overlap_stock_dat %>%
  group_by(predator, season) %>%
  summarize(correlation = cor(`Overlap index`, `Assessment index`)) %>%
  mutate(comparison = "overlap, assessment",
         correlation = signif(correlation, 2))

all_cor <- bind_rows(list(docor, dacor, oacor)) %>%
  group_by(predator) %>%
  arrange(desc(correlation), .by_group = TRUE)


# 3. Make plots --------------------------------------------------------------
# Adjust label spacing for output
hjust_c = -0.2 # -0.2
vjust_c = -11 # -14.2

p1 <- ggplot(diet_overlap_dat, aes(x = `Overlap index`, y = `Diet index`)) +
  geom_smooth(method = "lm", se = FALSE, color = "grey") +
  geom_point() +
  geom_text(data = docor, 
            aes(x = -Inf, y = -Inf, label = correlation), 
            hjust   = hjust_c,
            vjust   = vjust_c,
            inherit.aes = FALSE) +
  coord_fixed(ratio = diff(range(diet_overlap_dat$`Overlap index`))/diff(range(diet_overlap_dat$`Diet index`))) +
  facet_grid(predator ~ season) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())
ggsave(plot = p1, here("output", "plots", "overlap-diet-comp-1to1.pdf"), width = 4, height = 8, units = "in")

p2 <- ggplot(diet_stock_dat, aes(x = `Assessment index`, y = `Diet index`)) +
  geom_smooth(method = "lm", se = FALSE, color = "grey") +
  geom_point() +
  geom_text(data = dacor, 
            aes(x = -Inf, y = -Inf, label = correlation), 
            hjust   = hjust_c,
            vjust   = vjust_c,
            inherit.aes = FALSE) +
  coord_fixed(ratio = diff(range(diet_stock_dat$`Assessment index`))/diff(range(diet_stock_dat$`Diet index`))) +
  facet_grid(predator ~ season) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())
ggsave(plot = p2, here("output", "plots", "assessment-diet-comp-1to1.pdf"), width = 4, height = 8, units = "in")


p3 <- ggplot(overlap_stock_dat, aes(x = `Assessment index`, y = `Overlap index`)) +
  geom_smooth(method = "lm", se = FALSE, color = "grey") +
  geom_point() +
  geom_text(data = oacor, 
            aes(x = -Inf, y = -Inf, label = correlation), 
            hjust   = hjust_c,
            vjust   = vjust_c,
            inherit.aes = FALSE) +
  coord_fixed(ratio = diff(range(overlap_stock_dat$`Assessment index`))/diff(range(overlap_stock_dat$`Overlap index`))) +
  facet_grid(predator ~ season) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())
ggsave(plot = p3, here("output", "plots", "assessment-overlap-comp-1to1.pdf"), width = 4, height = 8, units = "in")


