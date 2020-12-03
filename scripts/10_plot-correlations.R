# Script to plot index comparisons
# 1. plot one to one comparison of diet index and overlap index
# 2. plot one to one comparison of diet index and assessment biomass index
# 3. plot one to one of overlap and assessment

library("tidyverse")
library("here")

save_output <- FALSE
assessment_cv <- 0.02


# 0. Load data ------------------------------------------------------------
dietindexr <- read_rds(here::here("output", "index_diet.rds"))
assessdatr <- readxl::read_xlsx(here("data", "raw", "TimeSeries.xlsx"))
overlapindexr <- read_rds(here::here("output", "index_range-overlap.rds"))%>%  
  rename(season = Season, overlap = range_overlap) %>%
  mutate(predator = tolower(predator), season = tolower(season)) %>%
  select(-lcb, -ucb) %>%
  rename(lcb = lcb_b, ucb = ucb_b)


# 1. Scale and rename -----------------------------------------------------
diet_index <- dietindexr %>%
  mutate(cv_diet = density_se/density) %>%
  filter(!is.na(density)) %>%
  group_by(season, predator) %>%
  mutate(diet_index = scale(density)[,1]) %>%
  select(-density, -density_se) %>%
  ungroup()

overlap_index <- overlapindexr %>%
  mutate(overlap_se = ro_sd,
         cv_overlap = overlap_se/overlap) %>%
  group_by(season, predator) %>%
  mutate(overlap_index = scale(overlap)[,1]) %>%
  select(-overlap, -lcb, -ucb) %>%
  ungroup()

pred_seas <- select(diet_index, predator, season) %>% distinct()

stock_index <- assessdatr %>%
  select(Year, `SSB (mt)`) %>%
  mutate(stock_index = scale(`SSB (mt)`)[,1]) %>%
  select(-`SSB (mt)`) %>%
  rename(year = Year) %>%
  expand_grid(pred_seas, .) %>%
  ungroup() %>%
  mutate(cv_assessment = assessment_cv)


# 2. Format data for plotting ---------------------------------------------
# Diet index vs overlap index
diet_overlap_dat <- inner_join(diet_index, overlap_index, by = c("season", "predator", "year")) %>%
  rename("Diet index" = diet_index,
         "Overlap index" = overlap_index) %>%
  mutate(predator = str_to_sentence(predator),
         season = str_to_sentence(season)) %>%
  group_by(season, predator) %>%
  mutate(Correlation = cor(`Diet index`, `Overlap index`)) %>%
  mutate(cv_d = cv_diet*`Diet index`,
         cv_o = cv_overlap*`Overlap index`)

# Diet index vs assessment index
diet_stock_dat <- inner_join(diet_index, stock_index, by = c("season", "predator", "year")) %>%
  rename("Diet index" = diet_index,
         "Assessment index" = stock_index) %>%
  mutate(predator = str_to_sentence(predator),
         season = str_to_sentence(season)) %>%
  group_by(season, predator) %>%
  mutate(Correlation = cor(`Diet index`, `Assessment index`)) %>%
  mutate(cv_d = cv_diet*`Diet index`,
         cv_a = cv_assessment*`Assessment index`)

# Overlap index vs assessment index
overlap_stock_dat <- inner_join(overlap_index, stock_index, by = c("season", "predator", "year")) %>%
  rename("Overlap index" = overlap_index,
         "Assessment index" = stock_index) %>%
  mutate(predator = str_to_sentence(predator),
         season = str_to_sentence(season)) %>%
  group_by(season, predator) %>%
  mutate(Correlation = cor(`Overlap index`, `Assessment index`)) %>%
  mutate(cv_o = cv_overlap*`Overlap index`,
         cv_a = cv_assessment*`Assessment index`)


# 3. Calculate correlations --------------------------------------------------
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


# 4. Make plots --------------------------------------------------------------
# Adjust label spacing for output
hjust_c = -0.2 # -0.2
vjust_c = -11 # -14.2

p1 <- ggplot(
  diet_overlap_dat, 
  aes(x = `Overlap index`, 
      y = `Diet index`,
      xmin = (`Overlap index` - cv_o),
      xmax = (`Overlap index` + cv_o),
      ymin = (`Diet index` - cv_d), 
      ymax = (`Diet index` + cv_d))
) +
  geom_smooth(method = "lm", se = FALSE, color = "grey") +
  geom_point() +
  geom_errorbarh(height = 0) +
  geom_errorbar(width = 0) +
  geom_text(data = docor, 
            aes(x = -Inf, y = -Inf, label = correlation), 
            hjust   = hjust_c,
            vjust   = vjust_c,
            inherit.aes = FALSE) +
  coord_fixed(ratio = diff(range(c(diet_overlap_dat$`Overlap index` - diet_overlap_dat$cv_o,
            diet_overlap_dat$`Overlap index` + diet_overlap_dat$cv_o))/diff(
              range(c(diet_overlap_dat$`Diet index` - diet_overlap_dat$cv_d,
                      diet_overlap_dat$`Diet index` + diet_overlap_dat$cv_d))))
  ) +
  facet_grid(predator ~ season) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())
if(save_output){
  ggsave(plot = p1, here("output", "plots", "range-overlap-diet-comp-1to1.pdf"), width = 4, height = 8, units = "in")  
}


p2 <- ggplot(
  diet_stock_dat, 
  aes(x = `Assessment index`, 
      y = `Diet index`,
      xmin = (`Assessment index` - `Assessment index`*cv_assessment),
      xmax = (`Assessment index` + `Assessment index`*cv_assessment),
      ymin = (`Diet index` - `Diet index`*cv_diet), 
      ymax = (`Diet index` + `Diet index`*cv_diet))
) +
  geom_smooth(method = "lm", se = FALSE, color = "grey") +
  geom_point() +
  geom_errorbarh(height = 0) +
  geom_errorbar(width = 0) +
  geom_text(data = dacor, 
            aes(x = -Inf, y = -Inf, label = correlation), 
            hjust   = hjust_c,
            vjust   = vjust_c,
            inherit.aes = FALSE) +
  coord_fixed(ratio = diff(range(c(diet_stock_dat$`Assessment index` - diet_stock_dat$cv_a,
                                   diet_stock_dat$`Assessment index` + diet_stock_dat$cv_a))/diff(
                                     range(c(diet_stock_dat$`Diet index` - diet_stock_dat$cv_d,
                                             diet_stock_dat$`Diet index` + diet_stock_dat$cv_d))))) +
  facet_grid(predator ~ season) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())
if(save_output){
  ggsave(plot = p2, here("output", "plots", "assessment-diet-comp-1to1.pdf"), width = 4, height = 8, units = "in")  
}


p3 <- ggplot(
  overlap_stock_dat, 
  aes(x = `Assessment index`, 
      y = `Overlap index`,
      xmin = (`Assessment index` - `Assessment index`*cv_assessment),
      xmax = (`Assessment index` + `Assessment index`*cv_assessment),
      ymin = (`Overlap index` - `Overlap index`*cv_overlap), 
      ymax = (`Overlap index` + `Overlap index`*cv_overlap))
) +
  geom_smooth(method = "lm", se = FALSE, color = "grey") +
  geom_point() +
  geom_errorbarh(height = 0) +
  geom_errorbar(width = 0) +
  geom_text(data = oacor, 
            aes(x = -Inf, y = -Inf, label = correlation), 
            hjust   = hjust_c,
            vjust   = vjust_c,
            inherit.aes = FALSE) +
  coord_fixed(ratio = diff(range(c(overlap_stock_dat$`Assessment index` - overlap_stock_dat$cv_a,
                                   overlap_stock_dat$`Assessment index` + overlap_stock_dat$cv_a))/diff(
                                     range(c(overlap_stock_dat$`Overlap index` - overlap_stock_dat$cv_o,
                                             overlap_stock_dat$`Overlap index` + overlap_stock_dat$cv_o))))) +
  
  facet_grid(predator ~ season) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())
if(save_output){
  ggsave(plot = p3, here("output", "plots", "assessment-overlap-comp-1to1.pdf"), width = 4, height = 8, units = "in")  
}

