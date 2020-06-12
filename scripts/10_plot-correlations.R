# Script to plot index comparisons
# 1. plot one to one comparison of diet index and overlap index
# 2. plot one to one comparison of diet index and assessment biomass index
# 3. plot one to one of overlap and assessment

# Add herring, then add error bars
# for plotting attemps, can just add arbitraty SE to overlap index?

library("tidyverse")
library("here")

use_assessment <- TRUE

overlap_cv <- 0.02877875 # from test-repo
assessment_cv <- 0.02

# Load data and format for combining
dietindexr <- read_rds(here::here("output", "index_diet.rds"))
overlapindexr <- read_rds(here("output", "EXTERNAL_schoeners_D.rds")) %>% 
  rename(season = Season, year = Year, overlap = Estimate, overlap_se = `Std. Error`) %>%
  mutate(predator = tolower(predator), season = tolower(season)) # read_rds(here::here("output", "index_overlap.rds"))
assessdatr <- readxl::read_xlsx(here("data", "raw", "TimeSeries.xlsx"))



# Plot indices as time-series ---------------------------------------------
if(use_assessment == FALSE){
  # add my Atlantic herring index
  herringdatr <- readr::read_rds(here::here("output", "top_final_trawl.rds")) %>%
    filter(species == "atlantic herring") %>%
    select(season, species, output) %>%
    mutate(output = map(output, "result")) %>%
    mutate(output = map(output, "index")) %>%
    unnest(output)
  
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
  
  # Distinguish between different types (don't include the spatiotemproal index, i.e. VAST fit to herring biomass)
  abundance_indices %>%
    ungroup() %>%
    mutate(name = ifelse(name == "fall", "st herring fall", name)) %>%
    mutate(name = ifelse(name == "spring", "st herring spring", name)) %>%
  ggplot(aes(x = Year, y = value_z, group = name, color = name)) +
    geom_point() +
    facet_grid(source ~.) +
    geom_line(size = 1, alpha = 0.5) +
    scale_color_manual(
      values = c(
        "Jan.1 Biomass (mt)" = "dodgerblue4", # "Jan.1 Biomass (mt)"
        "Catch (mt)" = "dodgerblue2", # "Catch (mt)"
        "SSB (mt)" = "lightskyblue1", # "SSB (mt)"
        "atlantic cod fall" = "chartreuse4",# "atlantic cod fall"
        "atlantic cod spring"= "chartreuse1", # "atlantic cod spring"
        "goosefish fall" = "darkseagreen4",# "goosefish fall"
        "goosefish spring" = "darkseagreen",# "goosefish spring"
        "silver hake fall" = "olivedrab4" ,# "silver hake fall"
        "silver hake spring" = "olivedrab", # "silver hake spring"
        "spiny dogfish fall" = "seagreen4", # "spiny dogfish fall"
        "spiny dogfish spring" = "seagreen4",# "spiny dogfish spring"
        "white hake fall"= "royalblue4",# "white hake fall"
        "white hake spring" = "royalblue", # "white hake spring"
        "st herring fall" = "purple4", # "st herring fall"
        "st herring spring" = "purple"# "st herring spring"
      ),
      breaks = c(
        "Jan.1 Biomass (mt)", 
        "Catch (mt)", 
        "SSB (mt)", 
        "atlantic cod fall", 
        "atlantic cod spring", 
        "goosefish fall", 
        "goosefish spring", 
        "silver hake fall", 
        "silver hake spring", 
        "spiny dogfish fall", 
        "spiny dogfish spring",
        "white hake fall", 
        "white hake spring",
        "st herring fall", 
        "st herring spring")
    ) +
    theme_bw()
  ggsave(here("output", "plots", "index-ts-multipanel.pdf"), width = 8.5, height = 12)
  
  
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
}



# 0. Scale and rename -----------------------------------------------------
diet_index <- dietindexr %>%
  mutate(cv_diet = density_se/density) %>%
  filter(!is.na(density)) %>%
  group_by(season, predator) %>%
  mutate(diet_index = scale(density)[,1]) %>%
  select(-density, -density_se) %>%
  ungroup()

overlap_index <- overlapindexr %>%
  mutate(cv_overlap = overlap_se/overlap) %>%
  group_by(season, predator) %>%
  mutate(overlap_index = scale(overlap)[,1]) %>%
  select(-overlap, -lcb, -ucb) %>%
  ungroup()

if(use_assessment == TRUE){
  pred_seas <- select(diet_index, predator, season) %>% distinct()
  
  stock_index <- assessdatr %>%
    select(Year, `SSB (mt)`) %>%
    mutate(stock_index = scale(`SSB (mt)`)[,1]) %>%
    select(-`SSB (mt)`) %>%
    rename(year = Year) %>%
    expand_grid(pred_seas, .) %>%
    ungroup() %>%
    mutate(cv_assessment = assessment_cv)
  
}else{
  stock_index <- herringdatr %>%
    select(season, 
           year = Year,
           stock_index = Estimate_metric_tons) %>%
    group_by(season) %>%
    mutate(stock_index = scale(stock_index)[,1]) %>%
    ungroup()
}


plim <- max(c(diet_index$diet_index + diet_index$cv_diet,
              overlap_index$overlap_index + overlap_index$cv_overlap,
              stock_index$stock_index + stock_index$cv_assessment))
nlim <- min(c(diet_index$diet_index + diet_index$cv_diet,
              overlap_index$overlap_index + overlap_index$cv_overlap,
              stock_index$stock_index + stock_index$cv_assessment))


# 1. Format data for plotting ---------------------------------------------

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

if(use_assessment == TRUE){
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

#   geom_errorbar(aes(ymin = (Density - density_se), 
#                     ymax = (Density + density_se)),
#                 width = 0) 

p1 <- ggplot(diet_overlap_dat, aes(x = `Overlap index`, 
                                   y = `Diet index`,
                                   xmin = (`Overlap index` - cv_o),
                                   xmax = (`Overlap index` + cv_o),
                                   ymin = (`Diet index` - cv_d), 
                                   ymax = (`Diet index` + cv_d))) +
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
                                             diet_overlap_dat$`Diet index` + diet_overlap_dat$cv_d))))) +
  facet_grid(predator ~ season) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())
ggsave(plot = p1, here("output", "plots", "overlap-diet-comp-1to1.pdf"), width = 4, height = 8, units = "in")

p2 <- ggplot(diet_stock_dat, aes(x = `Assessment index`, 
                                 y = `Diet index`,
                                 xmin = (`Assessment index` - `Assessment index`*cv_assessment),
                                 xmax = (`Assessment index` + `Assessment index`*cv_assessment),
                                 ymin = (`Diet index` - `Diet index`*cv_diet), 
                                 ymax = (`Diet index` + `Diet index`*cv_diet))) +
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
ggsave(plot = p2, here("output", "plots", "assessment-diet-comp-1to1.pdf"), width = 4, height = 8, units = "in")


p3 <- ggplot(overlap_stock_dat, aes(x = `Assessment index`, 
                                    y = `Overlap index`,
                                    xmin = (`Assessment index` - `Assessment index`*cv_assessment),
                                    xmax = (`Assessment index` + `Assessment index`*cv_assessment),
                                    ymin = (`Overlap index` - `Overlap index`*cv_overlap), 
                                    ymax = (`Overlap index` + `Overlap index`*cv_overlap))) +
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
ggsave(plot = p3, here("output", "plots", "assessment-overlap-comp-1to1.pdf"), width = 4, height = 8, units = "in")



# 4. Plot diet-index with overlap-index colors ----------------------------
diet_overlap <- dietindexr %>%
  mutate(predator = str_to_sentence(predator),
         season = str_to_sentence(season)) %>%
  right_join(diet_overlap_dat, dietindexr, by = c("season", "predator", "year")) %>%
  mutate(cv = density_se/density)

ggplot(diet_overlap, aes(x = year, y = `Diet index`, color = `Overlap index`)) +
         geom_point() +
  scale_color_viridis_c(option = "C") +
  geom_errorbar(aes(ymin = (`Diet index` - cv), 
                    ymax = (`Diet index` + cv), 
                    color = `Overlap index`),
                width = 0) +
  facet_grid(season ~ predator) +
  theme_bw() +
  theme(# legend.position = c(0.8, 0.2),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())


# 5. Play with lm fit -----------------------------------------------------







