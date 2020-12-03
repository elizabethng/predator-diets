# Make appendix figure comparing different indices

library("tidyverse")
library("here")



# 0. Load data ------------------------------------------------------------
# Load data and format for combining

dietindexr <- read_rds(here::here("output", "index_diet.rds"))
assessdatr <- readxl::read_xlsx(here("data", "raw", "TimeSeries.xlsx"))

# read_rds(here::here("output", "index_overlap.rds"))
# read_rds(here("output", "EXTERNAL_schoeners_D.rds"))
overlapindexr <- read_rds(here::here("output", "index_range-overlap.rds"))%>%  
  rename(season = Season, overlap = range_overlap) %>%
  mutate(predator = tolower(predator), season = tolower(season)) %>%
  select(-lcb, -ucb) %>%
  rename(lcb = lcb_b, ucb = ucb_b)


# Plot indices as time-series ---------------------------------------------
# [ ] This chunk creates appendix figure with index comparison. Should move it
# into a new script and tidy up. 
if(use_assessment == FALSE){
  # add my Atlantic herring index
  herringdatr <- readr::read_rds(here::here("output", "top_final_trawl.rds")) %>%
    filter(species == "atlantic herring") %>%
    select(season, species, output) %>%
    mutate(output = map(output, "result")) %>%
    mutate(output = map(output, "index")) %>%
    unnest(output)
  
  # Add in herring index used in stock assessment
  assessindex <- readxl::read_xlsx(here("data", "raw", "HerringIndices.xlsx"))
  
  ggplot(
    assessindex, 
    aes(x = YEAR, y = INDEX)
  ) +
    geom_point()
  
  abundance_indices <- pivot_longer(assessdatr, cols = -Year) %>%
    filter(name %in% c("SSB (mt)",
                       "Jan.1 Biomass (mt)",
                       "Catch (mt)",
                       "Age-1 Recruitment (000s)")
    ) %>%
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
    bind_rows(assessindex %>%
                select(
                  Year = YEAR,
                  value = INDEX
                ) %>%
                mutate(
                  source = "Survey",
                  name = "Atlantic herring"
                )
    ) %>%
    group_by(name) %>%
    mutate(value_z = scale(value)[,1])
  
  # ggplot(abundance_indices, aes(x = Year, y = value_z, group = name, color = source)) +
  #   geom_point() +
  #   geom_line(size = 1, alpha = 0.5) +
  #   scale_color_manual(values = c(
  #     scales::muted("blue", l = 50, c = 100), 
  #     "grey",
  #     scales::muted("red", l = 50, c = 100))) +
  #   theme_bw()
  
  # Distinguish between different types (don't include the spatiotemproal index, i.e. VAST fit to herring biomass)
  jj <- abundance_indices %>%
    ungroup() %>%
    mutate(name = ifelse(name == "fall", "st herring fall", name)) %>%
    mutate(name = ifelse(name == "spring", "st herring spring", name))
  
  jj_formatted <- jj %>%
    mutate(
      new_name = gsub(" \\(mt\\)", "", name),
      new_name = gsub("st ", "", new_name),
      new_name = str_to_sentence(new_name),
      new_name = gsub("Ssb", "SSB", new_name),
      new_name = gsub(" \\(000s\\)", "", new_name),
      new_name = gsub("spring", "(Spring)", new_name),
      new_name = gsub("fall", "(Fall)", new_name),
      new_name = gsub("Herring", "Atlantic herring", new_name)
    ) %>%
    mutate(
      Index = gsub("st", "Spatio-temporal", source),
      Index = gsub(" index", "", Index),
      Index = str_to_sentence(Index)
    ) 
  # Change level of new_name
  # assessment rec, SSB, jan, Catch,
  # spatio temporal herring herring
  # rest of diet inds.
  forder <- c(
    "Age-1 recruitment", "Jan.1 biomass", "SSB", "Catch", "Atlantic herring",
    "Atlantic herring (Spring)", "Atlantic herring (Fall)", "Atlantic cod (Spring)", "Atlantic cod (Fall)",
    "Goosefish (Spring)", "Goosefish (Fall)", "Silver hake (Spring)", "Silver hake (Fall)",
    "Spiny dogfish (Spring)", "Spiny dogfish (Fall)", "White hake (Spring)", "White hake (Fall)")
  plotdat <- jj_formatted %>%
    mutate(
      new_name_fac = factor(new_name, levels = forder)
    )
  
  ggplot(plotdat, aes(x = Year, y = value_z, color = Index)) +
    geom_point() +
    geom_line() +
    ylab("Standardized value") +
    facet_wrap(~new_name_fac, ncol = 4) +
    scale_color_viridis_d(
      name = "Index source",
      end = 0.8
    ) +
    guides(colour = guide_legend(nrow = 1)) +
    theme_bw() +
    theme(
      legend.position = c(0.6, 0.1),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_blank()
    )
  
  ggsave(
    here("output", "plots", "index-ts-multipanel.pdf"), 
    width = 8.5, 
    height = 12
  )
  
  
  ggplot(jj, aes(x = Year, y = value_z, group = name, color = name)) +
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
  if(save_output){
    ggsave(here("output", "plots", "index-ts-multipanel.pdf"), width = 8.5, height = 12)  
  }
  
  
  
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
  if(save_output){
    ggsave(here("output", "plots", "index-ts.pdf"), width = 8.5, height = 4) 
  }
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
  mutate(overlap_se = ro_sd,
         cv_overlap = overlap_se/overlap) %>%
  group_by(season, predator) %>%
  mutate(overlap_index = scale(overlap)[,1]) %>%
  select(-overlap, -lcb, -ucb) %>%
  ungroup()

if(use_assessment == TRUE){
  pred_seas <- select(diet_index, predator, season) %>% distinct()
  
  stock_index <- assessdatr %>%
    select(Year, `Jan.1 Biomass (mt)`) %>%
    mutate(stock_index = scale(`Jan.1 Biomass (mt)`)[,1]) %>%
    select(-`Jan.1 Biomass (mt)`) %>%
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