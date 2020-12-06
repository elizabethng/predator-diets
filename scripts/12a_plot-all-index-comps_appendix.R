# Make appendix figure comparing different Atlantic herring indices
# (Including VAST, diet, survey, assessment)

library("tidyverse")
library("here")


# 0. Load data ------------------------------------------------------------
# Load data and format for combining

# Diet indices
dietindexr <- read_rds(here::here("output", "index_diet.rds"))

# VAST Atlantic herring index
herringdatr <- readr::read_rds(here::here("output", "top_final_trawl.rds")) %>%
    filter(species == "atlantic herring") %>%
    select(season, species, output) %>%
    mutate(output = map(output, "result")) %>%
    mutate(output = map(output, "index")) %>%
    unnest(output)

# Assessment indices
assessdatr <- readxl::read_xlsx(here("data", "raw", "TimeSeries.xlsx"))

# Atlantic herring trawl index used for stock assessment
assessindex <- readxl::read_xlsx(here("data", "raw", "HerringIndices.xlsx"))


# Format indices ----------------------------------------------------------
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

# Distinguish between different types
formatted_names <- abundance_indices %>%
  ungroup() %>%
  mutate(name = ifelse(name == "fall", "st herring fall", name)) %>%
  mutate(name = ifelse(name == "spring", "st herring spring", name)) %>%
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

# Desired order for plot panels
panel_order <- c(
  "Age-1 recruitment", "Jan.1 biomass", "SSB", "Catch", "Atlantic herring",
  "Atlantic herring (Spring)", "Atlantic herring (Fall)", "Atlantic cod (Spring)", "Atlantic cod (Fall)",
  "Goosefish (Spring)", "Goosefish (Fall)", "Silver hake (Spring)", "Silver hake (Fall)",
  "Spiny dogfish (Spring)", "Spiny dogfish (Fall)", "White hake (Spring)", "White hake (Fall)")

plotdat <- formatted_names %>%
  mutate(
    new_name_fac = factor(new_name, levels = panel_order)
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

