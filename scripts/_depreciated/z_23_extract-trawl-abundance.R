# Extract herring and predator abundance indices for comparison

library(tidyverse)

trawlmods <- readr::read_rds(path = here::here("TRAWL", "modruns.rds"))

trawlindex <- trawlmods %>%
  dplyr::mutate(errors = purrr::map(output,"error")) %>% 
  dplyr::mutate(worked = purrr::map_lgl(errors, is.null)) %>% 
  dplyr::filter(worked) %>%
  dplyr::mutate(output = purrr::map(output, "result")) %>%
  dplyr::select(
    -contains("_"),
    -errors,
    -data) %>%
  dplyr::mutate(aic = purrr::map_dbl(output, "aic")) %>%
  dplyr::group_by(pdcomnam, myseason) %>%
  dplyr::top_n(n = -1, wt = aic) %>%
  dplyr::transmute(indexdat = purrr::map(output, "index")) %>%
  tidyr::unnest(cols = c(indexdat)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-Unit, -Fleet, -SD_log, -SD_mt) %>%
  dplyr::rename(
    species = pdcomnam,
    season = myseason,
    year = Year,
    density = Estimate_metric_tons
  ) %>%
  dplyr::mutate(
    species = tolower(species),
    season = tolower(season)
  )

gitdir <- "C:/Users/Elizabeth Ng/Documents/GitHub/predator-diets"
write_rds(trawlindex, path = file.path(gitdir, "output", "trawl_index.rds"))
