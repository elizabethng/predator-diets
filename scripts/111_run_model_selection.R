# Script to run model selection workflow


test <- FALSE

source(here::here("scripts", "01_fit-ranef-structure.R"))
.rs.restartR()

source(here::here("scripts", "02_pick-ranef-structure.R"))
.rs.restartR()

source(here::here("scripts", "03_fit-covariates.R"))
.rs.restartR()

source(here::here("scripts", "04_pick-covariates.R"))
.rs.restartR()

source(here::here("scripts", "05_fit-final-models.R"))
.rs.restartR()

source(here::here("scripts", "06_calculate-overlap.R"))
.rs.restartR()