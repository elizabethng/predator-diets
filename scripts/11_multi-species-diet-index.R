# Script to do a simple combination of diet indices

# From JT 
# Use a statistical (e.g linear) model with 
# response of abundance index and 
# predictors each diet index, 
# estimate coefficients, 
# and extract prediction as multispecies diet index.

library("tidyverse")
library("here")


# 0. Load data ------------------------------------------------------------


# Load data and extract indices
# Somehow use weights 