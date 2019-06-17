# Check for patterns in residuals to see if other things are important in model


library(tidyverse)
library(here)


# Overlap -----------------------------------------------------------------
# How important is overlap? I compared the annual trends, but how much does
# it matter in space? 

# Read in overlap data
overlap_indices = read_rds(here("output", "data_formatted", "overlap_indices.rds"))

# But I'd like to look at a finer spatial scale, so I need the data
# at the knot scale. 

cod_index = read_rds(here("output", "data_formatted", 
                                "cod_overlap_knots.rds"))

dogfish_index = read_rds(here("output", "data_formatted", 
                                "dogfish_overlap_knots.rds"))

