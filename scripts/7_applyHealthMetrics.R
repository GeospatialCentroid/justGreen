###
# developing the workflow for review before functionalizing the process
###

pacman::p_load(dplyr, sf, tidyr, tmap, readr)

# run on all cities  ------------------------------------------------------
print("city metrics")
source("scripts/metricsToAllCities.R")

# run on census tracts
print("tracts metrics")
source("scripts/metricsToAllTracts.R")
