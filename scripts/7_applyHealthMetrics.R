###
# developing the workflow for review before functionalizing the process
### 

pacman::p_load(dplyr,sf, tidyr, tmap, readr)

# parameters  -------------------------------------------------------------
# these are standard variables 
baseNDVI <- 0.1 
doseResponse <- 0.146
relativeRisk <- 0.961 
populationAttributableFraction <- function(ndviVal, rr, doseResponse){
  paf <- 1 - (1/rr ^ ( ndviVal/doseResponse))
  return(paf)
} 
crudeDeathPrevented <- function(population, mortalityRate, paf){
  cdp <- population * mortalityRate * paf * -1
  return(cdp)
}


# run on all cities  ------------------------------------------------------
source("scripts/metricsToAllCities.R")

# run on census tracts 
