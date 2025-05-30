###
# developing the workflow for review before functionalizing the process
### 

pacman::p_load(dplyr,sf, tidyr, tmap, readr)

# parameters  -------------------------------------------------------------
# these are standard variables 
## the dose response is 
baseNDVI <- 0.1 
doseResponse <- 0.146
relativeRisk <- 0.961 
populationAttributableFraction <- function(ndviVal, rr,baseNDVI, doseResponse){
  paf <- 1 - (1/rr ^ ( (ndviVal-baseNDVI)/doseResponse))
  return(paf)
} 
# 
# population is 20yr and older per city 
# mortalityRate is the deaths/100000 of the county with the most areas within the city 
# paf ; defined above based on average city ndvi (population attributable fraction)
crudeDeathPrevented <- function(population, mortalityRate, paf){
    cdp <- population * mortalityRate * paf * -1
  return(cdp)
}


# run on all cities  ------------------------------------------------------
source("scripts/metricsToAllCities.R")

# run on census tracts 
