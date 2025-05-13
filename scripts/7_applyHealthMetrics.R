###
# developing the workflow for review before functionalizing the process
### 

pacman::p_load(dplyr,sf, tidyr, tmap, readr)
tmap_mode("view")

# inputs 
allCities <- read_csv("data/processed/summaryNDVI/allCitiesNDVI.csv")
# need the population measure too 

# ndvi per census tract by city 
ndvi_ct <- list.files("data/processed/summaryNDVI",
                      pattern = ".csv",
                      full.names = TRUE)
test <- sf::st_read(ndvi_ct[2])
# parameters  -------------------------------------------------------------
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


# quick test --------------------------------------------------------------
population <- 100000
mortalityRate <- 792.2/100000
### reference from paper 
# .1 == 4% reduction 

for(i in seq(0, 0.8, by =0.1)){
  print(i)
  paf <- populationAttributableFraction(
    ndviVal = i,
    rr = relativeRisk,
    doseResponse = doseResponse
  )
  deathPresented <- crudeDeathPrevented(
    population = population,
    mortalityRate = mortalityRate,
    paf = paf
  )
  print(deathPresented)
}
