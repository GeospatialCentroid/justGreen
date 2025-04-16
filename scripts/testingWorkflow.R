###
# developing the workflow for review before functionalizing the process
### 

pacman::p_load(dplyr,sf, tidyr, tmap)
tmap_mode("view")
# working wiht Milwaukee

# inputs 
city <- st_read("data/processed/top200/top200Cities.gpkg")
# census tracks by state 
pop_ct <- list.files("data/processed/censusGeographies",
                     pattern = ".gpkg",
                     full.names = TRUE)
# ndvi per census tract by city 
ndvi_ct <- list.files("data/processed/summaryNDVI",
                      pattern = ".gpkg",
                      full.names = TRUE)
# remove non buffered features 
# noBuff <- ndvi_ct[!grepl(pattern = "_buffer", x = ndvi_ct)]
# lapply(X = noBuff,FUN = file.remove)

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


# quick testing 
population <- 100000
mortalityRate <- 792.2/100000

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




