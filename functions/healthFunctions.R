# 
# doseResponse <- doseResponseMortality
# this function is the same for all indicies 
relativeRateMortiality <- function(ndviVal, baseNDVI, doseResponse){
  ndviDiff <- ndviVal - baseNDVI 
  rr <- exp(log(doseResponse) * (ndviDiff/0.1))
}
relativeRateMortiality10 <- function(ndviVal, baseNDVI, doseResponse){
  ndviDiff <- ndviVal - baseNDVI
  # add 10% increase 
  ndviDiff <- ndviDiff + (ndviDiff*0.1)
  rr <- exp(log(doseResponse) * (ndviDiff/0.1))
}


relativeRateStrokeDem<- function(ndviVal, baseNDVI, doseResponse){
  ndviDiff <- ndviVal - baseNDVI 
  rr <- exp(log(doseResponse) * (ndviDiff/0.12))
}
relativeRateStrokeDem10<- function(ndviVal, baseNDVI, doseResponse){
  ndviDiff <- ndviVal - baseNDVI 
  # add 10% increase 
  ndviDiff <- ndviDiff + (ndviDiff*0.1)
  rr <- exp(log(doseResponse) * (ndviDiff/0.12))
}



# relativeRisk <- 0.961 # this the ndvi value per city  - base NDVI 
populationAttributableFraction <- function(relativeRate){
  paf <- (relativeRate - 1)/relativeRate
  return(paf)
} 

# Expected incidence 
expectedIncidence <- function(population, incidenceRate){
  pop <- population * incidenceRate
  return(pop)
}
# lives saved 
livesSaved <- function(nIncidence, paf){
  lives <- nIncidence * paf
}


