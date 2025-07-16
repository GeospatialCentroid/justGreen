# parameters  -------------------------------------------------------------
# these are standard variables 
## the dose response is 
baseNDVI <- 0.1 
# mortality 
doseResponseMortality <- 0.96 
drfMortality_low <- 0.94
drfMortality_high <- 0.97
# Stroke 
doseResponseStroke <- 0.97 
drfStroke_low <- 0.96
drfStroke_high <- 0.98
# dementia 
doseResponseDementia <- 0.96 
drfDementia_low <- 0.95
drfDementia_high <- 0.98
# test 
ndviVal <- 0.526565
doseResponse <- doseResponseMortality
# this function is the same for all indicies 
relativeRate <- function(ndviVal, baseNDVI, doseResponse){
  ndviDiff <- ndviVal - baseNDVI 
  rr <- exp(log(doseResponse) * (ndviDiff/0.1))
  
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


