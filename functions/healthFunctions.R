#
# doseResponse <- doseResponseMortality
# this function is the same for all indicies
relativeRateMortality <- function(ndviVal, baseNDVI, doseResponse) {
  ndviDiff <- ndviVal - baseNDVI
  rr <- exp(log(doseResponse) * (ndviDiff / 0.1))
}
## need to check this DAVID as I think it's wrong as written
## ndvi increase are based on a absoulate increase of 10% on the measured NDVI values, rather than an adjust NDVI to the base.
relativeRateMortality10 <- function(ndviVal, baseNDVI, doseResponse) {
  # add 10% increase
  ndviPlus10 <- ndviVal + (ndviVal * 0.1)
  # then subtract out the base rate
  ndviDiff <- ndviPlus10 - baseNDVI
  # calcualte the rate
  rr <- exp(log(doseResponse) * (ndviDiff / 0.1))
}

# health metrics are based on Ndvi value above the base rate
relativeRateStrokeDem <- function(ndviVal, baseNDVI, doseResponse) {
  ndviDiff <- ndviVal - baseNDVI
  rr <- exp(log(doseResponse) * (ndviDiff / 0.12))
}

relativeRateStrokeDem10 <- function(ndviVal, baseNDVI, doseResponse) {
  # add 10% increase
  ndviPlus10 <- ndviVal + (ndviVal * 0.1)
  # then subtract out the base rate
  ndviDiff <- ndviPlus10 - baseNDVI
  # calcualte the rate
  rr <- exp(log(doseResponse) * (ndviDiff / 0.12))
}


# relativeRisk <- 0.961 # this the ndvi value per city  - base NDVI
populationAttributableFraction <- function(relativeRate) {
  paf <- (relativeRate - 1) / relativeRate
  return(paf)
}

# Expected incidence
expectedIncidence <- function(population, incidenceRate) {
  pop <- population * incidenceRate
  return(pop)
}
# lives saved
livesSaved <- function(nIncidence, paf) {
  lives <- nIncidence * paf
}
