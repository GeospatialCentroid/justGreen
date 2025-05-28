# inputs 
citiesNDVI <- read_csv("data/processed/summaryNDVI/allCitiesNDVI.csv")
citiesPOP <- read_csv("data/processed/top200/top200Cities.csv") |>
  dplyr::select(
    geoid = GEOID,
    "totalPopulation",
    "pop18andOlder"
  )
allCities <- dplyr::left_join(x = citiesNDVI, y = citiesPOP, by = "geoid")

# Mortality --------------------------------------------------------------
mortalityRate <- 792.2/100000
# once I have the mortality data I'll need to develop this step. might be a whole workflow in itself 

# add measures the cities data  -------------------------------------------
allCities2 <- allCities |>
  dplyr::mutate(
    paf = populationAttributableFraction(ndviVal = meanNDVI, 
                                         rr = relativeRisk, 
                                         doseResponse = doseResponse),
    livesSaved = crudeDeathPrevented(population = pop18andOlder, 
                                     mortalityRate = mortalityRate, 
                                     paf = paf)
  )
readr::write_csv(allCities2, "data/products/healthMeasures/allCities.csv")
