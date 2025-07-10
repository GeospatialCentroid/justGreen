# inputs 
citiesNDVI <- read_csv("data/processed/summaryNDVI/allCitiesNDVI_2023.csv")
citiesPop <- st_read("data/processed/top200_2023/allCities.gpkg")

allCities <- dplyr::left_join(x = citiesNDVI, y = citiesPop, by = c("geoid" = "GEOID"))

source("functions/healthFunctions.R")

# Mortality -------------------------------------------------------------
countyMortality <-read_csv("data/raw/mortality/All Cause of Death 2023.csv") |>
  dplyr::mutate(countyGEOID = as.character(`County Code`),
                mortalityRate = as.numeric(`Crude Rate`)/100000)|>
  dplyr::mutate(countyGEOID = case_when(
    nchar(countyGEOID) == 4 ~ paste0("0",countyGEOID),
    TRUE ~ countyGEOID
    )
  )|>
  dplyr::select(countyGEOID, mortalityRate)
# assign mortalityRate 
allCities_c <- dplyr::left_join(x = allCities, y = countyMortality, by = "countyGEOID")  


# add measures the cities data  -------------------------------------------
allCities2 <- allCities_c |>
  dplyr::mutate(
    ndviPlus10 = meanNDVI + 0.1*meanNDVI,
    paf = populationAttributableFraction(ndviVal = meanNDVI, 
                                         rr = relativeRisk, 
                                         baseNDVI = 0.1,
                                         doseResponse = doseResponse),
    pafPlus10 = populationAttributableFraction(ndviVal = ndviPlus10, 
                                         rr = relativeRisk, 
                                         baseNDVI = 0.1,
                                         doseResponse = doseResponse),
    livesSaved = case_when(
      !is.na(mortalityRate) ~ crudeDeathPrevented(population = popOver20_2023,
                                     mortalityRate = mortalityRate,
                                     paf = paf),
      is.na(mortalityRate) ~ NA
    ),
    livesSavedPlus10 = case_when(
      !is.na(mortalityRate) ~ crudeDeathPrevented(population = popOver20_2023,
                                                  mortalityRate = mortalityRate,
                                                  paf = pafPlus10),
      is.na(mortalityRate) ~ NA
    )
  )|>
  st_drop_geometry() |>
  dplyr::select(-geom)


readr::write_csv(allCities2, "data/products/healthMeasures/allCities.csv")
