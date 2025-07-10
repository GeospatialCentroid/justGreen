


source("functions/healthFunctions.R")



# inputs 
ctFiles <- list.files(path = "data/processed/summaryNDVI",
                      pattern = "_2023NDVI.csv",
                      full.names = TRUE)|>
  read_csv()|>
  dplyr::mutate(geoid = as.character(geoid))

ctFiles$countyGEOID <- stringr::str_sub(string = ctFiles$geoid, start = 1, end = 5)
# population data 
ctPop <- list.files(path = "data/processed/censusGeographies",
                      pattern = ".gpkg",
                      full.names = TRUE) |>
  lapply(st_read)|>
  lapply(st_drop_geometry)|>
  dplyr::bind_rows() |>
  dplyr::select(geoid = "GEOID",
                over18)
# join the datasets
allCT <- dplyr::left_join(ctFiles, ctPop, by = "geoid")

# Mortality --------------------------------------------------------------
mortalityRate <- 792.2/100000
# once I have the mortality data I'll need to develop this step. might be a whole workflow in itself 
countyMortality <-read_csv("data/raw/mortality/All Cause of Death 2023.csv") |>
  dplyr::mutate(countyGEOID = as.character(`County Code`),
                mortalityRate = as.numeric(`Crude Rate`)/100000)|>
  dplyr::mutate(countyGEOID = case_when(
    nchar(countyGEOID) == 4 ~ paste0("0",countyGEOID),
    TRUE ~ countyGEOID
  )
  )|>
  dplyr::select(countyGEOID, mortalityRate)
# join on county GEOID 
allCT_c <- dplyr::left_join(x = allCT, y = countyMortality, by = "countyGEOID")

# add measures the cities data  -------------------------------------------
allct <- allCT_c |>
  dplyr::mutate(
    # current 
    paf = populationAttributableFraction(ndviVal = meanNDVI, 
                                         rr = relativeRisk, 
                                         baseNDVI = 0.1,
                                         doseResponse = doseResponse),
    livesSaved = crudeDeathPrevented(population = over18, 
                                     mortalityRate = mortalityRate, 
                                     paf = paf),
    #plus 10% 
    ndviPlus10 = meanNDVI + 0.1*meanNDVI,
    pafPlus10 = populationAttributableFraction(ndviVal = ndviPlus10, 
                                               rr = relativeRisk, 
                                               baseNDVI = 0.1,
                                               doseResponse = doseResponse),
    livesSavedPlus10 = case_when(
      !is.na(mortalityRate) ~ crudeDeathPrevented(population = over18,
                                                  mortalityRate = mortalityRate,
                                                  paf = pafPlus10),
      is.na(mortalityRate) ~ NA
    )
  )
readr::write_csv(allct, "data/products/healthMeasures/allcensusTracts.csv")
