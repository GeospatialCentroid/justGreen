# inputs 
ctFiles <- list.files(path = "data/processed/summaryNDVI",
                      pattern = "_NDVI.csv",
                      full.names = TRUE)|>
  read_csv()
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

# add measures the cities data  -------------------------------------------
allct <- allCT |>
  dplyr::mutate(
    paf = populationAttributableFraction(ndviVal = meanNDVI, 
                                         rr = relativeRisk, 
                                         doseResponse = doseResponse),
    livesSaved = crudeDeathPrevented(population = over18, 
                                     mortalityRate = mortalityRate, 
                                     paf = paf)
  )
readr::write_csv(allct, "data/products/healthMeasures/allcensusTracts.csv")
