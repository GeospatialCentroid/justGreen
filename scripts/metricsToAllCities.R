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
# add in "Puerto Rico" mortality measure 
allCities_c$mortalityRate[allCities_c$state == "Puerto Rico"] <- 0.012828


# Dementia ----------------------------------------------------------------
demData <- read_csv("data/raw/dementia/Dementia_55.csv") |>
  dplyr::select(state = location, 
                DementiaRate = val)|>
  dplyr::mutate(DementiaRate = DementiaRate/100000)

allCities_d <- dplyr::left_join(x = allCities_c, demData, by = "state")
# Stroke ------------------------------------------------------------------
strokeData <- read_csv("data/raw/stroke/Stroke Incidence 2021.csv")|>
  dplyr::select(state = location, 
                StrokeRate = val)|>
  dplyr::mutate(StrokeRate = StrokeRate/100000)
allCities_s <- dplyr::left_join(x = allCities_d,strokeData, by = "state")

# add city pop for 55 + 
pop55 <- st_read("data/processed/top200_2023/allCities_55plus.gpkg") |>
  st_drop_geometry()|>
  dplyr::select(geoid = GEOID,popOver55_2023)
allCities_p <- dplyr::left_join(x = allCities_s, y = pop55, by = "geoid" )


# add measures the cities data  -------------------------------------------
allCities2 <- allCities_p |>
  dplyr::mutate(
    # mortality
    rr_Mortality = relativeRate(ndviVal = meanNDVI, baseNDVI = 0.1, doseResponse = doseResponseMortality),
    rr_Mortality_low = relativeRate(ndviVal = meanNDVI, baseNDVI = 0.1, doseResponse = drfMortality_low),
    rr_Mortality_high = relativeRate(ndviVal = meanNDVI, baseNDVI = 0.1, doseResponse = drfMortality_high),
    paf_Mortality = populationAttributableFraction(rr_Mortality),
    paf_Mortality_low = populationAttributableFraction(rr_Mortality_low),
    paf_Mortality_high = populationAttributableFraction(rr_Mortality_high),
    ls_Mortality = expectedIncidence(popOver20_2023, mortalityRate) |> livesSaved(paf_Mortality),
    ls_Mortality_low = expectedIncidence(popOver20_2023, mortalityRate) |> livesSaved(paf_Mortality_low),
    ls_Mortality_high = expectedIncidence(popOver20_2023, mortalityRate) |> livesSaved(paf_Mortality_high),
    # stroke 
    rr_Stroke = relativeRate(ndviVal = meanNDVI, baseNDVI = 0.1, doseResponse = doseResponseStroke),
    rr_Stroke_low = relativeRate(ndviVal = meanNDVI, baseNDVI = 0.1, doseResponse = drfStroke_low),
    rr_Stroke_high = relativeRate(ndviVal = meanNDVI, baseNDVI = 0.1, doseResponse = drfStroke_high),
    paf_Stroke = populationAttributableFraction(rr_Stroke),
    paf_Stroke_low = populationAttributableFraction(rr_Stroke_low),
    paf_Stroke_high = populationAttributableFraction(rr_Stroke_high),
    ls_Stroke = expectedIncidence(popOver55_2023, StrokeRate) |> livesSaved(paf_Stroke),
    ls_Stroke_low = expectedIncidence(popOver55_2023, StrokeRate) |> livesSaved(paf_Stroke_low),
    ls_Stroke_high = expectedIncidence(popOver55_2023, StrokeRate) |> livesSaved(paf_Stroke_high),
    # dementia
    rr_Dementia = relativeRate(ndviVal = meanNDVI, baseNDVI = 0.1, doseResponse = doseResponseDementia),
    rr_Dementia_low = relativeRate(ndviVal = meanNDVI, baseNDVI = 0.1, doseResponse = drfDementia_low),
    rr_Dementia_high = relativeRate(ndviVal = meanNDVI, baseNDVI = 0.1, doseResponse = drfDementia_high),
    paf_Dementia = populationAttributableFraction(rr_Dementia),
    paf_Dementia_low = populationAttributableFraction(rr_Dementia_low),
    paf_Dementia_high = populationAttributableFraction(rr_Dementia_high),
    ls_Dementia = expectedIncidence(popOver55_2023, DementiaRate) |> livesSaved(paf_Dementia),
    ls_Dementia_low = expectedIncidence(popOver55_2023, DementiaRate) |> livesSaved(paf_Dementia_low),
    ls_Dementia_high = expectedIncidence(popOver55_2023, DementiaRate) |> livesSaved(paf_Dementia_high)
  )|>
  st_drop_geometry() |>
  dplyr::select(-geom)


readr::write_csv(allCities2, "data/products/healthMeasures/allCities_2023_morDemStroke.csv")
