# inputs 
citiesNDVI <- read_csv("data/processed/summaryNDVI/allCitiesNDVI_2023.csv")
citiesPop <- st_read("data/processed/top200_2023/allCities.gpkg")

allCities <- dplyr::left_join(x = citiesNDVI, y = citiesPop, by = c("geoid" = "GEOID")) |>
  dplyr::select(
    # city data layer 
    geoid, city, state, totalCells, meanNDVI, standardDevNDVI, 
    # county join 
    countyGEOID, popOver20_2023,
    #geom 
    geom
  )

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



# Set the based measures  -------------------------------------------------
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


# add measures the cities data  -------------------------------------------
allCities2 <- allCities_p |>
  dplyr::mutate(
    # mortality
    rr_Mortality = relativeRateMortiality(ndviVal = meanNDVI, baseNDVI = baseNDVI, doseResponse = doseResponseMortality),
    rr_Mortality_low = relativeRateMortiality(ndviVal = meanNDVI, baseNDVI = baseNDVI, doseResponse = drfMortality_low),
    rr_Mortality_high = relativeRateMortiality(ndviVal = meanNDVI, baseNDVI = baseNDVI, doseResponse = drfMortality_high),
    paf_Mortality = populationAttributableFraction(rr_Mortality),
    paf_Mortality_low = populationAttributableFraction(rr_Mortality_low),
    paf_Mortality_high = populationAttributableFraction(rr_Mortality_high),
    ls_Mortality = expectedIncidence(popOver20_2023, mortalityRate) |> livesSaved(paf_Mortality),
    ls_Mortality_low = expectedIncidence(popOver20_2023, mortalityRate) |> livesSaved(paf_Mortality_low),
    ls_Mortality_high = expectedIncidence(popOver20_2023, mortalityRate) |> livesSaved(paf_Mortality_high),
    ls_Mortality10 = relativeRateMortiality10(ndviVal = meanNDVI, baseNDVI = baseNDVI, doseResponse = doseResponseMortality),
    paf_Mortality10 = populationAttributableFraction(ls_Mortality10),
    ls_Mortality10 = expectedIncidence(popOver20_2023, mortalityRate) |> livesSaved(paf_Mortality10),
    # stroke 
    rr_Stroke = relativeRateStrokeDem(ndviVal = meanNDVI, baseNDVI = baseNDVI, doseResponse = doseResponseStroke),
    rr_Stroke_low = relativeRateStrokeDem(ndviVal = meanNDVI, baseNDVI = baseNDVI, doseResponse = drfStroke_low),
    rr_Stroke_high = relativeRateStrokeDem(ndviVal = meanNDVI, baseNDVI = baseNDVI, doseResponse = drfStroke_high),
    paf_Stroke = populationAttributableFraction(rr_Stroke),
    paf_Stroke_low = populationAttributableFraction(rr_Stroke_low),
    paf_Stroke_high = populationAttributableFraction(rr_Stroke_high),
    ls_Stroke = expectedIncidence(popOver55_2023, StrokeRate) |> livesSaved(paf_Stroke),
    ls_Stroke_low = expectedIncidence(popOver55_2023, StrokeRate) |> livesSaved(paf_Stroke_low),
    ls_Stroke_high = expectedIncidence(popOver55_2023, StrokeRate) |> livesSaved(paf_Stroke_high),
    rr_Stroke10 = relativeRateStrokeDem10(ndviVal = meanNDVI, baseNDVI = baseNDVI, doseResponse = doseResponseStroke),
    paf_Stroke10 = populationAttributableFraction(rr_Stroke10),
    ls_Stroke10 = expectedIncidence(popOver55_2023, StrokeRate) |> livesSaved(paf_Stroke10),
    
    # dementia
    rr_Dementia = relativeRateStrokeDem(ndviVal = meanNDVI, baseNDVI = baseNDVI, doseResponse = doseResponseDementia),
    rr_Dementia_low = relativeRateStrokeDem(ndviVal = meanNDVI, baseNDVI = baseNDVI, doseResponse = drfDementia_low),
    rr_Dementia_high = relativeRateStrokeDem(ndviVal = meanNDVI, baseNDVI = baseNDVI, doseResponse = drfDementia_high),
    paf_Dementia = populationAttributableFraction(rr_Dementia),
    paf_Dementia_low = populationAttributableFraction(rr_Dementia_low),
    paf_Dementia_high = populationAttributableFraction(rr_Dementia_high),
    ls_Dementia = expectedIncidence(popOver55_2023, DementiaRate) |> livesSaved(paf_Dementia),
    ls_Dementia_low = expectedIncidence(popOver55_2023, DementiaRate) |> livesSaved(paf_Dementia_low),
    ls_Dementia_high = expectedIncidence(popOver55_2023, DementiaRate) |> livesSaved(paf_Dementia_high),
    rr_Dementia10 = relativeRateStrokeDem10(ndviVal = meanNDVI, baseNDVI = baseNDVI, doseResponse = doseResponseDementia),
    paf_Dementia10 = populationAttributableFraction(rr_Dementia10),
    ls_Dementia10 = expectedIncidence(popOver55_2023, DementiaRate) |> livesSaved(paf_Dementia10),
  )|>
  st_drop_geometry() |>
  dplyr::select(-geom)

# %over 55
percentOld <- allCities2 |>
  dplyr::select("geoid","city","state","totalCells","popOver20_2023", "popOver55_2023")|>
  dplyr::mutate(percent55plus = (popOver55_2023/popOver20_2023) * 100)


readr::write_csv(allCities2, "data/products/healthMeasures/allCities_2023_morDemStroke_with10percentAdjust.csv")
#testing 
df2 <- read_csv("data/products/healthMeasures/allCities_2023_morDemStroke.csv")
