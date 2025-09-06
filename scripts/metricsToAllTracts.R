


source("functions/healthFunctions.R")

export1 <- "data/products/healthMeasures/ct_pop.csv"

if(!file.exists(export1)){
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
    dplyr::select(geoid = "GEOID",over20,over35, over55)
  # join the datasets
  allCT <- dplyr::left_join(ctFiles, ctPop, by = "geoid")
  # export 
  write_csv(allCT, export1)
    }else{
  allCT <- read_csv(export1)
}



# Mortality -------------------------------------------------------------
countyMortality <-read_csv("data/raw/mortality/All Cause of Death 2023.csv")|>
  dplyr::mutate(countyGEOID = as.character(`County Code`),
                mortalityRate = as.numeric(`Crude Rate`)/100000)|>
  dplyr::mutate(countyGEOID = case_when(
    nchar(countyGEOID) == 4 ~ paste0("0",countyGEOID),
    TRUE ~ countyGEOID
  )
  )|>
  dplyr::select(countyGEOID, mortalityRate)
# assign mortalityRate 
allCT_c <- dplyr::left_join(x = allCT, y = countyMortality, by = "countyGEOID")  
# add in "Puerto Rico" mortality measure 
allCT_c$mortalityRate[allCT_c$state == "Puerto Rico"] <- 0.012828


# Dementia ----------------------------------------------------------------
demData <- read_csv("data/raw/dementia/Dementia_55.csv") |>
  dplyr::select(state = location, 
                DementiaRate = val)|>
  dplyr::mutate(DementiaRate = DementiaRate/100000)

allCT_d <- dplyr::left_join(x = allCT_c, demData, by = "state")


# Stroke ------------------------------------------------------------------
strokeData <- read_csv("data/raw/stroke/Stroke Incidence 2021.csv")|>
  dplyr::select(state = location, 
                StrokeRate = val)|>
  dplyr::mutate(StrokeRate = StrokeRate/100000)
allCT_s <- dplyr::left_join(x = allCT_d,strokeData, by = "state")

# prepped all ct data ---- 
ct <- allCT_s |>
  dplyr::select(
    "geoid","city","state","totalCells","meanNDVI","standardDevNDVI", "countyGEOID",  
    popOver20_2023 = "over20",
    popOver35_2023 = "over35",
    popOver55_2023 = "over55",
    "mortalityRate","DementiaRate","StrokeRate" 
  )


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
allCT_2 <- ct |>
  dplyr::mutate(
    # mortality - uses pop over 20 
    rr_Mortality = relativeRateMortiality(ndviVal = meanNDVI, baseNDVI = baseNDVI, doseResponse = doseResponseMortality),
    rr_Mortality_low = relativeRateMortiality(ndviVal = meanNDVI, baseNDVI = baseNDVI, doseResponse = drfMortality_low),
    rr_Mortality_high = relativeRateMortiality(ndviVal = meanNDVI, baseNDVI = baseNDVI, doseResponse = drfMortality_high),
    paf_Mortality = populationAttributableFraction(rr_Mortality),
    paf_Mortality_low = populationAttributableFraction(rr_Mortality_low),
    paf_Mortality_high = populationAttributableFraction(rr_Mortality_high),
    ls_Mortality = expectedIncidence(popOver20_2023, mortalityRate) |> livesSaved(paf_Mortality),
    ls_Mortality_low = expectedIncidence(popOver20_2023, mortalityRate) |> livesSaved(paf_Mortality_low),
    ls_Mortality_high = expectedIncidence(popOver20_2023, mortalityRate) |> livesSaved(paf_Mortality_high),
    ls_Mortality_Rate = (ls_Mortality/popOver20_2023) * 100000,
    ls_Mortality_low_Rate = (ls_Mortality_low/popOver20_2023) * 100000,
    ls_Mortality_high_Rate = (ls_Mortality_high/popOver20_2023) * 100000,
    
    # 10% ndvi increase 
    rr_Mortality10 = relativeRateMortiality10(ndviVal = meanNDVI, baseNDVI = baseNDVI, doseResponse = doseResponseMortality),
    rr_Mortality_low10 = relativeRateMortiality10(ndviVal = meanNDVI, baseNDVI = baseNDVI, doseResponse = drfMortality_low),
    rr_Mortality_high10 = relativeRateMortiality10(ndviVal = meanNDVI, baseNDVI = baseNDVI, doseResponse = drfMortality_high),
    paf_Mortality10 = populationAttributableFraction(rr_Mortality10),
    paf_Mortality_low10 = populationAttributableFraction(rr_Mortality_low10),
    paf_Mortality_high10 = populationAttributableFraction(rr_Mortality_high10),
    ls_Mortality10 = expectedIncidence(popOver20_2023, mortalityRate) |> livesSaved(paf_Mortality10),
    ls_Mortality_low10 = expectedIncidence(popOver20_2023, mortalityRate) |> livesSaved(paf_Mortality_low10),
    ls_Mortality_high10 = expectedIncidence(popOver20_2023, mortalityRate) |> livesSaved(paf_Mortality_high10),
    ls_Mortality10_Rate	= (ls_Mortality10/popOver20_2023) * 100000,
    ls_Mortality_low10_Rate	= (ls_Mortality_low10/popOver20_2023) * 100000,
    ls_Mortality_high10_Rate	= (ls_Mortality/popOver20_2023) * 100000,
    
    # change in mortality 
    ls_Mortality_Change = ls_Mortality10 - ls_Mortality,
    ls_Mortality_Change_low = ls_Mortality_low10 - ls_Mortality_low,
    ls_Mortality_Change_high = ls_Mortality_high10 - ls_Mortality_high,
    ls_Mortality_Change_Rate = (ls_Mortality_Change/popOver20_2023) * 100000,
    ls_Mortality_Change_low_Rate = (ls_Mortality_Change_low/popOver20_2023) * 100000,
    ls_Mortality_Change_high_Rate = (ls_Mortality_Change_high/popOver20_2023) * 100000,
    
    # stroke - uses pop over 35 
    rr_Stroke = relativeRateStrokeDem(ndviVal = meanNDVI, baseNDVI = baseNDVI, doseResponse = doseResponseStroke),
    rr_Stroke_low = relativeRateStrokeDem(ndviVal = meanNDVI, baseNDVI = baseNDVI, doseResponse = drfStroke_low),
    rr_Stroke_high = relativeRateStrokeDem(ndviVal = meanNDVI, baseNDVI = baseNDVI, doseResponse = drfStroke_high),
    paf_Stroke = populationAttributableFraction(rr_Stroke),
    paf_Stroke_low = populationAttributableFraction(rr_Stroke_low),
    paf_Stroke_high = populationAttributableFraction(rr_Stroke_high),
    ls_Stroke = expectedIncidence(popOver35_2023, StrokeRate) |> livesSaved(paf_Stroke),
    ls_Stroke_low = expectedIncidence(popOver35_2023, StrokeRate) |> livesSaved(paf_Stroke_low),
    ls_Stroke_high = expectedIncidence(popOver35_2023, StrokeRate) |> livesSaved(paf_Stroke_high),
    ls_Stroke_Rate	= (ls_Stroke/popOver35_2023) * 100000,
    ls_Stroke_low_Rate		= (ls_Stroke_low/popOver35_2023) * 100000,
    ls_Stroke_high_Rate = (ls_Stroke_high/popOver35_2023) * 100000,
    
    # 10% ndvi increase 
    rr_Stroke10 = relativeRateStrokeDem10(ndviVal = meanNDVI, baseNDVI = baseNDVI, doseResponse = doseResponseStroke),
    rr_Stroke_low10 = relativeRateStrokeDem10(ndviVal = meanNDVI, baseNDVI = baseNDVI, doseResponse = drfStroke_low),
    rr_Stroke_high10 = relativeRateStrokeDem10(ndviVal = meanNDVI, baseNDVI = baseNDVI, doseResponse = drfStroke_high),
    paf_Stroke10 = populationAttributableFraction(rr_Stroke10),
    paf_Stroke_low10 = populationAttributableFraction(rr_Stroke_low10),
    paf_Stroke_high10 = populationAttributableFraction(rr_Stroke_high10),
    ls_Stroke10 = expectedIncidence(popOver35_2023, StrokeRate) |> livesSaved(paf_Stroke10),
    ls_Stroke_low10 = expectedIncidence(popOver35_2023, StrokeRate) |> livesSaved(paf_Stroke_low10),
    ls_Stroke_high10 = expectedIncidence(popOver35_2023, StrokeRate) |> livesSaved(paf_Stroke_high10),
    ls_Stroke10_Rate = (ls_Stroke10/popOver35_2023) * 100000,
    ls_Stroke_low10_Rate	= (ls_Stroke_low10/popOver35_2023) * 100000,
    ls_Stroke_high10_Rate	= (ls_Stroke_high10/popOver35_2023) * 100000,
    # lives saved change 
    ls_Stroke_Change = ls_Stroke10 - ls_Stroke,
    ls_Stroke_Change_low = ls_Stroke_low10 - ls_Stroke_low,
    ls_Stroke_Change_high = ls_Stroke_high10 - ls_Stroke_high,
    ls_Stroke_Change_Rate	= (ls_Stroke_Change/popOver35_2023) * 100000,
    ls_Stroke_Change_low_Rate	= (ls_Stroke_Change_low/popOver35_2023) * 100000,
    ls_Stroke_Change_high_Rate= (ls_Stroke_Change_high/popOver35_2023) * 100000,
    
    # dementia - uses pop over 55 
    rr_Dementia = relativeRateStrokeDem(ndviVal = meanNDVI, baseNDVI = baseNDVI, doseResponse = doseResponseDementia),
    rr_Dementia_low = relativeRateStrokeDem(ndviVal = meanNDVI, baseNDVI = baseNDVI, doseResponse = drfDementia_low),
    rr_Dementia_high = relativeRateStrokeDem(ndviVal = meanNDVI, baseNDVI = baseNDVI, doseResponse = drfDementia_high),
    paf_Dementia = populationAttributableFraction(rr_Dementia),
    paf_Dementia_low = populationAttributableFraction(rr_Dementia_low),
    paf_Dementia_high = populationAttributableFraction(rr_Dementia_high),
    ls_Dementia = expectedIncidence(popOver55_2023, DementiaRate) |> livesSaved(paf_Dementia),
    ls_Dementia_low = expectedIncidence(popOver55_2023, DementiaRate) |> livesSaved(paf_Dementia_low),
    ls_Dementia_high = expectedIncidence(popOver55_2023, DementiaRate) |> livesSaved(paf_Dementia_high),
    ls_Dementia_Rate	= (ls_Dementia/popOver55_2023) * 100000,
    ls_Dementia_low_Rate	= (ls_Dementia_low/popOver55_2023) * 100000,
    ls_Dementia_high_Rate = (ls_Dementia_high/popOver55_2023) * 100000,
    #10% NDVI increase 
    rr_Dementia10 = relativeRateStrokeDem10(ndviVal = meanNDVI, baseNDVI = baseNDVI, doseResponse = doseResponseDementia),
    paf_Dementia10 = populationAttributableFraction(rr_Dementia10),
    ls_Dementia10 = expectedIncidence(popOver55_2023, DementiaRate) |> livesSaved(paf_Dementia10),
    
    
    rr_Dementia_low10 = relativeRateStrokeDem10(ndviVal = meanNDVI, baseNDVI = baseNDVI, doseResponse = drfDementia_low),
    paf_Dementia_low10 = populationAttributableFraction(rr_Dementia_low10),
    ls_Dementia_low10 = expectedIncidence(popOver55_2023, DementiaRate) |> livesSaved(paf_Dementia_low10),
    
    rr_Dementia_high10 = relativeRateStrokeDem10(ndviVal = meanNDVI, baseNDVI = baseNDVI, doseResponse = drfDementia_high),
    paf_Dementia_high10 = populationAttributableFraction(rr_Dementia_high10),
    ls_Dementia_high10 = expectedIncidence(popOver55_2023, DementiaRate) |> livesSaved(paf_Dementia_high10),
    ls_Dementia_10_Rate = (ls_Dementia10/popOver55_2023) * 100000,
    ls_Dementia_low10_Rate	= (ls_Dementia_low10/popOver55_2023) * 100000,
    ls_Dementia_high10_Rate= (ls_Dementia_high10/popOver55_2023) * 100000,
    
    # change in NDVI increase 
    ls_Dementia_Change = ls_Dementia10 - ls_Dementia,
    ls_Dementia_Change_low = ls_Dementia_low10 - ls_Dementia_low,
    ls_Dementia_Change_high = ls_Dementia_high10 - ls_Dementia_high,
    ls_Dementia_Change_Rate	= (ls_Dementia_Change/popOver55_2023) * 100000,
    ls_Dementia_Change_low_Rate	= (ls_Dementia_Change_low/popOver55_2023) * 100000,
    ls_Dementia_Change_high_Rate= (ls_Dementia_Change_high/popOver55_2023) * 100000,
  )|>
  st_drop_geometry()

# export the results 
readr::write_csv(allCT_2, "data/products/healthMeasures/allCT_2023_morDemStroke_with10percentAdjust.csv")
