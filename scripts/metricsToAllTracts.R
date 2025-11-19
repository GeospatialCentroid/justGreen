pacman::p_load(readr, terra, dplyr, sf, stringr)

source("functions/healthFunctions.R")

cache_file <- "data/products/healthMeasures/ct_pop.csv"
# 1. Load processed NDVI files

ctFiles <- readr::read_csv(
  "data/processed/summaryNDVI/allCensusTractsNDVI_2023.csv"
) |>
  dplyr::mutate(
    countyGEOID = str_sub(GEOID, 1, 5)
  ) |>
  dplyr::select(
    geoid = GEOID,
    state,
    totalCells,
    meanNDVI,
    standardDevNDVI,
    countyGEOID,
    city = cityName
  )

ctFiles2 <- list.files(
  "data/processed/summaryNDVI",
  pattern = "_2023NDVI.csv",
  full.names = TRUE
) |>
  purrr::map_dfr(
    readr::read_csv,
    col_types = cols(
      .default = "c", # Forces all columns (incl. geoid) to char initially
      meanNDVI = "d",
      totalCells = "d",
      standardDevNDVI = "d"
    )
  ) |>
  dplyr::mutate(
    # Ensure clean 11-digit character string
    geoid = str_pad(geoid, width = 11, side = "left", pad = "0"),
    countyGEOID = str_sub(geoid, 1, 5)
  )

# 2. Load Census Geographies (Populations)
ctPop <- list.files(
  "data/processed/censusGeographies",
  pattern = ".gpkg",
  full.names = TRUE
) |>
  purrr::map_dfr(function(x) {
    st_read(x, quiet = TRUE) |>
      st_drop_geometry() |>
      # Select and clean IMMEDIATELY inside the loop to avoid schema mismatches
      dplyr::select(geoid = GEOID, over20, over35, over55) |>
      dplyr::mutate(
        # FIX: Force character and pad zeros to match NDVI data
        geoid = str_pad(
          as.character(geoid),
          width = 11,
          side = "left",
          pad = "0"
        )
      )
  })

# 3. Join and Save
allCT <- dplyr::left_join(ctFiles, ctPop, by = "geoid")

# Validation: Check if rows exploded or were lost
if (nrow(allCT) > nrow(ctFiles)) {
  warning("Join duplicated rows! Check inputs.")
}

write_csv(allCT, cache_file)

# browser()

# 2. Load Disease Data ----------------------------------------------------

# Mortality
countyMortality <- readr::read_csv(
  "data/raw/mortality/All Cause of Death 2023.csv"
) |>
  dplyr::transmute(
    countyGEOID = str_pad(
      as.character(`County Code`),
      width = 5,
      side = "left",
      pad = "0"
    ),
    mortalityRate = as.numeric(`Crude Rate`) / 100000
  )

# Dementia
demData <- readr::read_csv("data/raw/dementia/Dementia_55.csv") |>
  dplyr::transmute(state = location, DementiaRate = val / 100000)

# Stroke
strokeData <- readr::read_csv("data/raw/stroke/Stroke Incidence 2021.csv") |>
  dplyr::transmute(state = location, StrokeRate = val / 100000)

# SVI Data (Loaded here to be ready for the join)
sliData <- list.files("data/raw/socialVul", full.names = TRUE) |>
  readr::read_csv() |>
  dplyr::transmute(
    geoid = str_pad(as.character(FIPS), width = 11, side = "left", pad = "0"),
    RPL_THEMES
  )


# 3. Prepare & Join Data --------------------------------------------------

ct_prep <- allCT |>
  # Join Mortality
  left_join(countyMortality, by = "countyGEOID") |>
  # Join Dementia
  left_join(demData, by = "state") |>
  # Join Stroke
  left_join(strokeData, by = "state") |>
  # Join SVI
  left_join(sliData, by = "geoid") |>
  # Clean Rates and Rename Columns for Functions
  mutate(
    mortalityRate = case_when(
      state == "Puerto Rico" ~ 0.012828,
      city == "Bridgeport city" ~ 0.006106, # Validation needed
      TRUE ~ mortalityRate
    )
  ) |>
  dplyr::select(
    geoid,
    city,
    state,
    countyGEOID,
    meanNDVI,
    totalCells,
    standardDevNDVI,
    # Rename pops to match function requirements
    popOver20_2023 = over20,
    popOver35_2023 = over35,
    popOver55_2023 = over55,
    # Rates
    mortalityRate,
    DementiaRate,
    StrokeRate,
    # Social Vulnerability
    RPL_THEMES
  )


# 4. Define Parameters ----------------------------------------------------

baseNDVI <- 0.1

# Dose Response Parameters
doseResponseMortality <- 0.96
drfMortality_low <- 0.94
drfMortality_high <- 0.97

doseResponseStroke <- 0.97
drfStroke_low <- 0.96
drfStroke_high <- 0.98

doseResponseDementia <- 0.96
drfDementia_low <- 0.95
drfDementia_high <- 0.98


# 5. Calculate Health Measures --------------------------------------------

allCT_final <- ct_prep |>
  dplyr::mutate(
    # --- Mortality (Pop > 20) --------------------------------------------
    rr_Mortality = relativeRateMortality(
      meanNDVI,
      baseNDVI,
      doseResponseMortality
    ),
    rr_Mortality_low = relativeRateMortality(
      meanNDVI,
      baseNDVI,
      drfMortality_low
    ),
    rr_Mortality_high = relativeRateMortality(
      meanNDVI,
      baseNDVI,
      drfMortality_high
    ),

    paf_Mortality = populationAttributableFraction(rr_Mortality),
    paf_Mortality_low = populationAttributableFraction(rr_Mortality_low),
    paf_Mortality_high = populationAttributableFraction(rr_Mortality_high),

    ls_Mortality = livesSaved(
      expectedIncidence(popOver20_2023, mortalityRate),
      paf_Mortality
    ),
    ls_Mortality_low = livesSaved(
      expectedIncidence(popOver20_2023, mortalityRate),
      paf_Mortality_low
    ),
    ls_Mortality_high = livesSaved(
      expectedIncidence(popOver20_2023, mortalityRate),
      paf_Mortality_high
    ),

    # 10% Increase
    rr_Mortality10 = relativeRateMortality10(
      meanNDVI,
      baseNDVI,
      doseResponseMortality
    ),
    rr_Mortality_low10 = relativeRateMortality10(
      meanNDVI,
      baseNDVI,
      drfMortality_low
    ),
    rr_Mortality_high10 = relativeRateMortality10(
      meanNDVI,
      baseNDVI,
      drfMortality_high
    ),

    paf_Mortality10 = populationAttributableFraction(rr_Mortality10),
    paf_Mortality_low10 = populationAttributableFraction(rr_Mortality_low10),
    paf_Mortality_high10 = populationAttributableFraction(rr_Mortality_high10),

    ls_Mortality10 = livesSaved(
      expectedIncidence(popOver20_2023, mortalityRate),
      paf_Mortality10
    ),
    ls_Mortality_low10 = livesSaved(
      expectedIncidence(popOver20_2023, mortalityRate),
      paf_Mortality_low10
    ),
    ls_Mortality_high10 = livesSaved(
      expectedIncidence(popOver20_2023, mortalityRate),
      paf_Mortality_high10
    ),

    # Mortality Deltas
    ls_Mortality_Change = ls_Mortality10 - ls_Mortality,
    ls_Mortality_Change_low = ls_Mortality_low10 - ls_Mortality_low,
    ls_Mortality_Change_high = ls_Mortality_high10 - ls_Mortality_high,

    # --- Stroke (Pop > 35) -----------------------------------------------
    rr_Stroke = relativeRateStrokeDem(meanNDVI, baseNDVI, doseResponseStroke),
    rr_Stroke_low = relativeRateStrokeDem(meanNDVI, baseNDVI, drfStroke_low),
    rr_Stroke_high = relativeRateStrokeDem(meanNDVI, baseNDVI, drfStroke_high),

    paf_Stroke = populationAttributableFraction(rr_Stroke),
    paf_Stroke_low = populationAttributableFraction(rr_Stroke_low),
    paf_Stroke_high = populationAttributableFraction(rr_Stroke_high),

    ls_Stroke = livesSaved(
      expectedIncidence(popOver35_2023, StrokeRate),
      paf_Stroke
    ),
    ls_Stroke_low = livesSaved(
      expectedIncidence(popOver35_2023, StrokeRate),
      paf_Stroke_low
    ),
    ls_Stroke_high = livesSaved(
      expectedIncidence(popOver35_2023, StrokeRate),
      paf_Stroke_high
    ),

    # 10% Increase
    rr_Stroke10 = relativeRateStrokeDem10(
      meanNDVI,
      baseNDVI,
      doseResponseStroke
    ),
    rr_Stroke_low10 = relativeRateStrokeDem10(
      meanNDVI,
      baseNDVI,
      drfStroke_low
    ),
    rr_Stroke_high10 = relativeRateStrokeDem10(
      meanNDVI,
      baseNDVI,
      drfStroke_high
    ),

    paf_Stroke10 = populationAttributableFraction(rr_Stroke10),
    paf_Stroke_low10 = populationAttributableFraction(rr_Stroke_low10),
    paf_Stroke_high10 = populationAttributableFraction(rr_Stroke_high10),

    ls_Stroke10 = livesSaved(
      expectedIncidence(popOver35_2023, StrokeRate),
      paf_Stroke10
    ),
    ls_Stroke_low10 = livesSaved(
      expectedIncidence(popOver35_2023, StrokeRate),
      paf_Stroke_low10
    ),
    ls_Stroke_high10 = livesSaved(
      expectedIncidence(popOver35_2023, StrokeRate),
      paf_Stroke_high10
    ),

    # Stroke Deltas
    ls_Stroke_Change = ls_Stroke10 - ls_Stroke,
    ls_Stroke_Change_low = ls_Stroke_low10 - ls_Stroke_low,
    ls_Stroke_Change_high = ls_Stroke_high10 - ls_Stroke_high,

    # --- Dementia (Pop > 55) ---------------------------------------------
    rr_Dementia = relativeRateStrokeDem(
      meanNDVI,
      baseNDVI,
      doseResponseDementia
    ),
    rr_Dementia_low = relativeRateStrokeDem(
      meanNDVI,
      baseNDVI,
      drfDementia_low
    ),
    rr_Dementia_high = relativeRateStrokeDem(
      meanNDVI,
      baseNDVI,
      drfDementia_high
    ),

    paf_Dementia = populationAttributableFraction(rr_Dementia),
    paf_Dementia_low = populationAttributableFraction(rr_Dementia_low),
    paf_Dementia_high = populationAttributableFraction(rr_Dementia_high),

    ls_Dementia = livesSaved(
      expectedIncidence(popOver55_2023, DementiaRate),
      paf_Dementia
    ),
    ls_Dementia_low = livesSaved(
      expectedIncidence(popOver55_2023, DementiaRate),
      paf_Dementia_low
    ),
    ls_Dementia_high = livesSaved(
      expectedIncidence(popOver55_2023, DementiaRate),
      paf_Dementia_high
    ),

    # 10% Increase
    rr_Dementia10 = relativeRateStrokeDem10(
      meanNDVI,
      baseNDVI,
      doseResponseDementia
    ),
    rr_Dementia_low10 = relativeRateStrokeDem10(
      meanNDVI,
      baseNDVI,
      drfDementia_low
    ),
    rr_Dementia_high10 = relativeRateStrokeDem10(
      meanNDVI,
      baseNDVI,
      drfDementia_high
    ),

    paf_Dementia10 = populationAttributableFraction(rr_Dementia10),
    paf_Dementia_low10 = populationAttributableFraction(rr_Dementia_low10),
    paf_Dementia_high10 = populationAttributableFraction(rr_Dementia_high10),

    ls_Dementia10 = livesSaved(
      expectedIncidence(popOver55_2023, DementiaRate),
      paf_Dementia10
    ),
    ls_Dementia_low10 = livesSaved(
      expectedIncidence(popOver55_2023, DementiaRate),
      paf_Dementia_low10
    ),
    ls_Dementia_high10 = livesSaved(
      expectedIncidence(popOver55_2023, DementiaRate),
      paf_Dementia_high10
    ),

    # Dementia Deltas
    ls_Dementia_Change = ls_Dementia10 - ls_Dementia,
    ls_Dementia_Change_low = ls_Dementia_low10 - ls_Dementia_low,
    ls_Dementia_Change_high = ls_Dementia_high10 - ls_Dementia_high
  ) |>
  # Calculate Rates (per 100k)
  mutate(
    across(
      contains("ls_Mortality"),
      ~ (.x / popOver20_2023) * 100000,
      .names = "{.col}_Rate"
    ),
    across(
      contains("ls_Stroke"),
      ~ (.x / popOver35_2023) * 100000,
      .names = "{.col}_Rate"
    ),
    across(
      contains("ls_Dementia"),
      ~ (.x / popOver55_2023) * 100000,
      .names = "{.col}_Rate"
    )
  )


# 6. Export ---------------------------------------------------------------

readr::write_csv(
  allCT_final,
  "data/products/healthMeasures/allCT_2023_morDemStroke_with10percentAdjust_svi.csv"
)
