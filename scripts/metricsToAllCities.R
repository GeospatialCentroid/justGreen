pacman::p_load(readr, terra, dplyr, sf, stringr)

source("functions/healthFunctions.R")

# 1. Load Inputs ----------------------------------------------------------

# Load Cities NDVI
citiesNDVI <- read_csv(
  "data/processed/summaryNDVI/allCitiesNDVI_2023.csv",
  col_types = cols(geoid = col_character()) # Ensure GEOID is char
)

# Load City Populations
citiesPop <- st_read(
  "data/processed/top200_2023/allCities.gpkg",
  quiet = TRUE
) |>
  dplyr::select(-popOver20_2023)

# Load Mortality Data
countyMortality <- read_csv("data/raw/mortality/All Cause of Death 2023.csv") |>
  dplyr::transmute(
    countyGEOID = str_pad(as.character(`County Code`), width = 5, pad = "0"),
    mortalityRate = as.numeric(`Crude Rate`) / 100000
  )

# Load Dementia Data
demData <- read_csv("data/raw/dementia/Dementia_55.csv") |>
  dplyr::transmute(
    state = location,
    DementiaRate = val / 100000
  )

# Load Stroke Data
strokeData <- read_csv("data/raw/stroke/Stroke Incidence 2021.csv") |>
  dplyr::transmute(
    state = location,
    StrokeRate = val / 100000
  )

# Load Age-Specific Populations
population_subgroups <- st_read(
  "data/processed/top200_2023/allCities_35_55.gpkg",
  quiet = TRUE
) |>
  st_drop_geometry() |>
  dplyr::select(geoid = GEOID, popOver20_2023, popOver35_2023, popOver55_2023)


# 2. Join & Prepare Data --------------------------------------------------

allCities_prep <- citiesNDVI |>
  # Join City Geometry
  left_join(citiesPop, by = c("geoid" = "GEOID")) |>
  # Join Mortality
  left_join(countyMortality, by = "countyGEOID") |>
  # Join Dementia
  left_join(demData, by = "state") |>
  # Join Stroke
  left_join(strokeData, by = "state") |>
  # Join Population Subgroups
  left_join(population_subgroups, by = "geoid") |>
  # Clean up Rates (Manual Overrides)
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
    totalCells,
    meanNDVI,
    standardDevNDVI,
    popOver20_2023,
    popOver35_2023,
    popOver55_2023,
    mortalityRate,
    DementiaRate,
    StrokeRate,
    geom
  )


# 3. Define Parameters ----------------------------------------------------

baseNDVI <- 0.1

# Dose Response Parameters (List format for cleaner reference)
params <- list(
  mortality = list(est = 0.96, low = 0.94, high = 0.97),
  stroke = list(est = 0.97, low = 0.96, high = 0.98),
  dementia = list(est = 0.96, low = 0.95, high = 0.98)
)
## freeze function to look at variables up to this point. Seem like the best alternative at the moment to replace run all lines above cursor...
# browser()

# 4. Calculate Health Measures --------------------------------------------

allCities_final <- allCities_prep |>

  # --- Mortality Calculations (Pop > 20) ---
  mutate(
    # Current NDVI
    rr_Mortality = relativeRateMortality(
      meanNDVI,
      baseNDVI,
      params$mortality$est
    ),
    rr_Mortality_low = relativeRateMortality(
      meanNDVI,
      baseNDVI,
      params$mortality$low
    ),
    rr_Mortality_high = relativeRateMortality(
      meanNDVI,
      baseNDVI,
      params$mortality$high
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

    # 10% Increase Scenario
    rr_Mortality10 = relativeRateMortality10(
      meanNDVI,
      baseNDVI,
      params$mortality$est
    ),
    rr_Mortality_low10 = relativeRateMortality10(
      meanNDVI,
      baseNDVI,
      params$mortality$low
    ),
    rr_Mortality_high10 = relativeRateMortality10(
      meanNDVI,
      baseNDVI,
      params$mortality$high
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

    # Deltas (Change)
    ls_Mortality_Change = ls_Mortality10 - ls_Mortality,
    ls_Mortality_Change_low = ls_Mortality_low10 - ls_Mortality_low,
    ls_Mortality_Change_high = ls_Mortality_high10 - ls_Mortality_high
  ) |>

  # --- Stroke Calculations (Pop > 35) ---
  mutate(
    rr_Stroke = relativeRateStrokeDem(meanNDVI, baseNDVI, params$stroke$est),
    rr_Stroke_low = relativeRateStrokeDem(
      meanNDVI,
      baseNDVI,
      params$stroke$low
    ),
    rr_Stroke_high = relativeRateStrokeDem(
      meanNDVI,
      baseNDVI,
      params$stroke$high
    ),

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
      params$stroke$est
    ),
    rr_Stroke_low10 = relativeRateStrokeDem10(
      meanNDVI,
      baseNDVI,
      params$stroke$low
    ),
    rr_Stroke_high10 = relativeRateStrokeDem10(
      meanNDVI,
      baseNDVI,
      params$stroke$high
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

    # Deltas
    ls_Stroke_Change = ls_Stroke10 - ls_Stroke,
    ls_Stroke_Change_low = ls_Stroke_low10 - ls_Stroke_low,
    ls_Stroke_Change_high = ls_Stroke_high10 - ls_Stroke_high
  ) |>

  # --- Dementia Calculations (Pop > 55) ---
  mutate(
    rr_Dementia = relativeRateStrokeDem(
      meanNDVI,
      baseNDVI,
      params$dementia$est
    ),
    rr_Dementia_low = relativeRateStrokeDem(
      meanNDVI,
      baseNDVI,
      params$dementia$low
    ),
    rr_Dementia_high = relativeRateStrokeDem(
      meanNDVI,
      baseNDVI,
      params$dementia$high
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
      params$dementia$est
    ),
    rr_Dementia_low10 = relativeRateStrokeDem10(
      meanNDVI,
      baseNDVI,
      params$dementia$low
    ),
    rr_Dementia_high10 = relativeRateStrokeDem10(
      meanNDVI,
      baseNDVI,
      params$dementia$high
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

    # Deltas
    ls_Dementia_Change = ls_Dementia10 - ls_Dementia,
    ls_Dementia_Change_low = ls_Dementia_low10 - ls_Dementia_low,
    ls_Dementia_Change_high = ls_Dementia_high10 - ls_Dementia_high
  ) |>

  # --- Final Rate Conversions (Batched) ---
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
  ) |>
  st_drop_geometry() |>
  dplyr::select(-geom)


# 5. Save Output ----------------------------------------------------------

readr::write_csv(
  allCities_final,
  "data/products/healthMeasures/allCities_2023_morDemStroke_with10percentAdjust.csv"
)
