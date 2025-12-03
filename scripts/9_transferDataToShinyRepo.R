# move all the datasets requires for the application
pacman::p_load(readr, sf, rmapshaper, tmap)
tmap::tmap_mode("view")
justGreenData <- "~/trueNAS/work/justGreenShiny/data"


# city NDVI metrics
cm <- read_csv(
  "~/trueNAS/work/justGreen/data/products/healthMeasures/allCities_2023_morDemStroke_with10percentAdjust.csv"
)

# census tract ndvi metrics
ctm <- read_csv("data/processed/summaryNDVI/allCensusTractsNDVI_2023.csv")


# top 200 cities
## simplify objects and export
cities <- sf::st_read("data/processed/top200/top200Cities_vect.gpkg") |>
  rmapshaper::ms_simplify() |>
  sf::st_make_valid()
qtm(cities)
# census tracts
# gather call census tracts per city?
index <- 1

grabCT_data <- function(index, cities, ctm) {
  # name
  id <- cities$GEOID[index]
  name <- cities$NAME[index]
  state <- cities$State[index]
  # gather id from census tract data
  ct_id <- ctm |>
    dplyr::filter(cityGEOID == id)
  # use ids to pull the
  stateCT <- sf::st_read(paste0(
    "data/processed/censusGeographies/",
    state,
    "_ct.gpkg"
  ))
  cityCT <- stateCT[stateCT$GEOID %in% ct_id$GEOID, ] |>
    rmapshaper::ms_simplify() |>
    sf::st_make_valid()
  # export
  return(cityCT)
}

vals <- purrr::map(
  .x = 1:200,
  .f = grabCT_data,
  cities = cities,
  ctm = ctm
)
names(vals) <- cities$GEOID

# store all features in a rds file
dataBundles <- list(
  cityHealth <- cm,
  tractHealth <- ctm,
  cityGPKG <- cities,
  tractsGPKG <- vals
)

saveRDS(object = dataBundles, file = "data/products/dataForShiny/data.rds")

#copy file to the shiny app folder location
file.copy(
  "data/products/dataForShiny/data.rds",
  justGreenData,
  overwrite = TRUE
)
