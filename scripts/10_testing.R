# So I need to determine if thereren cities tt reside in more tn one count.

# I also need to look at myVI data and determine which cities are returningA values.

pacman::p_load(terra, sf, dplyr, tmap)
tmap_mode("view")

tracts <- readRDS("data/products/dataForShiny/tractsGPKG.rds") |> bind_rows()
health <- readRDS("data/products/dataForShiny/healthData.rds")

# Select all rows of data from the health object where the NDVI value is NA
na_health_data <- filter(health[[2]], is.na(meanNDVI))

# Join the na_health_data to the tracks and select out all the cities that have at least one NA value associated with them.
cities_with_na_ndvi <- tracts |>
  dplyr::filter(GEOID %in% na_health_data$GEOID)

# unique cities with zero population tracts
zeroPop <- cities_with_na_ndvi |>
  as.data.frame() |>
  dplyr::filter(over20 == 0) |>
  dplyr::group_by(fullCity) |>
  dplyr::count()
names(zeroPop) <- c("city", "no population")
# unique cities with na ndvi values
selectCities <- cities_with_na_ndvi |>
  as.data.frame() |>
  dplyr::group_by(fullCity) |>
  dplyr::count()

names(selectCities) <- c("city", "no ndvi")

selectCities <- dplyr::left_join(selectCities, zeroPop, by = "city")
