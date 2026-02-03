# move all the datasets requires for the application
pacman::p_load(readr, sf, rmapshaper, tmap, purrr, stringr, dplyr)
tmap::tmap_mode("view")

# source city names funciton 
source("functions/cleanCityNames.R")


# city NDVI metrics
cm <- read_csv(
  "~/trueNAS/work/justGreen/data/products/healthMeasures/allCities_2023_morDemStroke_with10percentAdjust.csv"
) |>
  clean_city_names(city_col = city, state_col = state)

# census tract ndvi metrics
ctm <- read_csv("data/products/healthMeasures/allCT_2023_morDemStroke_with10percentAdjust_svi.csv")|>
  clean_city_names(city_col = city, state_col = state) |>
  dplyr::select(
    GEOID = geoid, 
    popOver20_2023,
    popOver35_2023,
    popOver55_2023,
    meanNDVI,
    RPL_THEMES,
    ls_Mortality_Rate,
    ls_Stroke_Rate,
    ls_Dementia_Rate,
    fullCity
  )|>
  dplyr::mutate(
    ls_Mortality_Rate = abs(ls_Mortality_Rate),
      ls_Stroke_Rate = abs(ls_Stroke_Rate),
      ls_Dementia_Rate = abs(ls_Dementia_Rate) 
  )|>
  dplyr::mutate(
    popup = paste0(
      "<b>",
      "Tract ID: ", GEOID,
      "</b><br>",
      "Population Over 20: ",
      format(popOver20_2023, big.mark = ",", scientific = FALSE),
      "<br>",
      "Average NDVI: ",
      round(meanNDVI, 2)
    )
    )

# top 200 cities
## simplify objects and export
cities <- sf::st_read("data/processed/top200/top200Cities_vect.gpkg") |>
  rmapshaper::ms_simplify() |>
  sf::st_make_valid() |>
  dplyr::left_join(
    y = cm,
    by = c("GEOID" = "geoid")
  ) |>
  dplyr::select(
    GEOID,
    State,
    fullCity,
    totalPopulation,
    meanNDVI,
    ls_Mortality_Rate,
    ls_Stroke_Rate,
    ls_Dementia_Rate
  ) |>
  dplyr::mutate(
    popup = paste0(
      "<b>",
      fullCity,
      "</b><br>",
      "Total Population: ",
      format(totalPopulation, big.mark = ",", scientific = FALSE),
      "<br>",
      "Average NDVI: ",
      round(meanNDVI, 2)
    ),
    ls_Mortality_Rate = abs(ls_Mortality_Rate),
    ls_Stroke_Rate = abs(ls_Stroke_Rate),
    ls_Dementia_Rate = abs(ls_Dementia_Rate)
  )

# city Centroids
cityCentroid <- sf::st_centroid(cities) |>
  dplyr::mutate(
    popup = paste0(
      "<b>",
      fullCity,
      "</b><br>",
      "Total Population: ",
      format(totalPopulation, big.mark = ",", scientific = FALSE),
      "<br>",
      "Average NDVI: ",
      round(meanNDVI, 2)
    )
  )


# qtm(cities)
# census tracts
# gather call census tracts per city?
grabCT_data <- function(index, cities, ctm) {
  # name
  id <- cities$GEOID[index]
  name <- cities$fullCity[index]
  state <- cities$State[index]
  # gather id from census tract data
  ct_id <- ctm |>
    dplyr::filter(name == fullCity)
  # use ids to pull the
  stateCT <- sf::st_read(paste0(
    "data/processed/censusGeographies/",
    state,
    "_ct.gpkg"
  ))
  cityCT <- stateCT[stateCT$GEOID %in% ct_id$GEOID, ] |>
    rmapshaper::ms_simplify() |>
    sf::st_make_valid() |>
    dplyr::mutate(fullCity = cities$fullCity[index])
  # export
  return(cityCT)
}

ct_gpkg <- purrr::map(
  .x = 1:200,
  .f = grabCT_data,
  cities = cities,
  ctm = ctm
)
names(ct_gpkg) <- cities$GEOID

# get the reports for all 200 cities into a list
file_paths <- list.files(
  path = "data/products/citySummaries",
  pattern = "\\.html$",
  full.names = TRUE
)

# 2. Read all files into a list
# This creates a list where every element is a long HTML string
html_list <- map(file_paths, read_file)

# 3. Create clean "Keys" for the list
clean_names <- basename(file_paths) %>% # Remove folder path
  str_remove("^Report-") %>% # Remove "Report-" prefix
  str_remove("\\.html$") # Remove ".html" suffix

# Assign names to the list
names(html_list) <- clean_names

# 4. Save as a compressed RDS
# 'compress = "xz"' is slower to save but creates the smallest file size
saveRDS(
  html_list,
  "data/products/dataForShiny/citySummary.rds",
  compress = "xz"
)


# generate the rds files  ------------------------------------------------

# store all features in a rds file
healthData <- list(
  cityHealth <- cm,
  tractHealth <- ctm
)
saveRDS(object = healthData, file = "data/products/dataForShiny/healthData.rds")

# export the city gpkg
saveRDS(object = cities, file = "data/products/dataForShiny/citiesGPKG.rds")
saveRDS(
  object = cityCentroid,
  file = "data/products/dataForShiny/centroidGPKG.rds"
)
# export the census tract data
saveRDS(object = ct_gpkg, file = "data/products/dataForShiny/tractsGPKG.rds")


#data path
justGreenData <- "~/trueNAS/work/justGreenShiny/data"

#copy file to the shiny app folder location
for (i in list.files(
  "data/products/dataForShiny",
  full.names = TRUE
)) {
  name <- basename(i)
  file.copy(
    from = i,
    justGreenData,
    overwrite = TRUE
  )
}
