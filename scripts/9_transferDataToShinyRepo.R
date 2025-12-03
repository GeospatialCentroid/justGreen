# move all the datasets requires for the application
pacman::p_load(readr, sf, rmapshaper, tmap, purrr, stringr)
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
# qtm(cities)
# census tracts
# gather call census tracts per city?
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

# export the census tract data
saveRDS(object = ct_gpkg, file = "data/products/dataForShiny/tractsGPKG.rds")


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
