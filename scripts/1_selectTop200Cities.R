# libraries
pacman::p_load(sf, readr, dplyr, tigris, stringr, terra)

pacman::p_load(sf, readr, dplyr, tigris, stringr, terra)

# 1. Read in all .gpkg files from the directory and bind them
files <- list.files(
  path = "data/raw/esri",
  pattern = ".gpkg$", # Use '$' to match the end
  full.names = TRUE
)

# Use lapply to read all files into a list, then bind
places_raw <- lapply(files, sf::st_read) |>
  dplyr::bind_rows()

# 2. Process places data once
places_processed <- places_raw |>
  top_n(200, P0010001) |>
  select(
    "OBJECTID",
    "PLACENS",
    "GEOID",
    "CLASSFP",
    "NAME",
    "State",
    "totalPopulation" = "P0150001",
    "pop18andOlder" = "P0150003"
  )

# 3. Write the "Temp" version before st_make_valid() and name cleaning
sf::st_write(
  places_processed,
  "data/processed/top200/top200CitiesTemp.gpkg",
  delete_layer = TRUE
)

# 4. Clean names and geometry
places <- places_processed |>
  sf::st_make_valid() |>
  mutate(
    # Clean names in one step
    NAME = stringr::str_replace(NAME, "/", "-"),
    NAME = stringr::str_replace(NAME, stringr::fixed(" (balance)"), "")
  )

# 5. Export total population CSV (using cleaned names)
places |>
  st_drop_geometry() |>
  select(GEOID, NAME, State, totalPopulation) |>
  write_csv("data/processed/top200_2023/totalPopCities.csv")

# 6. Load/Cache Counties
countiesExport <- "data/raw/counties/counties.gpkg"
if (!file.exists(countiesExport)) {
  counties <- tigris::counties() |>
    st_transform(crs = st_crs(places)) # Ensure CRS match

  st_write(obj = counties, dsn = countiesExport)
} else {
  counties <- st_read(countiesExport)
}
# select the names of interset
counties <- counties |>
  dplyr::select(
    GEOID,
    NAME,
    NAMELSAD
  )


# 7. Assign Counties using a Spatial Join
## adding conditional to
if (!file.exists("data/processed/top200/top200Cities.gpkg")) {
  # get the centroid of all cities
  place_points <- sf::st_point_on_surface(places)
  # join the points to county object
  point_county_join <- sf::st_join(
    place_points |> select(GEOID),
    counties |> select(countyGEOID = GEOID)
  )
  #drop geom to join back to places
  lookup_table <- sf::st_drop_geometry(point_county_join)
  # join back
  places_with_county <- places |>
    dplyr::left_join(lookup_table, by = "GEOID")

  # 8. Final Exports
  sf::st_write(
    places_with_county,
    "data/processed/top200/top200Cities.gpkg",
    delete_layer = TRUE
  )

  write_csv(
    x = st_drop_geometry(places_with_county),
    "data/processed/top200/top200Cities.csv"
  )

  terra::writeVector(
    terra::vect(places_with_county),
    "data/processed/top200/top200Cities_vect.gpkg",
    overwrite = TRUE
  )
}
