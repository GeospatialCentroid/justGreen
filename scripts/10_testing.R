# So I need to determine if thereren cities tt reside in more tn one count.

# I also need to look at myVI data and determine which cities are returningA values.

pacman::p_load(terra, sf, dplyr, tmap)
tmap_mode("view")

tracts <- readRDS("data/products/dataForShiny/tractsGPKG.rds") |> bind_rows()|>
  sf::st_make_valid()
health <- readRDS("data/products/dataForShiny/healthData.rds")
counties <- st_read("data/raw/counties/counties.gpkg")
cities <- st_read("data/processed/top200_2023/allCities.gpkg") |>
  sf::st_make_valid()

# population counts per tract 
n1 <- cities[cities$GEOID == "2255000", ]

tracts1 <- tracts |> 
  dplyr::filter(fullCity == "New Orleans, Louisiana" )
centroid <- sf::st_centroid(tracts1) |>
  sf::st_intersection(n1)
# filter to locations that fall within the city boundaries 
qtm(tracts1) + qtm(n1, fill= "#91229950")


# tracts 
## split out the NAME coumn into three 
t2 <- tracts |>
  tidyr::separate(col = NAME, into = c("ct","County","State"), sep = "; ")

# 1. Identify which cities (fullCity) span more than one county
multi_county_cities_list <- t2 %>%
  st_drop_geometry() %>%                  # Remove spatial data for faster grouping
  group_by(fullCity) %>%                  # Group by city name
  summarise(
    num_counties = n_distinct(County),    # Count unique counties per city
    counties_present = paste(unique(County), collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(num_counties > 1)                # Keep only cities with 2+ counties

# use this area after intersection to remove edge case elements 
t2$originalArea <- sf::st_area(t2)
# run intersection to crop to cities 
t3 <- sf::st_intersection(t2, y = cities |> dplyr::select(geom))
t3$newArea <- sf::st_area(t3)
t3$areaDiff <-  as.numeric(((t3$originalArea - t3$newArea ) / t3$originalArea) * 100)
# filter to features with great then 10% of area inside of city 
t4 <- t3 |>
  dplyr::filter(areaDiff < 10)

# so with this we will then summarize the population count per count 





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

