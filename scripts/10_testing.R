# So I need to determine if thereren cities tt reside in more tn one count.

# I also need to look at myVI data and determine which cities are returningA values.
# install.packages("pacman")
pacman::p_load(terra, sf, dplyr, tmap, tidyr, readr)
tmap_mode("view")

tracts <- readRDS("data/products/dataForShiny/tractsGPKG.rds") |>
  bind_rows() |>
  sf::st_make_valid() |>
  dplyr::mutate(
    countyGEOID = substr(GEOID, 1, 5)
  )
# county mortality rate data
cHealth <- read_csv("data/raw/mortality/All Cause of Death 2023.csv") |>
  # format so thet county code is 5 digits and add leading zero to state code if needed
  dplyr::mutate(
    countyFips = sprintf("%05d", `County Code`)
  )

health <- readRDS("data/products/dataForShiny/healthData.rds")
counties <- st_read("data/raw/counties/counties.gpkg")
cities <- st_read("data/processed/top200_2023/allCities.gpkg") |>
  sf::st_make_valid()

# refline the city selections
# remove
# denver county from Aurora
# yonkers - remove the bronx county
# oklahoma city - add pottawatomie county
citiesToAlter <- read_csv("temp/citiesWithMultipleCounties.csv") |>
  dplyr::select(fullCity, County) |>
  # remove denver from aurora
  dplyr::filter(
    !(fullCity == "Aurora, Colorado" & County == "Denver County")
  ) |>
  # remove the bronx from yonkers
  dplyr::filter(
    !(fullCity == "Yonkers, New York" & County == "Bronx County")
  ) |>
  # add pottawatomie county to oklahoma city
  bind_rows(tibble(
    fullCity = "Oklahoma City, Oklahoma",
    County = "Pottawatomie County"
  )) |>
  # serate the city and state into two columns
  tidyr::separate(
    col = fullCity,
    into = c("City", "State"),
    sep = ", ",
    remove = FALSE
  ) |>
  # append the word city to the City column to match the city names in the cities dataset
  mutate(City = paste0(City, " city"))

# now run a spatial intersection between the city and the counties associated with
# the cities to a percentage area of each county inside a specific city

storageDF <- data.frame()

for (i in seq_along(unique(citiesToAlter$fullCity))) {
  #full city
  fc <- unique(citiesToAlter$fullCity)[i]

  # subset specific city from citiesToAlter dataset to get the state name for filtering the cities dataset
  cityData <- citiesToAlter[citiesToAlter$fullCity == fc, ]
  print(i)
  name <- cityData$City |> unique()
  selState <- cityData$State |> unique()

  selCounty <- cityData$County |> unique()
  # using the reference information from city data select the city and county polygons
  cityGeo <- cities |>
    dplyr::filter(
      State == selState,
      NAME == name
    )
  # get the state fips code
  fips <- tigris::fips_codes
  countyFips <- fips[fips$state_name == selState, ]
  selFips <- countyFips |>
    dplyr::filter(county %in% selCounty) |>
    dplyr::mutate(countyFips = paste0(state_code, county_code))
  # grab the county polygons
  selTracts <- tracts |>
    dplyr::filter(countyGEOID %in% selFips$countyFips)
  # mask the tracts to the city boundary
  maskTracts <- sf::st_intersection(selTracts, cityGeo) |>
    sf::st_drop_geometry() |>
    dplyr::select(GEOID, over20, countyGEOID) |>
    group_by(countyGEOID) |>
    # summarize population over 20 by county
    summarise(countyPop = sum(over20, na.rm = TRUE))

  # from here I need to adjust the produce a new city mortality rate that is weighted by the percentage of the city population in each county
  ## select the mortality rate using county GEOID
  countyMortality <- cHealth |>
    dplyr::filter(countyFips %in% selFips$countyFips) |>
    dplyr::mutate(mortalityRate = as.numeric(`Crude Rate`) / 100000) |>
    dplyr::select(countyFips, mortalityRate)

  # using the population percentage calculate a weighted mortality rate for the city
  weightedMortality <- countyMortality |>
    sf::st_drop_geometry() |>
    dplyr::left_join(maskTracts, by = c("countyFips" = "countyGEOID")) |>
    # calculate the weighted mortality rate
    mutate(
      city = fc,
      weightedMortality = weighted.mean(
        mortalityRate,
        w = countyPop,
        na.rm = TRUE
      ),
      cityAdjustMortality = weightedMortality * 100000
    ) |>
    dplyr::select(city, countyFips, cityAdjustMortality)
  # bind to storage object
  storageDF <- bind_rows(storageDF, weightedMortality)
}


# join the city adjusted mortality rate back to the chealth dataset and export the results
adjustedMortality <- cHealth |>
  dplyr::left_join(storageDF, by = c("countyFips"))
write_csv(
  adjustedMortality,
  "data/raw/mortality/adjustedCityMortality_2023.csv"
)
## previous

# # population counts per tract
# n1 <- cities[cities$GEOID == "2255000", ]
#
# tracts1 <- tracts |>
#   dplyr::filter(fullCity == "New Orleans, Louisiana" )
# centroid <- sf::st_centroid(tracts1) |>
#   sf::st_intersection(n1)
# # filter to locations that fall within the city boundaries
# qtm(tracts1) + qtm(n1, fill= "#91229950")
#

# tracts
## split out the NAME coumn into three
# t2 <- tracts |>
#   tidyr::separate(col = NAME, into = c("ct", "County", "State"), sep = "; ")

# # 1. Get centroids of the tracts
# tract_centroids <- st_centroid(t2)

# # 2. Join using 'st_within' instead of 'st_intersects'
# # This ensures a tract is only matched if its center is strictly inside the city
# clean_join <- sf::st_intersection(x = tract_centroids, y = cities)

# # 3. Filter your original tracts based on this strict list
# valid_combinations <- clean_join %>%
#   st_drop_geometry() %>%
#   select(GEOID, valid_city = fullCity)

# # filter main data
# t2_centroid_filtered <- t2 %>%
#   dplyr::filter(GEOID %in% valid_combinations$GEOID) |>
#   sf::st_drop_geometry()

# # get a count of people in each unique count
# t2_multi_county_only <- t2_centroid_filtered %>%
#   group_by(fullCity) %>%
#   filter(n_distinct(County) > 1) %>%
#   ungroup()

# summary_check <- t2_multi_county_only %>%
#   st_drop_geometry() %>%
#   group_by(fullCity) %>%
#   summarise(counties = paste(unique(County), collapse = ", "))
# summary_check

# # 1. Calculate the population for each county segment and its percentage of the city total
# county_pct_summary <- t2_multi_county_only %>%
#   st_drop_geometry() %>% # Drop geometry for calculation
#   group_by(fullCity, County) %>%
#   summarise(
#     county_sum_over20 = sum(over20, na.rm = TRUE),
#     .groups = "drop_last" # Keep grouped by fullCity to calculate city total
#   ) %>%
#   mutate(
#     city_total_over20 = sum(county_sum_over20),
#     percent_of_city = (county_sum_over20 / city_total_over20) * 100
#   ) %>%
#   ungroup()

# # 2. View the results
# # This will show each city, its counties, the population in that county, and the % share
# print(county_pct_summary)

# # Optional: Arrange by city and percentage for better readability
# county_pct_summary_sorted <- county_pct_summary %>%
#   arrange(fullCity, desc(percent_of_city))

# print(county_pct_summary_sorted)

# # total cities - 51 cities
# nCity <- unique(county_pct_summary_sorted$fullCity)

# # Assuming your dataset is named county_pct_summary
# multi_county_cities10 <- county_pct_summary %>%
#   group_by(fullCity) %>%
#   # Keep only cities where the count of counties with > 10% population is more than 1
#   filter(sum(percent_of_city > 10) > 1) %>%
#   ungroup()
# multi_county_cities20 <- county_pct_summary %>%
#   group_by(fullCity) %>%
#   # Keep only cities where the count of counties with > 10% population is more than 1
#   filter(sum(percent_of_city > 20) > 1) %>%
#   ungroup()

# # View the filtered results
# print(significant_multi_county_cities)
# View(multi_county_cities20)

# # use this area after intersection to remove edge case elements
# t2$originalArea <- sf::st_area(t2)
# # run intersection to crop to cities
# if (!file.exists("temp/cityCropCensustracts.gpkg")) {
#   t3 <- sf::st_intersection(t2, y = cities)
#   sf::st_write(t3, "temp/cityCropCensustracts.gpkg", delete_dsn = TRUE)
# } else {
#   t3 <- st_read("temp/cityCropCensustracts.gpkg")
# }

# t3$newArea <- as.numeric(sf::st_area(t3))

# t3$areaDiff <- as.numeric(
#   ((t3$originalArea - t3$newArea) / t3$originalArea) * 100
# )

# # filter to features with great then 10% of area inside of city
# totalAreaChange <- t3 |>
#   sf::st_drop_geometry() |>
#   group_by(GEOID) |>
#   dplyr::summarize(totalDiff = sum(areaDiff))

# uniqueTracts <- t3 |>
#   as.data.frame() |>
#   dplyr::group_by(GEOID) |>
#   slice(1) |>
#   ungroup() |>
#   dplyr::left_join(y = totalAreaChange, by = "GEOID")

# # need to filter to keep census tracts with at least 10% of the area inside the city after the intersection
# # Assuming your dataframe is named 'tracts_data'
# filtered_tracts <- uniqueTracts %>%
#   filter(newArea >= (originalArea * 0.10))

# # now starting to summarize by city and county
# t6 <- filtered_tracts |>
#   st_drop_geometry() %>% # Drop geometry for calculation
#   group_by(fullCity, County) %>%
#   summarise(
#     county_sum_over20 = sum(over20, na.rm = TRUE),
#     .groups = "drop_last" # Keep grouped by fullCity to calculate city total
#   ) %>%
#   mutate(
#     city_total_over20 = sum(county_sum_over20),
#     percent_of_city = (county_sum_over20 / city_total_over20) * 100
#   ) %>%
#   ungroup()

# # Find cities with at two counties
# c1 <- t6 |>
#   group_by(fullCity) |>
#   summarize(n())
# # Find cities with more then 90% population in single county
# p90 <- t6 |>
#   dplyr::filter(percent_of_city >= 90)
# p80 <- t6 |>
#   dplyr::filter(percent_of_city >= 80)

# cities_split_counties20 <- t6 %>%
#   group_by(fullCity) %>%
#   # sum(condition) counts how many rows (counties) meet the criteria for that city
#   filter(sum(percent_of_city >= 20) > 1) %>%
#   ungroup()

# cities_split_counties10 <- t6 %>%
#   group_by(fullCity) %>%
#   # sum(condition) counts how many rows (counties) meet the criteria for that city
#   filter(sum(percent_of_city >= 10) > 1) %>%
#   ungroup()
# write_csv(cities_split_counties10, "temp/citiesWithMultipleCounties.csv")
# # so with this we will then summarize the population count per count

# t5 <- t4 %>% # Remove spatial data for faster grouping
#   group_by(fullCity) %>% # Group by city name
#   summarise(
#     num_counties = n_distinct(County), # Count unique counties per city
#     counties_present = paste(unique(County), collapse = ", "),
#     .groups = "drop"
#   ) %>%
#   filter(num_counties > 1)

# #

# # Select all rows of data from the health object where the NDVI value is NA
# na_health_data <- filter(health[[2]], is.na(meanNDVI))

# # Join the na_health_data to the tracks and select out all the cities that have at least one NA value associated with them.
# cities_with_na_ndvi <- tracts |>
#   dplyr::filter(GEOID %in% na_health_data$GEOID)

# # unique cities with zero population tracts
# zeroPop <- cities_with_na_ndvi |>
#   as.data.frame() |>
#   dplyr::filter(over20 == 0) |>
#   dplyr::group_by(fullCity) |>
#   dplyr::count()
# names(zeroPop) <- c("city", "no population")
# # unique cities with na ndvi values
# selectCities <- cities_with_na_ndvi |>
#   as.data.frame() |>
#   dplyr::group_by(fullCity) |>
#   dplyr::count()

# names(selectCities) <- c("city", "no ndvi")

# selectCities <- dplyr::left_join(selectCities, zeroPop, by = "city")
