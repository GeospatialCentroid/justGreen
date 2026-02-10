# So I need to determine if thereren cities tt reside in more tn one count.

# I also need to look at myVI data and determine which cities are returningA values.
# install.packages("pacman")
pacman::p_load(terra, sf, dplyr, tmap,tidyr)
tmap_mode("view")

tracts <- readRDS("data/products/dataForShiny/tractsGPKG.rds") |> bind_rows()|>
  sf::st_make_valid()
health <- readRDS("data/products/dataForShiny/healthData.rds")
counties <- st_read("data/raw/counties/counties.gpkg")
cities <- st_read("data/processed/top200_2023/allCities.gpkg") |>
  sf::st_make_valid()

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
t2 <- tracts |>
  tidyr::separate(col = NAME, into = c("ct","County","State"), sep = "; ")

# 1. Get centroids of the tracts
tract_centroids <- st_centroid(t2)

# 2. Join using 'st_within' instead of 'st_intersects'
# This ensures a tract is only matched if its center is strictly inside the city
clean_join <- sf::st_intersection(x = tract_centroids, y = cities)

# 3. Filter your original tracts based on this strict list
valid_combinations <- clean_join %>% 
  st_drop_geometry() %>% 
  select(GEOID, valid_city = fullCity)

# filter main data
t2_centroid_filtered <- t2 %>%
  dplyr::filter(GEOID %in% valid_combinations$GEOID) |>
  sf::st_drop_geometry()

# get a count of people in each unique count 
t2_multi_county_only <- t2_centroid_filtered %>%
  group_by(fullCity) %>%
  filter(n_distinct(County) > 1) %>%
  ungroup()

summary_check <- t2_multi_county_only %>%
  st_drop_geometry() %>%
  group_by(fullCity) %>%
  summarise(counties = paste(unique(County), collapse = ", "))
summary_check

# 1. Calculate the population for each county segment and its percentage of the city total
county_pct_summary <- t2_multi_county_only %>%
  st_drop_geometry() %>%                  # Drop geometry for calculation
  group_by(fullCity, County) %>%
  summarise(
    county_sum_over20 = sum(over20, na.rm = TRUE), 
    .groups = "drop_last"                 # Keep grouped by fullCity to calculate city total
  ) %>%
  mutate(
    city_total_over20 = sum(county_sum_over20),
    percent_of_city = (county_sum_over20 / city_total_over20) * 100
  ) %>%
  ungroup()

# 2. View the results
# This will show each city, its counties, the population in that county, and the % share
print(county_pct_summary)

# Optional: Arrange by city and percentage for better readability
county_pct_summary_sorted <- county_pct_summary %>%
  arrange(fullCity, desc(percent_of_city))

print(county_pct_summary_sorted)

# total cities - 51 cities 
nCity <- unique(county_pct_summary_sorted$fullCity)

# Assuming your dataset is named county_pct_summary
multi_county_cities10 <- county_pct_summary %>%
  group_by(fullCity) %>%
  # Keep only cities where the count of counties with > 10% population is more than 1
  filter(sum(percent_of_city > 10) > 1) %>%
  ungroup()
multi_county_cities20 <- county_pct_summary %>%
  group_by(fullCity) %>%
  # Keep only cities where the count of counties with > 10% population is more than 1
  filter(sum(percent_of_city > 20) > 1) %>%
  ungroup()


# View the filtered results
print(significant_multi_county_cities)
View(multi_county_cities20)

# use this area after intersection to remove edge case elements 
t2$originalArea <- sf::st_area(t2)
# run intersection to crop to cities 
if(!file.exists( "temp/cityCropCensustracts.gpkg")){
  t3 <- sf::st_intersection(t2, y = cities)
  sf::st_write(t3, "temp/cityCropCensustracts.gpkg",delete_dsn = TRUE)
}else{
  t3 <- st_read("temp/cityCropCensustracts.gpkg")
}

t3$newArea <- as.numeric(sf::st_area(t3))

t3$areaDiff <-  as.numeric(((t3$originalArea - t3$newArea ) / t3$originalArea) * 100)


# filter to features with great then 10% of area inside of city 
totalAreaChange  <- t3 |>
  sf::st_drop_geometry()|>
  group_by(GEOID)|>
  dplyr::summarize(totalDiff = sum(areaDiff))

uniqueTracts  <- t3 |>
  as.data.frame() |> 
  dplyr::group_by(GEOID)|>
  slice(1) |>
  ungroup() |> 
  dplyr::left_join(y = totalAreaChange, by = "GEOID")

# need to filter to keep census tracts with at least 10% of the area inside the city after the intersection 
# Assuming your dataframe is named 'tracts_data'
filtered_tracts <- uniqueTracts %>%
  filter(newArea >= (originalArea * 0.10))


# now starting to summarize by city and county  
t6 <- filtered_tracts |>
    st_drop_geometry() %>%                  # Drop geometry for calculation
  group_by(fullCity, County) %>%
  summarise(
    county_sum_over20 = sum(over20, na.rm = TRUE), 
    .groups = "drop_last"                 # Keep grouped by fullCity to calculate city total
  ) %>%
  mutate(
    city_total_over20 = sum(county_sum_over20),
    percent_of_city = (county_sum_over20 / city_total_over20) * 100
  ) %>%
  ungroup()

# Find cities with at two counties 
c1 <- t6 |>
  group_by(fullCity) |>
   summarize(n())
# Find cities with more then 90% population in single county 
p90 <- t6 |> 
  dplyr::filter(percent_of_city >= 90)
p80 <- t6 |> 
  dplyr::filter(percent_of_city >= 80)


cities_split_counties20 <- t6 %>%
  group_by(fullCity) %>%
  # sum(condition) counts how many rows (counties) meet the criteria for that city
  filter(sum(percent_of_city >= 20) > 1) %>% 
  ungroup()

cities_split_counties10 <- t6 %>%
  group_by(fullCity) %>%
  # sum(condition) counts how many rows (counties) meet the criteria for that city
  filter(sum(percent_of_city >= 10) > 1) %>% 
  ungroup()

# so with this we will then summarize the population count per count 

t5 <- t4 %>%                  # Remove spatial data for faster grouping
  group_by(fullCity) %>%                  # Group by city name
  summarise(
    num_counties = n_distinct(County),    # Count unique counties per city
    counties_present = paste(unique(County), collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(num_counties > 1) 

# 



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

