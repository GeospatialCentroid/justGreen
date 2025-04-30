pacman::p_load("arcpullr", "tigris", "dplyr", "sf", "tmap")


# pull in colorado 
colorado <- tigris::states() |>
  dplyr::filter(NAME == "Colorado") |>
  sf::st_transform(crs = 4326)

# data url 
# Base URL of the FeatureServer
designatedPlace <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/USA_Census_2020_DHC_Population_Households_Place/FeatureServer/2" # 0 usually represents the first layer
incorporatedPlace <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/USA_Census_2020_DHC_Population_Households_Place/FeatureServer/3" # 0 usually represents the first layer

## download the feature 
d1 <- arcpullr::get_spatial_layer(designatedPlace)
d2 <- arcpullr::get_spatial_layer(incorporatedPlace)

# export full data 
sf::write_sf(d1, "data/raw/esri/designatedPlace.gpkg")
sf::write_sf(d2, "data/raw/esri/incorporatedPlace.gpkg")
# process to state level 
d1c <- d1 |>
  sf::st_crop(colorado)

d2c <- d2 |>
  sf::st_crop(colorado)

# export 
sf::write_sf(d1c, "data/processed/esri/designatedPlaceColorado.gpkg")
sf::write_sf(d2c, "data/processed/esri/incorporatedPlaceColorado.gpkg")
# export for GEE 


## Visualize
tmap::tmap_mode("view")
qtm(d2c)
## export 











# first attempt -----------------------------------------------------------



library(httr)
library(sf)
library(jsonlite) # For parsing JSON responses

# 1. Construct the Query URL:

# Base URL of the FeatureServer
base_url <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/USA_Census_2020_DHC_Population_Households_Place/FeatureServer/0" # 0 usually represents the first layer

# Construct the query URL (example: getting all features)
query_url <- paste0(base_url, "/query?where=1%3D1&outFields=*&f=geojson") # 1%3D1 means "where 1=1" (all features), outFields=* gets all fields, f=geojson specifies GeoJSON format

# More complex queries (example):
# where_clause <- URLencode("STATE_NAME = 'Colorado'") # Example where clause
# query_url <- paste0(base_url, "/query?where=", where_clause, "&outFields=*&f=geojson")

# 2. Make the API Request:

response <- GET(query_url)

# 3. Parse the Response:

if (http_status(response)$category == "Success") { # Check for successful response
  geojson_data <- content(response, as = "text") # Get the content as text (GeoJSON)
  sf_data <- read_sf(geojson_data) # Use sf to read GeoJSON into a spatial dataframe
  print(head(sf_data)) # Print the first few rows
  # Now you have your spatial data in sf_data (an sf object)
  st_write(sf_data, "data/census_data.shp") # Save as Shapefile (or other formats)
  # st_write(sf_data, "census_data.geojson", driver = "GeoJSON") # Save as GeoJSON
  # write.csv(st_drop_geometry(sf_data), "census_data.csv", row.names = FALSE) # Save attribute table as CSV
} else {
  print(paste("Error:", status_code(response)))
  print(content(response, as = "text")) # Print error details if available
}


# Example for getting a specific feature by ID:
# feature_id <- 123 # Replace with the actual feature ID
# feature_url <- paste0(base_url, "/", feature_id, "?f=geojson")
# response <- GET(feature_url)
# ... (parse response as above)

# Example for getting the feature service metadata:
metadata_url <- base_url
metadata_response <- GET(metadata_url)
metadata <- fromJSON(content(metadata_response, as = "text"))
print(metadata) # Explore the metadata to understand fields, etc.