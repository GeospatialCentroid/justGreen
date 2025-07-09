pacman::p_load("arcpullr", "tigris", "dplyr", "sf", "tmap")


# pull in colorado 
colorado <- tigris::states() |>
  dplyr::filter(NAME == "Colorado") |>
  sf::st_transform(crs = 4326)

# data url 
if(!file.exists("data/raw/esri/designatedPlace.gpkg")){
  designatedPlace <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/USA_Census_2020_DHC_Population_Households_Place/FeatureServer/2" # 0 usually represents the first layer
  d1 <- arcpullr::get_spatial_layer(designatedPlace)
  sf::write_sf(d1, "data/raw/esri/designatedPlace.gpkg")
  # process to state level 
  d1c <- d1 |>
    sf::st_crop(colorado)
  sf::write_sf(d1c, "data/processed/esri/designatedPlaceColorado.gpkg")
  
}

if(!file.exists("data/raw/esri/designatedPlace.gpkg")){
  # Base URL of the FeatureServer
  incorporatedPlace <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/USA_Census_2020_DHC_Population_Households_Place/FeatureServer/3" # 0 usually represents the first layer
  ## download the feature 
  d2 <- arcpullr::get_spatial_layer(incorporatedPlace)
  # export full data 
  sf::write_sf(d2, "data/raw/esri/incorporatedPlace.gpkg")
  # crop to colorado 
  d2c <- d2 |>
    sf::st_crop(colorado)
  # export 
  sf::write_sf(d2c, "data/processed/esri/incorporatedPlaceColorado.gpkg")
}


