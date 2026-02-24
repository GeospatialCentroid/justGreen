# generate a population weighted mortality rate for specific cities with multiple counties 
# requires the census tract data, but is used to rewrite the county mortality data used by the health metrics calculations so 
# it doesn't fit well into the structure 

# cities 
cityList <- readr::read_csv("data/processed/citiesWithMultipleCountiesCleaned.csv")


# city data 
citiesNDVI <- read_csv(
  "data/processed/summaryNDVI/allCitiesNDVI_2023.csv",
  col_types = cols(geoid = col_character()) # Ensure GEOID is char
)

# tract data 
ctData <- read_csv("data/processed/summaryNDVI/allCensusTractsNDVI_2023.csv")

# counties 
counties <- sf::st_read("data/raw/counties/counties.gpkg")
