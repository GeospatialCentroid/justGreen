pacman::p_load(terra, dplyr, furrr, purrr, tictoc)

# crop features to waterways 
# only 188 NDVI are coming in from GEE download
# many of these are likely from the change in the city names by the GEE export process... 
print("water mask")
source("scripts/maskWaterFromNDVI.R")

# Generate an summary ndvi file for all cities  
print("NDVI in cities")
source("scripts/summaryNDVIAllCities.R")

# Generate a summary of NDVI values for all census tracts 
## this takes a while to run so only do if needed. 
print("census tract by NDVI")
source("scripts/summaryNDVIPerCensusTract.R")

