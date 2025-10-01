# calling the workflow \
pacman::p_load(dplyr,terra,sf,tmap,purrr,furrr, readr)


# download population data 
print("downloading population data")
source("scripts/0_downloadPopulationData.R")

# select top 200 cities 
print("prepping top 200 cities")
if(!file.exists("data/processed/top200/top200Cities.gpkg")){
  source("scripts/1_selectTop200Cities.R")
}

# download NDVI 
## python method 
# download census geographies 

print("grabbing census data")
source("scripts/3_downloadCensusGeographies.R")
# generate city pop for 35 and 55 and older 
source("scripts/3b_55plusinCities.R")

# process water files 
print("processing lakes and oceans")
source("scripts/4_processWaterFiles.R")

# download data from google drive 
## unlikely to work due to unique id to drive folder. 
## should only need after python pull from GEE script is rerun 

print("grabbing data from google drive")
# requires the authenitcation so can't run from terminal 
try(source("scripts/5_pullDataFromDrive.R"))

# calculate ndvi per city 
# does a lot of processing through calling three independent scripts 
## process water from NDVI 
## process NDVI to city area 
## process NDVI to census Tract area 
print("Processing NDVI values for cities and tracts")
source("scripts/6_ndviPerCity.R")


# apply the metric to the ndvi and population data 
source("scripts/7_applyHealthMetrics.R")
# city export : "data/products/healthMeasures/allCities_2023_morDemStroke_with10percentAdjust.csv"
# move data to the shiny app 