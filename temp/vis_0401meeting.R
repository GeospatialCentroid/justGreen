
# temp for viewing some results 

pacman::p_load(sf, tmap)
tmap_mode("view")

# files 
files <- list.files("data/processed/summaryNDVI",
                    full.names = TRUE, 
                    pattern = ".gpkg")

# cities 
allCities <- read.csv("data/processed/summaryNDVI/allCitiesNDVI.csv")
  