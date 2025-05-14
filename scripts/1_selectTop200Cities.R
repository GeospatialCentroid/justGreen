

pacman::p_load(sf, readr, dplyr)

# read in places data 
files <- list.files(path = "data/raw/esri",
                    pattern = ".gpkg",
                    full.names = TRUE)
d1 <- sf::st_read(files[1])
i1 <- sf::st_read(files[2])
# bind to single features 
places <- dplyr::bind_rows(d1, i1)|>
  top_n(200, P0010001) |>
  select("OBJECTID","PLACENS","GEOID","CLASSFP","NAME","State",
         "totalPopulation" = "P0150001",
         "pop18andOlder" = "P0150003")
  
# need to filter to total pop 18 and older 
# P0150002 - pop under 18 
# P0150003 -  Population in households 18 years and over

# export 
sf::st_write(places, "data/processed/top200/top200Cities.gpkg", delete_layer = TRUE)
sf::st_write(places, "data/processed/top200/top200Cities.shp", delete_layer = TRUE)
write_csv(x = st_drop_geometry(places), "data/processed/top200/top200Cities.csv")
sf::st_write(places, "data/processed/top200/top200Cities.gpkg", delete_layer = TRUE)
