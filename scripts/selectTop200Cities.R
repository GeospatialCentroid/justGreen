

pacman::p_load(sf, readr, dplyr)

# read in places data 
files <- list.files(path = "data/raw/esri",
                    pattern = ".gpkg",
                    full.names = TRUE)
d1 <- sf::st_read(files[1])
i1 <- sf::st_read(files[2])
# bind to single features 
places <- dplyr::bind_rows(d1, i1)|>
  top_n(200, P0010001) 
  
# export 
<<<<<<< HEAD
geePlaces <- places |>
  dplyr::select("OBJECTID","PLACENS","GEOID","NAME","State")
sf::st_write(geePlaces, "data/processed/top200/top200Cities.gpkg")
sf::st_write(geePlaces, "data/processed/top200/top200Cities.shp")
write_csv(x = st_drop_geometry(places), "data/processed/top200/top200Cities.csv")
=======
sf::st_write(places, "data/processed/top200/top200Cities.gpkg")
>>>>>>> parent of 14d8f29 (adjust planning notes and export process)
