pacman::p_load(terra, dplyr, furrr, purrr, tictoc)



# list all ndvi files 
ndviFiles <- list.files("data/processed/ndvi",
                        full.names = TRUE,
                        pattern = "_buffered") 
# land 
land <- terra::vect("data/processed/naturalEarthData/ne_10m_land.gpkg")
lake1 <- terra::vect("data/processed/naturalEarthData/ne_10m_lakes_north_america.gpkg")
lake2 <- terra::vect("data/processed/naturalEarthData/ne_10m_lakes.gpkg")

# generate a dateframe with the average NDVI per citis 
# issue with city being in two states (137)


## spliting this out to
##a funtion for summarizing all cities 
## a function for summarizing census tracts within a citin 

processNDVIImages <- function(ndviFile, land, lake1, lake2){
  n1 <- basename(ndviFile)
  export <- paste0("data/processed/ndvi_noWater/", n1)
  print(n1)
  if(!file.exists(export)){
    r1 <- terra::rast(ndviFile)|>
      terra::mask(land) |>
      terra::mask(lake1, inverse = TRUE)|>
      terra::mask(lake2, inverse = TRUE)
    # return(list(
    #   name = n1,
    #   rast = r1
    # ))
    terra::writeRaster(x= r1, filename = export, overwrite = TRUE)
  }
  gc()
}


# plan("future::multisession", workers = 8) # this was erroring out 
# plan(multicore, workers = 10) # works but have to run from terminal. 
plan(sequential)
## not a super long run time but fast with multicore! 
tic()
future_map(.x = ndviFiles, .f = processNDVIImages,
           land=land,
           lake1 = lake1,
           lake2 = lake2)
toc()