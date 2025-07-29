pacman::p_load(sf, dplyr)


citiesPop <- st_read("data/processed/top200_2023/allCities.gpkg")
exportPath <- "data/processed/top200_2023/allCities_35_55.gpkg"
if(!file.exists(exportPath)){
  for(i in 1:nrow(citiesPop)){
    # city 
    city <- citiesPop[i,]
    state <- city$State
    c2 <- terra::vect(city) |>
      terra::makeValid()
    
    # read in block groups for state 
    path <- list.files(path = "data/processed/censusGeographies",
                       pattern = paste0(state,"_bg"),
                       full.names = TRUE)
    # read in data 
    bg <- terra::vect(path) 
    # crop
    crop <- terra::crop(bg, c2)
    # visualize the city area 
    # terra::plot(c2, col = "blue")
    # terra::plot(bg, add = TRUE)
    # add the over 35 population to the file here  
    pop35 <-   sum(crop$over35, na.rm = TRUE)
    # pop over 55
    pop55 <- sum(crop$over55, na.rm = TRUE)
    
    #assign val to city and export 
    city$popOver35_2023 <- pop35
    city$popOver55_2023 <- pop55
    
    if(city$NAME == "Bridgeport city"){
      pop20 <- sum(crop$over20)
      city$popOver20_2023 <- pop20
    }
    
    if(i == 1){
      new200 <- st_as_sf(city)
    }else{
      new200 <- bind_rows(new200, st_as_sf(city))
    }
  }
  # export 
  sf::st_write(obj = new200, dsn = exportPath)
}
