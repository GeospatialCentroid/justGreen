pacman::p_load(sf, dplyr)

citiesPop <- st_read("data/processed/top200_2023/allCities.gpkg")

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
  # pop over 55
  pop55 <- sum(crop$over55, na.rm = TRUE)
  #assign val to city and export 
  city$popOver55_2023 <- pop55
  if(i == 1){
    new200 <- st_as_sf(city)
  }else{
    new200 <- bind_rows(new200, st_as_sf(city))
  }
}
# export 
sf::st_write(obj = new200, dsn = "data/processed/top200_2023/allCities_55plus.gpkg")
