pacman::p_load(terra, dplyr)


# read in cities 
cities <- terra::vect("data/processed/top200/top200Cities.gpkg")
# list all ndvi files 
ndviFiles <- list.files("data/processed/ndvi",
                        full.names = TRUE,
                        pattern = "_buffered") 
# land 
land <- terra::vect("data/processed/naturalEarthData/ne_10m_land.gpkg")
lake1 <- terra::vect("data/processed/naturalEarthData/ne_10m_lakes_north_america.gpkg")
lake2 <- terra::vect("data/processed/naturalEarthData/ne_10m_lakes.gpkg")

# generate a dateframe with the average NDVI per citis 
for(i in seq_along(cities)){
  city <- cities[i] |> terra::makeValid()
  name <- city$NAME
  print(name)
  id <- city$GEOID
  state <- city$State
  # export Files 
  ref <- paste0(id,"_",name)
  
  # grab the ndvi file of interest 
  file <- ndviFiles[grepl(pattern = id, x = ndviFiles)] 
  # select buffer only 
  file <- file[grepl(pattern = "_buffered", file)]
  if(length(file) > 0){
    ndvi <- file |> 
      terra::rast()|>
      terra::mask(land) |>
      terra::mask(lake1, inverse = TRUE)|>
      terra::mask(lake2, inverse = TRUE)
    
    # Calculate mean and standard deviation for each block group
    vals <- terra::values(ndvi) 
    mean <- mean(vals, na.rm = TRUE)
    sd <- sd(vals, na.rm = TRUE)
    if(i==1){
      df <- data.frame(city = name, geoid = id, meanNDVI = mean, sdNDVI = sd )
    }else{
      df[i,] <- c(name,id, mean, sd)
    }  
    # set export file name  
    e1 <-  paste0("data/processed/summaryNDVI/",ref,"_buffer_summaryNDVI.gpkg")
    if(!file.exists(e1)){
    # get values for each census tract
    # read in census tracts for the state
    ctFiles <- list.files("data/processed/censusGeographies", full.names = TRUE)
    ct <- ctFiles[grepl(pattern = state, x = ctFiles)] |>
      terra::vect()|>
      terra::intersect(city)
    # redefine names 
    names(ct) <- c("GEOID","NAME","B01003_001E","B01003_001M","OBJECTID","PLACENS","GEOID_city","NAME_city","State")
    # buffer each object before extraction
    for(feat in 1:nrow(ct)){
      buf <- terra::buffer(x = ct[feat,], width = 500 )
      extracted_values <- terra::extract(ndvi,buf, weight = TRUE)|>
        dplyr::mutate(area = weight * 100)|>
        summarise(
          mean_ndvi = mean(layer, na.rm = TRUE),
          sd_ndvi = sd(layer, na.rm = TRUE),
          prop_area = sum(area)
        )
      # bind to ct features 
      ct[feat, c("mean_ndvi","sd_ndvi","prop_area") ] <- extracted_values[1,]
      }
      # export result
      terra::writeVector(ct,e1)
    }
  }else{
    df[i,] <- c(name, id, NA, NA)
  }
  # export result 
  write.csv(df, paste0("data/processed/summaryNDVI/allCitiesNDVI.csv"))
  
}




