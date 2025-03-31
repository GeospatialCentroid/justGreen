pacman::p_load(terra, dplyr)


# read in cities 
cities <- terra::vect("data/processed/top200/top200Cities.gpkg")
# list all ndvi files 
ndviFiles <- list.files("data/processed/ndvi",
                        full.names = TRUE) 

# generate a dateframe with the average NDVI per citis 
for(i in seq_along(cities)){
  city <- cities[i]
  name <- city$NAME
  id <- city$GEOID
  state <- city$State
  # export Files 
  ref <- paste0(id,"_",name)
  
  # grab the ndvi file of interest 
  file <- ndviFiles[grepl(pattern = id, x = ndviFiles)] 
  if(length(file) > 0){
    ndvi <- file |> 
      terra::rast()
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
    e1 <-  paste0("data/processed/summaryNDVI/",ref,"_summaryNDVI.gpkg")
    if(!file.exists(e1)){
    # get values for each census tract 
    # read in census tracts for the state 
    ctFiles <- list.files("data/processed/censusGeographies", full.names = TRUE)
    ct <- ctFiles[grepl(pattern = state, x = ctFiles)] |> 
      terra::vect()|>
      terra::intersect(city)
    # Extract NDVI values for each census block group
    extracted_values <- terra::extract(ndvi, ct, weight = TRUE)
    
    # Calculate mean and standard deviation for each block group
    stats <- extracted_values |>
      dplyr::mutate(area = weight * 100) |> 
      group_by(ID) |>
      summarise(
        mean_ndvi = mean(NDVI, na.rm = TRUE),
        sd_ndvi = sd(NDVI, na.rm = TRUE),
        prop_area = sum(area)
      )
    # join back to census tracts 
    ct2 <- cbind(ct, stats)
    ct2$totalArea <- expanse(ct2,unit="m")
    # set names for proper export 
    names(ct2) <- c("GEOID","NAME","B01003_001E", "B01003_001M", "OBJECTID","PLACENS",
                    "GEOID_city","NAME_city","State","ID","mean_ndvi","sd_ndvi", "prop_area","totalArea")  
    # export result 
    terra::writeVector(ct2,e1)  
    }
    
  }else{
    df[i,] <- c(name, id, NA, NA)
  }
  # export result 
  write.csv(df, paste0("data/processed/summaryNDVI/allCitiesNDVI.csv"))
  
}
i
