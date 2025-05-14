pacman::p_load(terra, dplyr, furrr, purrr, tictoc)

# crop features to waterways 
source("scripts/maskWaterFromNDVI.R")

# Generate an summary ndvi file for all cities  
source("scripts/summaryNDVIAllCities.R")

# Generate a summary of NDVI values for all census tracts 
## this takes a while to run so only do if needed. 
source("scripts/summaryNDVIAllCities.R")



# for(i in 1:nrow(cities)){
#   print(i)
#   city <- cities[i] |> terra::makeValid()
#   name <- city$NAME
#   print(name)
#   id <- city$GEOID
#   state <- city$State
#   # export Files 
#   ref <- paste0(id,"_",name)
#   
#   # grab the ndvi file of interest 
#   file <- ndviFiles[grepl(pattern = id, x = ndviFiles)] 
#   # select buffer only 
#   file <- file[grepl(pattern = "_buffered", file)]
#   if(length(file) > 0){
#     ndvi <- file |> 
#       terra::rast()|>
#     
#     # Calculate mean and standard deviation for each block group
#     vals <- terra::values(ndvi) 
#     mean <- mean(vals, na.rm = TRUE)
#     sd <- sd(vals, na.rm = TRUE)
#     if(i==1){
#       df <- data.frame(city = name, geoid = id, meanNDVI = mean, sdNDVI = sd )
#     }else{
#       df[i,] <- c(name,id, mean, sd)
#     }  
#     # set export file name  
#     e1 <-  paste0("data/processed/summaryNDVI/",ref,"_buffer_summaryNDVI.gpkg")
#     if(!file.exists(e1)){
#     # get values for each census tract
#     # read in census tracts for the state
#     ctFiles <- list.files("data/processed/censusGeographies", full.names = TRUE)
#     ct <- ctFiles[grepl(pattern = state, x = ctFiles)] |>
#       terra::vect()|>
#       terra::intersect(city)
#     # redefine names 
#     names(ct) <- c("GEOID","NAME","over18","OBJECTID","PLACENS","GEOID_city","NAME_city","State")
#     # buffer each object before extraction
#     for(feat in 1:nrow(ct)){
#       buf <- terra::buffer(x = ct[feat,], width = 500 )
#       extracted_values <- terra::extract(ndvi,buf, weight = TRUE)|>
#         dplyr::mutate(area = weight * 100)|>
#         summarise(
#           mean_ndvi = mean(layer, na.rm = TRUE),
#           sd_ndvi = sd(layer, na.rm = TRUE),
#           prop_area = sum(area)
#         )
#       # bind to ct features 
#       ct[feat, c("mean_ndvi","sd_ndvi","prop_area") ] <- extracted_values[1,]
#       }
#       # export result
#       terra::writeVector(ct,e1, overwrite = TRUE)
#     }
#   }else{
#     df[i,] <- c(name, id, NA, NA)
#   }
#   # export result 
#   write.csv(df, paste0("data/processed/summaryNDVI/allCitiesNDVI.csv"))
#   
# }
# 
# 
# 
# 
