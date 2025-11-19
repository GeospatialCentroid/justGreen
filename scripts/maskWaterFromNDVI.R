pacman::p_load(terra, dplyr, furrr, purrr, tictoc,sf,stringr)



# list all ndvi files 
ndviFiles1 <- list.files("data/processed/ndvi",
                        full.names = TRUE,
                        pattern = "buffered10k.tif")
# altering this for the second run of files 
ndviFiles2 <- list.files("data/processed/ndvi",
                        full.names = TRUE,
                        pattern = "buffered10k_2.tif")

# drop the _2 from path names to get list of features to drop from group 1
all_bgs_to_drop_from_ndvi1 <- gsub("_2\\.tif$", ".tif", ndviFiles2)
# filter ndvi1
ndvi1_to_keep <- ndviFiles1[!(ndviFiles1 %in% all_bgs_to_drop_from_ndvi1)]
# bind the groups
ndvi <- c(ndviFiles2, ndvi1_to_keep)
# remove the duplicated values
# (?<=noWater/) is a "lookbehind" that finds the text but doesn't include it
# \\d+ matches one or more digits
geoids <- str_extract(ndvi, "(?<=ndvi/)\\d+")
# elvaluate the duplicate dvalue
dup <- ndvi[duplicated(geoids)]
# exclude
ndvi <- ndvi[!duplicated(geoids)]



# land 
land <- terra::vect("data/processed/naturalEarthData/ne_10m_land.gpkg")
lake1 <- terra::vect("data/processed/naturalEarthData/ne_10m_lakes_north_america.gpkg")
lake2 <- terra::vect("data/processed/naturalEarthData/ne_10m_lakes.gpkg")

# generate a dateframe with the average NDVI per citis 
# test for ndvi files missing 
t200 <- st_read("data/processed/top200/top200Cities.gpkg")
cNames <- t200$GEOID
missing <- c()
for(i in cNames){
  t1 <- grepl(pattern = i, x = ndvi)
  if(!TRUE %in% t1){
    missing <- c(missing, i)
  }
}
missingCities <- t200[t200$GEOID %in% missing, ]


## spliting this out to
##a funtion for summarizing all cities 
## a function for summarizing census tracts within a citin 

processNDVIImages <- function(ndviFile, land, lake1, lake2){
  n1 <- basename(ndviFile)
  export <- paste0("data/processed/ndvi_noWater/", n1)
  # print(n1)
  # if(!file.exists(export)){
    print(n1)
    r1 <- terra::rast(ndviFile)|>
      terra::mask(land) |>
      terra::mask(lake1, inverse = TRUE)|>
      terra::mask(lake2, inverse = TRUE)
    # return(list(
    #   name = n1,
    #   rast = r1
    # ))
    terra::writeRaster(x= r1, filename = export, overwrite = TRUE)
  # }
  gc()
}

# run in for loop for trouble shooting 
# for(i in ndviFiles){
#   # print(i)
#   processNDVIImages(ndviFile = i,
#                     land = land,
#                     lake1 = lake1,
#                     lake2 = lake2)
# }




# plan("future::multisession", workers = 8) # this was erroring out 
plan(multicore, workers = 10) # works but have to run from terminal.
# plan(sequential)
## not a super long run time but fast with multicore! 
tic()
future_map(.x = ndvi, .f = processNDVIImages,
           land=land,
           lake1 = lake1,
           lake2 = lake2)
toc()
