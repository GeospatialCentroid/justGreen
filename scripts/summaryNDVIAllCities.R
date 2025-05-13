
pacman::p_load(terra, dplyr, furrr, purrr, tictoc, readr)


# read in masked NDVI files 
ndvi <- list.files("data/processed/ndvi_noWater",
                   full.names = TRUE)
# read in city layer 
cities <- terra::vect("data/processed/top200/top200Cities.gpkg")
# traspose to a list 
splitting_factor <- 1:nrow(cities)
# Use split to create the list of single-row SpatVectors
cityList <- terra::split(cities, f = splitting_factor)


city <- cities[20]
# ndviFiles <- ndvi
processNDVI <- function(city, ndviFiles){
  # Get indexing values 
  name <- city$NAME
  geoid <- city$GEOID
  state <- city$State
  # pull ndvi value 
  f1 <- ndviFiles[grepl(pattern = geoid, x = ndviFiles)]
  # condition for missing data 
  if(length(f1)>0){
    r1 <- rast(f1)
    # buffer city by 500 m 
    c2 <- terra::buffer(x = city, width = 500)
    # mask raster to city buffer 
    r2 <- terra::mask(x = r1, mask = c2)
    # summarize the data 
    vals <- terra::values(r2)
    mean <- mean(vals, na.rm = TRUE)
    sd <- sd(vals, na.rm = TRUE)
    # construct a data for results 
    df <- data.frame(
      geoid = geoid,
      city = name, 
      state = state,
      totalCells = length(vals),
      meanNDVI = mean,
      standardDevNDVI = sd
    )
    # return 
    return(df)
    gc()
    }
  }

plan(multicore, workers = 10) # works but have to run from terminal.
# plan(sequential)
## not a super long run time but fast with multicore! 
tic()
results <- future_map(.x = cityList, .f = processNDVI,
           ndviFiles = ndvi)
toc()
# 50 features sequential 9.5 sec
# 50 features multicore 2.756 sec

# bind results and export 
allData <- bind_rows(results)
readr::write_csv(allData, "data/processed/summaryNDVI/allCitiesNDVI.csv")
