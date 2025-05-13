# summaryNDVIPerCensusTRact



pacman::p_load(terra, dplyr, furrr, purrr, tictoc, readr)


# read in masked NDVI files 
ndvi <- list.files("data/processed/ndvi_noWater",
                   full.names = TRUE)
# read in cenus tract paths 
ct <- list.files("data/processed/censusGeographies",
                 full.names = TRUE)

# read in city layer 
cities <- terra::vect("data/processed/top200/top200Cities.gpkg")
# traspose to a list 
splitting_factor <- 1:nrow(cities)
# Use split to create the list of single-row SpatVectors
cityList <- terra::split(cities, f = splitting_factor)


gatherNDVI <- function(tract, ndvi, name, state){
  geoid = tract$GEOID
  # buffer area by 500 m 
  c2 <- terra::buffer(x = tract, width = 500)
  # crop raster to city buffer 
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
  return(df)
  gc()
}


processNDVItoTracks <- function(city, ndviFiles,ctFiles,overwrite ){
  # Get indexing values 
  name <- city$NAME
  geoid <- city$GEOID
  state <- city$State
  # pull ndvi value 
  f1 <- ndviFiles[grepl(pattern = geoid, x = ndviFiles)]
  # pull ct from stata 
  ct1 <- ctFiles[grepl(pattern = state, x = ctFiles)]
  
  # export path 
  exportPath <- paste0("~/trueNAS/work/justGreen/data/processed/summaryNDVI/",
                       geoid,"_",name,"_500mBuffer_CT_NDVI.csv")
  #
  if(!file.exists(exportPath) | overwrite == TRUE){
    # condition for missing data 
    if(length(f1)>0){
      r1 <- rast(f1)
      
      # read in tracks 
      ct <- terra::vect(ct1)
      # crop to id all intersection tracks 
      trackID <- terra::crop(ct, r1) |>
        as.data.frame()|>
        dplyr::select(GEOID)|>
        dplyr::pull()
      # select tracts 
      sel_ct <- ct[ct$GEOID %in% trackID, ]
      # split in list 
      range <- 1:nrow(sel_ct)
      ctList <- terra::split(sel_ct, f = range)
      
      # works with furrr but limiting parallization to the top level calls 
      data <- furrr::future_map(.x = ctList, .f = gatherNDVI,
                                ndvi = r1, 
                                name = name, 
                                state = state)
      # compile and export 
      d2 <- bind_rows(data)
      readr::write_csv(x = d2, file = exportPath)
    }
  }
  
 
}

plan(multicore, workers = 18) # works but have to run from terminal.
# plan(sequential)
## not a super long run time but fast with multicore! 
tic()
furrr::future_map(.x = cityList, .f = processNDVItoTracks,
                      ndviFiles = ndvi,
                      ctFiles = ct)
toc()
# 50 features sequential 9.5 sec
# 50 features multicore 2.756 sec
