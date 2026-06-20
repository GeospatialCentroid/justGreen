pacman::p_load(terra, dplyr, furrr, purrr, tictoc, readr, future, stringr)

#original run
ndvi1 <- list.files(
  "data/processed/ndvi_noWater",
  full.names = TRUE,
  pattern = "buffered10k\\.tif$" # Safer regex
)
# updated run for cities with poor geometries
ndvi2 <- list.files(
  "data/processed/ndvi_noWater",
  full.names = TRUE,
  pattern = "buffered10k_2\\.tif$" # Safer regex
)
# drop the _2 from path names to get list of features to drop from group 1
paths_to_drop_from_ndvi1 <- gsub("_2\\.tif$", ".tif", ndvi2)
# filter ndvi1
ndvi1_to_keep <- ndvi1[!(ndvi1 %in% paths_to_drop_from_ndvi1)]
# bind the groups
ndvi <- c(ndvi2, ndvi1_to_keep)
# remove the duplicated values
# (?<=noWater/) is a "lookbehind" that finds the text but doesn't include it
# \\d+ matches one or more digits
geoids <- str_extract(ndvi, "(?<=noWater/)\\d+")
# elvaluate the duplicate dvalue
dup <- ndvi[duplicated(geoids)]
# exclude
ndvi <- ndvi[!duplicated(geoids)]


# read in city layer
cities <- terra::vect("data/processed/top200/top200Cities.gpkg")

# traspose to a list
splitting_factor <- 1:nrow(cities)
# Use split to create the list of single-row SpatVectors
cityList <- terra::split(cities, f = splitting_factor)

processNDVI <- function(city, ndviFiles) {
  # Get indexing values
  name <- city$NAME
  geoid <- city$GEOID
  state <- city$State
  
  # pull ndvi value
  f1 <- ndviFiles[grepl(
    pattern = paste0(geoid, "_"),
    x = ndviFiles,
    fixed = TRUE
  )]
  
  # condition for missing data
  if (length(f1) == 1) {
    r1 <- rast(f1)
    
    # 1. Create buffers for both 500m and 250m
    c_500 <- terra::buffer(x = city, width = 500)
    c_250 <- terra::buffer(x = city, width = 250)
    
    # 2. Mask raster to respective city buffers
    r_500 <- terra::mask(x = r1, mask = c_500)
    r_250 <- terra::mask(x = r1, mask = c_250)
    
    # 3. Summarize the 500m data
    vals_500 <- terra::values(r_500)
    mean_500 <- mean(vals_500, na.rm = TRUE)
    sd_500 <- sd(vals_500, na.rm = TRUE)
    cells_500 <- sum(!is.na(vals_500))
    # 4. Summarize the 250m data
    vals_250 <- terra::values(r_250)
    mean_250 <- mean(vals_250, na.rm = TRUE)
    sd_250 <- sd(vals_250, na.rm = TRUE)
    cells_250 <- sum(!is.na(vals_250))
    # 5. Construct a widened dataframe for results
    df <- data.frame(
      geoid = geoid,
      city = name,
      state = state,
      totalCells_500m = cells_500,
      meanNDVI_500m = mean_500,
      standardDevNDVI_500m = sd_500,
      totalCells_250m = cells_250,
      meanNDVI_250m = mean_250,
      standardDevNDVI_250m = sd_250
    )
    
    # return
    return(df)
    gc()
  }
}
# testing
cityNames <- cities$NAME

testCity <- grep(pattern = "Bridgeport city", x = cities$NAME)
t1 <- processNDVI(city = cityList[[18]], ndviFiles = ndvi)


plan(multicore, workers = 10) # works but have to run from terminal.
plan(sequential)
## not a super long run time but fast with multicore!
tic()
results <- future_map(.x = cityList, .f = processNDVI, ndviFiles = ndvi)
toc()
# 50 features sequential 9.5 sec
# 50 features multicore 2.756 sec

# bind results and export
allData <- bind_rows(results)
readr::write_csv(allData, "data/processed/summaryNDVI/allCitiesNDVI_2023.csv")

## read in result to evalutes
# t1 <- read_csv("data/processed/summaryNDVI/allCitiesNDVI_2023.csv")
