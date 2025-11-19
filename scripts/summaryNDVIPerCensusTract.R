# summaryNDVIPerCensusTRact
pacman::p_load(terra, dplyr, furrr, purrr, tictoc, readr, tmap, stringr)
tmap_mode("view")

# read in masked NDVI files
ndvi1 <- list.files(
  "data/processed/ndvi_noWater",
  full.names = TRUE,
  pattern = "2023NDVI"
)
# altering this for the second run of files
ndvi2 <- list.files(
  "data/processed/ndvi_noWater",
  full.names = TRUE,
  pattern = "buffered10k_2.tif"
)
# drop the _2 from path names to get list of features to drop from group 1
all_bgs_to_drop_from_ndvi1 <- gsub("_2\\.tif$", ".tif", ndvi2)
# filter ndvi1
ndvi1_to_keep <- ndvi1[!(ndvi1 %in% all_bgs_to_drop_from_ndvi1)]
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


# read in cenus tract all_bgs
ct <- list.files("data/processed/censusGeographies", full.names = TRUE)

# read in city layer
cities <- sf::st_read("data/processed/top200/top200Cities.gpkg")
# traspose to a list
splitting_factor <- 1:nrow(cities)
# Use split to create the list of single-row SpatVectors
cityList <- terra::split(cities, f = splitting_factor)


processNDVI <- function(city, ndviFiles, ct) {
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

  # grab the ct for the state
  ct1 <- ct[grepl(pattern = paste0(state, "_ct.gpkg"), x = ct)] |>
    sf::st_read() |>
    dplyr::mutate(cityName = name, state = state, cityGEOID = geoid)
  # Limit to the spatial boundies of the city buffered
  # buffer city by 500 m
  cityBuff <- terra::buffer(x = terra::vect(city), width = 500)

  ct2 <- terra::vect(ct1)[cityBuff, ]
  # read in NDVI values and
  r1 <- rast(f1) |>
    terra::crop(ct2)

  # 1. Define your statistics function
  #    (na.rm = TRUE is essential)
  get_stats <- function(x) {
    c(
      mean = mean(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      totalCells = sum(!is.na(x))
    )
  }
  # 2. Run terra::extract()
  zonal_stats <- terra::extract(
    r1,
    ct2,
    fun = get_stats
  ) |>
    as.data.frame() |>
    dplyr::select(
      -ID
    )
  # rename layers for consistency
  names(zonal_stats) <- c("meanNDVI", "standardDevNDVI", "totalCells")

  # 3. Combine with polygon attributes
  final_df <- cbind(
    as.data.frame(ct2),
    zonal_stats
  )
  return(final_df)
}

# test
# miami was missing a lot of data
city <- cities[cities$GEOID == 1245000, ]
#bridgeport how too many char in geoid for censusblocks
city <- cityList[[18]]

t1 <- processNDVI(city = city, ndviFiles = ndvi, ct = ct)


plan(multicore, workers = 10) # works but have to run from terminal.
# plan(sequential)
## not a super long run time but fast with multicore!
tic()
results <- future_map(
  .x = cityList,
  .f = processNDVI,
  ndviFiles = ndvi,
  ct = ct
)
toc()
# 50 features sequential 9.5 sec
# 50 features multicore 2.756 sec

# bind results and export
allData <- bind_rows(results)
readr::write_csv(
  allData,
  "data/processed/summaryNDVI/allCensusTractsNDVI_2023.csv"
)

# eval
t1 <- readr::read_csv("data/processed/summaryNDVI/allCensusTractsNDVI_2023.csv")
# where were the errors
## not quite errors, just locations that are being removed from the water mask

t2 <- t1[is.na(t1$standardDevNDVI), ]

## cities to rerender ndvi values
reruns <- unique(t2$cityGEOID)
# Use which() to get all row numbers that match
positions <- which(cities$GEOID %in% reruns)

print(positions)
