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

# ## issues with the cities layers when read in as before the st_make valid,
# ## will need to rerun the gee ndvi export with new shape files
# # cities2 <- terra::vect("data/processed/top200/top200CitiesTemp.gpkg")
# ## generate a bounding box
# # bb <- purrr::map(1:nrow(cities), ~ sf::st_bbox(cities[.x, ]))
# # bb2 <- purrr::map(1:nrow(cities), ~ terra::ext(cities2[.x, ]))
# # errors <- c()
# # for(i in 1:200){
# #   # sf - xmin      ymin      xmax      ymax
# #   # terra - (xmin, xmax, ymin, ymax)
# #   # test the xmin
# #   x1 <- round(bb[[i]][1],2 )
# #   x2 <- round(bb2[[i]][1],2 )
# #   t1 <- x1==x2
# #   # test the xmax
# #   x_1 <- round(bb[[i]][3],2 )
# #   x_2 <- round(bb2[[i]][2],2 )
# #   t2 <- x_1==x_2
# #   # test the ymin
# #   y1 <- round(bb[[i]][2],2 )
# #   y2 <- round(bb2[[i]][3],2 )
# #   t3 <- y1==y2
# #   # test the  y max
# #   y_1 <- round(bb[[i]][4],2 )
# #   y_2 <- round(bb2[[i]][4],2 )
# #   t4 <- x1==x2
# #   # if any fail then report i
# #   test <- all(c(t1,t2,t3,t4))
# #   # collected errros
# #   if(isFALSE(test)){
# #     errors <- c(errors,i)
# #   }
# # }
# # write out

# # ## what cities or ok or bad
# # cArea <- as.numeric(st_area(cities))
# # cArea2 <- as.numeric(terra::expanse(cities2))
# # # Calculate the difference
# # difference <- cArea - cArea2

# # Calculate the percent difference
# # percent_difference <- (difference / pmax(cArea, cArea2)) * 100
# # percent_difference > 1

# # c1 <- cities[111,]
# # c2 <- cities2[111,]

# # fc <- cities[cities$NAME =="Fort Collins city", ]
# # fc2 <- cities2[cities2$NAME =="Fort Collins city", ]

# # traspose to a list
# splitting_factor <- 1:nrow(cities)
# # Use split to create the list of single-row SpatVectors
# cityList <- terra::split(cities, f = splitting_factor)

# # missing cities
# cname <- cities$NAME
# missing <- c()
# for (i in cname) {
#   vals <- grepl(pattern = i, x = ndvi)
#   if (!TRUE %in% vals) {
#     missing <- c(missing, i)
#   }
# }

# gatherNDVI <- function(tract, ndvi, name, state) {
#   geoid = tract$GEOID
#   print(geoid)
#   # buffer area by 500 m
#   c2 <- terra::buffer(x = tract, width = 500)
#   # crop raster to city buffer
#   r2 <- terra::mask(x = ndvi, mask = c2)
#   # summarize the data
#   vals <- terra::values(r2, na.rm = TRUE)
#   mean <- mean(vals, na.rm = TRUE)
#   sd <- sd(vals, na.rm = TRUE)
#   # construct a data for results
#   df <- data.frame(
#     geoid = geoid,
#     city = name,
#     state = state,
#     totalCells = length(vals),
#     meanNDVI = mean,
#     standardDevNDVI = sd
#   )
#   return(df)
#   gc()
# }

# # city <- fc
# # ndviFiles <- ndvi
# # ctFiles <- ct

# processNDVItoTracks <- function(city, ndviFiles, ctFiles, overwrite) {
#   # Get indexing values
#   name <- city$NAME
#   geoid <- city$GEOID
#   state <- city$State
#   print(name)
#   # pull ndvi value
#   f1 <- ndviFiles[grepl(pattern = geoid, x = ndviFiles)]
#   # pull ct from stata
#   ct <- ctFiles[grepl(pattern = state, x = ctFiles)]
#   ct1 <- sf::st_read(ct[grepl(pattern = "ct", x = ct)])

#   # export path
#   exportPath <- paste0(
#     "~/trueNAS/work/justGreen/data/processed/summaryNDVI/",
#     geoid,
#     "_",
#     name,
#     "_500mBuffer_CT_2023NDVI.csv"
#   )
#   #
#   # if(!file.exists(exportPath) | overwrite == TRUE){
#   # condition for missing data
#   if (length(f1) > 0) {
#     r1 <- rast(f1)
#     # read in tracks
#     ct <- terra::vect(ct1)
#     # crop to id all intersection tracks
#     trackID <- terra::crop(ct, r1) |>
#       as.data.frame() |>
#       dplyr::select(GEOID) |>
#       dplyr::pull()
#     # select tracts
#     sel_ct <- ct[ct$GEOID %in% trackID, ]
#     # split in list
#     range <- 1:nrow(sel_ct)
#     ctList <- terra::split(sel_ct, f = range)
#     #
#     # # works with furrr but limiting parallization to the top level calls
#     data <- purrr::map(
#       .x = ctList,
#       .f = gatherNDVI,
#       ndvi = r1,
#       name = name,
#       state = state
#     )
#     # compile and export
#     d2 <- bind_rows(data)
#     readr::write_csv(x = d2, file = exportPath)
#   }
#   # } # end of condition for If statement
# }

# # testing
# ## issues with fort wayne, Indianapolis
# # Birmingham city
# for (i in cityList) {
#   print(i)
#   processNDVItoTracks(
#     city = i,
#     ndviFiles = ndvi,
#     ctFiles = ct,
#     overwrite = FALSE
#   )
# }
# # processNDVItoTracks(city = city,
# #                     ndviFiles = ndvi,
# # #                     ctFiles = ct,
# # #                     overwrite = TRUE)
# # #
# # purrr::map(.x = cityList,
# #            .f = processNDVItoTracks,
# #            ndviFiles = ndvi,
# #            ctFiles = ct,
# #            overwrite = FALSE)

# #
# plan(multicore, workers = 4) # works but have to run from terminal.
# # # plan(sequential)
# # ## not a super long run time but fast with multicore!

# # removed bridgeport city

# tic()
# furrr::future_map(
#   .x = cityList,
#   .f = processNDVItoTracks,
#   ndviFiles = ndvi,
#   ctFiles = ct,
#   overwrite = FALSE
# )
# toc()
# # 50 features sequential 9.5 sec
# # 50 features multicore 2.756 sec
