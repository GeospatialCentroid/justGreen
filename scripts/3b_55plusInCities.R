## not sure about this .... maybe come back
# pacman::p_load(sf, dplyr, purrr, stringr)

# # Load the cities file
# citiesPop <- st_read("data/processed/top200_2023/allCities.gpkg")

# # Define the output path
# exportPath <- "data/processed/top200_2023/allCities_35_55.gpkg"

# if (!file.exists(exportPath)) {
#   # 1. LOAD ALL BLOCK GROUPS *ONCE*
#   # ---
#   # Get list of all state block group files
#   bg_files <- list.files(
#     path = "data/processed/censusGeographies",
#     pattern = "_bg.gpkg$", # Ensures we only get BG files
#     full.names = TRUE
#   )
#   # grap unique ids of counties to filter all bg objects
#   target_county_geoids <- unique(citiesPop$countyGEOID)
#   # Read all files into a single 'sf' object
#   # We use map_dfr from 'purrr' which is a fast way to read and bind files
#   print("Loading all block group files... (This may take a minute)")
#   all_bgs <- purrr::map_dfr(bg_files, sf::st_read) |>
#     mutate(
#       # Create a temporary county GEOID from the BG's 12-digit GEOID
#       countyGEOID_from_bg = stringr::str_sub(GEOID, 1, 5)
#     ) |>
#     filter(
#       # Keep only BGs whose county GEOID is in our city list
#       countyGEOID_from_bg %in% target_county_geoids
#     )
#   # assign the orginal object area
#   all_bgs$original_bg_area = st_area(all_bgs$geom)

#   print("All block groups loaded and filtered.")

#   # 2. PERFORM AREAL-WEIGHTED INTERPOLATION
#   # ---
#   print("Intersecting cities and block groups...")
#   # This intersects cities with block groups, creating new smaller
#   # polygons where they overlap.
#   # This will be memory intensive but much faster than a loop.
#   sf::st_agr(citiesPop) = "constant"
#   sf::st_agr(all_bgs) = "constant"

#   # 3. PERFORM AREAL-WEIGHTED INTERPOLATION (in parallel)
#   # ---

#   # 3a. Set up your parallel plan
#   # This uses all available cores minus one
#   print("Setting up parallel workers...")
#   future::plan(multisession, workers = availableCores() - 1)

#   # 3b. Split 'citiesPop' into a list of single-row sf objects.
#   # We group by GEOID (or any unique city ID) and use group_split.
#   cities_list <- citiesPop |>
#     dplyr::group_split(GEOID)

#   print(paste(
#     "Beginning parallel intersection for",
#     length(cities_list),
#     "cities..."
#   ))

#   # 3c. Run the parallel intersection using future_map_dfr
#   # future_map_dfr iterates over 'cities_list', runs the function,
#   # and row-binds all the results (the 'dfr' part).
#   intersections <- furrr::future_map_dfr(
#     .x = cities_list[1:2],
#     .f = function(single_city) {
#       # Set AGR for this single city
#       sf::st_agr(single_city) = "constant"

#       # Run the intersection for *just this one city*
#       # against the (already loaded and filtered) block groups
#       sf::st_intersection(single_city, all_bgs) |>
#         mutate(
#           # Calculate the area of the *new* overlapping polygon
#           overlap_area = st_area(geometry),

#           # Find what fraction of the *original* BG this overlap represents
#           area_fraction = as.numeric(overlap_area / original_bg_area),

#           # Apportion the population based on that fraction
#           popOver20_apportioned = over20 * area_fraction,
#           popOver35_apportioned = over35 * area_fraction,
#           popOver55_apportioned = over55 * area_fraction
#         )
#     },
#     # .options helps manage globals and required packages for the workers
#     .options = furrr_options(
#       globals = "all_bgs", # Make sure 'all_bgs' is sent to workers
#       packages = c("sf", "dplyr") # Make sure workers load sf and dplyr
#     )
#   )

#   # 3d. Shut down the parallel workers when done
#   future::plan(sequential)

#   print("Intersection complete. Summarizing results...")
#   # 3. SUM THE APPORTIONED POPULATIONS
#   # ---
#   # We group by the city's unique ID and sum the apportioned values
#   # We use the 'GEOID' from citiesPop, but any unique city ID works
#   final_pops <- intersections |>
#     st_drop_geometry() |>
#     group_by(GEOID, NAME) |> # Group by your city's unique ID
#     summarize(
#       # Sum all the little apportioned pieces for each city
#       popOver20_2023 = sum(popOver20_apportioned, na.rm = TRUE),
#       popOver35_2023 = sum(popOver35_apportioned, na.rm = TRUE),
#       popOver55_2023 = sum(popOver55_apportioned, na.rm = TRUE),
#       .groups = "drop" # Drop the grouping
#     )

#   # 4. JOIN RESULTS AND EXPORT
#   # ---
#   # Join the new population data back to the original city polygons
#   new200 <- citiesPop |>
#     # Left join the new summary data
#     left_join(final_pops, by = c("GEOID", "NAME")) |>
#     # Handle the special "Bridgeport" case from your original script
#     # This sets pop20 to NA for all cities *except* Bridgeport
#     mutate(
#       popOver20_2023 = if_else(
#         NAME != "Bridgeport city",
#         NA_real_,
#         popOver20_2023
#       )
#     )

#   # Export the final file
#   print("Writing final file...")
#   sf::st_write(obj = new200, dsn = exportPath, delete_layer = TRUE)
# } else {
#   print("File already exists. Skipping.")
# }

# old

pacman::p_load(sf, dplyr)


citiesPop <- st_read("data/processed/top200_2023/allCities.gpkg")
exportPath <- "data/processed/top200_2023/allCities_35_55.gpkg"
if (!file.exists(exportPath)) {
  for (i in 1:nrow(citiesPop)) {
    # city
    city <- citiesPop[i, ]
    state <- city$State
    c2 <- terra::vect(city) |>
      terra::makeValid()

    # read in block groups for state
    path <- list.files(
      path = "data/processed/censusGeographies",
      pattern = paste0(state, "_bg"),
      full.names = TRUE
    )
    # read in data
    bg <- terra::vect(path)
    # crop
    crop <- terra::crop(bg, c2)
    # visualize the city area
    # terra::plot(c2, col = "blue")
    # terra::plot(bg, add = TRUE)
    # add the over 35 population to the file here
    pop35 <- sum(crop$over35, na.rm = TRUE)
    # pop over 55
    pop55 <- sum(crop$over55, na.rm = TRUE)

    #assign val to city and export
    city$popOver35_2023 <- pop35
    city$popOver55_2023 <- pop55

    if (city$NAME == "Bridgeport city") {
      pop20 <- sum(crop$over20)
      city$popOver20_2023 <- pop20
    }

    if (i == 1) {
      new200 <- st_as_sf(city)
    } else {
      new200 <- bind_rows(new200, st_as_sf(city))
    }
  }
  # export
  sf::st_write(obj = new200, dsn = exportPath)
}
