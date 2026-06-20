# summaryNDVIPerCensusTract
pacman::p_load(terra, dplyr, furrr, purrr, tictoc, readr, tmap, stringr, sf)
tmap_mode("view")

# [Keep your existing Step 1 and Step 2 code here for loading file lists]

# --- NEW: Define Output Directory for Checkpoints ---
checkpoint_dir <- "data/processed/summaryNDVI/city_checkpoints"
dir.create(checkpoint_dir, showWarnings = FALSE, recursive = TRUE)

# 3. Define the processing function
processNDVI_Hybrid <- function(city_index, cities, ndvi_list, tract_list, out_dir) {
  
  # Redirect terra's temp files to your project drive
  temp_dir_path <- "data/processed/terra_temp"
  dir.create(temp_dir_path, showWarnings = FALSE, recursive = TRUE)
  terra::terraOptions(tempdir = temp_dir_path)
  
  # Extract the specific city row
  city_row <- cities[city_index, ]
  name <- city_row$NAME
  geoid <- city_row$GEOID
  state <- city_row$State
  
  # --- NEW: Checkpoint Logic ---
  # Define what the output file for this city should be named
  out_file <- file.path(out_dir, paste0("city_", geoid, "_ndvi.csv"))
  
  # If it already exists, skip processing entirely
  if (file.exists(out_file)) {
    return(NULL) 
  }
  # -----------------------------
  
  # Find the NDVI raster for this specific city
  f1 <- ndvi_list[grepl(pattern = paste0(geoid, "_"), x = ndvi_list, fixed = TRUE)]
  
  # Find the census tract file for the state
  ct_file <- tract_list[grepl(pattern = paste0(state, "_ct.gpkg"), x = tract_list)]
  
  if (length(f1) == 1 && length(ct_file) == 1) {
    
    r1 <- terra::rast(f1)
    ct_all <- sf::st_read(ct_file, quiet = TRUE) |> terra::vect()
    city_vect <- terra::vect(city_row)
    
    cityBuff <- terra::buffer(x = city_vect, width = 500)
    ct_city <- ct_all[cityBuff, ]
    r1_crop <- terra::crop(r1, ct_city)
    
    results_list <- list()
    
    for (i in 1:nrow(ct_city)) {
      tract <- ct_city[i, ]
      tract_geoid <- tract$GEOID
      
      t_500 <- terra::buffer(x = tract, width = 500)
      t_250 <- terra::buffer(x = tract, width = 250)
      
      r_500 <- terra::mask(x = r1_crop, mask = t_500)
      r_250 <- terra::mask(x = r1_crop, mask = t_250)
      
      vals_500 <- terra::values(r_500)
      mean_500 <- mean(vals_500, na.rm = TRUE)
      sd_500 <- sd(vals_500, na.rm = TRUE)
      cells_500 <- sum(!is.na(vals_500))
      
      vals_250 <- terra::values(r_250)
      mean_250 <- mean(vals_250, na.rm = TRUE)
      sd_250 <- sd(vals_250, na.rm = TRUE)
      cells_250 <- sum(!is.na(vals_250))
      
      df <- data.frame(
        geoid = tract_geoid,
        cityName = name,
        state = state,
        cityGEOID = geoid,
        totalCells_500m = cells_500,
        meanNDVI_500m = mean_500,
        standardDevNDVI_500m = sd_500,
        totalCells_250m = cells_250,
        meanNDVI_250m = mean_250,
        standardDevNDVI_250m = sd_250
      )
      
      results_list[[i]] <- df
    }
    
    final_city_df <- dplyr::bind_rows(results_list)
    
    # --- NEW: Write to disk immediately instead of returning to RAM ---
    readr::write_csv(final_city_df, out_file)
    
    # Aggressive Cleanup
    terra::tmpFiles(remove = TRUE)
    gc()
    
    # Return NULL so the parallel mapping doesn't hold data in memory
    return(NULL)
    
  } else {
    return(NULL) 
  }
}

# 4. Execute Parallel Processing
plan(multisession, workers = 10) 

tic()
# future_map (not _dfr) because we are returning NULLs now, not dataframes
future_map(
  .x = 1:nrow(cities_sf),
  .f = ~processNDVI_Hybrid(
    city_index = .x, 
    cities = cities_sf, 
    ndvi_list = ndvi, 
    tract_list = ct_files,
    out_dir = checkpoint_dir # Pass the output directory
  ),
  .options = furrr_options(packages = c("sf", "terra", "dplyr", "readr")) 
)
toc()

# 5. Assembly: Bind results and export the master file
# Find all the individual city CSVs we just created
all_city_files <- list.files(checkpoint_dir, full.names = TRUE, pattern = "\\.csv$")

# Read them all and bind them into one master dataframe
master_results <- purrr::map_dfr(all_city_files, readr::read_csv, show_col_types = FALSE)

readr::write_csv(
  master_results,
  "data/processed/summaryNDVI/allCensusTractsNDVI_2023.csv"
)