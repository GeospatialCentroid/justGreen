# summaryNDVIPerCensusTract
pacman::p_load(terra, dplyr, furrr, purrr, tictoc, readr, tmap, stringr, sf)
tmap_mode("view")

# 1. Read in and filter masked NDVI files
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
ndvi <- ndvi[!duplicated(geoids)]

# 2. Read in census tract files and cities
ct_files <- list.files("data/processed/censusGeographies", full.names = TRUE)
cities_sf <- sf::st_read("data/processed/top200/top200Cities.gpkg")

# --- Define Output Directory for Checkpoints ---
checkpoint_dir <- "data/processed/summaryNDVI/city_checkpoints"
dir.create(checkpoint_dir, showWarnings = FALSE, recursive = TRUE)

# 3. Define the processing function
# 3. Define the processing function
processNDVI_Hybrid <- function(city_index, cities, ndvi_list, tract_list, out_dir) {
  
  # Extract the specific city row
  city_row <- cities[city_index, ]
  name <- city_row$NAME
  geoid <- city_row$GEOID
  state <- city_row$State
  
  # --- NEW: Worker Isolation for Temp Files ---
  # Create a UNIQUE temp folder for this specific city to prevent collisions
  temp_dir_path <- file.path("data/processed/terra_temp", paste0("worker_", geoid))
  dir.create(temp_dir_path, showWarnings = FALSE, recursive = TRUE)
  terra::terraOptions(tempdir = temp_dir_path)
  # ------------------------------------------
  
  # Checkpoint Logic
  out_file <- file.path(out_dir, paste0("city_", geoid, "_ndvi.csv"))
  
  if (file.exists(out_file)) {
    # Cleanup empty isolated dir before skipping
    unlink(temp_dir_path, recursive = TRUE)
    return(NULL) 
  }
  
  f1 <- ndvi_list[grepl(pattern = paste0(geoid, "_"), x = ndvi_list, fixed = TRUE)]
  ct_file <- tract_list[grepl(pattern = paste0(state, "_ct.gpkg"), x = tract_list)]
  
  if (length(f1) == 1 && length(ct_file) == 1) {
    
    # --- NEW: tryCatch Safety Net ---
    # Wrap the dangerous spatial operations in tryCatch
    process_status <- tryCatch({
      
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
      readr::write_csv(final_city_df, out_file)
      
      # If everything above succeeds, return TRUE
      TRUE 
      
    }, error = function(e) {
      # If any error occurs in the block above, catch it here
      # Write to an error log file without crashing the main script
      log_message <- paste(Sys.time(), "- Failed City GEOID:", geoid, "- Error:", e$message, "\n")
      cat(log_message, file = "data/processed/summaryNDVI/error_log.txt", append = TRUE)
      
      # Return FALSE to indicate failure
      return(FALSE)
    })
    # --------------------------------
    
    # --- NEW: Aggressive Cleanup Update ---
    # Instead of terra::tmpFiles(), we completely delete this worker's isolated folder
    unlink(temp_dir_path, recursive = TRUE)
    gc()
    
    return(NULL)
    
  } else {
    # Cleanup empty isolated dir before skipping
    unlink(temp_dir_path, recursive = TRUE)
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



### attempting a sequential run for troubleshooting 
# summaryNDVIPerCensusTract - Sequential & Stable
pacman::p_load(terra, dplyr, purrr, tictoc, readr, tmap, stringr, sf)
tmap_mode("view")

# 1. Read in and filter masked NDVI files
ndvi1 <- list.files("data/processed/ndvi_noWater", full.names = TRUE, pattern = "2023NDVI")
ndvi2 <- list.files("data/processed/ndvi_noWater", full.names = TRUE, pattern = "buffered10k_2.tif")

all_bgs_to_drop_from_ndvi1 <- gsub("_2\\.tif$", ".tif", ndvi2)
ndvi1_to_keep <- ndvi1[!(ndvi1 %in% all_bgs_to_drop_from_ndvi1)]
ndvi <- c(ndvi2, ndvi1_to_keep)

geoids <- str_extract(ndvi, "(?<=noWater/)\\d+")
ndvi <- ndvi[!duplicated(geoids)]

# 2. Read in census tract files and cities
ct_files <- list.files("data/processed/censusGeographies", full.names = TRUE)
cities_sf <- sf::st_read("data/processed/top200/top200Cities.gpkg")

# --- Define Output Directory for Checkpoints ---
checkpoint_dir <- "data/processed/summaryNDVI/city_checkpoints"
dir.create(checkpoint_dir, showWarnings = FALSE, recursive = TRUE)

# Redirect terra's temp files to your project drive (only need one shared folder now)
temp_dir_path <- "data/processed/terra_temp"
dir.create(temp_dir_path, showWarnings = FALSE, recursive = TRUE)
terra::terraOptions(tempdir = temp_dir_path)

# 3. Sequential For Loop Processing
tic()

for (city_index in 1:nrow(cities_sf)) {
  
  # Extract the specific city row
  city_row <- cities_sf[city_index, ]
  name <- city_row$NAME
  geoid <- city_row$GEOID
  state <- city_row$State
  
  # Print status to console so you can monitor progress
  cat("Processing City", city_index, "of", nrow(cities_sf), "-", name, "\n")
  
  # --- Checkpoint Logic ---
  out_file <- file.path(checkpoint_dir, paste0(geoid, "_", name, "_500mBuffer_CT_2023NDVI.csv"))
  
  if (file.exists(out_file)) {
    cat("  -> Already processed. Skipping.\n")
    next # Instantly jumps to the next city in the loop
  }
  
  # Find the required files
  f1 <- ndvi[grepl(pattern = paste0(geoid, "_"), x = ndvi, fixed = TRUE)]
  ct_file <- ct_files[grepl(pattern = paste0(state, "_ct.gpkg"), x = ct_files)]
  
  if (length(f1) == 1 && length(ct_file) == 1) {
    
    # --- tryCatch Safety Net ---
    process_status <- tryCatch({
      
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
      
      # Bind, Write, and Clean
      final_city_df <- dplyr::bind_rows(results_list)
      readr::write_csv(final_city_df, out_file)
      
      terra::tmpFiles(remove = TRUE)
      gc()
      
      cat("  -> Success.\n")
      TRUE
      
    }, error = function(e) {
      log_message <- paste(Sys.time(), "- Failed City GEOID:", geoid, "- Error:", e$message, "\n")
      cat(log_message, file = "data/processed/summaryNDVI/error_log.txt", append = TRUE)
      cat("  -> FAILED. Logged to error_log.txt.\n")
      
      # Clean up temp files even if it fails so the drive doesn't fill up
      terra::tmpFiles(remove = TRUE) 
      gc()
      
      return(FALSE)
    })
    
  } else {
    cat("  -> Missing input files. Skipping.\n")
  }
}

toc()

# 4. Assembly: Bind results and export the master file
all_city_files <- list.files(checkpoint_dir, full.names = TRUE, pattern = "\\.csv$")
master_results <- purrr::map_dfr(
  all_city_files, 
  readr::read_csv, 
  col_types = readr::cols(
    geoid = readr::col_character(),
    cityGEOID = readr::col_character() # Added this line
  ), 
  show_col_types = FALSE
)

readr::write_csv(
  master_results,
  "data/processed/summaryNDVI/allCensusTractsNDVI_2023.csv"
)
