pacman::p_load(tidycensus, dplyr, sf, stringr)

# Set your Census API key (replace "YOUR_API_KEY" with your actual key)
# census_api_key("YOUR_API_KEY", install = TRUE, overwrite = TRUE)

# 1. SIMPLIFIED STATE LIST
# Get all census block groups for the United States
# You can access the 'State' column directly with '$' and call unique()
states <- sf::st_read("data/processed/top200/top200Cities.gpkg")$State |>
  unique()

# --- Variable lists ---

# mortality age (20+)
age_variables <- c(
  "B01001_008E", # Male: 20 years
  "B01001_009E", # Male: 21 years
  "B01001_010E", # Male: 22 to 24 years
  "B01001_011E", # Male: 25 to 29 years
  "B01001_012E", # Male: 30 to 34 years
  "B01001_013E", # Male: 35 to 39 years
  "B01001_014E", # Male: 40 to 44 years
  "B01001_015E", # Male: 45 to 49 years
  "B01001_016E", # Male: 50 to 54 years
  "B01001_017E", # Male: 55 to 59 years
  "B01001_018E", # Male: 60 to 64 years
  "B01001_019E", # Male: 65 to 74 years
  "B01001_020E", # Male: 75 to 84 years
  "B01001_021E", # Male: 85 years and over
  "B01001_032E", # Female: 20 years
  "B01001_033E", # Female: 21 years
  "B01001_034E", # Female: 22 to 24 years
  "B01001_035E", # Female: 25 to 29 years
  "B01001_036E", # Female: 30 to 34 years
  "B01001_037E", # Female: 35 to 39 years
  "B01001_038E", # Female: 40 to 44 years
  "B01001_039E", # Female: 45 to 49 years
  "B01001_040E", # Female: 50 to 54 years
  "B01001_041E", # Female: 55 to 59 years
  "B01001_042E", # Female: 60 to 64 years
  "B01001_043E", # Female: 65 to 74 years
  "B01001_044E", # Female: 75 to 84 years
  "B01001_045E" # Female: 85 years and over
)
# stroke age - 35+
age_35 <- c(
  "B01001_013E", # Male: 35 to 39 years
  "B01001_014E", # Male: 40 to 44 years
  "B01001_015E", # Male: 45 to 49 years
  "B01001_016E", # Male: 50 to 54 years
  "B01001_017E", # Male: 55 to 59 years
  "B01001_018E", # Male: 60 to 64 years
  "B01001_019E", # Male: 65 to 74 years
  "B01001_020E", # Male: 75 to 84 years
  "B01001_021E", # Male: 85 years and over
  "B01001_037E", # Female: 35 to 39 years
  "B01001_038E", # Female: 40 to 44 years
  "B01001_039E", # Female: 45 to 49 years
  "B01001_040E", # Female: 50 to 54 years
  "B01001_041E", # Female: 55 to 59 years
  "B01001_042E", # Female: 60 to 64 years
  "B01001_043E", # Female: 65 to 74 years
  "B01001_044E", # Female: 75 to 84 years
  "B01001_045E" # Female: 85 years and over
)
# dementia age - 55+
age_55 <- c(
  "B01001_017E", # Male: 55 to 59 years
  "B01001_018E", # Male: 60 to 64 years
  "B01001_019E", # Male: 65 to 74 years
  "B01001_020E", # Male: 75 to 84 years
  "B01001_021E", # Male: 85 years and over
  "B01001_041E", # Female: 55 to 59 years
  "B01001_042E", # Female: 60 to 64 years
  "B01001_043E", # Female: 65 to 74 years
  "B01001_044E", # Female: 75 to 84 years
  "B01001_045E" # Female: 85 years and over
)

# 2. CREATE A REUSABLE FUNCTION
# This function contains all the logic you had in your loop
# It takes the state (s) and geography level (geo_level) as arguments
fetch_and_process_acs <- function(s, geo_level) {
  # Create a short name for the file (e.g., "_ct" or "_bg")
  geo_abbr <- if (geo_level == "tract") "_ct" else "_bg"

  # Build the export path dynamically
  exportPath <- paste0(
    "data/processed/censusGeographies/",
    s,
    geo_abbr,
    ".gpkg"
  )

  print(paste("--- Processing:", s, geo_level, "---"))

  # Check if file exists (your original, good logic)
  if (file.exists(exportPath)) {
    print("... File already exists. Skipping.")
    return(NULL) # Exit the function
  }

  # 3. ADD ERROR HANDLING
  # Wrap the download in tryCatch so one state's error doesn't stop the loop
  tryCatch(
    {
      # Call get_acs with arguments from the function
      acs <- get_acs(
        geography = geo_level,
        variables = age_variables,
        year = 2023,
        survey = "acs5", # Be explicit that you want 5-year data
        state = s,
        geometry = TRUE,
        output = "wide"
      )

      # Process the data
      # Using all_of() inside across() is safer when using character vectors
      acs2 <- acs |>
        dplyr::mutate(
          over20 = rowSums(across(all_of(age_variables)), na.rm = TRUE),
          over35 = rowSums(across(all_of(age_35)), na.rm = TRUE),
          over55 = rowSums(across(all_of(age_55)), na.rm = TRUE)
        ) |>
        dplyr::select(
          "GEOID",
          "NAME",
          "over20",
          "over35",
          "over55",
          "geometry"
        ) |>
        sf::st_transform(crs = 4326)

      # project and export
      sf::st_write(obj = acs2, exportPath, quiet = TRUE)
      print(paste("... Success. File written to", exportPath))
    },
    error = function(e) {
      # If an error occurs, print it and continue the loop
      print(paste("!!! ERROR processing", s, "-", geo_level, ":", e$message))
    }
  )
}

# 4. SIMPLIFIED LOOP
# Your main loop is now much cleaner and easier to read
for (s1 in states) {
  fetch_and_process_acs(s = s1, geo_level = "tract")
  fetch_and_process_acs(s = s1, geo_level = "block group")
}

print("--- All states processed ---")

# # old
# pacman::p_load(tidycensus, dplyr, sf)

# # Set your Census API key (replace "YOUR_API_KEY" with your actual key)
# #
# # Get all census block groups for the United States
# states <- sf::st_read("data/processed/top200/top200Cities.gpkg") |>
#   dplyr::select(State) |>
#   sf::st_drop_geometry() |>
#   dplyr::pull() |>
#   unique()

# # mortality age
# age_variables <- c(
#   # "B01001_007E", # Male: 18 and 19 years
#   "B01001_008E", # Male: 20 years
#   "B01001_009E", # Male: 21 years
#   "B01001_010E", # Male: 22 to 24 years
#   "B01001_011E", # Male: 25 to 29 years
#   "B01001_012E", # Male: 30 to 34 years
#   "B01001_013E", # Male: 35 to 39 years
#   "B01001_014E", # Male: 40 to 44 years
#   "B01001_015E", # Male: 45 to 49 years
#   "B01001_016E", # Male: 50 to 54 years
#   "B01001_017E", # Male: 55 to 59 years
#   "B01001_018E", # Male: 60 to 64 years
#   "B01001_019E", # Male: 65 to 74 years
#   "B01001_020E", # Male: 75 to 84 years
#   "B01001_021E", # Male: 85 years and over
#   # "B01001_031E", # Female: 18 and 19 years
#   "B01001_032E", # Female: 20 years
#   "B01001_033E", # Female: 21 years
#   "B01001_034E", # Female: 22 to 24 years
#   "B01001_035E", # Female: 25 to 29 years
#   "B01001_036E", # Female: 30 to 34 years
#   "B01001_037E", # Female: 35 to 39 years
#   "B01001_038E", # Female: 40 to 44 years
#   "B01001_039E", # Female: 45 to 49 years
#   "B01001_040E", # Female: 50 to 54 years
#   "B01001_041E", # Female: 55 to 59 years
#   "B01001_042E", # Female: 60 to 64 years
#   "B01001_043E", # Female: 65 to 74 years
#   "B01001_044E", # Female: 75 to 84 years
#   "B01001_045E" # Female: 85 years and over
# )
# # stroke age - 35+
# age_35 <- c(
#   "B01001_013E", # Male: 35 to 39 years
#   "B01001_014E", # Male: 40 to 44 years
#   "B01001_015E", # Male: 45 to 49 years
#   "B01001_016E", # Male: 50 to 54 years
#   "B01001_017E", # Male: 55 to 59 years
#   "B01001_018E", # Male: 60 to 64 years
#   "B01001_019E", # Male: 65 to 74 years
#   "B01001_020E", # Male: 75 to 84 years
#   "B01001_021E", # Male: 85 years and over
#   "B01001_037E", # Female: 35 to 39 years
#   "B01001_038E", # Female: 40 to 44 years
#   "B01001_039E", # Female: 45 to 49 years
#   "B01001_040E", # Female: 50 to 54 years
#   "B01001_041E", # Female: 55 to 59 years
#   "B01001_042E", # Female: 60 to 64 years
#   "B01001_043E", # Female: 65 to 74 years
#   "B01001_044E", # Female: 75 to 84 years
#   "B01001_045E" # Female: 85 years and over
# )
# # dementia age - 55+
# age_55 <- c(
#   "B01001_017E", # Male: 55 to 59 years
#   "B01001_018E", # Male: 60 to 64 years
#   "B01001_019E", # Male: 65 to 74 years
#   "B01001_020E", # Male: 75 to 84 years
#   "B01001_021E", # Male: 85 years and over
#   "B01001_041E", # Female: 55 to 59 years
#   "B01001_042E", # Female: 60 to 64 years
#   "B01001_043E", # Female: 65 to 74 years
#   "B01001_044E", # Female: 75 to 84 years
#   "B01001_045E" # Female: 85 years and over
# )

# # iterate over each states and download ACS geomentries are
# for (i in seq_along(states)) {
#   # state
#   s1 <- states[i]
#   # export path
#   print(s1)
#   print("census tracts")
#   exportPath <- paste0("data/processed/censusGeographies/", s1, "_ct.gpkg")
#   if (!file.exists(exportPath)) {
#     # pull acs
#     acs <- get_acs(
#       geography = "tract",
#       variables = age_variables, # Total Population (any variable works for geography)
#       year = 2023, # Or the desired year
#       state = s1,
#       geometry = TRUE,
#       output = "wide" #output wide data for easier manipulation of the geometry.
#     )
#     # thin to over 18 pop
#     acs2 <- acs |>
#       dplyr::mutate(
#         over20 = rowSums(across(age_variables)),
#         over35 = rowSums(across(age_35)),
#         over55 = rowSums(across(age_55))
#       ) |>
#       dplyr::select(
#         "GEOID",
#         "NAME",
#         "over20",
#         "over35",
#         "over55",
#         "geometry"
#       ) |>
#       sf::st_transform(crs = 4326)

#     # project and export
#     sf::st_write(obj = acs2, exportPath)
#   }
#   print("block groups")

#   # export path
#   exportPath2 <- paste0("data/processed/censusGeographies/", s1, "_bg.gpkg")
#   if (!file.exists(exportPath2)) {
#     # pull acs
#     acs <- get_acs(
#       geography = "block group",
#       variables = age_variables, # Total Population (any variable works for geography)
#       year = 2023, # Or the desired year
#       state = s1,
#       geometry = TRUE,
#       output = "wide" #output wide data for easier manipulation of the geometry.
#     )
#     # thin to over 18 pop
#     acs2 <- acs |>
#       dplyr::mutate(
#         over20 = rowSums(across(age_variables)),
#         over35 = rowSums(across(age_35)),
#         over55 = rowSums(across(age_55))
#       ) |>
#       dplyr::select(
#         "GEOID",
#         "NAME",
#         "over20",
#         "over35",
#         "over55",
#         "geometry"
#       ) |>
#       sf::st_transform(crs = 4326)

#     # project and export
#     sf::st_write(obj = acs2, exportPath2)
#   }
# }
