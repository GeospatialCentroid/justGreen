pacman::p_load(tidycensus, dplyr, sf)


# Set your Census API key (replace "YOUR_API_KEY" with your actual key)
# 
# Get all census block groups for the United States
states <- sf::st_read("data/processed/top200/top200Cities.gpkg") |>
  dplyr::select(State) |>
  sf::st_drop_geometry()|>
  dplyr::pull()|>
  unique()

age_variables <- c(
  "B01001_007E", # Male: 18 and 19 years
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
  "B01001_031E", # Female: 18 and 19 years
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
  "B01001_045E"  # Female: 85 years and over
)


# iterate over each states and download ACS geomentries are  
for(i in seq_along(states)){
  # state 
  s1 <- states[i]
  # export path 
  exportPath <- paste0("data/processed/censusGeographies/",s1,"_ct.gpkg")
  if(!file.exists(exportPath)){
    # pull acs 
    acs <- get_acs(
      geography = "tract",
      variables = age_variables, # Total Population (any variable works for geography)
      year = 2020, # Or the desired year
      state = s1,
      geometry = TRUE,
      output = "wide" #output wide data for easier manipulation of the geometry.
    )
    # thin to over 18 pop 
    acs2 <- acs |>
      dplyr::mutate(
        over18 = rowSums(across(age_variables))
      )|>
      dplyr::select(
        "GEOID","NAME","over18","geometry"
      ) |>
      sf::st_transform(x = acs, crs = 4326)
    
    # project and export 
    sf::st_write(obj = acs2, exportPath)
  }
}

