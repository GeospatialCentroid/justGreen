pacman::p_load(tidycensus, dplyr, sf)


# Set your Census API key (replace "YOUR_API_KEY" with your actual key)
# 
# Get all census block groups for the United States
states <- sf::st_read("data/processed/top200/top200Cities.gpkg") |>
  dplyr::select(State) |>
  sf::st_drop_geometry()|>
  dplyr::pull()|>
  unique()

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
      variables = "B01003_001", # Total Population (any variable works for geography)
      year = 2020, # Or the desired year
      state = s1,
      geometry = TRUE,
      output = "wide" #output wide data for easier manipulation of the geometry.
    )
    # project and export 
    acs2 <- sf::st_transform(x = acs, crs = 4326)
    sf::st_write(obj = acs2, exportPath)
  }
}

