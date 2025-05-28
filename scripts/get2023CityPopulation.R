pacman::p_load(dplyr, tidycensus, sf, tigris, purrr, stringr, readr, tmap, furrr)
tmap::tmap_mode("view")


# problem : we dont want to use 2020 population data for our cities 
# solution : use census block centroid to determine census blocks inside of cities, 
# Use the acs 5 year estimate to get the expected population for each of those blocks 
# summ all those values to get the new population measure for the cities 


# read in city data 
cities <- sf::st_read("data/processed/top200/top200Cities.gpkg")
# grab unique states 
states <- unique(cities$State)

# download census block data 
# tidycensus::census_api_key(key =, overwrite = TRUE, install = TRUE)

# get 2020 data for the block centroid 
## this is the spatial data element 
downloadBlocks <- function(state){
  exportPath <- paste0("data/raw/censusBlocks/", state,".gpkg")
  if(!file.exists(exportPath)){
    print(state)
    t1 <- tigris::blocks(state = state, year = 2020)
    sf::st_write(obj = t1, dsn = exportPath)
  }else{
    print(paste0(state, " downloaded"))
  }
}

# apply download 
# purrr::map(.x = states, .f = downloadBlocks)


# determine the blocks inside of each city 
blocks <- list.files(path = "data/raw/censusBlocks", 
                     full.names = TRUE)


# generate a spatail object that shows all block centroids within a city, includes the block group GEOID
blocksInCities <- function(state, blocks, cities){
  # select cities of interest 
  c2 <- cities[cities$State == state, ]
  # get the data read in 
  f1 <- blocks[grepl(pattern = state, x = blocks)] |> 
    sf::st_read() 
  # grab CRS
  crs <- st_crs(f1)
  # simplify elements 
  f1 <- f1 |>
    st_drop_geometry()|>
    dplyr::select( "GEOID20", "INTPTLAT20", "INTPTLON20")
  # Prep the lat lon columns 
  f2 <- f1 |>
    dplyr::mutate(
        lat = as.numeric(str_remove(INTPTLAT20, fixed("+"))),
        lon = as.numeric(INTPTLON20)
      ) |>
    dplyr::select("GEOID" = GEOID20,
                  lat, lon) |>
    sf::st_as_sf(
      coords = c("lon", "lat"), # Specify lon and lat columns
      crs = crs,               # Set the CRS to WGS 84
      remove = FALSE            # Optional: keep the original lon/lat columns
    )|>
    st_transform(crs = st_crs(c2))
  
  #For each city, test what blocks are present in the city 
  for(i in 1:nrow(c2)){
    # select city 
    c1 <- c2[i, ]
    # export path 
    exportPath <- paste0("data/processed/blocksInCity/",c1$NAME,"_",c1$State,".gpkg")
    if(!file.exists(exportPath)){
      print(c1$NAME)
      # crop points to city 
      crop <- st_crop(x = f2, y = c1)
      # intersect 
      inter <- st_intersects(crop, y = c1, )
      # flip to binary
      flipBinary <- function(element){
        if(length(element) == 1){
          return(TRUE)
        }else{
          return(FALSE)
        }
      }
      # binary selector
      binary <- lapply(X = inter, FUN = flipBinary)|> unlist()
      # points in city 
      p1 <- crop[binary, ] |>
        dplyr::mutate(
          bgGEOID = stringr::str_sub(GEOID, start = 1, end = 12 )
        )
      # export 
      sf::st_write(obj = p1, dsn = exportPath )
    }else{
      print( paste0("data exists",c1$NAME))
    }
  }
}
# generate data 
# purrr::map(.x = states, .f = blocksInCities,
           # blocks = blocks,
           # cities = cities)

# produce a new total population per city 
blocksInCity <- list.files(path = "data/processed/blocksInCity", 
                     full.names = TRUE)
blockGroups <- list.files("data/processed/censusGeographies",
                          full.names = TRUE,
                          pattern = "_bg.gpkg")

generate2023cityPop <- function(state, cities, blocksInCity,blockGroups ){
  # filter out features per state 
  c1 <- cities[cities$State == state, ]
  blocks <- blocksInCity[grepl(pattern = state, x = blocksInCity)]
  bg <- blockGroups[grepl(pattern = state, x = blockGroups)]
  
  # export path 
  exportPath <- paste0("data/processed/top200_2023/",state,".gpkg")
  if(!file.exists(exportPath)){
    print(state)
    for(i in 1:nrow(c1)){
      city <- c1[i, ]
      # select blocks 
      b1 <- blocks[grepl(pattern = paste0("blocksInCity/",city$NAME), blocks)] |> 
        st_read() 
      # filter block groups 
      bg1 <- bg |>
        sf::st_read() |>
        dplyr::filter(GEOID %in% b1$bgGEOID)
      city$popOver20_2023 <- sum(bg1$over18, na.rm = TRUE)
      if(i == 1){
        c2 <- city
      }else{
        c2 <- bind_rows(c2, city)
      }
    }
    #export 
    sf::st_write(obj = c2, dsn = exportPath)
  }else{
    print(paste0("complete", state))
  }
  
}

purrr::map(.x = states, .f = generate2023cityPop,
           cities = cities,
           blocksInCity = blocksInCity,
           blockGroups = blockGroups)


    