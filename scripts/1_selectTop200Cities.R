

pacman::p_load(sf, readr, dplyr, tigris)

# read in places data 
files <- list.files(path = "data/raw/esri",
                    pattern = ".gpkg",
                    full.names = TRUE)
d1 <- sf::st_read(files[1])
i1 <- sf::st_read(files[2])
# bind to single features 
places <- dplyr::bind_rows(d1, i1)|>
  top_n(200, P0010001) |>
  select("OBJECTID","PLACENS","GEOID","CLASSFP","NAME","State",
         "totalPopulation" = "P0150001",
         "pop18andOlder" = "P0150003") # where is this used? 
# remove all "/" characters from city names 
places$NAME <- stringr::str_replace(string = places$NAME, pattern = "/", replacement = "-")
# remove all " (balance)" characters from city names as this is causing some odd issues with grepl indexing 
places$NAME <- stringr::str_replace(string = places$NAME, pattern = fixed(" (balance)"), replacement = "")

# attribute counties to the cities 
countiesExport <- "data/raw/counties/counties.gpkg"
if(!file.exists(countiesExport)){
  counties <- tigris::counties()|>
    st_transform(crs = st_crs(places))
  st_write(obj = counties, dsn = countiesExport)
}else{
  counties <- st_read(countiesExport)
}
place <- places[80,]
assignCounty <- function(place, counties){
  state <- stringr::str_sub(place$GEOID, start = 1, end = 2)
  #filter counties 
  c1 <- counties[counties$STATEFP == state, ]
  # issues with boarder connections 
  # crop then intersect - 
  p1 <- sf::st_crop(c1, place) 
  if(nrow(p1)== 1){
    geoid <- p1$GEOID
  }else{
    qtm(p1)
    print(p1$NAME)
    # run an intersection 
    p1 <- st_intersection(p1, st_make_valid(place))
    p1$area <- st_area(p1)
    p1 <- p1[p1$area == max(p1$area),]
    geoid <- p1$GEOID
  }
  return(geoid)
}
for(i in 1:nrow(places)){
  print(i)
  val <- assignCounty(place = places[i,], counties = counties)
  if( i == 1){
    geoid <- val
  }else{
    geoid <- c(geoid,val)
  }
}
# assign count GEOID 
places$countyGEOID <- geoid

# need to filter to total pop 18 and older 
# P0150002 - pop under 18 
# P0150003 -  Population in households 18 years and over

# export 
sf::st_write(places, "data/processed/top200/top200Cities.gpkg", delete_layer = TRUE)
sf::st_write(places, "data/processed/top200/top200Cities.shp", delete_layer = TRUE)
write_csv(x = st_drop_geometry(places), "data/processed/top200/top200Cities.csv")
sf::st_write(places, "data/processed/top200/top200Cities.gpkg", delete_layer = TRUE)
