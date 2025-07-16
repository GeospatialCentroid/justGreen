##
# preprocess some natural earth data used to remove water ways 
## 

pacman::p_load(dplyr, tigris, sf, tmap)
tmap_mode("view")
sf::sf_use_s2(FALSE)
# grab us area 
# Get state boundaries (e.g., all states)
us <- states() |> 
  st_transform(crs = 4326)|>
  st_make_valid()

#list files 
files <- list.files(
  path = "data/raw/naturalEarth",
  pattern = ".shp",
  full.names = TRUE
)

for( i in seq_along(files)){
  # grab name for export 
  name <- basename(files[i]) |> tools::file_path_sans_ext()
  # test for presence 
  exportPath <- paste0("data/processed/naturalEarthData/", name, ".gpkg")
  if(!file.exists(exportPath)){
    # read in data 
    d1 <- st_read(files[i]) |>
      sf::st_make_valid()|>
      sf::st_crop(us)
    # export 
    st_write(d1, exportPath, delete_layer = TRUE)
  }
}
