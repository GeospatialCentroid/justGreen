sf_use_s2(use_s2 = TRUE)
# buffered city 
t1 <- sf::st_read("data/processed/top200_2023/Washington.gpkg") |>
  dplyr::filter(NAME == "Tacoma city") 
t2< - t1 |>
  sf::st_buffer(dist = 5000 )

# NDVI per city 
r1 <- terra::rast("data/processed/ndvi/5370000_Tacoma city_2023NDVI_buffered10k_2.tif")
# NDVI per city - no water
r2 <- terra::rast("data/processed/ndvi_noWater/5370000_Tacoma city_2023NDVI_buffered10k_2.tif")

terra::plot(r2)
# buffer city 
c1 <- terra::vect(t1) |>
  terra::buffer(500)
terra::plot(r2)
terra::plot(c1, 
            add = TRUE,
            border = 'red',     # Sets the outline color
            col = NA,           # Sets the fill to NA (transparent)
            lwd = 3  )

# census 
ct <-terra::vect("data/processed/censusGeographies/Washington_ct.gpkg") |>
  terra::crop(t1)
r3 <- terra::crop(r2, ct)
ct1 <- terra::buffer(ct[15, ], 500)

terra::plot(r3)
terra::plot(ct,
            add = TRUE,
            border = '#B216F5',     # Sets the outline color
            col = NA,           # Sets the fill to NA (transparent)
            lwd = 1 )
terra::plot(ct1, 
            add = TRUE,
            border = '#F52117',     # Sets the outline color
            col = NA,           # Sets the fill to NA (transparent)
            lwd = 3  )
