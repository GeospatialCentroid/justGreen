###
# developing the workflow for review before functionalizing the process
### 

pacman::p_load(dplyr,sf, tidyr, tmap)
tmap_mode("view")

# inputs 
city <- st_read("data/processed/top200/top200Cities.gpkg")
# census tracks by state 
pop_ct <- list.files("data/processed/censusGeographies",
                     pattern = ".gpkg",
                     full.names = TRUE)
# ndvi per census tract by city 
ndvi_ct <- list.files("data/processed/summaryNDVI",
                      pattern = ".gpkg",
                      full.names = TRUE)
# remove non buffered features 
# noBuff <- ndvi_ct[!grepl(pattern = "_buffer", x = ndvi_ct)]
# lapply(X = noBuff,FUN = file.remove)

# parameters  -------------------------------------------------------------
baseNDVI <- 0.1 
doseResponse <- 0.146
relativeRisk <- 0.961 
populationAttributableFraction <- function(ndviVal, rr, doseResponse){
  paf <- 1 - (1/rr ^ ( ndviVal/doseResponse))
  return(paf)
} 
crudeDeathPrevented <- function(population, mortalityRate, paf){
  cdp <- population * mortalityRate * paf * -1
  return(cdp)
}


# quick test --------------------------------------------------------------
population <- 100000
mortalityRate <- 792.2/100000

for(i in seq(0, 0.8, by =0.1)){
  print(i)
  paf <- populationAttributableFraction(
    ndviVal = i,
    rr = relativeRisk,
    doseResponse = doseResponse
  )
  deathPresented <- crudeDeathPrevented(
    population = population,
    mortalityRate = mortalityRate,
    paf = paf
  )
  print(deathPresented)
}


# Test on specific city  --------------------------------------------------
c1 <- city[1, ]
# grap the state level census data 
ct1 <- pop_ct[grepl(pattern = c1$State, x = pop_ct)] |>
  st_read()
# census tracks that touch the city 
ct2 <- st_crop(ct1, c1) 
ct2$area <- st_area(ct2)|> round()
# use geoid from crop to select all ct of interest 
ct <- ct1[ct1$GEOID %in% ct2$GEOID, ]
ct$area <- st_area(ct) |> round()
# select all census tracts that are fully inside of the 
ct_contained <- ct[ct$area == ct2$area,]

# going to be a little tricky working out the cut off of what's present in the city or not
tm_shape(c1, name = "City")+
  tm_polygons(col = "blue", alpha = 1)+
  tm_shape(ct_contained, name = "Tract inside city")+
  tm_polygons(col = "white",  alpha = 0.8)+
  tm_shape(ct, name = "All nearby tracts")+
  tm_polygons(alpha = 0.6)
  
# read in the NDVI layer 
ndvi <- ndvi_ct[grepl(c1$GEOID, ndvi_ct)] |> 
  st_read()

# filter based on the locations contained within the city 
ndviSelect <- ndvi[ndvi$GEOID %in% ct_contained$GEOID, ]

# join the datasets 
ndviSelect$over18 <- ct_contained$over18

#run the calculations 
# 
# ndviSelect$paf <- lapply(X = ndviSelect$mean_ndvi, FUN = populationAttributableFraction,
#               rr = relativeRisk,
#               doseResponse = doseResponse) |> unlist()
output <- ndviSelect |>
  dplyr::mutate(
    ndvi_diff = mean_ndvi - 0.1, 
    paf = populationAttributableFraction(ndvi_diff,
                                         rr = relativeRisk,
                                         doseResponse = doseResponse),
    livesSaved = crudeDeathPrevented(population = over18,
                                     mortalityRate = 800/100000,
                                     paf = paf)
  )|>
  dplyr::select("GEOID","NAME","NAME_city","State","over18","paf","livesSaved" )

write.csv(st_drop_geometry(output), file = "temp/initialExample.csv")
