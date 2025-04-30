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
### reference from paper 
# .1 == 4% reduction 

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
selectedCity <- city[25, ]
state <- selectedCity$State
cityName <- selectedCity$NAME
# grap the state level census data 
stateCensusTracts <- pop_ct[grepl(pattern = state, x = pop_ct)] |>
  st_read()
# census tracks that touch the city boundary 
touchingCT <- st_crop(stateCensusTracts, selectedCity) |>
  as.data.frame() |>
  dplyr::select(GEOID)|>
  dplyr::pull()
# ct of interest 
## all cts intersecting with the city boundaries
selCT <- stateCensusTracts[stateCensusTracts$GEOID %in% touchingCT,]
selCT$area <- st_area(selCT) |> round() |> as.numeric()
selCT$area50 <- round(selCT$area * 0.5) 
selCT$area10 <- round(selCT$area * 0.1) 
# census tracks masked to the city 
### clip area of census tracts to the city bounaries 
cityCTs <- st_intersection(selCT, selectedCity) 
cityCTs$area <- st_area(cityCTs) |> round() |> as.numeric()

# use the clip area to filter the select CT 
selCT <- selCT[selCT$GEOID %in% cityCTs$GEOID, ]

# select all census tracts that are fully inside of the 
## same area between features -- 100% of census track inside city boundaries
ct_contained <- cityCTs[cityCTs$area == selCT$area,]
ct_contained50 <- cityCTs[cityCTs$area >= selCT$area50,]
ct_contained10 <- cityCTs[cityCTs$area >= selCT$area10,]
# going to be a little tricky working out the cut off of what's present in the city or not
tm_shape(selectedCity, name = "City")+
  tm_polygons(col = "blue", alpha = 1, group = "city")+
  # 10% contained 
  tm_shape(ct_contained, name = "Tract 100% inside city")+
  tm_polygons(col = "white",  alpha = 0.8, , group = "100% tracts")+
  # 50% contained 
  tm_shape(ct_contained50, name = "Tract > 50% inside city")+
  tm_polygons(col = "white",  alpha = 0.8, , group = "50% tracts")+
  # 10% contained 
  tm_shape(ct_contained10, name = "Tract > 10% inside city")+
  tm_polygons(col = "white",  alpha = 0.8, , group = "10% tracts")+
  # all tracts considers 
  tm_shape(selCT, name = "All nearby tracts")+
  tm_polygons(alpha = 0.6, , group = "All Census Tracts")+
  tm_layout(title = paste0("City of ", cityName))


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
