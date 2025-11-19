# running some file copying to move resources into correct place for the shiny repo 
pacman::p_load(readr)

# City level health metrics 
cityHealth <- "data/products/healthMeasures/allCities_2023_morDemStroke_with10percentAdjust.csv"
t1 <- read_csv(cityHealth)

# census tract level health metrics 
censusTractHealth <- "data/products/healthMeasures/allCT_2023_morDemStroke_with10percentAdjust_svi.csv"
t2 <- read_csv(censusTractHealth)
# a

