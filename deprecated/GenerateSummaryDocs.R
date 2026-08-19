pacman::p_load(dplyr, readr, sf, terra)


# Generate input for the rmd 

# city name 
# state 
# current NDVI of city 
# NDVI Ranking of city 
# Population of the city 
## total, over 20, over 35, over 55

# Gauge chart from the shiny app 

# health benifits 
## premature deaths prevented 
## stroke prevented 
## dementia prevented 

# Potential for improvement 
## current ndvi, mortaility, stroke, demetia 
## nvdi +10% , mortaility, stroke, demetia

# map of the census track areas in the city 
# include a population layer 

# environmental justice 
## population group average NDVI access health disparity gap 
## quintile ~ not sure what this is. 

# map 2 
## priority to equity - locaiton with the greatest health needs and lowerst green spaces 



ct <- read_csv("~/trueNAS/work/justGreen/data/products/healthMeasures/allCT_2023_morDemStroke_with10percentAdjust_svi.csv")
