# justGreen
Code base for the JustGreen shiny app. This app aims to showcase the relationship between NDVI distributions within a city and health outcomes.


## general notes on methods 

Overall Goal : For the 200 most populated cities within the United States, showcase NDVI values, and preprocess 
a quantitative assessment of the relationship between NDVI values and health metrics 

### Data sources 

Population data: https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/USA_Census_2020_DHC_Population_Households_Place/FeatureServer 

NDVI : Generate from Landsat or Sentinel-2 data

Census tracts : US Census 


## workflow summary 
- gather max yearly NDVI values from GEE using 1k buffer geographies of city 
- Get the mean and sd of each census tract, buffered to 500m that intersects with the city 
- For all census tracts gather the total population 18 years old and older 
- NDVI null value is considered 0.1 
- For each census tract 
  - calculated the difference between the NDVI null value and average value for that area 
  - Apply the dose reponse function to relate current NDVI values to health improvements 
    - dose function from Garber et al 2024. 0.146 increase in NDVI results in 4.5% change in non-accidental mortality (decrease)
  - convert this to a attributiable fraction (define) 
    - Garber et al 2024 : PAF =  1 - (1 / (RR^(NDVIdiff/0.146))). 
    - RR -; 0.954 
  - use the relationship between mortality x/100,000 people, census tract population, and attributable fraction 
    - garber et al 2024 : crude number of deaths prevented = population * crude mortality rate * PAF * -1 
  -result; number of deaths prevented by NDVI exposure 
  
  

  