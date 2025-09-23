# 1. Load the necessary library
library(rmarkdown)

# 2. Prepare the data you want to pass as parameters
cityHealth <-read_csv("~/trueNAS/work/justGreen/data/products/healthMeasures/allCities_2023_morDemStroke_with10percentAdjust.csv") 

# 3. Call the render() function to create the report
for(i in 19:nrow(cityHealth)){
  # error with "Bridgeport city"
  target_city_name <- cityHealth$city[i]
  
  
  rmarkdown::render(
    input = "scripts/CitySummary.Rmd",  # The Rmd file to render
    output_file = paste0("~/trueNAS/work/justGreen/data/products/citySummaries/Report-", target_city_name, ".html"), # A dynamic name for the output
    params = list(
      cityName = target_city_name,
      state = cityHealth$state[i]
    )
  )
  
  print(paste("Report for", target_city_name, "has been generated!"))
}
