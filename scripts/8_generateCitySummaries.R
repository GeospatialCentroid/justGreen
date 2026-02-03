# 1. Load the necessary library
pacman::p_load(stringr, rmarkdown, readr,dplyr)

# source city names funciton 
source("functions/cleanCityNames.R")

# 2. Prepare the data you want to pass as parameters
cityHealth2 <- read_csv(
  "~/trueNAS/work/justGreen/data/products/healthMeasures/allCities_2023_morDemStroke_with10percentAdjust.csv"
) |> 
  clean_city_names(city_col = city, state_col = state)

overwrite <- TRUE
# 3. Call the render() function to create the report
for (i in 1:10 ) { # 
  #error 
  # 20 : "Springfield city"
  # 26 : "Glendale city"
  # 61 : "Columbus city"
  # 62 : "Kansas City city"
  # 86 : "Glendale city"
  # 108 : "Aurora city"
  # 125 : "Aurora city" ? 
  # 144 : "Kansas City city"
  # 145 : "Springfield city"
  # 164 : "Columbus city"
  # these are all based on variablity in the city name between features 
  target_city_name <- cityHealth2$city[i]
  export_name <- cityHealth2$fullCity[i]
  export <- paste0(
    "~/trueNAS/work/justGreen/data/products/citySummaries/Report-",
    export_name,
    ".html"
  )
  # if (!file.exists(export) || overwrite == TRUE) {
    rmarkdown::render(
      input = "scripts/citySummary_3.Rmd", # The Rmd file to render
      output_file = export, # A dynamic name for the output
      params = list(
        cityName = target_city_name,
        state = cityHealth2$state[i]
      )
    )
  # }

  print(paste("Report for", target_city_name, "has been generated!"))
}
