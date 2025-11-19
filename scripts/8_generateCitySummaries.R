# 1. Load the necessary library
pacman::p_load(stringr, rmarkdown, readr)

# 2. Prepare the data you want to pass as parameters
cityHealth <- read_csv(
  "~/trueNAS/work/justGreen/data/products/healthMeasures/allCities_2023_morDemStroke_with10percentAdjust.csv"
)

# test
i <- grep(pattern = "Louisville city", cityHealth$city)


overwrite <- TRUE
# 3. Call the render() function to create the report
for (i in 177:nrow(cityHealth)) {
  # error with "Bridgeport city" 18, "Washington city" 32, "Augusta-Richmond County consolidated government",
  # ?? "Fort Wayne city" "Indianapolis city",
  # "Louisville-Jefferson County metro government","Nashville-Davidson metropolitan government"
  # these are all based on variablity in the city name between features 
  target_city_name <- cityHealth$city[i]

  export <- paste0(
    "~/trueNAS/work/justGreen/data/products/citySummaries/Report-",
    target_city_name,
    ".html"
  )
  if (!file.exists(export) || overwrite == TRUE) {
    rmarkdown::render(
      input = "scripts/CitySummary.Rmd", # The Rmd file to render
      output_file = export, # A dynamic name for the output
      params = list(
        cityName = target_city_name,
        state = cityHealth$state[i]
      )
    )
  }

  print(paste("Report for", target_city_name, "has been generated!"))
}
