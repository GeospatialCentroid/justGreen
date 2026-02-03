# generate fullCity column used in the shiny app 
clean_city_names <- function(data, city_col, state_col) {
  data %>%
    mutate(
      city_clean = {{ city_col }} %>%
        
        # 1. Remove standard suffixes
        str_remove("\\s+(city|CDP|town|municipality|zona urbana)$") %>%
        
        # 2. Fix "Urban Honolulu" specifically
        str_replace("^Urban Honolulu.*", "Honolulu") %>%
        
        # 3. Fix Consolidated City-Counties
        str_replace("^Nashville-Davidson.*", "Nashville") %>%
        str_replace("^Louisville-Jefferson.*", "Louisville") %>%
        str_replace("^Lexington-Fayette.*", "Lexington") %>%
        str_replace("^Augusta-Richmond.*", "Augusta") %>%
        str_replace("^Macon-Bibb.*", "Macon") %>%
        str_replace("^Athens-Clarke.*", "Athens")
    ) |>
    dplyr::mutate(
      fullCity = paste0(city_clean, ", ", {{ state_col }})
    )
}