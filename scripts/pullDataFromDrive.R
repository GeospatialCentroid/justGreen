# pull gee data from drive 

# Install and load necessary packages
pacman::p_load(googledrive, fs)

# 1. Authenticate with Google Drive
# drive_auth()

# 2. Specify the Google Drive folder ID or file name containing the TIFF files
# Replace 'your_folder_id' or 'your_file_name' with the actual ID or name.
# It is best to use the folder ID.
folder_id <- "Earth Engine" # Example: "1abc2def3ghi4jkl5mno6pqr"

# 3. List the TIFF files in the specified folder
files <- googledrive::drive_ls(path = "Earth Engine", recursive = TRUE)

#Alternative if you only know the file name.
#files <- drive_find(pattern = "your_file_name.tif", type = "tiff")

# 4. Specify the local directory where you want to download the files
local_dir <- "data/processed/ndvi" # Replace with your local directory path
# Create the directory if it doesn't exist
dir_create(local_dir, recurse = TRUE)

# 5. Download each TIFF file to the local directory
if (nrow(files) > 0) {
  for (i in 1:nrow(files)) {
    file_id <- files$id[i]
    file_name <- files$name[i]
    local_file_path <- file.path(local_dir, file_name)
    
    if(!file.exists(local_file_path)){
      # Download the file
      tryCatch({
        drive_download(as_id(file_id), path = local_file_path, overwrite = TRUE)
        cat(paste("Downloaded:", file_name, "\n"))
        
      }, error = function(e) {
        cat(paste("Error downloading", file_name, ":", e$message, "\n"))
      })
    }
  }
}

