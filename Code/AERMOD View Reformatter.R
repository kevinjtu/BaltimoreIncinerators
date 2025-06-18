library(sf)
library(dplyr)
library(readr)
library(tidygeocoder)
library(stringr)

# Input and output folder paths
input_folder <- "/Users/kevintu/Library/CloudStorage/OneDrive-UniversityofMarylandSchoolofMedicine/Climate Health Internship/AERMOD View Modeling Results"
output_folder <- "/Users/kevintu/Library/CloudStorage/OneDrive-UniversityofMarylandSchoolofMedicine/Climate Health Internship/AERMOD View Modeling Results/BenMAP Reformatted"

# List all CSV files
csv_files <- list.files(input_folder, pattern = "\\.csv$", full.names = TRUE)

# UTM zone – adjust if needed
utm_crs <- 32618  # EPSG:32618 for UTM Zone 18N (East Coast US)

# Function to process one file
process_file <- function(file_path, monitor_start_id = 1) {
  df <- read_csv(file_path, show_col_types = FALSE) %>%
    select(where(~ !all(is.na(.))))  # Remove empty columns
  
  # Convert UTM to lat/long
  sf_pts <- st_as_sf(df, coords = c("X", "Y"), crs = utm_crs)
  sf_pts <- st_transform(sf_pts, 4326)
  coords <- st_coordinates(sf_pts)
  df$Longitude <- coords[, 1]
  df$Latitude <- coords[, 2]
  
  # Reverse geocode (optional: comment out if you want to skip for speed)
  locs <- tryCatch(
    reverse_geocode(data = df, lat = Latitude, long = Longitude, method = "osm", full_results = FALSE),
    error = function(e) tibble(address = rep("Unknown", nrow(df)))
  )
  
  df$`Monitor Name` <- sprintf("M%03d", monitor_start_id:(monitor_start_id + nrow(df) - 1))
  df$`Monitor Description` <- locs$address
  df$Metric <- "D24HourMean"
  df$`Seasonal Metric` <- "Annual"
  df$Statistic <- "Mean"
  df$Values <- df$`Concentration (AVERAGE CONC) [ug/m^3]`
  
  output_df <- df %>%
    select(`Monitor Name`, `Monitor Description`, Latitude, Longitude,
           Metric, `Seasonal Metric`, Statistic, Values)
  
  return(output_df)
}

# Process all files and save
for (i in seq_along(csv_files)) {
  input_file <- csv_files[i]
  result <- process_file(input_file, monitor_start_id = (i - 1) * 100 + 1)
  
  # Generate output file name
  output_name <- paste0("reformatted_", tools::file_path_sans_ext(basename(input_file)), ".csv")
  output_path <- file.path(output_folder, output_name)
  
  # Save result
  write_csv(result, output_path)
  message("Saved: ", output_path)
}
