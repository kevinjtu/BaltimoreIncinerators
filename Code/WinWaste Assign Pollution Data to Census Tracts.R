# R Script: Assign Pollution Data to Census Tracts (Batch Processing)####
#
# Description: This script automates the process of assigning pollution data to
# census tracts for multiple input files. It iterates through all files ending
# in "Annual.csv" within a specified directory.
#
# Logic for each file:
# 1. Extracts year and pollutant name from the filename.
# 2. Reads the pollution data (latitude, longitude, concentration).
# 3. For census tracts with points, it averages the concentration values.
# 4. For empty tracts, it uses the concentration from the nearest point.
# 5. Combines all results into a single file for export.
#
# Packages Required: sf, dplyr, tigris, readr, stringr
# You can install them with: install.packages(c("sf", "dplyr", "tigris", "readr", "stringr"))
# ---

# 1. SETUP: Load libraries and define file paths####
# Load necessary libraries
library(sf)
library(dplyr)
library(tigris)
library(readr)
library(stringr)

# Set the directory where your pollution data files are located.
input_directory <- "/Users/kevintu/Library/CloudStorage/OneDrive-UniversityofMarylandSchoolofMedicine/Climate Health Internship/AERMOD View Modeling Results/BenMAP Reformatted/WinWaste"

# Set the desired path and name for the final output file.
output_file_path <- file.path(dirname(input_directory), "WinWaste Concentrations by Census Tract.csv")

# 2. PREPARATION: Download and prepare census tract data####
# This section runs only once to improve efficiency.

# Download census tract boundaries for Maryland and Washington D.C.
tracts_sf_md <- tracts(state = c("MD"), cb = TRUE)
tracts_sf_dc <- tracts(state = c("DC"), cb = TRUE)
tracts_sf_raw <- rbind(tracts_sf_md, tracts_sf_dc)

# Define the target projected CRS from the downloaded tract data for accurate
# distance calculations.
target_crs <- st_crs(tracts_sf_raw)

# 3. BATCH PROCESSING: Loop through each specified data file####
# Get a list of all files in the directory that end with "Annual.csv"
files_to_process <- list.files(
  path = input_directory,
  pattern = "Annual\\.csv$",
  full.names = TRUE
)

# Initialize an empty list to store the results from each file
all_results_list <- list()

# Loop over each file path
for (file_path in files_to_process) {
  message("Processing file: ", basename(file_path))
  
  # --- Data Ingestion and Cleaning ---
  file_name <- basename(file_path)
  year <- str_extract(file_name, "\\d{4}")
  # Extract pollutant name robustly between year and "Annual"
  pollutant <- str_match(file_name, paste0(year, "\\s(.*?)\\sAnnual\\.csv"))[,2]
  
  pollution_df <- read_csv(file_path, show_col_types = FALSE) %>%
    select(latitude = Latitude, longitude = Longitude, Conc = Values) %>%
    mutate(
      pollutant = pollutant,
      year = as.integer(year)
    )
  
  # Convert to a projected sf object
  pollution_sf_proj <- st_as_sf(pollution_df,
                                coords = c("longitude", "latitude"),
                                crs = 4326) %>%
    st_transform(crs = target_crs)
  
  # --- Spatial Analysis ---
  points_in_tracts <- st_join(pollution_sf_proj, tracts_sf_raw)
  
  tracts_with_data <- points_in_tracts %>%
    st_drop_geometry() %>%
    filter(!is.na(GEOID)) %>%
    group_by(GEOID) %>%
    summarise(Avg_Conc = mean(Conc, na.rm = TRUE), .groups = 'drop')
  
  # --- Handle Empty Tracts ---
  empty_tracts_sf <- tracts_sf_raw %>%
    filter(!GEOID %in% tracts_with_data$GEOID)
  
  # Check if there are any empty tracts to process
  if (nrow(empty_tracts_sf) > 0) {
    nearest_point_indices <- st_nearest_feature(empty_tracts_sf, pollution_sf_proj)
    empty_tracts_sf$Nearest_Conc <- pollution_sf_proj$Conc[nearest_point_indices]
    
    final_tracts_from_nearest <- empty_tracts_sf %>%
      rename(Final_Conc = Nearest_Conc) %>%
      mutate(Source = "Nearest Neighbor")
  } else {
    final_tracts_from_nearest <- NULL # Create an empty object if no empty tracts
  }
  
  
  # --- Assemble Results for Current File ---
  final_tracts_with_data <- tracts_sf_raw %>%
    filter(GEOID %in% tracts_with_data$GEOID) %>%
    left_join(tracts_with_data, by = "GEOID") %>%
    rename(Final_Conc = Avg_Conc) %>%
    mutate(Source = "Averaged within Tract")
  
  # Combine results and add metadata
  combined_results_for_file <- bind_rows(final_tracts_with_data, final_tracts_from_nearest) %>%
    mutate(
      pollutant = pollutant,
      year = year
    )
  
  # Add the processed data to our main list
  all_results_list[[file_path]] <- combined_results_for_file
}

# 4. FINAL EXPORT: Combine all results and write to a single file####
# Combine all the data frames from the list into a single data frame
final_combined_data <- bind_rows(all_results_list)

# Select and reorder columns for clarity in the final output
final_combined_data_for_export <- final_combined_data %>%
  st_drop_geometry() %>% # Drop spatial geometry for CSV export
  select(
    GEOID,
    TRACTCE,
    NAME,
    NAMELSADCO,
    STATEFP,
    pollutant,
    year,
    Final_Conc,
    Source
  )

# Write the final combined data to a CSV file
write_csv(final_combined_data_for_export, output_file_path)

message("Processing complete.")
message("Output saved to: ", output_file_path)
