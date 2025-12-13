
# IMPORTANT: Adjust 'data_folder' if your files are not in the current working directory.
# Since you mentioned a folder named "Oldtown NO2 Air Quality Sensor 1990 - 2021",
# you should set the path accordingly.
setwd("/Users/kevintu/Library/CloudStorage/OneDrive-UniversityofMarylandSchoolofMedicine/Climate Health Internship/Air Quality Monitoring")

# R Script for Plotting Annual Air Quality Trends Across Multiple Locations
#
# This script performs the following steps:
# 1. Installs/loads necessary packages (tidyverse).
# 2. Defines a list of sensor locations and their parameters.
# 3. Uses a function to read files, filter the data, and return a clean data frame.
# 4. Combines the four PM sensor data sets and plots them on a single graph for comparison.
# 5. Plots the single NO2 sensor data set separately.

# --- 1. Load Libraries ---
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
if (!requireNamespace("scales", quietly = TRUE)) {
  install.packages("scales") # Needed for custom Y-axis number formatting
}
library(tidyverse)
library(scales)

# --- 2. Define Locations and Parameters ---

# Define all locations, their folder names, the parameter to filter, and the units.
locations <- list(
  Oldtown = list(
    folder = "Oldtown NO2 Air Quality Sensor 1990 - 2021",
    parameter = "Nitrogen dioxide (NO2)",
    units = "Parts per billion" # ppb
  ),
  Westport = list(
    folder = "Westport PM Air Quality Sensor 1999-2005",
    parameter = "PM2.5 - Local Conditions",
    units = "Micrograms/cubic meter (LC)" # ug/m^3
  ),
  FMC = list(
    folder = "FMC PM Air Quality Sensor 1999-2008",
    parameter = "PM2.5 - Local Conditions",
    units = "Micrograms/cubic meter (LC)" # ug/m^3
  ),
  Riviera = list(
    folder = "Riviera PM Air Quality Sensor 1999-2005",
    parameter = "PM2.5 - Local Conditions",
    units = "Micrograms/cubic meter (LC)" # ug/m^3
  ),
  `Glen Burnie` = list( # Use backticks for names with spaces
    folder = "Glen Burnie PM Air Quality Sensor 1999-2005",
    parameter = "PM2.5 - Local Conditions",
    units = "Micrograms/cubic meter (LC)" # ug/m^3
  )
)

# --- 3. Define Function to Read and Clean Data ---

# This function loads the data for a single location, filters it, and returns the data tibble.
read_and_clean_data <- function(location_name, folder_path, param_name) {
  
  # Check if the folder exists
  if (!dir.exists(folder_path)) {
    cat(paste0("Warning: Folder '", folder_path, "' not found. Skipping ", location_name, ".\n"))
    return(NULL)
  }
  
  # Find all relevant CSV files in the folder
  all_files <- list.files(
    path = folder_path,
    pattern = "^annual_.*\\.csv$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  if (length(all_files) == 0) {
    cat(paste0("Warning: No 'annual_*.csv' files found in '", folder_path, "'. Skipping ", location_name, ".\n"))
    return(NULL)
  }
  
  # Read all files into a single dataframe
  raw_data <- map_df(all_files, ~read_csv(
    .x,
    col_types = cols_only(
      `Year` = col_integer(),
      `Parameter Name` = col_character(),
      `Arithmetic Mean` = col_double(),
      `Arithmetic Standard Dev` = col_double()
    )
  ))
  
  # Filter and clean data
  processed_data <- raw_data %>%
    filter(`Parameter Name` == param_name) %>%
    rename(
      Year = `Year`,
      Mean_Concentration = `Arithmetic Mean`,
      Std_Dev = `Arithmetic Standard Dev`
    ) %>%
    distinct(Year, .keep_all = TRUE)
  
  
  if (nrow(processed_data) == 0) {
    cat(paste0("Warning: No '", param_name, "' data found for ", location_name, ".\n"))
    return(NULL)
  }
  
  cat(paste0("Data points loaded for: ", location_name, " (", nrow(processed_data), " years)\n"))
  
  return(processed_data)
}

# --- 4. Main Execution ---

all_cleaned_data <- list()
for (loc_name in names(locations)) {
  loc_details <- locations[[loc_name]]
  
  # Read and clean the data for the current location
  data_result <- read_and_clean_data(
    location_name = loc_name,
    folder_path = loc_details$folder,
    param_name = loc_details$parameter
  )
  
  if (!is.null(data_result)) {
    # Add location and parameter info and store in the list
    data_result <- data_result %>% 
      mutate(Location = loc_name, Parameter = loc_details$parameter, Units = loc_details$units)
    all_cleaned_data[[loc_name]] <- data_result
  }
}

# Combine all data into one tibble
final_data <- bind_rows(all_cleaned_data)

# --- 5. Generate Plots ---

if (nrow(final_data) == 0) {
  stop("No valid air quality data was loaded from any folder.")
}

# 5a. Split Data: PM2.5 (for combined plot) and NO2 (for single plot)
pm_data <- final_data %>% filter(Parameter == "PM2.5 - Local Conditions")
no2_data <- final_data %>% filter(Parameter == "Nitrogen dioxide (NO2)")


# --- Combined PM2.5 Plot ---
if (nrow(pm_data) > 0) {
  
  # Extract common units for the Y-axis label
  pm_units <- unique(pm_data$Units)[1]
  pm_y_axis_label <- expression(paste("Average PM2.5 Concentration (", mu, "g/", m^3, ")"))
  
  # Define custom color palette for the four PM locations
  pm_colors <- c(
    "Westport" = "#1B9E77",
    "FMC" = "#D95F02",
    "Riviera" = "#7570B3",
    "Glen Burnie" = "#E7298A"
  )
  
  combined_pm_plot <- ggplot(pm_data, aes(x = Year, y = Mean_Concentration, group = Location, color = Location)) +
    
    # Error bars for each location
    geom_errorbar(
      aes(ymin = Mean_Concentration - Std_Dev, ymax = Mean_Concentration + Std_Dev),
      width = 0.2,
      alpha = 0.5
    ) +
    
    # Line connecting the mean concentration points for each location
    geom_line(linewidth = 1) +
    
    # Mean concentration points
    geom_point(
      size = 3,
      shape = 21,
      fill = "white",
      stroke = 1.2
    ) +
    
    scale_color_manual(values = pm_colors) +
    
    # Customize Axis Ticks and Labels
    scale_x_continuous(
      name = "Year",
      breaks = unique(final_data$Year) # Show a tick mark for every year present
    ) +
    scale_y_continuous(
      name = pm_y_axis_label,
      labels = scales::number_format(accuracy = 0.1)
    ) +
    
    # Add Title and Subtitle
    labs(
      title = "Annual Average PM2.5 Concentration",
      subtitle = "Mean concentration with standard deviation error bars across four locations."
    ) +
    
    # Apply a clean theme
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray30"),
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.line = element_line(color = "gray50")
    )
  
  print(combined_pm_plot)
} else {
  cat("\nNo PM2.5 data was loaded for the combined plot.\n")
}


# --- Individual NO2 Plot ---
if (nrow(no2_data) > 0) {
  
  # Define NO2 plot details
  no2_y_axis_label <- expression(paste("Average NO2 Concentration (ppb)"))
  line_color <- "#0072B2" # Blue for the trend line
  point_color <- "#D55E00" # Orange for the points
  
  no2_single_plot <- ggplot(no2_data, aes(x = Year, y = Mean_Concentration)) +
    
    # Error bars: Mean +/- one standard deviation
    geom_errorbar(
      aes(ymin = Mean_Concentration - Std_Dev, ymax = Mean_Concentration + Std_Dev),
      width = 0.2, 
      color = "gray60",
      linewidth = 0.5
    ) +
    
    # Line connecting the mean concentration points
    geom_line(
      color = line_color,
      linewidth = 1
    ) +
    
    # Mean concentration points
    geom_point(
      color = point_color,
      size = 3,
      shape = 21,
      fill = "white",
      stroke = 1.2
    ) +
    
    # Customize Axis Ticks and Labels
    scale_x_continuous(
      name = "Year",
      breaks = unique(no2_data$Year)
    ) +
    scale_y_continuous(
      name = no2_y_axis_label,
      labels = scales::number_format(accuracy = 0.1)
    ) +
    
    # Add Title and Subtitle
    labs(
      title = "Annual NO2 Air Quality Trend (Oldtown Sensor)",
      subtitle = "Mean concentration with standard deviation error bars."
    ) +
    
    # Apply a clean theme
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray30"),
      panel.grid.major.x = element_blank(),
      axis.line = element_line(color = "gray50")
    )
  
  print(no2_single_plot)
} else {
  cat("\nNo NO2 data was loaded for the single plot.\n")
}

cat("\nAll required air quality trend plots have been generated and displayed.\n")
