# R Script: Assign Pollution Data to Census Tracts####
#
# Description: This script takes a dataframe of pollution monitoring points (latitude,
# longitude, and concentration value) and assigns a value to each census tract
# in a specified area.
#
# Logic:
# 1. For census tracts with one or more pollution points, the assigned value is the
#    average of the concentrations of those points.
# 2. For census tracts with no pollution points, the assigned value is taken from the
#    single nearest pollution point.

library(sf)
library(dplyr)
library(tigris)

# 1. input data####
# Set your filename
file_path <- "reformatted_WinWaste 2017 PM Annual.csv"
file_name <- basename(file_path)
year <- str_extract(file_name, "\\d{4}")
pollutant <- str_match(file_name, paste0(year, " ([A-Za-z0-9]+)"))[,2]

pollution <- read_csv(file_path) %>%
  select(latitude = Latitude, longitude = Longitude, Conc = Values) %>%
  mutate(
    pollutant = pollutant,
    year = as.integer(year)
  )


# 2. DATA PREPARATION: Convert to Spatial Objects and Align CRS####
pollution_sf <- st_as_sf(pollution,
                         coords = c("longitude", "latitude"),
                         crs = 4326)

# Download census tract boundaries using the 'tigris' package.
tracts_sf_md <- tracts(state = c("MD"), cb = TRUE)
tracts_sf_dc <- tracts(state = c("DC"), cb = TRUE)
tracts_sf_raw <- rbind(tracts_sf_md, tracts_sf_dc)

# ---
# CRITICAL STEP: Coordinate Reference System (CRS) Transformation
# For accurate distance and intersection calculations, both spatial layers must
# be in the same *projected* CRS. A geographic CRS (like WGS 84) uses degrees
# and is not suitable for measuring distances in meters or feet.
# We will transform both layers to a common projected CRS.
# ---

# Find the CRS of the downloaded tracts data
target_crs <- st_crs(tracts_sf_raw)

# Transform the pollution points to match the tracts' CRS
pollution_sf_proj <- st_transform(pollution_sf, crs = target_crs)

# For clarity, let's rename the raw tracts sf object
tracts_sf_proj <- tracts_sf_raw


# 3. SPATIAL ANALYSIS: Assign Points to Tracts and Aggregate####
# Perform a spatial join. This adds the tract information (like GEOID) to each
# pollution point that falls within a tract.
points_in_tracts <- st_join(pollution_sf_proj, tracts_sf_proj)

# Calculate the average concentration for each census tract that has points.
# We group by the unique tract identifier (GEOID) and then calculate the mean.
tracts_with_data <- points_in_tracts %>%
  st_drop_geometry() %>% # Drop geometry for faster non-spatial grouping
  filter(!is.na(GEOID)) %>% # Ensure we only consider points that fell in a tract
  group_by(GEOID) %>%
  summarise(Avg_Conc = mean(Conc, na.rm = TRUE), .groups = 'drop')

# 4. HANDLING EMPTY TRACTS: Find Nearest Neighbor####

# Identify which tracts did NOT have any pollution points.
# These are the tracts in our original `tracts_sf_proj` whose GEOID is not
# in our `tracts_with_data` summary.
empty_tracts_sf <- tracts_sf_proj %>%
  filter(!GEOID %in% tracts_with_data$GEOID)

# For each empty tract, find the index of the NEAREST point from the pollution data.
# `st_nearest_feature` is highly optimized for this task.
nearest_point_indices <- st_nearest_feature(empty_tracts_sf, pollution_sf_proj)

# Use the indices to get the concentration value from the nearest pollution point.
empty_tracts_sf$Nearest_Conc <- pollution_sf_proj$Conc[nearest_point_indices]


# 5. FINAL ASSEMBLY: Combine Results into a Single Dataset####
# First, create the final dataset for tracts that had data by joining the
# calculated averages back to their geometries.
final_tracts_with_data <- tracts_sf_proj %>%
  filter(GEOID %in% tracts_with_data$GEOID) %>%
  left_join(tracts_with_data, by = "GEOID") %>%
  rename(Final_Conc = Avg_Conc) %>% # Rename to a common column name
  mutate(Source = "Averaged within Tract") # Add a column to track the method

# Second, prepare the empty tracts dataset with the nearest neighbor values.
final_tracts_from_nearest <- empty_tracts_sf %>%
  rename(Final_Conc = Nearest_Conc) %>% # Rename to a common column name
  mutate(Source = "Nearest Neighbor") # Add a column to track the method

# Combine the two datasets into one final sf object.
# We only need to select the relevant columns before binding.
final_pollution_by_tract <- bind_rows(
  final_tracts_with_data %>% select(GEOID, TRACTCE, NAME, Final_Conc, Source, STUSPS, NAMELSADCO, geometry),
  final_tracts_from_nearest %>% select(GEOID, TRACTCE, NAME, Final_Conc, Source, STUSPS, NAMELSADCO, geometry)
)



