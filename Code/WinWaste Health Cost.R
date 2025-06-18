#libraries####
library(tidycensus)
library(dplyr)
library(tidyverse)
library(ggsci)
library(scales)
library(gt)
library(stringr)
library(sf)
library(tigris)    # for shapefiles
options(tigris_use_cache = TRUE)

#merge pollution data with census data####
setwd("/Users/kevintu/Library/CloudStorage/OneDrive-UniversityofMarylandSchoolofMedicine/Climate Health Internship/AERMOD View Modeling Results/BenMAP Reformatted")
pollutants <- read_csv("WinWaste Concentrations by Census Tract.csv")

# Extract the unique state and county combinations from your data
counties_to_get <- pollutants %>%
  distinct(STATEFP, NAMELSADCO) %>%
  mutate(COUNTY = str_remove(NAMELSADCO, " County"))

# Define the variables we want from the Census
census_variables <- c(
  total_pop = "B02001_001",
  white = "B02001_002",
  black = "B02001_003",
  asian = "B02001_005",
  hispanic = "B03001_003",
  median_income = "B19013_001"
)

#get census data
# Get unique years in pollution data
years <- unique(pollutants$year)

# Check if 2024 is in the pollution dataset
needs_2024 <- 2024 %in% years
years_to_download <- years[years != 2024]

# Fetch ACS data for all years except 2024
all_census_data <- list()

for (y in years_to_download) {
  message("Fetching ACS for year ", y)
  census_data_year <- get_acs(
    geography = "tract",
    variables = census_variables,
    state = c("MD", "DC"),
    year = y,
    survey = "acs5"
  )
  
  census_data_wide <- census_data_year %>%
    select(-moe) %>%
    pivot_wider(names_from = variable, values_from = estimate) %>%
    mutate(year = y)
  
  all_census_data[[as.character(y)]] <- census_data_wide
}

# If 2024 is needed, duplicate 2023 data
if (needs_2024 && "2023" %in% names(all_census_data)) {
  message("Copying 2023 census data for 2024")
  data_2024 <- all_census_data[["2023"]] %>%
    mutate(year = 2024)
  all_census_data[["2024"]] <- data_2024
}

# Combine all years into a single data frame
final_census_data <- bind_rows(all_census_data)

# Merge the census data with the pollutants data
merged_data <- pollutants %>%
  mutate(GEOID = as.character(GEOID)) %>%
  left_join(final_census_data, by = c("GEOID", "year"))

merged_data <- merged_data %>%
  mutate(
    county_fips = str_sub(GEOID, 1, 5)
  ) %>%
  mutate(STATEFP = as.character(STATEFP)) %>%
  select(GEOID, TRACTCE, NAMELSADCO, pollutant, year, Final_Conc, 
         total_pop, white, black, asian, hispanic, median_income, county_fips)


# Add Baseline Data####
setwd("/Users/kevintu/Library/CloudStorage/OneDrive-UniversityofMarylandSchoolofMedicine/Climate Health Internship")

incidence <- read_excel("Health Endpoints Relative Risk.xlsx", sheet = 2) %>%
  select(-c(`County`))

merged_incidence <- merged_data %>%
  merge(incidence, by = c("county_fips", "year"), all.x = TRUE)
  
# import relative risk####
setwd("/Users/kevintu/Library/CloudStorage/OneDrive-UniversityofMarylandSchoolofMedicine/Climate Health Internship")

risk <- read_excel("Health Endpoints Relative Risk.xlsx", sheet = 1) %>%
  select(-`RR Source`) %>%
  rename(RR = `RR per 1 µg m3`, endpoint = `Health Endpoint`)

# calculate number of additional cases####
# Step 1: Reshape merged_incidence to long format
incidence_long <- merged_incidence %>%
  pivot_longer(
    cols = ends_with("_rate"),
    names_to = "endpoint",
    values_to = "rate"
  ) %>%
  # Clean endpoint names to match the risk table
  mutate(endpoint = case_when(
    endpoint == "mortality_rate" ~ "All-cause mortality",
    endpoint == "diabetes_rate" ~ "Diabetes",
    endpoint == "cancer_rate" ~ "Respiratory Tract Cancer",
    endpoint == "stroke_rate" ~ "Stroke",
    endpoint == "asthma_rate" ~ "Asthma",
    endpoint == "ihd_rate" ~ "Ischemic heart disease",
    endpoint == "copd_rate" ~ "Chronic obstructive pulmonary disease",
    TRUE ~ endpoint
  ))

# Step 2: Join with risk table
incidence_with_risk <- incidence_long %>%
  left_join(risk, by = c("pollutant", "endpoint")) %>%
  mutate(
    beta = log(RR),
    baseline_rate = as.numeric(rate) / 100000,  # convert to per-person rate
    additional_cases = baseline_rate * total_pop * (1 - exp(-beta * Final_Conc))
  )

# Step 3: Summarize results
case_summary_by_endpoint <- incidence_with_risk %>%
  group_by(pollutant, endpoint, year) %>%
  summarise(
    total_additional_cases = sum(additional_cases, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(pollutant, endpoint, year) %>%
  filter(total_additional_cases > 0)

#calcualte economic cost ####
setwd("/Users/kevintu/Library/CloudStorage/OneDrive-UniversityofMarylandSchoolofMedicine/Climate Health Internship")

costofillness <- read_excel("Health Endpoints Relative Risk.xlsx", sheet = 3) %>%
  select(-`Source`)

# Step 1: Join cost per case
cases_with_cost <- case_summary_by_endpoint %>%
  left_join(costofillness, by = c("endpoint" = "Health Endpoint")) %>%
  mutate(
    total_cost = total_additional_cases * `Economic Cost`
  )

# Step 2: Summarize total cost by pollutant and year
cost_summary <- cases_with_cost %>%
  group_by(pollutant, year) %>%
  summarise(
    total_cost = sum(total_cost, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(pollutant, year)

# Optionally, save the final results to a CSV file
write_csv(incidence_with_risk, "WinWaste_Cases_by_Tract.csv")


#plot damages trending over time data####
# Prepare data: summarize and convert to millions
cost_by_year_pollutant <- cases_with_cost %>%
  group_by(year, pollutant) %>%
  summarise(total_cost = sum(total_cost, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    total_cost_million = total_cost / 1e6,
    pollutant = factor(pollutant, levels = c("NOx", "PM", "SO2", "CO"))  # Desired legend order
  )

# Calculate total cost per year for bar labels
ordered_endpoints <- c(
  "All-cause mortality",
  "Diabetes",
  "Asthma",
  "Stroke",
  "Ischemic heart disease",
  "Respiratory Tract Cancer"
)

endpoint_trends <- cases_with_cost %>%
  group_by(year, endpoint) %>%
  summarise(total_cases = sum(total_additional_cases, na.rm = TRUE), .groups = "drop") %>%
  mutate(endpoint = factor(endpoint, levels = ordered_endpoints))

# GraphPad-style plot
ggplot(endpoint_trends, aes(x = year, y = total_cases)) +
  geom_line(size = 1.2, color = "steelblue") +
  geom_point(size = 2) +
  facet_wrap(~ endpoint, scales = "free_y", nrow = 3) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Health Endpoint Cases Over Time",
    x = "Year",
    y = "Total Additional Cases"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
  )

#summarize economic costs in table####
# Summarize data by year and endpoint
pollutant_order <- c("PM", "NOx", "SO2", "CO")

# --- 2. Prepare the data for the gt table ---
# This involves pivoting the data from a "long" format to a "wide" format,
# creating separate columns for cases and costs for each year.
gt_data <- cases_with_cost %>%
  select(pollutant, endpoint, year, total_additional_cases, total_cost) %>%
  # Pivot wider to create columns like `total_additional_cases_2014`, `total_cost_2014`, etc.
  pivot_wider(
    names_from = year,             # Use 'year' to create new column names
    values_from = c(total_additional_cases, total_cost), # Values to spread
    names_glue = "{.value}_{year}" # Defines how new column names are constructed
  ) %>%
  # Convert 'pollutant' to a factor with the desired order for proper grouping
  mutate(pollutant = factor(pollutant, levels = pollutant_order)) %>%
  # Arrange data by pollutant to ensure correct grouping order in the table
  arrange(pollutant) %>%
  # Convert total_cost values to millions for all relevant columns
  mutate(across(starts_with("total_cost_"), ~ . / 1e6))


# --- 3. Create the gt table ---
gt_table <- gt(
  gt_data,
  rowname_col = "endpoint", # Use 'endpoint' as the row labels within each group
  groupname_col = "pollutant" # Use 'pollutant' to create row groups
) %>%
  # Ensure the pollutant row groups appear in the specified order
  row_group_order(groups = pollutant_order) %>%
  # Add a main title and subtitle to the table
  tab_header(
    title = md("**Health Impacts and Economic Costs**"),
    subtitle = "Estimated cases and associated costs by pollutant, endpoint, and year"
  ) %>%
  # Create column spanners for each year (e.g., "2014")
  # Each spanner will cover the "Total Cases" and "Cost ($USD Millions)" columns for that year.
  tab_spanner(
    label = md("**2014**"), # Spanner label (bolded using Markdown)
    columns = c(total_additional_cases_2014, total_cost_2014) # Columns under this spanner
  ) %>%
  tab_spanner(
    label = md("**2017**"),
    columns = c(total_additional_cases_2017, total_cost_2017)
  ) %>%
  tab_spanner(
    label = md("**2020**"),
    columns = c(total_additional_cases_2020, total_cost_2020)
  ) %>%
  tab_spanner(
    label = md("**2023**"),
    columns = c(total_additional_cases_2023, total_cost_2023)
  ) %>%
  tab_spanner(
    label = md("**2024**"),
    columns = c(total_additional_cases_2024, total_cost_2024)
  ) %>%
  # Relabel the individual columns under the spanners
  # These are the "Total Cases" and "Cost ($USD Millions)" labels for each year.
  cols_label(
    total_additional_cases_2014 = md("Total Cases"),
    total_cost_2014 = md("Cost ($USD Millions)"), # Updated label for cost
    total_additional_cases_2017 = md("Total Cases"),
    total_cost_2017 = md("Cost ($USD Millions)"), # Updated label for cost
    total_additional_cases_2020 = md("Total Cases"),
    total_cost_2020 = md("Cost ($USD Millions)"), # Updated label for cost
    total_additional_cases_2023 = md("Total Cases"),
    total_cost_2023 = md("Cost ($USD Millions)"), # Updated label for cost
    total_additional_cases_2024 = md("Total Cases"),
    total_cost_2024 = md("Cost ($USD Millions)")  # Updated label for cost
  ) %>%
  # Format the 'total_additional_cases' columns as numbers with 3 decimal places
  fmt_number(
    columns = starts_with("total_additional_cases"),
    decimals = 3, # Number of decimal places
    use_seps = FALSE # Do not use thousands separators
  ) %>%
  # Format the 'total_cost' columns as numbers with 1 decimal place (now in millions)
  fmt_number(
    columns = starts_with("total_cost"),
    decimals = 1, # One decimal place for values in millions
    use_seps = FALSE # Do not use thousands separators for these values
  ) %>%
  # Replace any missing (NA) values in the table with "XX" as seen in the image
  sub_missing(
    columns = everything(), # Apply to all columns
    missing_text = "XX" # Text to display for missing values
  ) %>%
  # Adjust column widths for better visual balance
  cols_width(
    starts_with("total_additional_cases") ~ px(90), # Width for cases columns
    starts_with("total_cost") ~ px(100) # Width for cost columns
  ) %>%
  # Apply styling for visual appearance
  # 1. Background color for row group headers (e.g., "PM", "NOx")
  tab_style(
    style = cell_fill(color = "lightgray"), # Light gray background
    locations = cells_row_groups() # Apply to row group cells
  ) %>%
  # 2. Add borders to all body cells
  tab_style(
    style = cell_borders(
      sides = "all", # All sides of the cell
      color = "#000000", # Black border color (hex code)
      weight = px(2) # 2-pixel border thickness
    ),
    locations = cells_body() # Apply to the main data cells
  ) %>%
  # 3. Add borders to column spanners (e.g., "2014")
  tab_style(
    style = cell_borders(
      sides = "all",
      color = "#000000", # Black border color (hex code)
      weight = px(2)
    ),
    locations = cells_column_spanners() # Apply to column spanner cells
  ) %>%
  # 4. Add borders to column labels ("Total Cases", "Cost ($USD Millions)")
  tab_style(
    style = cell_borders(
      sides = "all",
      color = "#000000", # Black border color (hex code)
      weight = px(2)
    ),
    locations = cells_column_labels() # Apply to column label cells
  ) %>%
  # 5. Add borders to row group cells themselves
  tab_style(
    style = cell_borders(
      sides = "all",
      color = "#000000", # Black border color (hex code)
      weight = px(2)
    ),
    locations = cells_row_groups() # Apply to row group cells
  ) %>%
  # Set the stubhead label to be empty to match the image
  tab_stubhead(label = "")

# Display the gt table
gt_table


table_path <- "/Users/kevintu/Library/CloudStorage/OneDrive-UniversityofMarylandSchoolofMedicine/Climate Health Internship/Manuscript/Tables"
gtsave(gt_table, "WinWaste Damages.docx", path = table_path)

#Economic costs by county in Maryland####
incidence_with_risk <- incidence_with_risk %>%
  left_join(costofillness, by = c("endpoint" = "Health Endpoint"))

incidence_with_risk_2024 <- incidence_with_risk %>%
  mutate(economic_cost = additional_cases * `Economic Cost`) %>%
  filter(year == 2024)

county_costs <- incidence_with_risk_2024 %>%
  group_by(county_fips) %>%
  summarise(total_economic_cost = sum(economic_cost, na.rm = TRUE))

# Get counties for MD and DC
counties_md <- counties(state = "MD", cb = TRUE, year = 2024)
counties_dc <- counties(state = "DC", cb = TRUE, year = 2024)

# Combine
md_dc_counties <- rbind(counties_md, counties_dc)

# Join cost data to spatial data
md_dc_map_data <- md_dc_counties %>%
  left_join(county_costs, by = c("GEOID" = "county_fips"))

ggplot(md_dc_map_data) +
  geom_sf(aes(fill = total_economic_cost / 1e6), color = "white", size = 0.3) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90", name = "Economic Cost ($Millions)") +
  labs(title = "Total Health-Related Economic Damages by County",
       subtitle = "Maryland and Washington, DC") +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid = element_blank(),      # Remove grid lines
    axis.title = element_blank(),      # Remove axis titles
    axis.text = element_blank(),       # Remove axis text
    axis.ticks = element_blank(),      # Remove axis ticks
    panel.border = element_blank()     # Optional: remove border if present
  )

#Baltimore census tracts damages####
# Step 1: Filter incidence data to Baltimore census tracts (Baltimore City FIPS: 24510)
baltimore_tract_data <- incidence_with_risk_2024 %>%
  filter(county_fips == "24510") %>%
  group_by(GEOID) %>%
  summarise(total_economic_cost = sum(economic_cost, na.rm = TRUE))

# Step 2: Get Baltimore census tracts shapefile
baltimore_tracts <- tracts(state = "MD", county = "Baltimore city", cb = TRUE, year = 2021)

# Step 3: Join with incidence data
baltimore_map_data <- baltimore_tracts %>%
  left_join(baltimore_tract_data, by = "GEOID")

# Step 4: Plot map
ggplot(baltimore_map_data) +
  geom_sf(aes(fill = total_economic_cost / 1e6), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90", name = "Economic Cost ($Millions)") +
  labs(title = "WinWaste Health-Related Economic Damages by Census Tract",
       subtitle = "Baltimore City, MD") +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank()
  )
