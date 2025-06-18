# libraries####
library(tidycensus)
library(dplyr)
library(tidyverse)
library(readxl) # Added for reading Excel files

# --- 1. Load and Prepare Data --- ####
setwd("/Users/kevintu/Library/CloudStorage/OneDrive-UniversityofMarylandSchoolofMedicine/Climate Health Internship/AERMOD View Modeling Results/BenMAP Reformatted")
pollutants <- read_csv("CurtisBay Concentrations by Census Tract.csv")

# --- 2. Get Most Recent Census Data --- ####
# Define the variables we want from the Census
census_variables <- c(
  total_pop = "B02001_001",
  white = "B02001_002",
  black = "B02001_003",
  asian = "B02001_005",
  hispanic = "B03001_003",
  median_income = "B19013_001"
)

# Define the single, most recent year for ACS data. 
# As of mid-2025, the 2023 5-year estimates are the most current.
LATEST_CENSUS_YEAR <- 2023

message("Fetching ", LATEST_CENSUS_YEAR, " ACS data for all calculations.")
census_data_latest <- get_acs(
  geography = "tract",
  variables = census_variables,
  state = c("MD", "DC"),
  year = LATEST_CENSUS_YEAR,
  survey = "acs5",
  output = "wide" # Get data in wide format directly
) %>%
  select(-ends_with("M")) # Remove margin-of-error columns

# --- 3. Merge Pollutant and Census Data --- ####
# Merge the single set of census data with the pollutants data by GEOID.
# This applies the same demographic data to each condition.
merged_data <- pollutants %>%
  mutate(GEOID = as.character(GEOID)) %>%
  left_join(census_data_latest, by = "GEOID")

# Clean up the merged dataset
merged_data <- merged_data %>%
  mutate(county_fips = str_sub(GEOID, 1, 5)) %>%
  rename_with(~str_remove(., "E$"), .cols = ends_with("E")) %>%
  dplyr::select(
    GEOID, TRACTC, NAMELSADCO, pollutant, condition, Final_Conc, # Using 'condition' instead of 'year'
    total_pop, white, black, asian, hispanic, median_income, county_fips
  ) %>%
  rename(TRACTCE = TRACTC)

# --- 4. Add Baseline Health Incidence Data --- ####
# Load incidence data
setwd("/Users/kevintu/Library/CloudStorage/OneDrive-UniversityofMarylandSchoolofMedicine/Climate Health Internship")
incidence_raw <- read_excel("Health Endpoints Relative Risk.xlsx", sheet = 2) %>%
  select(-`County`)

# Filter for the MOST RECENT year of incidence data available in your file
incidence_latest <- incidence_raw %>%
  filter(year == max(year)) %>%
  select(-year) # Drop year column before joining

# Merge latest incidence rates with the main dataset by county FIPS code
merged_incidence <- merged_data %>%
  merge(incidence_latest, by = "county_fips")


# --- 5. Calculate Additional Health Cases --- ####
# Import relative risk data
setwd("/Users/kevintu/Library/CloudStorage/OneDrive-UniversityofMarylandSchoolofMedicine/Climate Health Internship")
risk <- read_excel("Health Endpoints Relative Risk.xlsx", sheet = 1) %>%
  select(-`RR Source`) %>%
  rename(RR = `RR per 1 µg m3`, endpoint = `Health Endpoint`)

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

# Step 2: Join with risk table and calculate cases
incidence_with_risk <- incidence_long %>%
  left_join(risk, by = c("pollutant", "endpoint")) %>%
  mutate(
    beta = log(RR),
    baseline_rate = as.numeric(rate) / 100000,  # convert from per 100k to per-person rate
    additional_cases = baseline_rate * total_pop * (1 - exp(-beta * Final_Conc))
  )

# Step 3: Summarize results by condition
case_summary_by_endpoint <- incidence_with_risk %>%
  group_by(pollutant, endpoint, condition) %>% # Group by 'condition'
  summarise(
    total_additional_cases = sum(additional_cases, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(pollutant, endpoint, condition) %>% # Arrange by 'condition'
  filter(total_additional_cases > 0)

# --- 6. Calculate Economic Cost --- ####
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
  group_by(pollutant, condition) %>%
  summarise(
    total_cost = sum(total_cost, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(pollutant, condition)

# Optionally, save the final results to a CSV file
write_csv(incidence_with_risk, "WinWaste_Cases_by_Tract.csv")


#summarize economic costs in table####
# Summarize data by year and endpoint
# Define the desired order for pollutant row groups
pollutant_order <- c("PM", "NOx", "SO2", "CO") # Updated pollutant order

# Define the desired order for condition columns
condition_order <- c("Typical Controlled", "Typical Uncontrolled", "Max Controlled", "Max Uncontrolled")

# --- 2. Prepare the data for the gt table ---
# This involves pivoting the data from a "long" format to a "wide" format,
# creating separate columns for cases and costs for each condition.
gt_data <- cases_with_cost %>%
  select(pollutant, endpoint, condition, total_additional_cases, total_cost) %>%
  # Pivot wider to create columns like `total_additional_cases_Typical Controlled`, etc.
  pivot_wider(
    names_from = condition,             # Use 'condition' to create new column names
    values_from = c(total_additional_cases, total_cost), # Values to spread
    # Ensure glue correctly handles spaces in condition names for new column names
    names_glue = "{.value}_{condition}"
  ) %>%
  # Convert 'pollutant' to a factor with the desired order for proper grouping
  mutate(pollutant = factor(pollutant, levels = pollutant_order)) %>%
  # Relocate columns to ensure correct order
  # Note: `paste0` with `condition_order` automatically creates names with spaces
  # that are valid R names, so `gt` can select them using unquoted names or backticks.
  relocate(
    endpoint, pollutant,
    !!! syms(paste0("total_additional_cases_", condition_order)),
    !!! syms(paste0("total_cost_", condition_order))
  ) %>%
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
    subtitle = "Estimated cases and associated costs by pollutant, endpoint, and condition" # Updated subtitle
  ) %>%
  # Create column spanners for each condition
  # Use backticks around column names that contain spaces
  tab_spanner(
    label = md("**Typical Controlled**"), # Spanner label (bolded using Markdown)
    columns = c(`total_additional_cases_Typical Controlled`, `total_cost_Typical Controlled`) # Columns under this spanner
  ) %>%
  tab_spanner(
    label = md("**Typical Uncontrolled**"),
    columns = c(`total_additional_cases_Typical Uncontrolled`, `total_cost_Typical Uncontrolled`)
  ) %>%
  tab_spanner(
    label = md("**Max Controlled**"),
    columns = c(`total_additional_cases_Max Controlled`, `total_cost_Max Controlled`)
  ) %>%
  tab_spanner(
    label = md("**Max Uncontrolled**"),
    columns = c(`total_additional_cases_Max Uncontrolled`, `total_cost_Max Uncontrolled`)
  ) %>%
  # Relabel the individual columns under the spanners
  # Use backticks around column names that contain spaces
  cols_label(
    `total_additional_cases_Typical Controlled` = md("Total Cases"),
    `total_cost_Typical Controlled` = md("Cost ($USD Millions)"),
    `total_additional_cases_Typical Uncontrolled` = md("Total Cases"),
    `total_cost_Typical Uncontrolled` = md("Cost ($USD Millions)"),
    `total_additional_cases_Max Controlled` = md("Total Cases"),
    `total_cost_Max Controlled` = md("Cost ($USD Millions)"),
    `total_additional_cases_Max Uncontrolled` = md("Total Cases"),
    `total_cost_Max Uncontrolled` = md("Cost ($USD Millions)")
  ) %>%
  # Format the 'total_additional_cases' columns as numbers with 3 decimal places
  fmt_number(
    columns = starts_with("total_additional_cases"), # starts_with works with spaces
    decimals = 3, # Number of decimal places
    use_seps = FALSE # Do not use thousands separators
  ) %>%
  # Format the 'total_cost' columns as numbers with 1 decimal place (now in millions)
  fmt_number(
    columns = starts_with("total_cost"), # starts_with works with spaces
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
  # 3. Add borders to column spanners (e.g., "Typical Controlled")
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
gtsave(gt_table, "CurtisBay Damages.docx", path = table_path)


#range plots####
# Prepare data: summarize and convert to millions
cost_by_condition_pollutant <- cases_with_cost %>%
  group_by(condition, pollutant) %>%
  summarise(total_cost = sum(total_cost, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    total_cost_million = total_cost / 1e6,
    pollutant = factor(pollutant, levels = c("NOx", "PM", "SO2", "CO"))  # Desired legend order
  )

cost_totals_by_condition <- cost_by_condition_pollutant %>%
  group_by(condition) %>%
  summarise(
    pollutant = "Total", # Assign "Total" as the pollutant for these rows
    total_cost = sum(total_cost),
    total_cost_million = sum(total_cost_million)
  ) %>%
  ungroup() # Ungroup the data after summarising

# Combine the original data with the new total rows
cost_by_condition_pollutant_with_total <- bind_rows(
  cost_by_condition_pollutant,
  cost_totals_by_condition
) %>%
  arrange(condition, pollutant) # Arrange for better readability

cost_by_condition_pollutant_split <- cost_by_condition_pollutant_with_total %>%
  separate(condition, into = c("operations", "controls"), sep = " ")

# Calculate total cost per year for bar labels
# 1. Ensure 'pollutant' is an ordered factor for the y-axis
desired_pollutant_order <- c("Total", "CO", "SO2", "NOx", "PM")

cost_by_condition_pollutant_split <- cost_by_condition_pollutant_split %>%
  mutate(
    pollutant = factor(pollutant, levels = desired_pollutant_order),
    # Optionally, order controls for consistent plotting if needed,
    # though for geom_line connecting two points it's less critical.
    controls = factor(controls, levels = c("Controlled", "Uncontrolled"))
  )

# 2. Create the range plot with a log scale on the x-axis
ggplot(cost_by_condition_pollutant_split, aes(x = total_cost_million, y = pollutant)) +
  # Draw a line connecting 'Controlled' and 'Uncontrolled' for each pollutant
  geom_line(aes(group = pollutant), color = "darkgray", size = 1.2) +
  
  # Add points for 'Controlled' and 'Uncontrolled', colored by control status
  geom_point(aes(color = controls), size = 4) +
  
  # Add text labels for total_cost_million at each point
  geom_text(
    aes(label = sprintf("%.2f", total_cost_million), color = controls),
    size = 3.5,
    vjust = -0.9 # Adjust vertical position slightly above the point
  ) +
  
  # Facet by 'operations' with 2 rows (meaning 1 column)
  facet_wrap(~ operations, ncol = 1, strip.position = "top") +
  
  # Apply a log10 scale to the x-axis
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x), # Set major breaks at powers of 10
    labels = trans_format("log10", math_format(10^.x)) # Format labels as 10^x
  ) +
  # Or, for simpler numeric labels for log scale:
  # scale_x_log10(labels = scales::label_comma()) # Uses comma formatting with log scale
  
  # Customize labels and colors
  labs(
    title = "Cost Ranges by Pollutant and Control Status (Log Scale)",
    x = "Total Cost (Millions, Log Scale)",
    y = "Pollutant Type",
    color = "Control Status"
  ) +
  scale_color_manual(values = c("Controlled" = "steelblue", "Uncontrolled" = "darkred")) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    strip.text = element_text(face = "bold", size = 12),
    axis.text.y = element_text(face = "bold", size = 10),
    axis.title = element_text(size = 11),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )
#Economic costs by county in Maryland####
incidence_with_risk_coi <- incidence_with_risk %>%
  left_join(costofillness, by = c("endpoint" = "Health Endpoint"))

incidence_with_risk_2024 <- incidence_with_risk_coi %>%
  mutate(economic_cost = additional_cases * `Economic Cost`) %>%
  filter(condition == "Typical Uncontrolled")

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

#Economic savings from proper incinerator control####
incidence_with_risk_coi <- incidence_with_risk %>%
  left_join(costofillness, by = c("endpoint" = "Health Endpoint"))

incidence_with_risk_2024 <- incidence_with_risk_coi %>%
  mutate(economic_cost = additional_cases * `Economic Cost`)

geoid_costs <- incidence_with_risk_2024 %>%
  group_by(GEOID, condition, NAMELSADCO) %>%
  summarise(total_economic_cost = sum(economic_cost, na.rm = TRUE))

delta_economic_cost_df <- geoid_costs %>%
  pivot_wider(
    names_from = c(condition),
    values_from = total_economic_cost,
    names_prefix = "economic_cost_" # Add a prefix to the new column names
  ) %>%
  mutate(
    delta_economic_cost = `economic_cost_Typical Uncontrolled` - `economic_cost_Typical Controlled`
  )

# Get counties for MD and DC
counties_md <- counties(state = "MD", cb = TRUE, year = 2024)
counties_dc <- counties(state = "DC", cb = TRUE, year = 2024)

# Combine
md_dc_counties <- rbind(counties_md, counties_dc)

# Join cost data to spatial data
md_dc_map_data <- md_dc_counties %>%
  select(-"GEOID") %>%
  left_join(delta_economic_cost_df, by = c("NAMELSAD" = "NAMELSADCO"))

ggplot(md_dc_map_data) +
  geom_sf(aes(fill = delta_economic_cost / 1e6), color = "white", size = 0.3) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90", name = "Economic Cost ($Millions)") +
  labs(title = "Health Savings from Incinerator Controls",
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

# Baltimore census tracts cost savings####
# Step 2: Get Baltimore census tracts shapefile
baltimore_tracts <- tracts(state = "MD", county = "Baltimore city", cb = TRUE, year = 2021)

md_dc_map_data <- md_dc_map_data %>%
  st_drop_geometry() %>% # Drop the geometry column, converting to a regular dataframe
  select(GEOID, delta_economic_cost) # Select only the necessary columns for joining

# Step 3: Join with incidence data
baltimore_map_data <- baltimore_tracts %>%
  merge(md_dc_map_data, by = "GEOID")

# Step 4: Plot map
ggplot(baltimore_map_data) +
  geom_sf(aes(fill = delta_economic_cost), color = "black", linewidth = 0.2) +
  scale_fill_gradient(
    name = "Cost Savings (USD)",
    low = "white",
    high = "darkgreen",
    na.value = "grey90",
    labels = scales::comma, # Keep comma formatting for readability
    trans = "sqrt" # Apply square root transformation to skew the gradient
  ) +
  labs(
    title = "Health-Related Economic Cost Savings by Census Tract",
    subtitle = "Typical Uncontrolled vs. Typical Controlled, Baltimore City, MD\n(Color gradient is square-root transformed)" # Indicate transformation
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )