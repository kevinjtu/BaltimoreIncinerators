library(dplyr)
library(lubridate)
library(openair)
library(riem)
library(readr)
library(readxl)
library(geosphere)

# ==========================================
# 1. IMPORT AND TRANSFORM SENSOR DATA
# ==========================================
# Point directly to your local OneDrive file path
setwd("/Users/kevintu/Library/CloudStorage/OneDrive-UniversityofMarylandSchoolofMedicine/Climate Health Internship/")
breathe_data <- read_excel("Breathe Baltimore Air Quality Monitors.xlsx")

# Filter for the target sites and rename columns so 'openair' can recognize them
sensor_clean <- breathe_data %>%
  filter(`Site Name` %in% c("Bay Brook Elementary/Middle School", "Filbert Street Garden")) %>%
  mutate(
    date = ymd_hms(`Original Timestamp`),  # Standardize datetime format
    pm25 = `PM2.5 Avg (ug/m3)`,            # Openair looks for lowercase 'pm25'
    site = `Site Name`                     # Openair uses 'site' to split plots
  ) %>%
  select(date, site, pm25) %>%
  filter(!is.na(date), !is.na(pm25))       # Drop missing values for clean testing

# ==========================================
# 2. FETCH METEOROLOGICAL DATA (BWI AIRPORT)
# ==========================================
# Automatically detect the start and end dates from your sensor data
start_date <- as.Date(min(sensor_clean$date))
end_date <- as.Date(max(sensor_clean$date))

# Pull BWI Airport data (KBWI) via the reliable Iowa Mesonet (riem)
bwi_raw <- riem_measures(station = "KBWI", 
                         date_start = as.character(start_date), 
                         date_end = as.character(end_date + 1)) 

# Clean the weather data to match the hourly sensor data
met_clean <- bwi_raw %>%
  mutate(
    date = floor_date(valid, "hour"),  # Align weird ASOS minutes to the top of the hour
    ws = sknt * 0.514444,              # Convert Knots to Meters per Second (m/s)
    wd = drct             
  ) %>%
  # Average out instances where the airport takes multiple readings in the same hour
  group_by(date) %>%                   
  summarize(
    ws = mean(ws, na.rm = TRUE),
    wd = mean(wd, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  select(date, ws, wd) %>%
  filter(!is.na(ws), !is.na(wd))

# ==========================================
# 3. MERGE SENSOR AND WEATHER DATA
# ==========================================
merged_data <- inner_join(sensor_clean, met_clean, by = "date")

# ==========================================
# 4. VISUAL VALIDATION: POLAR PLOTS
# ==========================================
# This creates a hotspot heatmap for Bay Brook
polarPlot(filter(merged_data, site == "Bay Brook Elementary/Middle School"), 
          pollutant = "pm25", 
          main = "PM2.5 by Wind Vector: Bay Brook Elementary")

# This creates a hotspot heatmap for Filbert Street
polarPlot(filter(merged_data, site == "Filbert Street Garden"), 
          pollutant = "pm25", 
          main = "PM2.5 by Wind Vector: Filbert Street Garden")

# ==========================================
# 5. STATISTICAL VALIDATION: AUTOMATED WILCOXON TESTS
# ==========================================

# Your provided coordinates
site_coords <- tibble(
  `Site Name` = c(
    "A Lot Matter Project", "Ark Church", "Baltimore Community ToolBank",
    "Barclay Elementary/Middle School", "Bay Brook Elementary/Middle School",
    "Bilingual Christian Church", "BUGS", "Convention Center",
    "Federal Hill Preparatory School", "Filbert Street Garden",
    "Fort McHenry", "Intersection of Change: Strength to Love II Farm",
    "Stillmeadow Community Fellowship", "Waterfront Partnership Garage",
    "West Covington Park"
  ),
  Lat = c(39.297546, 39.312257, 39.278256, 39.323888, 39.226105,
          39.301240, 39.282113, 39.285905, 39.277727, 39.224991,
          39.264515, 39.304728, 39.281516, 39.286273, 39.261661),
  Lon = c(-76.647623, -76.600793, -76.631189, -76.611048, -76.598537,
          -76.544680, -76.596758, -76.618115, -76.610840, -76.591130,
          -76.584988, -76.647507, -76.699541, -76.605007, -76.616087)
)

# Curtis Bay Medical Waste Incinerator coordinates
curtis_bay_lat <- 39.202798
curtis_bay_lon <- -76.555803

# 1. Calculate the exact Target Bearing for EVERY site automatically
site_bearings <- site_coords %>%
  rowwise() %>%
  mutate(
    # geosphere requires c(longitude, latitude) vectors
    raw_bearing = geosphere::bearing(
      c(Lon, Lat),
      c(curtis_bay_lon, curtis_bay_lat)
    ),
    # Convert from (-180 to 180) standard to meteorological (0 to 360) standard
    target_bearing = (raw_bearing + 360) %% 360
  ) %>%
  ungroup() %>%
  select(`Site Name`, target_bearing)

# 2. Join the calculated bearings to our hourly data
analysis_df <- merged_data %>%
  left_join(site_bearings, by = c("site" = "Site Name")) %>%
  mutate(
    cone_width = 20, # +/- 20 degrees is a 40 degree sweeping cone
    
    # Calculate shortest angular distance (handles the 0/360 True North crossover bug)
    angle_diff = (wd - target_bearing + 180) %% 360 - 180,
    
    wind_sector = case_when(
      abs(angle_diff) <= cone_width ~ "Direct_Hit",
      TRUE ~ "Background"
    )
  )

# 3. View the descriptive statistics
summary_stats <- analysis_df %>%
  group_by(site, wind_sector) %>%
  summarize(
    mean_pm25 = mean(pm25, na.rm = TRUE),
    median_pm25 = median(pm25, na.rm = TRUE),
    n_hours = n(),
    .groups = "drop"
  )
print("Descriptive Statistics by Wind Sector:")
print(summary_stats)

# 4. Run the Wilcoxon tests dynamically for the target sites
bay_brook_test <- wilcox.test(pm25 ~ wind_sector, 
                              data = filter(analysis_df, site == "Bay Brook Elementary/Middle School"))
print("Wilcoxon Test Result - Bay Brook:")
print(bay_brook_test)

filbert_test <- wilcox.test(pm25 ~ wind_sector, 
                            data = filter(analysis_df, site == "Filbert Street Garden"))
print("Wilcoxon Test Result - Filbert Street:")
print(filbert_test)

#visualization####
# ==========================================
# INSTALL AND LOAD VISUALIZATION PACKAGES
# ==========================================
# install.packages(c("ggplot2", "ggsignif", "patchwork", "geosphere", "sf"))

library(ggplot2)
library(ggsignif)
library(patchwork)
library(geosphere)
library(sf)
library(dplyr)

# ==========================================
# 1. PREP DATA FOR PANEL A (THE MAP)
# ==========================================
# Define the coordinates
cb_lon <- -76.555803
cb_lat <- 39.202798

target_sites <- site_coords %>%
  filter(`Site Name` %in% c("Bay Brook Elementary/Middle School", "Filbert Street Garden"))

# Calculate the distance from each monitor to the incinerator (to size the cones)
# And generate the polygon points for the ±20° "Direct Hit" wind cones
cone_polygons <- lapply(1:nrow(target_sites), function(i) {
  site_name <- target_sites$`Site Name`[i]
  origin <- c(target_sites$Lon[i], target_sites$Lat[i])
  dest <- c(cb_lon, cb_lat)
  
  # Calculate exact distance and bearing to the incinerator
  dist_m <- distGeo(origin, dest)
  bearing <- (bearing(origin, dest) + 360) %% 360
  
  # Generate points for the arc of the cone (sweeping from -20 to +20 degrees)
  arc_bearings <- seq(bearing - 20, bearing + 20, length.out = 50)
  
  # Extend the cone slightly past the incinerator for visual clarity (1.1x distance)
  arc_points <- destPoint(origin, arc_bearings, dist_m * 1.1)
  
  # Combine origin and arc points into a closed polygon dataframe
  poly_df <- data.frame(
    Lon = c(origin[1], arc_points[,1], origin[1]),
    Lat = c(origin[2], arc_points[,2], origin[2]),
    Site = site_name
  )
  return(poly_df)
}) %>% bind_rows()

# ==========================================
# 2. BUILD PANEL A (THE CONE MAP)
# ==========================================
panel_a <- ggplot() +
  # Draw the wind cones
  geom_polygon(data = cone_polygons, aes(x = Lon, y = Lat, group = Site), 
               fill = "firebrick", alpha = 0.2, color = "firebrick", linetype = "dashed") +
  
  # Plot the monitors
  geom_point(data = target_sites, aes(x = Lon, y = Lat), 
             color = "dodgerblue4", size = 4) +
  geom_text(data = target_sites, aes(x = Lon, y = Lat, label = `Site Name`), 
            vjust = -1.5, size = 3, fontface = "bold") +
  
  # Plot the incinerator
  geom_point(aes(x = cb_lon, y = cb_lat), 
             color = "black", fill = "firebrick", shape = 24, size = 5) +
  geom_text(aes(x = cb_lon, y = cb_lat, label = "Curtis Bay Incinerator"), 
            vjust = 2, size = 3.5, fontface = "bold", color = "firebrick") +
  
  # Map styling
  theme_minimal() +
  labs(
    title = "A. Spatial Alignment of Wind Sectors",
    subtitle = "±20° 'Direct Hit' trajectories from monitors to facility",
    x = "Longitude", y = "Latitude"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

# ==========================================
# 3. BUILD PANEL B (THE BOX PLOTS)
# ==========================================
# Filter analysis_df for just our two sites and format labels
boxplot_data <- analysis_df %>%
  filter(site %in% c("Bay Brook Elementary/Middle School", "Filbert Street Garden")) %>%
  mutate(wind_sector = factor(wind_sector, levels = c("Background", "Direct_Hit"),
                              labels = c("Background Sector\n(Outside Cone)", "Direct Hit Sector\n(Inside Cone)")))

panel_b <- ggplot(boxplot_data, aes(x = wind_sector, y = pm25, fill = wind_sector)) +
  # Use boxplots to show median and distribution. 
  # outlier.alpha tones down extreme urban spikes so the boxes are visible
  geom_boxplot(outlier.alpha = 0.2, outlier.size = 1, alpha = 0.8) +
  
  # Facet by site so they sit side-by-side
  facet_wrap(~ site) +
  
  # AUTOMATIC SIGNIFICANCE BRACKETS (Calculates the Wilcoxon test on the plot!)
  geom_signif(
    comparisons = list(c("Background Sector\n(Outside Cone)", "Direct Hit Sector\n(Inside Cone)")),
    test = "wilcox.test",
    map_signif_level = TRUE,  # Uses '**' for p < 0.01
    textsize = 5,
    vjust = -0.2,
    color = "black"
  ) +
  
  # Styling
  scale_fill_manual(values = c("Background Sector\n(Outside Cone)" = "gray70", 
                               "Direct Hit Sector\n(Inside Cone)" = "firebrick")) +
  scale_y_continuous(limits = c(0, 50)) + # Cap the Y axis to zoom in on the medians
  theme_classic() +
  labs(
    title = "B. Hourly PM2.5 by Wind Sector",
    subtitle = "Wilcoxon Rank-Sum tests (p < 0.01 **)",
    y = expression("PM"[2.5]*" Concentration ("*mu*"g/m"^3*")"),
    x = ""
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 11),
    axis.text.x = element_text(face = "bold", size = 10, color = "black")
  )

# ==========================================
# 4. COMBINE INTO COMPOSITE FIGURE
# ==========================================
# Use patchwork to place Panel A on the left and Panel B on the right
final_figure <- panel_a + panel_b + plot_layout(widths = c(1, 1.2))

# Display the plot
print(final_figure)

# Save the plot at high resolution (300 dpi is standard for journals like Environmental Research)
ggsave("Incinerator_Validation_Composite.png", plot = final_figure, 
       width = 14, height = 6, dpi = 300, bg = "white")
