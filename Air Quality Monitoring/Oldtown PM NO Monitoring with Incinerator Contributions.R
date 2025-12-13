library(tidyverse)

# --- 1. Load and Process Data ---
# (Assuming files are in the working directory)
df_2014 <- read_csv("annual_24_510_0040_2014.csv")
df_2017 <- read_csv("annual_24_510_0040_2017.csv")
df_2020 <- read_csv("annual_24_510_0040_2020.csv")

all_data <- bind_rows(df_2014, df_2017, df_2020)

trend_data <- all_data %>%
  filter(
    (`Parameter Name` == "Oxides of nitrogen (NOx)" & `Duration Description` == "1 HOUR") |
      (`Parameter Name` == "PM2.5 - Local Conditions" & `Duration Description` == "24 HOUR")
  ) %>%
  select(Year, `Parameter Name`, `Arithmetic Mean`, `Units of Measure`) %>%
  distinct() %>%
  mutate(
    `Arithmetic Mean` = ifelse(
      `Parameter Name` == "Oxides of nitrogen (NOx)", 
      `Arithmetic Mean` * 1.88, 
      `Arithmetic Mean`
    ),
    `Units of Measure` = "Micrograms/cubic meter"
  )

# --- 2. Define NAAQS Standards ---
# Create a dataframe for the horizontal lines. 
# Key Step: Ensure column names match the facet variable (`Parameter Name`)
naaqs_limits <- tibble(
  `Parameter Name` = c("Oxides of nitrogen (NOx)", "PM2.5 - Local Conditions"),
  NAAQS_Value = c(53 * 1.88, 9.0) # NOx converted to ug/m3, PM2.5 is 9.0
)

# --- 3. Graph with NAAQS Lines ---
ggplot(trend_data, aes(x = Year, y = `Arithmetic Mean`, color = `Parameter Name`)) +
  geom_line(size = 1.2) +
  geom_point(size = 4) +
  
  # Add the NAAQS dotted lines
  geom_hline(data = naaqs_limits, aes(yintercept = NAAQS_Value), 
             linetype = "dotted", color = "black", size = 1) +
  
  scale_x_continuous(breaks = c(2014, 2017, 2020)) +
  labs(
    title = "Ambient Air Quality Trends vs NAAQS Standards",
    subtitle = "Dotted lines represent NAAQS",
    y = "Concentration (µg/m³)",
    x = "Year",
    caption = "Source: AQS Annual Data. *NOx converted from ppb using factor 1.88"
  ) +
  facet_wrap(~`Parameter Name`, scales = "free_y", ncol = 1) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )


library(tidyverse)

# 1. Create the dataset manually
incinerator_data <- data.frame(
  Year = c(2014, 2017, 2020, 2014, 2017, 2020),
  Pollutant = c("PM 2.5", "PM 2.5", "PM 2.5", "NOx", "NOx", "NOx"),
  Value = c(0.063, 0.068, 0.063, 0.320, 0.397, 0.312)
)

# 2. Graph the Trends
ggplot(incinerator_data, aes(x = Year, y = Value, color = Pollutant)) +
  geom_line(size = 1.2) +
  geom_point(size = 4) +
  # Facet wrap splits the graphs so the small NO values aren't hidden by the larger PM 2.5 values
  facet_wrap(~Pollutant, scales = "free_y", ncol = 1) +
  scale_x_continuous(breaks = c(2014, 2017, 2020)) +
  labs(
    title = "Incinerator Emission Ambient Concentrations (2014 - 2020)",
    y = "Emission Level",
    x = "Year"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )
