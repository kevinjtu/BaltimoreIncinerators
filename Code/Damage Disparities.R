# libraries####
library(tidycensus)
library(dplyr)
library(tidyverse)
library(readxl) 
library(broom)
library(knitr)
library(purrr)
library(gtsummary)
library(gt)
library(ggplot2)
library(ggsci) 
library(stringr)
library(sf)
library(tigris)
library(spdep)       # Added for spatial neighbor weights
library(spatialreg)  # Added for Spatial Error Models (errorsarlm)


#import data####
setwd("/Users/kevintu/Library/CloudStorage/OneDrive-UniversityofMarylandSchoolofMedicine/Climate Health Internship")
cases <- read_csv("Combined_Cases_by_Tract.csv")
svi <- read_csv("SVI_2022_US.csv") %>%
  # Select only the FIPS code, the overall and theme-level ranks, and the indicator percentages
  select(
    FIPS, RPL_THEMES, RPL_THEME1, RPL_THEME2, RPL_THEME3, RPL_THEME4,
    EP_POV150, EP_UNEMP, EP_HBURD, EP_NOHSDP, EP_UNINSUR,
    EP_AGE65, EP_AGE17, EP_DISABL, EP_SNGPNT, EP_LIMENG,
    EP_MINRTY, 
    EP_MUNIT, EP_MOBILE, EP_CROWD, EP_NOVEH, EP_GROUPQ
  ) %>%
  # Rename the columns to be more descriptive based on the SVI documentation
  rename(
    # --- Overall and Theme Percentile Ranks ---
    SVI = RPL_THEMES,                  # Overall percentile ranking 
    SVI_SES = RPL_THEME1,        # Percentile ranking for Socioeconomic Status theme 
    SVI_HC = RPL_THEME2,       # Percentile ranking for Household Characteristics theme 
    SVI_REMS = RPL_THEME3,      # Percentile ranking for Racial and Ethnic Minority Status theme 
    SVI_HTT = RPL_THEME4,    # Percentile ranking for Housing Type/ Transportation theme 
    
    # --- Socioeconomic Status Indicators ---
    Pct_Below_150_Poverty = EP_POV150,              # Percentage of persons below 150% poverty 
    Pct_Unemployed = EP_UNEMP,                      # Unemployment Rate estimate 
    Pct_Housing_Cost_Burden = EP_HBURD,             # Percentage of housing cost-burdened occupied housing units 
    Pct_No_HS_Diploma_Age25_Plus = EP_NOHSDP,       # Percentage of persons with no high school diploma (age 25+) 
    Pct_No_Health_Insurance = EP_UNINSUR,         # Percentage uninsured 
    
    # --- Household Characteristics Indicators ---
    Pct_Age_65_Plus = EP_AGE65,                     # Percentage of persons aged 65 and older 
    Pct_Age_17_Younger = EP_AGE17,                  # Percentage of persons aged 17 and younger 
    Pct_Disability = EP_DISABL,                     # Percentage of civilian noninstitutionalized population with a disability 
    Pct_Single_Parent_HH = EP_SNGPNT,             # Percentage of single-parent households 
    Pct_Limited_English = EP_LIMENG,                # Percentage of persons (age 5+) who speak English "less than well" 
    
    # --- Racial & Ethnic Minority Status Indicator ---
    Pct_Minority = EP_MINRTY,                       # Percentage minority 
    
    # --- Housing Type & Transportation Indicators ---
    Pct_Multi_Unit_Structures = EP_MUNIT,           # Percentage of housing in structures with 10 or more units 
    Pct_Mobile_Homes = EP_MOBILE,                   # Percentage of mobile homes 
    Pct_Crowding = EP_CROWD,                        # Percentage of occupied housing units with more people than rooms 
    Pct_No_Vehicle = EP_NOVEH,                      # Percentage of households with no vehicle available 
    Pct_Group_Quarters = EP_GROUPQ                  # Percentage of persons in group quarters 
  )

cases_svi <- cases %>%
  mutate(GEOID = as.character(GEOID)) %>%
  rename(FIPS = GEOID) %>%
  merge(svi, by = "FIPS")

cases_svi <- cases_svi %>%
  mutate(across(20:last_col(), ~na_if(., -999))) %>% 
  filter(!is.na(SVI))
  

#1. plot SVI data on baltimore####
baltimore_tracts <- tracts(state = "MD", county = "Baltimore city", cb = TRUE, year = 2021)

# Step 2: Prepare the SVI data from 'cases_svi'
# We need a unique SVI value for each GEOID (which is equivalent to FIPS here).
# Assuming 'SVI' is constant for a given FIPS/GEOID in your dataset.
svi_data_for_join <- cases_svi %>%
  rename(GEOID = FIPS) %>% # Rename FIPS to GEOID to match baltimore_tracts
  select(GEOID, SVI) %>%   # Select only the relevant columns
  distinct(GEOID, .keep_all = TRUE) # Get unique SVI for each GEOID

# Step 3: Join the SVI data with the Baltimore tracts spatial data
baltimore_map_with_svi <- baltimore_tracts %>%
  left_join(svi_data_for_join, by = "GEOID")

# --- Plotting the Map ---

# Step 4: Plot the map of Baltimore with SVI in each census tract
ggplot(baltimore_map_with_svi) +
  geom_sf(aes(fill = SVI), color = "black", linewidth = 0.2) + # Black outlines for tracts
  scale_fill_viridis_c( # Using viridis color scale for SVI (good for continuous data)
    option = "viridis", # You can choose other options like "plasma", "magma", "cividis", "inferno"
    name = "Social Vulnerability Index (SVI)",
    na.value = "grey80", # Color for tracts with missing SVI data
    direction = 1, # 1 for increasing values being brighter, -1 for reverse
    labels = scales::comma # Format numbers with commas (if SVI is large)
  ) +
  labs(
    title = "Social Vulnerability Index (SVI) by Census Tract",
    subtitle = "Baltimore City, MD"
  ) +
  theme_minimal() + # Use a minimalist theme
  theme(
    legend.position = "right", # Position the legend on the right
    panel.grid = element_blank(), # Remove grid lines
    axis.title = element_blank(), # Remove axis titles
    axis.text = element_blank(), # Remove axis text (latitude/longitude)
    axis.ticks = element_blank(), # Remove axis ticks
    panel.border = element_blank(), # Remove panel border
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14), # Center and bold title
    plot.subtitle = element_text(hjust = 0.5, size = 10) # Center subtitle
  )

#faceted by SVI theme
# 1. Plot SVI Theme data on Baltimore ####
options(tigris_use_cache = TRUE)
baltimore_tracts <- tracts(state = "MD", county = "Baltimore city", cb = TRUE, year = 2021)

# Step 2: Prepare the SVI Theme data from 'cases_svi'
# Select GEOID and the four theme percentile rankings
svi_theme_data <- cases_svi %>%
  rename(GEOID = FIPS) %>% 
  select(GEOID, SVI_SES, SVI_HC, SVI_REMS, SVI_HTT) %>%   
  distinct(GEOID, .keep_all = TRUE) 

# Step 3: Join the SVI data with the spatial tracts
baltimore_map_themes <- baltimore_tracts %>%
  left_join(svi_theme_data, by = "GEOID")

# Step 4: Pivot the spatial data into a "long" format for faceting
# (sf objects handle pivot_longer beautifully!)
baltimore_map_long <- baltimore_map_themes %>%
  pivot_longer(
    cols = c(SVI_SES, SVI_HC, SVI_REMS, SVI_HTT),
    names_to = "SVI_Theme",
    values_to = "Percentile_Rank"
  ) %>%
  mutate(
    # Rename the themes to clean, human-readable labels for the plot headers
    SVI_Theme = case_when(
      SVI_Theme == "SVI_SES" ~ "Socioeconomic Status",
      SVI_Theme == "SVI_HC" ~ "Household Characteristics",
      SVI_Theme == "SVI_REMS" ~ "Minority Status & Language",
      SVI_Theme == "SVI_HTT" ~ "Housing Type & Transportation"
    ),
    # Set the factor levels to strictly control the order they appear in the 2x2 grid
    SVI_Theme = factor(SVI_Theme, levels = c(
      "Socioeconomic Status",
      "Household Characteristics",
      "Minority Status & Language",
      "Housing Type & Transportation"
    ))
  )

# --- Plotting the Faceted Map ---

# Step 5: Plot the 2x2 map grid
ggplot(baltimore_map_long) +
  geom_sf(aes(fill = Percentile_Rank), color = "black", linewidth = 0.1) + 
  
  # Using the "magma" palette here to differentiate it from your overall SVI map (viridis)
  scale_fill_viridis_c( 
    option = "magma", 
    name = "Percentile Rank",
    na.value = "grey80", 
    direction = 1 
  ) +
  
  # This creates the 2x2 grid based on the themes
  facet_wrap(~ SVI_Theme, ncol = 4) + 
  
  labs(
    title = "Social Vulnerability Index (SVI) Themes by Census Tract",
    subtitle = "Baltimore City, MD"
  ) +
  
  # theme_void() is perfect for maps—it completely removes the background grid and axis coordinates
  theme_void() + 
  theme(
    legend.position = "right", 
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(t = 10, b = 5)), 
    plot.subtitle = element_text(hjust = 0.5, size = 12, margin = margin(b = 15)),
    strip.text = element_text(face = "bold", size = 11, margin = margin(b = 5, t = 5)), # Formats the facet headers
    legend.title = element_text(face = "bold")
  )

#2a. DEPRACATED univariate regression for mortality, stratified by pollutant####
# change mortality to rate
cases_svi <- cases_svi %>%
  mutate(additional_cases_rate = additional_cases * 1000)

# Define SVI variables
svi_vars <- colnames(cases_svi)[25:40]

# Filter to endpoint == All-cause mortality
mort_data <- cases_svi %>% 
  filter(endpoint == "All-cause mortality", !is.na(additional_cases))

results_list <- list()

# Run univariate models and collect results
# Loop over pollutants
for (poll in unique(mort_data$pollutant)) {
  # Subset data for this pollutant
  data_poll <- mort_data %>% filter(pollutant == poll)
  
  # Loop over conditions
  for (cond in unique(data_poll$condition)) {
    data_cond <- data_poll %>% filter(condition == cond)
    
    # Loop over SVI vars
    for (svi_term in svi_vars) {
      # Construct formula for univariate logistic regression

      # Fit model
      fit <- glm(as.formula(paste0("additional_cases_rate ~ ", svi_term)), 
                 data = data_cond, 
                 family = gaussian())
      
      summary(fit)
      
      # Extract values
      coef_est <- coef(summary(fit))[2, ]
      or <- exp(coef_est["Estimate"])
      ci <- exp(confint(fit)[2, ])
      pval <- coef_est["Pr(>|t|)"]
      
      # Extract info
      coef_info <- tidy(fit, conf.int = TRUE, exponentiate = TRUE) %>% 
        filter(term == svi_term) %>%
        mutate(
          pollutant = poll,
          condition = cond,
          svi_variable = svi_term
        ) %>%
        select(pollutant, svi_variable, condition, estimate, conf.low, conf.high, p.value)
      
      results_list[[length(results_list) + 1]] <- coef_info
      
    }
  }
}

# Combine all into one dataframe
results_df <- bind_rows(results_list)

#2b. DEPRACATED multivariate regression for mortality, stratified by pollutant####
# change mortality to rate
cases_svi <- cases_svi %>%
  mutate(additional_cases_rate = additional_cases * 1000)

# Define SVI variables
svi_vars <- c("SVI_SES", "SVI_HC", "SVI_REMS", "SVI_HTT")

# Filter to endpoint == All-cause mortality
mort_data <- cases_svi %>% 
  filter(endpoint == "All-cause mortality", !is.na(additional_cases))

results_list <- list()

# Run univariate models and collect results
# Loop over pollutants
for (poll in unique(mort_data$pollutant)) {
  # Subset data for this pollutant
  data_poll <- mort_data %>% filter(pollutant == poll)
  
  # Loop over conditions
  for (cond in unique(data_poll$condition)) {
    data_cond <- data_poll %>% filter(condition == cond)
    
    # Loop over SVI vars
    for (svi_term in svi_vars) {
      # Construct formula for univariate logistic regression
      
      # Fit model
      fit <- glm(as.formula(paste0("additional_cases_rate ~ ", svi_term)), 
                 data = data_cond, 
                 family = gaussian())
      
      summary(fit)
      
      # Extract values
      coef_est <- coef(summary(fit))[2, ]
      or <- exp(coef_est["Estimate"])
      ci <- exp(confint(fit)[2, ])
      pval <- coef_est["Pr(>|t|)"]
      
      # Extract info
      coef_info <- tidy(fit, conf.int = TRUE, exponentiate = TRUE) %>% 
        filter(term == svi_term) %>%
        mutate(
          pollutant = poll,
          condition = cond,
          svi_variable = svi_term
        ) %>%
        select(pollutant, svi_variable, condition, estimate, conf.low, conf.high, p.value)
      
      results_list[[length(results_list) + 1]] <- coef_info
      
    }
  }
}

# Combine all into one dataframe
results_df <- bind_rows(results_list)

# Pivot wider so each condition is a column
results_wide <- results_df %>%
  mutate(across(c(estimate, conf.low, conf.high, p.value), round, 3)) %>%
  mutate(
    results = paste0(
      estimate, " (", conf.low, "–", conf.high, "), ",
      ifelse(p.value < 0.05, "**p = ", "p = "), p.value,
      ifelse(p.value < 0.05, "**", "")
    )
  ) %>%
  select(pollutant, svi_variable, condition, results) %>%
  pivot_wider(names_from = condition, values_from = results)


#select significant variables from univariate analysis in multivariate
# Filter significant results
# Filter to only significant results
significant_results <- results_df %>%
  filter(p.value < 0.05)

# Create nested list: pollutant → condition → list of significant SVI variables
significant_svi_by_pollutant_and_condition <- significant_results %>%
  distinct(pollutant, condition, svi_variable) %>%
  group_by(pollutant, condition) %>%
  summarise(svi_variables = list(unique(svi_variable)), .groups = "drop") %>%
  tidyr::nest(data = c(condition, svi_variables)) %>%
  mutate(condition_lists = purrr::map(data, ~ deframe(.x))) %>%
  select(pollutant, condition_lists) %>%
  deframe()

# change mortality to rate
cases_svi <- cases_svi %>%
  mutate(additional_cases_rate = additional_cases * 1000)

# Filter to endpoint == All-cause mortality
mort_data <- cases_svi %>% 
  filter(endpoint == "All-cause mortality", !is.na(additional_cases))

model_results <- list()

# Run univariate models and collect results
# Loop over pollutants
for (poll in unique(mort_data$pollutant)) {
  # Subset data for this pollutant
  data_poll <- mort_data %>% filter(pollutant == poll)
  
  # Loop over conditions
  for (cond in unique(data_poll$condition)) {
    # Subset by condition
    data_cond <- data_poll %>% filter(condition == cond)
    
    # Get the list of SVI variables for this pollutant-condition pair
    svi_vars <- significant_svi_by_pollutant_and_condition[[poll]][[cond]]
    
    # Construct the formula as a string
    svi_formula_part <- paste(svi_vars, collapse = " + ")
    full_formula_str <- paste0("additional_cases_rate ~ ", svi_formula_part, " + Final_Conc")
    
    # Convert to formula object
    formula_obj <- as.formula(full_formula_str)
    
    # Fit the model
    fit <- glm(formula_obj, data = data_cond, family = gaussian())
    
    # Extract model stats
    tidy_fit <- tidy(fit, conf.int = TRUE) %>%
      mutate(pollutant = poll,
             condition = cond)
    
    # Append to results list
    model_results[[paste(poll, cond, sep = "_")]] <- tidy_fit

    }
  }

# Combine all into one dataframe
all_model_results <- bind_rows(model_results)


#3a. DEPRACATED univariate and multivariate regresion for mortality, not stratified by pollutant####
svi_vars <- colnames(cases_svi)[25:40]

aggregated_mort_data <- cases_svi %>%
  filter(endpoint == "All-cause mortality", !is.na(additional_cases)) %>%
  mutate(additional_cases_rate = additional_cases * 100000) %>%
  group_by(FIPS, condition) %>%
  summarise(
    # Sum additional cases rate across all pollutants for each tract and condition
    total_additional_cases_rate = sum(additional_cases_rate, na.rm = TRUE),
    # Dynamically keep one instance of all specified SVI variables per tract
    # using 'across' and 'all_of' to select the columns defined in svi_vars.
    # We use mean() as an aggregation, assuming SVI values are constant per FIPS/GEOID
    # or that an average is appropriate if slight variations exist across rows for the same FIPS.
    across(all_of(svi_vars), ~mean(.x, na.rm = TRUE)),
    # Also include Final_Conc, aggregated by mean if it's a predictor
    Final_Conc = mean(Final_Conc, na.rm = TRUE),
    total_pop = mean(total_pop, na.rm = TRUE),
    .groups = "drop" # Drop grouping after summarizing
  )

# --- Univariate Regression Analysis (without pollutant stratification) ---
univariate_results_list <- list()

# Loop over conditions
for (cond in unique(aggregated_mort_data$condition)) {
  data_cond <- aggregated_mort_data %>% filter(condition == cond)
  
  # Loop over SVI vars (now defined from columns 25:40)
  for (svi_term in svi_vars) {
    # Check if the SVI term exists in the data_cond before building formula and fitting
    if (!svi_term %in% names(data_cond)) {
      warning(paste("SVI variable '", svi_term, "' not found in data for condition '", cond, "'. Skipping.", sep = ""))
      next
    }
    # Construct formula
    formula_str <- paste0("total_additional_cases_rate ~ ", svi_term)
    # Fit model
    fit <- glm(as.formula(formula_str),
               data = data_cond,
               family = gaussian()) # Using gaussian for continuous outcome
    
    # Extract info using tidy and save
    coef_info <- tidy(fit, conf.int = TRUE) %>%
      filter(term == svi_term) %>% # Filter for the specific SVI term
      mutate(
        condition = cond,
        svi_variable = svi_term
      ) %>%
      select(svi_variable, condition, estimate, conf.low, conf.high, p.value)
    
    univariate_results_list[[length(univariate_results_list) + 1]] <- coef_info
  }
}

# Combine all univariate results into one dataframe
univariate_results_df <- bind_rows(univariate_results_list)

# --- Process Univariate Results for Multivariate Model ---
# Format univariate results for display (similar to your original code)
univariate_results_wide <- univariate_results_df %>%
  mutate(across(c(estimate, conf.low, conf.high, p.value), round, 3)) %>%
  mutate(
    results = paste0(
      estimate, " (", conf.low, "–", conf.high, "), ",
      ifelse(p.value < 0.05, "**p = ", "p = "), p.value,
      ifelse(p.value < 0.05, "**", "")
    )
  ) %>%
  select(svi_variable, condition, results) %>%
  pivot_wider(names_from = condition, values_from = results)

print("Univariate Regression Results:")
print(univariate_results_wide)

# Select significant variables from univariate analysis for multivariate
significant_univariate_svi <- univariate_results_df %>%
  filter(p.value < 0.05) %>%
  distinct(condition, svi_variable) %>%
  group_by(condition) %>%
  summarise(svi_variables = list(unique(svi_variable)), .groups = "drop") %>%
  tidyr::nest(data = c(condition, svi_variables)) %>%
  mutate(condition_lists = purrr::map(data, ~ deframe(.x))) %>%
  select(condition_lists) %>%
  pull() # Pull the list of significant SVI variables for each condition

# --- Multivariate Regression Analysis (without pollutant stratification) ---
multivariate_model_results <- list()

# Loop over conditions
for (cond in unique(aggregated_mort_data$condition)) {
  # Subset by condition
  data_cond <- aggregated_mort_data %>% filter(condition == cond)
  
  # Get the list of significant SVI variables for this condition
  # *** THE CRITICAL CHANGE IS HERE: significant_univariate_svi[[1]][[cond]] ***
  svi_vars_for_model <- significant_univariate_svi[[1]][[cond]]
  
  if (is.null(svi_vars_for_model) || length(svi_vars_for_model) == 0) {
    message(paste("No significant SVI variables found for condition:", cond, ". Skipping multivariate model for this condition."))
    next # Skip to next condition if no significant SVI variables
  }
  
  # Ensure all variables in svi_vars_for_model exist in data_cond before constructing formula
  svi_vars_for_model_exist <- svi_vars_for_model[svi_vars_for_model %in% names(data_cond)]
  if (length(svi_vars_for_model_exist) == 0) {
    message(paste("None of the significant SVI variables for condition '", cond, "' found in data. Skipping multivariate model.", sep = ""))
    next
  }
  
  # Construct the formula as a string, including Final_Conc
  svi_formula_part <- paste(svi_vars_for_model_exist, collapse = " + ")
  full_formula_str <- paste0("total_additional_cases_rate ~ ", svi_formula_part, " + Final_Conc + total_pop")
  
  # Convert to formula object
  formula_obj <- as.formula(full_formula_str)
  
  # Fit the model
  fit <- glm(formula_obj, data = data_cond, family = gaussian())
  
  # Extract model stats
  tidy_fit <- tidy(fit, conf.int = TRUE) %>%
    mutate(condition = cond)
  
  # Append to results list
  multivariate_model_results[[cond]] <- tidy_fit
}

# Combine all multivariate model results into one dataframe
all_multivariate_model_results <- bind_rows(multivariate_model_results)

#3b. DEPRACATED Plot multivariate regression results####
# This initial preparation is common for both plots
base_plot_data <- all_multivariate_model_results %>%
  # Filter out the intercept and Final_Conc terms
  filter(term != "(Intercept)", term != "Final_Conc", term != "total_pop") %>%
  # Create a new column 'svi_theme' based on the SVI variable name (term)
  mutate(
    svi_theme = case_when(
      term %in% c(
        "Pct_Below_150_Poverty", "Pct_Unemployed", "Pct_Housing_Cost_Burden",
        "Pct_No_HS_Diploma_Age25_Plus", "Pct_No_Health_Insurance"
      ) ~ "Socioeconomic Status (SES)",
      term %in% c(
        "Pct_Age_65_Plus", "Pct_Age_17_Younger", "Pct_Disability",
        "Pct_Single_Parent_HH", "Pct_Limited_English"
      ) ~ "Household Composition & Disability (HC)",
      term %in% c(
        "Pct_Minority"
      ) ~ "Minority Status & Language (MSL)",
      term %in% c(
        "Pct_Multi_Unit_Structures", "Pct_Mobile_Homes", "Pct_Crowding",
        "Pct_No_Vehicle", "Pct_Group_Quarters"
      ) ~ "Housing Type & Transportation (HTT)",
      TRUE ~ "Other SVI Factor" # Fallback for any un-categorized terms
    )
  ) %>%
  # Rename terms for plotting labels (remove "Pct_" prefix, replace underscores)
  mutate(
    clean_term = term %>%
      stringr::str_remove("^Pct_") %>% # Remove "Pct_" from the beginning
      stringr::str_replace_all("_", " ") # Replace all underscores with spaces
  )

# Ensure SVI theme factor levels are set for consistent legend order
svi_theme_order_levels <- c(
  "Socioeconomic Status (SES)",
  "Household Composition & Disability (HC)",
  "Minority Status & Language (MSL)",
  "Housing Type & Transportation (HTT)",
  "Other SVI Factor"
)

base_plot_data <- base_plot_data %>%
  mutate(svi_theme = factor(svi_theme, levels = svi_theme_order_levels))


# 2. Prepare data for 'Typical Controlled' plot
plot_data_controlled <- base_plot_data %>%
  filter(condition == "Typical Controlled") %>%
  # Order the terms by their 'estimate' (beta) specifically for this subset
  arrange(estimate) %>%
  mutate(
    term_ordered = factor(clean_term, levels = unique(clean_term))
  )

# 3. Prepare data for 'Typical Uncontrolled' plot
plot_data_uncontrolled <- base_plot_data %>%
  filter(condition == "Typical Uncontrolled") %>%
  # Order the terms by their 'estimate' (beta) specifically for this subset
  arrange(estimate) %>%
  mutate(
    term_ordered = factor(clean_term, levels = unique(clean_term))
  )

# --- Create Plot for 'Typical Controlled' ---
plot_controlled <- ggplot(plot_data_controlled, aes(x = estimate, y = term_ordered, color = svi_theme)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  scale_color_lancet(name = "SVI Theme") +
  labs(
    title = "Multivariate Regression Beta Estimates (and 95% CIs)",
    subtitle = "Condition: Typical Controlled (Aggregated Mortality by Tract)",
    x = "Beta Estimate",
    y = "SVI Variable"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

# Print the "Typical Controlled" plot
print(plot_controlled)


# --- Create Plot for 'Typical Uncontrolled' ---
plot_uncontrolled <- ggplot(plot_data_uncontrolled, aes(x = estimate, y = term_ordered, color = svi_theme)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  scale_color_lancet(name = "SVI Theme") +
  labs(
    title = "Multivariate Regression Beta Estimates (and 95% CIs)",
    subtitle = "Condition: Typical Uncontrolled (Aggregated Mortality by Tract)",
    x = "Beta Estimate",
    y = "SVI Variable"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

# Print the "Typical Uncontrolled" plot
print(plot_uncontrolled)


#4a. DEPRACATED Univariate and multivariate regression for economic costs, in Baltimore, Typical Uncontrolled####
setwd("/Users/kevintu/Library/CloudStorage/OneDrive-UniversityofMarylandSchoolofMedicine/Climate Health Internship")

# 1. Load economic data
baltimore_costs <- read_excel("Health Burdens by Baltimore Tract.xlsx") %>%
  rename(FIPS = GEOID) %>%
  mutate(FIPS = as.character(FIPS))

svi_vars <- colnames(cases_svi)[25:40] # Ensures we are grabbing the 16 SVI columns

# 2. Aggregate cases_svi to the FIPS level for "Typical Uncontrolled" and join with costs
aggregated_cost_data <- cases_svi %>%
  filter(condition == "Typical Uncontrolled") %>%
  # Group by FIPS to get one row per tract
  group_by(FIPS) %>%
  summarise(
    # SVI values are constant per tract, but we use mean() to safely aggregate
    across(all_of(svi_vars), ~mean(.x, na.rm = TRUE)),
    Final_Conc = mean(Final_Conc, na.rm = TRUE),
    total_pop = mean(total_pop, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Join with the economic costs data
  inner_join(baltimore_costs, by = "FIPS")

# --- Univariate Regression Analysis for Economic Costs ---
univariate_cost_results_list <- list()

for (svi_term in svi_vars) {
  # Construct formula: modeling total economic cost by the specific SVI term
  formula_str <- paste0("total_economic_cost ~ ", svi_term)
  
  # Fit Gaussian model
  fit <- glm(as.formula(formula_str),
             data = aggregated_cost_data,
             family = Gamma(link = "log"))
  
  # Extract info using tidy
  coef_info <- tidy(fit, conf.int = TRUE) %>%
    filter(term == svi_term) %>%
    mutate(svi_variable = svi_term) %>%
    select(svi_variable, estimate, conf.low, conf.high, p.value)
  
  univariate_cost_results_list[[length(univariate_cost_results_list) + 1]] <- coef_info
}

# Combine all univariate results into a dataframe
univariate_cost_results_df <- bind_rows(univariate_cost_results_list)

# Format univariate results for easy reading
univariate_cost_results_formatted <- univariate_cost_results_df %>%
  mutate(across(c(estimate, conf.low, conf.high, p.value), round, 3)) %>%
  mutate(
    results = paste0(
      estimate, " (", conf.low, "–", conf.high, "), ",
      ifelse(p.value < 0.05, "**p = ", "p = "), p.value,
      ifelse(p.value < 0.05, "**", "")
    )
  ) %>%
  select(svi_variable, results)

print("--- Univariate Regression Results for Economic Cost ---")
print(univariate_cost_results_formatted)


# --- Multivariate Regression Analysis for Economic Costs ---
# Filter to only keep significant variables from the univariate analysis
significant_cost_svi <- univariate_cost_results_df %>%
  filter(p.value < 0.05) %>%
  pull(svi_variable)

if (length(significant_cost_svi) == 0) {
  message("No significant SVI variables found in univariate analysis. Skipping multivariate model.")
} else {
  # Construct the formula string with significant SVI vars + controls
  # Note: It is crucial to adjust for total_pop, as absolute economic cost is highly dependent on tract population size
  svi_formula_part <- paste(significant_cost_svi, collapse = " + ")
  full_formula_str <- paste0("total_economic_cost ~ ", svi_formula_part, " + Final_Conc + total_pop")
  
  # Fit the multivariate model
  multivariate_cost_fit <- glm(
    as.formula(full_formula_str), 
    data = aggregated_cost_data, 
    family = Gamma(link = "log")
  )
  
  # Extract and format multivariate results
  multivariate_cost_results <- tidy(multivariate_cost_fit, conf.int = TRUE, exponentiate = TRUE) %>%
    mutate(across(where(is.numeric), round, 3)) %>%
    mutate(
      significance = ifelse(p.value < 0.05, "Significant", "Not Significant")
    )
  
  print("--- Multivariate Regression Results for Economic Cost ---")
  print(multivariate_cost_results)
}

multivariate_cost_results <- tidy(multivariate_cost_fit, conf.int = TRUE, )
#5a. spatial multivariate for mortlity rates in MD/DC study region####

# (Assuming your other libraries like dplyr, tidyverse, broom, ggplot2 are already loaded)

# 1. Pull Spatial Data for Maryland and DC
options(tigris_use_cache = TRUE)
md_tracts <- tracts(state = "MD", cb = TRUE, year = 2021)
dc_tracts <- tracts(state = "DC", cb = TRUE, year = 2021)
dmv_tracts <- rbind(md_tracts, dc_tracts)

svi_vars <- colnames(cases_svi)[25:40]

# Aggregate data to the tract level
aggregated_mort_data <- cases_svi %>%
  filter(endpoint == "All-cause mortality", !is.na(additional_cases)) %>%
  mutate(additional_cases_rate = additional_cases * 100000) %>%
  group_by(FIPS, condition) %>%
  summarise(
    total_additional_cases_rate = sum(additional_cases_rate, na.rm = TRUE),
    across(all_of(svi_vars), ~mean(.x, na.rm = TRUE)),
    Final_Conc = mean(Final_Conc, na.rm = TRUE),
    total_pop = mean(total_pop, na.rm = TRUE),
    .groups = "drop"
  )

multivariate_model_results <- list()
univariate_results_list <- list()

# Loop over conditions
for (cond in unique(aggregated_mort_data$condition)) {
  
  # 1. Prepare Spatial Data for the specific condition
  data_cond <- aggregated_mort_data %>% filter(condition == cond)
  
  # Inner join ensures we only keep tracts with BOTH map geometry and your health data
  spatial_data_cond <- dmv_tracts %>%
    inner_join(data_cond, by = c("GEOID" = "FIPS"))
  
  # Create Spatial Weights Matrix
  neighbors <- poly2nb(spatial_data_cond, queen = TRUE)
  weights_list <- nb2listw(neighbors, style = "W", zero.policy = TRUE)
  
  # --- Spatial Univariate Screen ---
  significant_svi_for_multi <- c()
  
  for (svi_term in svi_vars) {
    if (!svi_term %in% names(spatial_data_cond)) next
    
    # Formula: testing one SVI term, controlling for population
    formula_str <- paste0("total_additional_cases_rate ~ ", svi_term, " + total_pop")
    
    # Fit SEM
    fit_uni <- errorsarlm(as.formula(formula_str),
                          data = spatial_data_cond,
                          listw = weights_list,
                          zero.policy = TRUE)
    
    # Extract coefficients
    coef_summary <- summary(fit_uni)$Coef
    p_val <- coef_summary[svi_term, "Pr(>|z|)"]
    est <- coef_summary[svi_term, "Estimate"]
    se <- coef_summary[svi_term, "Std. Error"]
    
    # Save univariate results for viewing
    univariate_results_list[[length(univariate_results_list) + 1]] <- data.frame(
      condition = cond,
      svi_variable = svi_term,
      estimate = est,
      conf.low = est - (1.96 * se),
      conf.high = est + (1.96 * se),
      p.value = p_val
    )
    
    # Keep if significant
    if (p_val < 0.05) {
      significant_svi_for_multi <- c(significant_svi_for_multi, svi_term)
    }
  }
  
  # --- Spatial Multivariate Model ---
  if (length(significant_svi_for_multi) == 0) {
    message(paste("No significant SVI variables found for condition:", cond))
    next
  }
  
  # Construct multivariate formula (Significant SVI vars + Controls)
  svi_formula_part <- paste(significant_svi_for_multi, collapse = " + ")
  full_formula_str <- paste0("total_additional_cases_rate ~ ", svi_formula_part, " + Final_Conc + total_pop")
  
  # Fit Multivariate SEM
  fit_multi <- errorsarlm(as.formula(full_formula_str), 
                          data = spatial_data_cond, 
                          listw = weights_list, 
                          zero.policy = TRUE)
  
  # Extract and format multivariate results manually to mimic broom::tidy()
  multi_coefs <- summary(fit_multi)$Coef
  
  tidy_fit <- data.frame(
    term = rownames(multi_coefs),
    estimate = multi_coefs[, "Estimate"],
    std.error = multi_coefs[, "Std. Error"],
    statistic = multi_coefs[, "z value"],
    p.value = multi_coefs[, "Pr(>|z|)"],
    condition = cond
  ) %>%
    mutate(
      conf.low = estimate - (1.96 * std.error),
      conf.high = estimate + (1.96 * std.error)
    )
  
  multivariate_model_results[[cond]] <- tidy_fit
}

# Combine all multivariate model results into one dataframe
all_multivariate_model_results <- bind_rows(multivariate_model_results)
rownames(all_multivariate_model_results) <- NULL

# Plotting Function for the Full Study Region (MD + DC)
create_regional_plot <- function(data, condition_name) {
  plot_data <- data %>%
    filter(condition == condition_name) %>%
    arrange(estimate) %>%
    mutate(
      term_ordered = factor(clean_term, levels = unique(clean_term)),
      # 1. Create formatted text label: beta (conf.low, conf.high)
      label_text = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high)
    )
  
  if(nrow(plot_data) == 0) return(message(paste("No plot data for", condition_name)))
  
  ggplot(plot_data, aes(x = estimate, y = term_ordered, color = svi_theme)) +
    geom_point(size = 3) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
    
    # 2. Add text labels to the right of the upper confidence interval
    geom_text(aes(x = conf.high, label = label_text), 
              hjust = -0.15,        # Nudge text to the right
              size = 3.5,           # Text size
              color = "black",      # Keep text black
              show.legend = FALSE) + 
    
    scale_color_lancet(name = "SVI Theme") +
    
    # 3. Expand the x-axis specifically on the right side to prevent text cutoff
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.35))) + 
    
    labs(
      title = "Maryland & DC: Spatial Multivariate Beta Estimates",
      subtitle = paste("Condition:", condition_name, "(Aggregated Mortality by Tract)"),
      x = "Beta Estimate (Change in additional cases per 100k)",
      y = "SVI Variable"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 12, face = "bold"),
      axis.title.y = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(face = "bold")
    )
}

# Print the Regional Plots
plot_controlled_regional <- create_regional_plot(base_plot_data, "Typical Controlled")
print(plot_controlled_regional)

plot_uncontrolled_regional <- create_regional_plot(base_plot_data, "Typical Uncontrolled")
print(plot_uncontrolled_regional)
#5b. spatial multivariate for mortlity rates in Baltimore####
# 1. Pull Spatial Data ONLY for Baltimore City
options(tigris_use_cache = TRUE)
baltimore_tracts <- tracts(state = "MD", county = "Baltimore city", cb = TRUE, year = 2021)

svi_vars <- colnames(cases_svi)[25:40]

# Aggregate data to the tract level
aggregated_mort_data_bmore <- cases_svi %>%
  filter(endpoint == "All-cause mortality", !is.na(additional_cases)) %>%
  mutate(additional_cases_rate = additional_cases * 100000) %>%
  group_by(FIPS, condition) %>%
  summarise(
    total_additional_cases_rate = sum(additional_cases_rate, na.rm = TRUE),
    across(all_of(svi_vars), ~mean(.x, na.rm = TRUE)),
    Final_Conc = mean(Final_Conc, na.rm = TRUE),
    total_pop = mean(total_pop, na.rm = TRUE),
    .groups = "drop"
  )

multivariate_model_results_bmore <- list()
univariate_results_list_bmore <- list()

# Loop over conditions
for (cond in unique(aggregated_mort_data_bmore$condition)) {
  
  data_cond <- aggregated_mort_data_bmore %>% filter(condition == cond)
  
  # The inner_join acts as our geographic filter. 
  # It automatically drops any Maryland/DC tracts not in 'baltimore_tracts'
  spatial_data_cond <- baltimore_tracts %>%
    inner_join(data_cond, by = c("GEOID" = "FIPS"))
  
  # Create Spatial Weights Matrix for Baltimore
  neighbors <- poly2nb(spatial_data_cond, queen = TRUE)
  weights_list <- nb2listw(neighbors, style = "W", zero.policy = TRUE)
  
  # --- Spatial Univariate Screen ---
  significant_svi_for_multi <- c()
  
  for (svi_term in svi_vars) {
    if (!svi_term %in% names(spatial_data_cond)) next
    
    # Formula: testing one SVI term, controlling for population
    formula_str <- paste0("total_additional_cases_rate ~ ", svi_term, " + total_pop")
    
    # Fit SEM
    fit_uni <- errorsarlm(as.formula(formula_str),
                          data = spatial_data_cond,
                          listw = weights_list,
                          zero.policy = TRUE)
    
    # Extract coefficients
    coef_summary <- summary(fit_uni)$Coef
    p_val <- coef_summary[svi_term, "Pr(>|z|)"]
    est <- coef_summary[svi_term, "Estimate"]
    se <- coef_summary[svi_term, "Std. Error"]
    
    # Save univariate results
    univariate_results_list_bmore[[length(univariate_results_list_bmore) + 1]] <- data.frame(
      condition = cond,
      svi_variable = svi_term,
      estimate = est,
      conf.low = est - (1.96 * se),
      conf.high = est + (1.96 * se),
      p.value = p_val
    )
    
    # Keep if significant
    if (p_val < 0.05) {
      significant_svi_for_multi <- c(significant_svi_for_multi, svi_term)
    }
  }
  
  # --- Spatial Multivariate Model ---
  if (length(significant_svi_for_multi) == 0) {
    message(paste("No significant SVI variables found for condition:", cond, "in Baltimore City."))
    next
  }
  
  # Construct multivariate formula (Significant SVI vars + Controls)
  svi_formula_part <- paste(significant_svi_for_multi, collapse = " + ")
  full_formula_str <- paste0("total_additional_cases_rate ~ ", svi_formula_part, " + Final_Conc + total_pop")
  
  # Fit Multivariate SEM
  fit_multi <- errorsarlm(as.formula(full_formula_str), 
                          data = spatial_data_cond, 
                          listw = weights_list, 
                          zero.policy = TRUE)
  
  # Extract and format multivariate results
  multi_coefs <- summary(fit_multi)$Coef
  
  tidy_fit <- data.frame(
    term = rownames(multi_coefs),
    estimate = multi_coefs[, "Estimate"],
    std.error = multi_coefs[, "Std. Error"],
    statistic = multi_coefs[, "z value"],
    p.value = multi_coefs[, "Pr(>|z|)"],
    condition = cond
  ) %>%
    mutate(
      conf.low = estimate - (1.96 * std.error),
      conf.high = estimate + (1.96 * std.error)
    )
  
  multivariate_model_results_bmore[[cond]] <- tidy_fit
}

# Combine all multivariate model results into one dataframe
all_multivariate_model_results_bmore <- bind_rows(multivariate_model_results_bmore)
rownames(all_multivariate_model_results_bmore) <- NULL


base_plot_data_bmore <- all_multivariate_model_results_bmore %>%
  filter(term != "(Intercept)", term != "Final_Conc", term != "total_pop") %>%
  mutate(
    svi_theme = case_when(
      term %in% c("Pct_Below_150_Poverty", "Pct_Unemployed", "Pct_Housing_Cost_Burden", "Pct_No_HS_Diploma_Age25_Plus", "Pct_No_Health_Insurance") ~ "Socioeconomic Status (SES)",
      term %in% c("Pct_Age_65_Plus", "Pct_Age_17_Younger", "Pct_Disability", "Pct_Single_Parent_HH", "Pct_Limited_English") ~ "Household Composition & Disability (HC)",
      term %in% c("Pct_Minority") ~ "Minority Status & Language (MSL)",
      term %in% c("Pct_Multi_Unit_Structures", "Pct_Mobile_Homes", "Pct_Crowding", "Pct_No_Vehicle", "Pct_Group_Quarters") ~ "Housing Type & Transportation (HTT)",
      TRUE ~ "Other SVI Factor"
    )
  ) %>%
  mutate(
    clean_term = term %>%
      stringr::str_remove("^Pct_") %>% 
      stringr::str_replace_all("_", " ") 
  )

svi_theme_order_levels <- c(
  "Socioeconomic Status (SES)",
  "Household Composition & Disability (HC)",
  "Minority Status & Language (MSL)",
  "Housing Type & Transportation (HTT)",
  "Other SVI Factor"
)

base_plot_data_bmore <- base_plot_data_bmore %>%
  mutate(svi_theme = factor(svi_theme, levels = svi_theme_order_levels))

# Plotting Function for typical scenarios
create_bmore_plot <- function(data, condition_name) {
  plot_data <- data %>%
    filter(condition == condition_name) %>%
    arrange(estimate) %>%
    mutate(
      term_ordered = factor(clean_term, levels = unique(clean_term)),
      # 1. Create formatted text label: beta (conf.low, conf.high)
      # Using %.2f keeps exactly 2 decimal places for a clean look
      label_text = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high)
    )
  
  if(nrow(plot_data) == 0) return(message(paste("No plot data for", condition_name)))
  
  ggplot(plot_data, aes(x = estimate, y = term_ordered, color = svi_theme)) +
    geom_point(size = 3) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
    
    # 2. Add text labels to the right of the upper confidence interval
    geom_text(aes(x = conf.high, label = label_text), 
              hjust = -0.15,        # Nudge the text slightly to the right of the error bar
              size = 3.5,           # Adjust text size to fit your plot
              color = "black",      # Keep text black for readability
              show.legend = FALSE) + # Don't add an 'a' to your SVI theme legend

scale_color_lancet(name = "SVI Theme") +
  
  # 3. Expand the x-axis specifically on the right side to make room for the text
  # mult = c(left_expansion, right_expansion)
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.35))) + 
  
  labs(
    title = "Baltimore City: Spatial Multivariate Beta Estimates",
    subtitle = paste("Condition:", condition_name, "(Aggregated Mortality by Tract)"),
    x = "Beta Estimate (Change in additional cases per 100k)",
    y = "SVI Variable"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )
}

# Print the plots
plot_controlled_bmore <- create_bmore_plot(base_plot_data_bmore, "Typical Controlled")
print(plot_controlled_bmore)

plot_uncontrolled_bmore <- create_bmore_plot(base_plot_data_bmore, "Typical Uncontrolled")
print(plot_uncontrolled_bmore)