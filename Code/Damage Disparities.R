# libraries####
library(tidycensus)
library(dplyr)
library(tidyverse)
library(readxl) # Added for reading Excel files
library(broom)
library(knitr)
library(purrr)
library(gtsummary)
library(gt)
library(ggplot2)
library(ggsci) # For Lancet color palette
library(stringr)
library(sf)
library(tigris)

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

#2a. univariate regression for mortality, stratified by pollutant####
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

#2b. multivariate regression for mortality, stratified by pollutant####
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


#3a. univariate and multivariate regresion for mortality, not stratified by pollutant####
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

#3b. Plot multivariate regression results####
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

