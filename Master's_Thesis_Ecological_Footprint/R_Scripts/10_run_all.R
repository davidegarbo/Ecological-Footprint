# ============================================================
# 📘 Master's Thesis: Ecological Footprint
# Author: Davide Garbo

# ---- 0) Packages -------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(janitor)
library(ggplot2)
library(here)        
library(GGally)
library(ggcorrplot)
library(corrplot)
library(maps)
library(gridExtra)
library(glue)

# ---- 1) Load data ------------------------------------------------------
# Put your file at: data/final_dataset2.csv
# If your name differs, change below accordingly
raw = read.csv("D:\\DAVIDE\\Università\\Master\\UNIBG\\Tesi\\Master's_Thesis_Ecological_Footprint\\Raw_Dataset\\raw_dataset.csv", header = T, sep = ",", na.strings = c("NA", "#N/A","", "N/A"))


glimpse(raw)

# ---- 2) Select relevant columns (original names) ----------------------
ds <- raw %>%
  dplyr::select(
    Country, SDGi, Life.Expectancy, HDI, Per.Capita.GDP, Region, Income.Group,
    Population, Total.Ecological.Footprint..Consumption., Year, Other.renewables,
    Biofuels.consumption, Solar.consumption, Wind.consumption, Hydro.consumption,
    Nuclear.consumption, Gas.consumption, Coal.consumption, Oil.consumption,
    CO2.emissions, Urbanization
  )

# ---- 3) Type conversions (handle commas as decimal if present) --------
# GDP may include $ or commas; Population may include thousands separators.
# Other indicators may use comma as decimal separator.

# Helper to convert possible comma-decimals safely
comma_to_numeric <- function(x) as.numeric(gsub(",", ".", x))

# Clean numeric-like columns by original names BEFORE clean_names()
ds <- ds %>%
  mutate(
    `Per.Capita.GDP` = as.numeric(gsub("[\\$,]", "", `Per.Capita.GDP`)),
    Population = as.numeric(gsub("[^0-9.]", "", gsub(",", "", Population)))
  ) %>%
  mutate(across(
    c(SDGi, HDI, Total.Ecological.Footprint..Consumption., Other.renewables:CO2.emissions),
    ~ comma_to_numeric(.)
  )) %>%
  mutate(across(
    c(SDGi, HDI, Total.Ecological.Footprint..Consumption., Other.renewables:Oil.consumption),
    ~ round(., 2)
  ))

# ---- 4) Clean column names to snake_case ------------------------------
ds <- janitor::clean_names(ds)
# Optional: rename long EF column for readability
names(ds)[names(ds) == "total_ecological_footprint_consumption"] <- "ecological_footprint"

glimpse(ds)

# ---- 5) Replace zeros with NA for all numeric EXCEPT nuclear ----------
# (User requirement: keep 0 as meaningful value for nuclear_consumption)
num_cols <- names(ds)[sapply(ds, is.numeric)]
non_nuclear_numeric <- setdiff(num_cols, "nuclear_consumption")

ds <- ds %>%
  mutate(across(all_of(non_nuclear_numeric), ~ ifelse(. == 0, NA, .)))

# ---- 6) Missingness per row & basic plot ------------------------------
ds <- ds %>% mutate(na_count = rowSums(is.na(.)))

thesis_theme <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.background = element_rect(fill = "#f7f7f7", color = NA),
      panel.grid.major = element_line(color = "#e0e0e0"),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = "#444444"),
      axis.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
    )
}

# Histogram of missing counts
ggplot(ds, aes(x = na_count)) +
  geom_histogram(binwidth = 1, fill = "#4C78A8", color = "white", boundary = 0) +
  labs(x = "Number of Missing Values", y = "Frequency") +
  thesis_theme()

# ---- 7) Drop countries with too many NAs ------------------------------
max_na_allowed <- 6
bad_countries <- ds %>%
  filter(na_count > max_na_allowed) %>%
  pull(country) %>%
  unique()

bad_countries

# Drop ALL observations for those countries (both years)
ds_clean <- ds %>%
  dplyr::filter(!(country %in% bad_countries)) %>%
  dplyr::select(-na_count)


# ---- 8) Outlier utilities --------------------------------------------
count_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  sum(x < lower | x > upper, na.rm = TRUE)
}

remove_outliers_iqr <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  ifelse(x < lower | x > upper, NA, x)
}

# Count total outliers
outlier_count <- ds_clean %>%
  dplyr::select(where(is.numeric)) %>%
  summarise(across(everything(), count_outliers)) %>%
  rowSums()

print(glue("Total outliers across numeric vars: {outlier_count}"))

# Outlier distribution (per variable)
outlier_distribution <- ds_clean %>%
  dplyr::select(where(is.numeric)) %>%
  summarise(across(everything(), count_outliers)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Outlier_Count")

ggplot(outlier_distribution, aes(x = reorder(Variable, -Outlier_Count), y = Outlier_Count)) +
  geom_bar(stat = "identity", fill = "#4C78A8", color = "black") +
  labs(x = "Variable", y = "Number of Outliers") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ---- 9) Boxplots (split facets to avoid overcrowding) -----------------

ds_long <- ds_clean %>%
  dplyr::select(where(is.numeric), -year) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

vars <- unique(ds_long$Variable)
# Split roughly in half
split_idx <- ceiling(length(vars) / 2)
split_1 <- vars[1:split_idx]
split_2 <- vars[(split_idx + 1):length(vars)]

plot_box_facets <- function(sel) {
  ds_long %>%
    filter(Variable %in% sel) %>%
    ggplot(aes(x = "", y = Value)) +
    geom_boxplot(fill = "#4C78A8", outlier.colour = "#D7263D") +
    facet_wrap(~ Variable, scales = "free") +
    labs(y = "Value", x = "") +
    thesis_theme()
}

plot_box_facets(split_1)
if (length(split_2) > 0) plot_box_facets(split_2)

# ---- 10) Ensure zeros -> NA for energy variables (except nuclear) -----
energy_vars <- c(
  "other_renewables", "biofuels_consumption", "solar_consumption", "wind_consumption",
  "hydro_consumption", "gas_consumption",
  "coal_consumption", "oil_consumption", "co2_emissions"
)

energy_vars <- setdiff(energy_vars, "nuclear_consumption")

# Re-apply just in case (idempotent)
ds_clean <- ds_clean %>% mutate(across(all_of(energy_vars), ~ ifelse(. == 0, NA, .)))

# ---- 11) Remove outliers -> NA (IQR) ----------------------------------
num_vars <- ds_clean %>% dplyr::select(where(is.numeric)) %>% names()

ds_no_outliers <- ds_clean %>%
  mutate(across(all_of(num_vars), remove_outliers_iqr))

# Plot NA counts after outlier removal
ds_no_outliers %>%
  summarise(across(all_of(num_vars), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "NA_Count") %>%
  ggplot(aes(x = reorder(Variable, -NA_Count), y = NA_Count)) +
  geom_bar(stat = "identity", fill = "#4C78A8") +
  labs(title = "Missing Values per Variable (incl. outliers set to NA)", x = "Variable", y = "Missing Values") +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "#f7f7f7", color = NA),
    panel.grid.major = element_line(color = "#e0e0e0"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
  )

# ---- 12) Median imputation by region ----------------------------------
# Ensure region/income_group/year are correctly typed

ds_no_outliers <- ds_no_outliers %>%
  mutate(
    region = as.factor(region),
    income_group = as.factor(income_group),
    year = as.integer(year)
  )

# Impute numerics by region median (keep non-numeric untouched)
ds_imputed <- ds_no_outliers %>%
  group_by(region) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  ungroup()

# Safety: For selected energy vars, impute remaining NAs globally (if any)
energy_fill <- c("other_renewables", "biofuels_consumption", "wind_consumption",
                 "hydro_consumption", "coal_consumption", "nuclear_consumption")

ds_imputed <- ds_imputed %>%
  mutate(across(all_of(intersect(energy_fill, names(.))), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Quick NA audit
stopifnot(sum(is.na(ds_imputed)) == 0)

# ---- 13) Density overviews --------------------------------------------
vars_to_plot <- c("ecological_footprint", "hdi", "per_capita_gdp", "co2_emissions")
plots <- lapply(vars_to_plot, function(v) {
  ggplot(ds_imputed, aes_string(x = v)) +
    geom_density(fill = "#4C78A8", alpha = 0.8) +
    labs(title = paste("Density:", v), x = v, y = "Density") +
    theme_minimal()
})

do.call(grid.arrange, c(plots, ncol = 2))

# Faceted densities (all numeric)

ds_dist <- ds_imputed %>% dplyr::select(where(is.numeric)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

ggplot(ds_dist, aes(x = Value)) +
  geom_density(fill = "#4C78A8", alpha = 0.8) +
  facet_wrap(~ Variable, scales = "free", ncol = 4) +
  labs(title = "Distributions of Numeric Variables", x = "Value", y = "Density") +
  theme_minimal(base_size = 12)

# Densities by year for selected groups

group_env <- c("ecological_footprint", "co2_emissions")

ds_imputed %>%
  dplyr::select(year, all_of(group_env)) %>%
  pivot_longer(cols = -year, names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Value, fill = factor(year), color = factor(year))) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ Variable, scales = "free", ncol = 2) +
  labs(x = "Value", y = "Density", fill = "Year", color = "Year", title = "Environmental Indicators by Year") +
  theme_minimal(base_size = 11)


group_dev <- c("per_capita_gdp", "hdi", "population", "sd_gi", "life_expectancy")

ds_imputed %>%
  dplyr::select(year, all_of(group_dev)) %>%
  pivot_longer(cols = -year, names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Value, fill = factor(year), color = factor(year))) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ Variable, scales = "free", ncol = 2) +
  labs(x = "Value", y = "Density", fill = "Year", color = "Year") +
  theme_minimal(base_size = 11)


group_energy <- c("coal_consumption", "oil_consumption", "gas_consumption",
                  "biofuels_consumption", "solar_consumption", "wind_consumption",
                  "hydro_consumption", "nuclear_consumption")

ds_imputed %>%
  dplyr::select(year, all_of(group_energy)) %>%
  pivot_longer(cols = -year, names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Value, fill = factor(year), color = factor(year))) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ Variable, scales = "free", ncol = 3) +
  labs(x = "Value", y = "Density", fill = "Year", color = "Year") +
  theme_minimal(base_size = 11)

# ---- 14) Correlations --------------------------------------------------
num_no_year <- ds_imputed %>% dplyr::select(where(is.numeric), -year)
cor_matrix <- cor(num_no_year, use = "complete.obs")

# corrplot
corrplot(cor_matrix, method = "color", tl.cex = 0.6, number.cex = 0.6, order = "hclust", mar = c(1,1,2,1))

# ggcorrplot
ggcorrplot(cor_matrix, colors = c("blue", "white", "red"), show.legend = TRUE, tl.cex = 8)

# ---- 15) Simple relationships -----------------------------------------
# SDGI vs HDI
ggplot(ds_imputed, aes(x = hdi, y = sd_gi)) +
  geom_point(alpha = 0.6, color = "#1b9e77") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "SDGI vs HDI", x = "HDI", y = "SDGI") +
  theme_minimal()

# Life expectancy vs HDI
ggplot(ds_imputed, aes(x = hdi, y = life_expectancy)) +
  geom_point(alpha = 0.6, color = "#d95f02") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Life Expectancy vs HDI", x = "HDI", y = "Life Expectancy") +
  theme_minimal()

# GDP per capita vs HDI
ggplot(ds_imputed, aes(x = hdi, y = per_capita_gdp)) +
  geom_point(alpha = 0.6, color = "#7570b3") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "GDP per Capita vs HDI", x = "HDI", y = "GDP per Capita") +
  theme_minimal()

# GDP per capita vs Life Expectancy
ggplot(ds_imputed, aes(x = per_capita_gdp, y = life_expectancy)) +
  geom_point(alpha = 0.6, color = "#e7298a") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Life Expectancy vs GDP per Capita", x = "GDP per Capita", y = "Life Expectancy") +
  theme_minimal()

# Pair plot
selected_data <- ds_imputed[, c("hdi", "life_expectancy", "per_capita_gdp", "ecological_footprint", "population")]

ggpairs(
  selected_data,
  lower = list(continuous = wrap("smooth", alpha = 0.3, size = 0.2, color = "blue")),
  upper = list(continuous = wrap("cor", size = 4)),
  diag = list(continuous = wrap("densityDiag", fill = "lightblue"))
)

# Energy consumption vs Population

ds_imputed %>%
  dplyr::select(population, all_of(group_energy)) %>%
  pivot_longer(cols = -population, names_to = "Energy_Type", values_to = "Consumption") %>%
  ggplot(aes(x = population, y = Consumption)) +
  geom_point(alpha = 0.5, color = "#4C78A8") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  facet_wrap(~ Energy_Type, scales = "free_y", ncol = 3) +
  labs(x = "Population", y = "Energy Consumption") +
  theme_minimal(base_size = 11)

# ---- 16) Region & Year boxplots ---------------------------------------

# Region vs EF

ggplot(ds_imputed, aes(x = region, y = ecological_footprint, fill = region)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Region", y = "Ecological Footprint") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# EF by year
unique_years <- sort(unique(ds_imputed$year))
fill_cols <- rep(c("steelblue", "tomato", "darkgreen", "goldenrod"), length.out = length(unique_years))

ds_imputed %>%
  ggplot(aes(x = factor(year), y = ecological_footprint)) +
  geom_boxplot(fill = fill_cols) +
  theme_minimal() +
  labs(title = "Ecological Footprint by Year", subtitle = "Distribution across observed years",
       x = "Year", y = "Ecological Footprint (gha per capita)") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))

# ---- 17) World maps ----------------------------------------------------
world <- map_data("world")

# Check name mismatches
countries_worldmap <- unique(world$region)
countries_dataset <- unique(ds_imputed$country)

countries_not_matching <- setdiff(countries_dataset, countries_worldmap)
print(countries_not_matching)

# Corrections (tweak as needed for your dataset)
country_corrections <- c(
  "United States of America" = "USA",
  "United Kingdom" = "UK",
  "Trinidad and Tobago" = "Trinidad",
  "Congo" = "Republic of Congo"
)

ds_imputed <- ds_imputed %>%
  mutate(country = ifelse(country %in% names(country_corrections),
                          country_corrections[country], country))

# Re-check remaining mismatches
countries_not_matching_after <- setdiff(unique(ds_imputed$country), countries_worldmap)
print(countries_not_matching_after)

# Faceted by year --------------------------------------------------------
years <- unique(ds_imputed$year)
world_expanded <- expand.grid(year = years, region = unique(world$region)) %>%
  left_join(world, by = "region")

map_data_combined <- world_expanded %>%
  left_join(ds_imputed, by = c("region" = "country", "year" = "year"))

# Map with NA countries shown in gray
ggplot(map_data_combined, aes(x = long, y = lat, group = group, fill = ecological_footprint)) +
  geom_polygon(color = "black", size = 0.1) +
  facet_wrap(~ year) +
  theme_minimal() +
  scale_fill_viridis_c(option = "C", na.value = "gray90") +
  labs(fill = "Footprint (gha)")

# Single-year map --------------------------------------------------------
year_selected <- if (length(years)) years[1] else 2019
world_year <- map_data_combined %>% filter(year == year_selected)

ggplot(world_year, aes(long, lat, group = group, fill = ecological_footprint)) +
  geom_polygon(color = "black", size = 0.1) +
  theme_minimal() +
  scale_fill_viridis_c(option = "C", na.value = "gray90") +
  labs(title = glue("Ecological Footprint — {year_selected}"), fill = "Footprint (gha)")

# ---- 18) Feature Engineering ------------------------------------------

# Per capita energy consumption
energy_sources <- c("other_renewables", "biofuels_consumption", "solar_consumption", 
                    "wind_consumption", "hydro_consumption", "nuclear_consumption",
                    "gas_consumption", "coal_consumption", "oil_consumption")

ds_imputed <- ds_imputed %>%
  mutate(across(all_of(energy_sources), ~ . / population))

# Split energy sources into groups for visualization
vars_group1 <- energy_sources[1:ceiling(length(energy_sources)/2)]
vars_group2 <- energy_sources[(ceiling(length(energy_sources)/2)+1):length(energy_sources)]

# Plot per capita energy consumption by year - Group 1
ds_imputed %>%
  dplyr::select(all_of(vars_group1), year) %>%
  pivot_longer(cols = -year, names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value, fill = factor(year))) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  labs(title = "Per Capita Energy Consumption Distributions - Group 1",
       x = "Value", y = "Density", fill = "Year") +
  theme_minimal(base_size = 11)

# Plot per capita energy consumption by year - Group 2
ds_imputed %>%
  dplyr::select(all_of(vars_group2), year) %>%
  pivot_longer(cols = -year, names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value, fill = factor(year))) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ variable, scales = "free", ncol = 2) +
  labs(title = "Per Capita Energy Consumption Distributions - Group 2",
       x = "Value", y = "Density", fill = "Year") +
  theme_minimal(base_size = 11)

# ---- 19) Box-Cox Transformation ----------------------------------------
library(MASS)
library(e1071)

# Get eligible variables: numeric and strictly positive
boxcox_vars <- ds_imputed %>%
  dplyr::select(where(is.numeric)) %>%
  names()

# Remove 'year' if present
boxcox_vars <- setdiff(boxcox_vars, "year")

# Filter to only variables that are strictly positive (> 0) after removing NAs
eligible_vars <- c()
for (v in boxcox_vars) {
  x <- ds_imputed[[v]]
  x_clean <- x[!is.na(x)]
  
  if (length(x_clean) >= 10 && all(x_clean > 0)) {
    eligible_vars <- c(eligible_vars, v)
  }
}

cat("Variables eligible for Box-Cox transformation:\n")
print(eligible_vars)
cat("\nVariables excluded (contains zeros/negatives or insufficient data):\n")
print(setdiff(boxcox_vars, eligible_vars))

# Apply Box-Cox transformation to eligible variables only
for (v in eligible_vars) {
  x <- ds_imputed[[v]]
  x_no_na <- x[!is.na(x)]
  
  # Double-check positivity (safety check)
  if (!all(x_no_na > 0)) {
    cat(sprintf("Skipping %s: contains non-positive values\n", v))
    next
  }
  
  model <- lm(y ~ 1, data = data.frame(y = x_no_na))
  bc <- boxcox(model, lambda = seq(-2, 2, 0.1), plotit = FALSE)
  lambda <- bc$x[which.max(bc$y)]
  
  new_var <- paste0(v, "_bc")
  ds_imputed[[new_var]] <- if (abs(lambda) < .Machine$double.eps) {
    log(x)
  } else {
    (x^lambda - 1) / lambda
  }
  
  cat(sprintf("Transformed %s -> %s (lambda = %.3f)\n", v, new_var, lambda))
}

# Update boxcox_vars to only include successfully transformed variables
boxcox_vars <- eligible_vars

# Create skewness comparison table
skew_report <- tibble(
  variable = boxcox_vars,
  original_skew = sapply(ds_imputed[boxcox_vars], skewness, na.rm = TRUE),
  boxcox_skew = sapply(ds_imputed[paste0(boxcox_vars, "_bc")], skewness, na.rm = TRUE)
)

# Pivot for plotting
skew_long <- skew_report %>%
  pivot_longer(cols = c(original_skew, boxcox_skew),
               names_to = "type", values_to = "skewness") %>%
  mutate(
    type = recode(type,
                  original_skew = "Original",
                  boxcox_skew = "Box-Cox"),
    variable = factor(variable, levels = skew_report$variable[order(-abs(skew_report$original_skew))])
  )

# Plot skewness comparison
ggplot(skew_long, aes(x = variable, y = skewness, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Skewness Comparison: Original vs Box-Cox Transformed",
       x = "Variable", y = "Skewness", fill = "Type") +
  scale_fill_manual(values = c("Original" = "tomato", "Box-Cox" = "steelblue")) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.y = element_text(size = 9)
  )

# ---- 20) Additional Feature Engineering --------------------------------
library(patchwork)

# Check which energy variables have Box-Cox versions
available_vars <- names(ds_imputed)
energy_bc_vars <- available_vars[endsWith(available_vars, "_bc")]

cat("Available Box-Cox transformed variables:\n")
print(energy_bc_vars)

# Define renewable and non-renewable energy sources with fallbacks
renewable_vars <- c("other_renewables_bc", "biofuels_consumption_bc", 
                    "solar_consumption_bc", "wind_consumption_bc", 
                    "hydro_consumption_bc")

non_renewable_vars <- c("nuclear_consumption_bc", "gas_consumption_bc", 
                        "coal_consumption_bc", "oil_consumption_bc")

# Check which variables actually exist and use originals if BC versions don't exist
renewable_available <- renewable_vars[renewable_vars %in% available_vars]
renewable_missing <- setdiff(renewable_vars, available_vars)

non_renewable_available <- non_renewable_vars[non_renewable_vars %in% available_vars]
non_renewable_missing <- setdiff(non_renewable_vars, available_vars)

# Add original versions for missing BC variables
if(length(renewable_missing) > 0) {
  renewable_originals <- sub("_bc$", "", renewable_missing)
  renewable_originals <- renewable_originals[renewable_originals %in% available_vars]
  renewable_available <- c(renewable_available, renewable_originals)
}

if(length(non_renewable_missing) > 0) {
  non_renewable_originals <- sub("_bc$", "", non_renewable_missing)
  non_renewable_originals <- non_renewable_originals[non_renewable_originals %in% available_vars]
  non_renewable_available <- c(non_renewable_available, non_renewable_originals)
}

cat("\nUsing renewable energy variables:\n")
print(renewable_available)
cat("\nUsing non-renewable energy variables:\n")
print(non_renewable_available)

# Create composite energy features
ds_transformed <- ds_imputed %>%
  mutate(
    # Total renewable energy (per capita)
    total_renewable = rowSums(across(all_of(renewable_available)), na.rm = TRUE),
    
    # Total non-renewable energy (per capita)
    total_non_renewable = rowSums(across(all_of(non_renewable_available)), na.rm = TRUE),
    
    # Renewable energy ratio
    renewable_ratio = total_renewable / (total_renewable + total_non_renewable + 1e-6),
    
    # GDP per capita squared (to capture non-linear effects)
    gdp_per_capita_sq = per_capita_gdp_bc^2
  )

# Apply Box-Cox to new features where appropriate
feature_vars <- c("total_renewable", "total_non_renewable", "renewable_ratio", "gdp_per_capita_sq")

boxcox_feature_vars <- feature_vars[
  sapply(ds_transformed[feature_vars], 
         function(x) all(x > 0, na.rm = TRUE) && sum(!is.na(x)) >= 10)
]

# Apply Box-Cox transformation to new features
lambda_list <- list()
for (v in boxcox_feature_vars) {
  x <- ds_transformed[[v]]
  tmp <- x[!is.na(x)]
  model <- lm(y ~ 1, data = data.frame(y = tmp))
  bc <- boxcox(model, lambda = seq(-2, 2, 0.1), plotit = FALSE)
  λ <- bc$x[which.max(bc$y)]
  
  lambda_list[[v]] <- λ
  newv <- paste0(v, "_bc")
  ds_transformed[[newv]] <- if (abs(λ) < .Machine$double.eps) {
    log(x)
  } else {
    (x^λ - 1) / λ
  }
}

# Visualize new features distributions
plot_vars <- c("total_renewable", "total_non_renewable", "renewable_ratio", "gdp_per_capita_sq")

ds_transformed %>%
  dplyr::select(all_of(plot_vars)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_density(fill = "#4C78A8", alpha = 0.8) +
  facet_wrap(~ variable, scales = "free", ncol = 2) +
  labs(title = "Distribution of New Engineered Features",
       x = "Value", y = "Density") +
  theme_minimal(base_size = 12)

# Compare GDP squared before and after Box-Cox
ds_transformed %>%
  dplyr::select(gdp_per_capita_sq, gdp_per_capita_sq_bc) %>%
  pivot_longer(cols = everything(), names_to = "version", values_to = "value") %>%
  mutate(version = recode(version,
                          gdp_per_capita_sq = "Original",
                          gdp_per_capita_sq_bc = "Box-Cox Transformed")) %>%
  ggplot(aes(x = value, fill = version)) +
  geom_density(alpha = 0.7) +
  facet_wrap(~ version, scales = "free", ncol = 2) +
  scale_fill_manual(values = c("Original" = "#1f77b4", "Box-Cox Transformed" = "tomato")) +
  labs(title = "GDP Per Capita Squared: Before vs After Box-Cox",
       x = "Value", y = "Density", fill = "") +
  theme_minimal(base_size = 12)

# ---- 21) Variable Selection --------------------------------------------

# Get all column names
all_vars <- names(ds_transformed)

# Identify Box-Cox transformed versions
bc_vars <- all_vars[endsWith(all_vars, "_bc")]

# Get base names (without "_bc" suffix)
base_names <- sub("_bc$", "", bc_vars)

# Identify original variables that don't have Box-Cox versions
orig_vars <- setdiff(
  all_vars[!endsWith(all_vars, "_bc")],
  base_names
)

# Final variable selection: Box-Cox versions + untransformed originals
keep_vars <- c(bc_vars, orig_vars)

# Create final dataset
ds_final <- ds_transformed %>%
  dplyr::select(all_of(keep_vars))

glimpse(ds_final)

# ---- 22) Principal Component Analysis ----------------------------------
library(factoextra)

# Select numeric predictors (exclude target and categorical variables)
pca_data <- ds_final %>%
  dplyr::select(where(is.numeric)) %>%
  dplyr::select(-ecological_footprint_bc, -year)

# Run PCA with centering and scaling
pca_mod <- prcomp(pca_data, center = TRUE, scale. = TRUE)

# Cumulative variance explained
eigval <- pca_mod$sdev^2
cum_var <- cumsum(eigval) / sum(eigval) * 100
scree_df <- data.frame(
  PC = seq_along(cum_var),
  CumVar = cum_var,
  highlight = seq_along(cum_var) == min(which(cum_var >= 80))
)

# Find first PC that achieves ≥80% cumulative variance
k_80 <- scree_df$PC[scree_df$highlight]

# Scree plot
ggplot(scree_df, aes(PC, CumVar, fill = highlight)) +
  geom_col(width = 0.8, colour = "white") +
  scale_fill_manual(values = c(`TRUE` = "#ff7f0e", `FALSE` = "#4C78A8"), guide = "none") +
  geom_text(aes(label = sprintf("%.1f%%", CumVar)), vjust = -0.3, size = 3) +
  labs(title = "PCA Scree Plot: Cumulative Variance Explained",
       x = "Principal Component", y = "Cumulative Variance (%)") +
  ylim(0, 105) +
  theme_minimal(base_size = 12)

# PCA biplot
fviz_pca_biplot(
  pca_mod,
  repel = TRUE,
  col.var = "#1f77b4",
  col.ind = "#999999",
  label = "var",
  addEllipses = FALSE,
  title = "PCA Biplot: First Two Principal Components"
)

# Decide how many PCs to keep (≥80% variance)
var_explained <- summary(pca_mod)$importance[3, ]
K <- min(which(var_explained >= 0.80))

print(glue("Keeping {K} principal components explaining {round(var_explained[K] * 100, 1)}% of variance"))

# Extract PC scores
pc_scores <- as.data.frame(pca_mod$x[, 1:K])
names(pc_scores) <- paste0("PC", 1:K)

# Combine with target and categorical variables
ml_data <- bind_cols(
  ecological_footprint = ds_final$ecological_footprint_bc,
  pc_scores,
  ds_final %>% dplyr::select(region, income_group)
)

# Top loadings for first 7 PCs
library(knitr)

top_loads <- function(pc_name, N = 5) {
  loads <- pca_mod$rotation[, pc_name]
  tibble(
    Variable = names(sort(abs(loads), decreasing = TRUE))[1:N],
    Loading = loads[Variable]
  )
}

pc_tables <- lapply(paste0("PC", 1:7), top_loads, N = 5)
names(pc_tables) <- paste0("PC", 1:7)

for (i in 1:7) {
  cat("\n### Top 5 Loadings for", names(pc_tables)[i], "\n")
  print(kable(pc_tables[[i]], digits = 3))
}

# ---- 23) Machine Learning Models ---------------------------------------
library(caret)
library(patchwork)

set.seed(123)

# Cross-validation control
cv10 <- trainControl(method = "cv", number = 10)

# Train/test split
train_idx <- createDataPartition(ml_data$ecological_footprint, p = 0.8, list = FALSE)
train <- ml_data[train_idx, ]
test <- ml_data[-train_idx, ]

# ---- 23a) Linear Regression on Principal Components --------------------
lm_fit <- train(
  ecological_footprint ~ .,
  data = train,
  method = "lm",
  trControl = cv10
)

# ---- 23b) K-Nearest Neighbors on Principal Components ------------------
knn_fit <- train(
  ecological_footprint ~ .,
  data = train,
  method = "knn",
  tuneLength = 10,
  trControl = cv10
)

# Model predictions
pred_lm <- predict(lm_fit, newdata = test)
pred_knn <- predict(knn_fit, newdata = test)

# Performance comparison
metrics <- data.frame(
  Model = c("Linear Regression (PC)", "KNN (PC)"),
  RMSE = c(RMSE(pred_lm, test$ecological_footprint),
           RMSE(pred_knn, test$ecological_footprint)),
  Rsquared = c(R2(pred_lm, test$ecological_footprint),
               R2(pred_knn, test$ecological_footprint))
)
print(metrics)

# Diagnostic plots for Linear Regression
results_lm <- data.frame(
  obs = test$ecological_footprint,
  pred = pred_lm,
  residuals = test$ecological_footprint - pred_lm
)

p_lm_obs_pred <- ggplot(results_lm, aes(x = obs, y = pred)) +
  geom_point(color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Linear Regression: Observed vs Predicted",
       x = "Observed", y = "Predicted") +
  theme_minimal()

p_lm_resid <- ggplot(results_lm, aes(x = pred, y = residuals)) +
  geom_point(color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Linear Regression: Residuals vs Predicted",
       x = "Predicted", y = "Residuals") +
  theme_minimal()

p_lm_qq <- ggplot(results_lm, aes(sample = residuals)) +
  stat_qq(color = "steelblue") +
  stat_qq_line(color = "red") +
  labs(title = "Linear Regression: QQ Plot") +
  theme_minimal()

# Display LM diagnostic plots
p_lm_obs_pred
p_lm_resid
p_lm_qq

# KNN tuning curve and diagnostics
best_k <- knn_fit$bestTune$k
cv_results_knn <- knn_fit$results

print(glue("Best k for KNN: {best_k}"))

# KNN tuning curve
ggplot(cv_results_knn, aes(x = k, y = RMSE)) +
  geom_line() +
  geom_point(size = 2) +
  labs(title = "KNN Cross-Validation: RMSE vs Number of Neighbors",
       x = "Number of Neighbors (k)", y = "RMSE (CV)") +
  theme_minimal()

# KNN diagnostic plots
results_knn <- data.frame(
  obs = test$ecological_footprint,
  pred = pred_knn,
  residuals = test$ecological_footprint - pred_knn
)

p_knn_obs_pred <- ggplot(results_knn, aes(x = obs, y = pred)) +
  geom_point(color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "KNN: Observed vs Predicted",
       x = "Observed", y = "Predicted") +
  theme_minimal()

p_knn_resid <- ggplot(results_knn, aes(x = pred, y = residuals)) +
  geom_point(color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "KNN: Residuals vs Predicted",
       x = "Predicted", y = "Residuals") +
  theme_minimal()

p_knn_qq <- ggplot(results_knn, aes(sample = residuals)) +
  stat_qq(color = "steelblue") +
  stat_qq_line(color = "red") +
  labs(title = "KNN: QQ Plot") +
  theme_minimal()

# Display KNN diagnostic plots
p_knn_obs_pred
p_knn_resid
p_knn_qq

# ---- 24) Random Forest on Original Variables ---------------------------
library(randomForest)

# Prepare data for Random Forest (using original variables, not PCs)
set.seed(123)

# Ensure categorical variables are factors
ds_final$country <- as.factor(ds_final$country)
ds_final$region <- as.factor(ds_final$region)
ds_final$income_group <- as.factor(ds_final$income_group)

# Train/test split for RF
train_idx_rf <- createDataPartition(ds_final$ecological_footprint_bc, p = 0.8, list = FALSE)
train_rf <- ds_final[train_idx_rf, ]
test_rf <- ds_final[-train_idx_rf, ]

# Hyperparameter grid for Random Forest
p <- ncol(train_rf) - 1
mtry_grid <- expand.grid(mtry = c(floor(sqrt(p)), floor(p/3), floor(p/2)))

# Train Random Forest
rf_fit <- train(
  ecological_footprint_bc ~ .,
  data = train_rf,
  method = "rf",
  tuneGrid = mtry_grid,
  trControl = cv10,
  ntree = 500,
  importance = TRUE
)

print(glue("Best mtry for Random Forest: {rf_fit$bestTune$mtry}"))

# RF predictions and performance
pred_rf <- predict(rf_fit, newdata = test_rf)
perf_rf <- postResample(pred = pred_rf, obs = test_rf$ecological_footprint_bc)

cat(sprintf("Random Forest Test Performance:\n"))
cat(sprintf("RMSE = %.4f\n", perf_rf["RMSE"]))
cat(sprintf("R² = %.4f\n", perf_rf["Rsquared"]))

# RF diagnostic plots
results_rf <- data.frame(
  obs = test_rf$ecological_footprint_bc,
  pred = pred_rf,
  residuals = test_rf$ecological_footprint_bc - pred_rf
)

p_rf_obs_pred <- ggplot(results_rf, aes(x = obs, y = pred)) +
  geom_point(color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Random Forest: Observed vs Predicted",
       x = "Observed EF", y = "Predicted EF") +
  theme_minimal()

p_rf_resid <- ggplot(results_rf, aes(x = pred, y = residuals)) +
  geom_point(color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Random Forest: Residuals vs Predicted",
       x = "Predicted EF", y = "Residuals") +
  theme_minimal()

p_rf_qq <- ggplot(results_rf, aes(sample = residuals)) +
  stat_qq(color = "steelblue") +
  stat_qq_line(color = "red") +
  labs(title = "Random Forest: QQ Plot") +
  theme_minimal()

# Display RF plots
p_rf_obs_pred
p_rf_resid
p_rf_qq

# Variable importance plot (top 20)
var_imp_rf <- varImp(rf_fit, scale = TRUE)
top20_rf <- var_imp_rf$importance %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Variable") %>%
  arrange(desc(Overall)) %>%
  slice(1:20)

ggplot(top20_rf, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Random Forest: Top 20 Variable Importance",
       x = "Variables", y = "Importance (scaled)") +
  theme_minimal()

# OOB Error evolution
rf_obj <- rf_fit$finalModel
plot(rf_obj, main = "Random Forest: OOB Error vs Number of Trees")

# ---- 25) XGBoost Model --------------------------------------------------
library(xgboost)
library(pdp)
library(VIM)
library(fastDummies)

set.seed(123)

# Data preprocessing for XGBoost
cat("=== XGBoost Data Preprocessing ===\n")

# Remove high cardinality country variable for XGBoost
train_xgb <- train_rf %>% dplyr::select(-country)
test_xgb <- test_rf %>% dplyr::select(-country)

# Encode categorical variables
# Income group: ordinal encoding
income_levels <- c("LI", "LM", "UM", "HI")
train_xgb$income_group_ord <- as.numeric(factor(train_xgb$income_group, 
                                                levels = income_levels, ordered = TRUE))
test_xgb$income_group_ord <- as.numeric(factor(test_xgb$income_group, 
                                               levels = income_levels, ordered = TRUE))

# Region: one-hot encoding
train_xgb <- dummy_cols(train_xgb, select_columns = "region", 
                        remove_first_dummy = TRUE, remove_selected_columns = TRUE)
test_xgb <- dummy_cols(test_xgb, select_columns = "region", 
                       remove_first_dummy = TRUE, remove_selected_columns = TRUE)

# Remove original income_group
train_xgb <- train_xgb %>% dplyr::select(-income_group)
test_xgb <- test_xgb %>% dplyr::select(-income_group)

cat(sprintf("XGBoost data dimensions - Train: %s, Test: %s\n", 
            paste(dim(train_xgb), collapse=" x "), 
            paste(dim(test_xgb), collapse=" x ")))

# Target distribution
ggplot(train_xgb, aes(x = ecological_footprint_bc)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7, color = "black") +
  geom_density(aes(y = after_stat(density) * (max(after_stat(count)) * 1.1)), 
               color = "red", size = 1) +
  labs(title = "Distribution of Ecological Footprint (Box-Cox Transformed)",
       x = "Ecological Footprint (Box-Cox)", y = "Frequency") +
  theme_minimal()

# XGBoost hyperparameter tuning
cv_control_xgb <- trainControl(
  method = "cv",
  number = 10,
  verboseIter = FALSE,
  savePredictions = "final"
)

# Reduced hyperparameter grid for faster training
tune_grid_xgb <- expand.grid(
  nrounds = c(100, 200),
  max_depth = c(3, 5),
  eta = c(0.1, 0.2),
  gamma = c(0, 0.1),
  colsample_bytree = c(0.8, 1.0),
  min_child_weight = c(1, 3),
  subsample = c(0.8, 1.0)
)

cat(sprintf("XGBoost will test %d parameter combinations\n", nrow(tune_grid_xgb)))

# Train XGBoost
cat("Training XGBoost model...\n")
start_time <- Sys.time()

xgb_fit <- train(
  ecological_footprint_bc ~ .,
  data = train_xgb,
  method = "xgbTree",
  trControl = cv_control_xgb,
  tuneGrid = tune_grid_xgb,
  verbose = FALSE
)

end_time <- Sys.time()
training_time <- end_time - start_time
cat(sprintf("XGBoost training completed in: %.2f %s\n", 
            as.numeric(training_time), attr(training_time, "units")))

# Best parameters and CV performance
best_params_xgb <- xgb_fit$bestTune
cv_results_xgb <- xgb_fit$results[which.min(xgb_fit$results$RMSE), ]

cat("Best XGBoost hyperparameters:\n")
print(best_params_xgb)

cat(sprintf("\nBest CV Performance:\n"))
cat(sprintf("CV RMSE = %.4f (±%.4f)\n", cv_results_xgb$RMSE, cv_results_xgb$RMSESD))
cat(sprintf("CV R² = %.4f (±%.4f)\n", cv_results_xgb$Rsquared, cv_results_xgb$RsquaredSD))

# Test set performance
pred_xgb <- predict(xgb_fit, newdata = test_xgb)
perf_xgb <- postResample(pred = pred_xgb, obs = test_xgb$ecological_footprint_bc)

cat(sprintf("\nXGBoost Test Performance:\n"))
cat(sprintf("Test RMSE = %.4f\n", perf_xgb["RMSE"]))
cat(sprintf("Test R² = %.4f\n", perf_xgb["Rsquared"]))

# XGBoost diagnostic plots
results_xgb <- data.frame(
  obs = test_xgb$ecological_footprint_bc,
  pred = pred_xgb,
  residuals = test_xgb$ecological_footprint_bc - pred_xgb
)

p_xgb_obs_pred <- ggplot(results_xgb, aes(x = obs, y = pred)) +
  geom_point(color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "XGBoost: Observed vs Predicted",
       x = "Observed EF", y = "Predicted EF") +
  theme_minimal()

p_xgb_resid <- ggplot(results_xgb, aes(x = pred, y = residuals)) +
  geom_point(color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "XGBoost: Residuals vs Predicted",
       x = "Predicted EF", y = "Residuals") +
  theme_minimal()

p_xgb_qq <- ggplot(results_xgb, aes(sample = residuals)) +
  stat_qq(color = "steelblue") +
  stat_qq_line(color = "red") +
  labs(title = "XGBoost: QQ Plot") +
  theme_minimal()

# Display XGBoost plots
p_xgb_obs_pred
p_xgb_resid
p_xgb_qq

# Variable importance
var_imp_xgb <- varImp(xgb_fit, scale = TRUE)$importance
var_imp_xgb$Variable <- rownames(var_imp_xgb)
var_imp_xgb <- var_imp_xgb[order(var_imp_xgb$Overall, decreasing = TRUE), ]

top20_xgb <- head(var_imp_xgb, 20)

ggplot(top20_xgb, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  coord_flip() +
  labs(title = "XGBoost: Top 20 Variable Importance",
       x = "Variables", y = "Importance Score") +
  theme_minimal()

# Learning curve for best hyperparameters
cv_profile_xgb <- xgb_fit$results
best_xgb <- xgb_fit$bestTune

cv_best_xgb <- cv_profile_xgb %>%
  filter(
    max_depth == best_xgb$max_depth,
    eta == best_xgb$eta,
    gamma == best_xgb$gamma,
    colsample_bytree == best_xgb$colsample_bytree,
    min_child_weight == best_xgb$min_child_weight,
    subsample == best_xgb$subsample
  )

# Plot RMSE vs nrounds for best hyperparameters
ggplot(cv_best_xgb, aes(x = nrounds, y = RMSE)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "XGBoost: Cross-Validated RMSE vs Number of Boosting Rounds",
       x = "Number of Rounds (nrounds)", y = "CV RMSE") +
  theme_minimal()

# Training vs Test error evolution (using xgboost directly)
dtrain_xgb <- xgb.DMatrix(
  data = as.matrix(train_xgb %>% dplyr::select(-ecological_footprint_bc)),
  label = train_xgb$ecological_footprint_bc
)

dtest_xgb <- xgb.DMatrix(
  data = as.matrix(test_xgb %>% dplyr::select(-ecological_footprint_bc)),
  label = test_xgb$ecological_footprint_bc
)

# Train with watchlist to monitor both train and test error
bst_xgb <- xgb.train(
  params = list(
    max_depth = best_xgb$max_depth,
    eta = best_xgb$eta,
    subsample = best_xgb$subsample,
    colsample_bytree = best_xgb$colsample_bytree,
    objective = "reg:squarederror"
  ),
  data = dtrain_xgb,
  nrounds = 300,
  watchlist = list(Train = dtrain_xgb, Test = dtest_xgb),
  verbose = 0
)

# Plot training evolution
eval_log_xgb <- bst_xgb$evaluation_log

ggplot(eval_log_xgb, aes(x = iter)) +
  geom_line(aes(y = Train_rmse, color = "Train RMSE"), size = 1) +
  geom_line(aes(y = Test_rmse, color = "Test RMSE"), size = 1) +
  labs(title = "XGBoost: Training vs Test RMSE Evolution",
       x = "Number of Rounds", y = "RMSE", color = "Dataset") +
  theme_minimal()

# Partial dependence plots for top features
top_vars_xgb <- head(var_imp_xgb$Variable, 4)

cat("\n=== Creating Partial Dependence Plots ===\n")
pdp_plots_xgb <- list()

for(i in 1:length(top_vars_xgb)) {
  var_name <- top_vars_xgb[i]
  
  # Create partial dependence data
  pdp_data_xgb <- partial(xgb_fit, pred.var = var_name, train = train_xgb, 
                          grid.resolution = 30)
  
  # Create plot
  pdp_plots_xgb[[i]] <- autoplot(pdp_data_xgb) +
    labs(title = paste("PDP:", var_name),
         x = var_name, y = "Predicted EF") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))
}

# Display partial dependence plots
if(length(pdp_plots_xgb) >= 4) {
  library(gridExtra)
  grid.arrange(pdp_plots_xgb[[1]], pdp_plots_xgb[[2]], 
               pdp_plots_xgb[[3]], pdp_plots_xgb[[4]], 
               ncol = 2, 
               top = "XGBoost: Partial Dependence Plots - Top 4 Features")
} else {
  for(plot in pdp_plots_xgb) {
    print(plot)
  }
}

# ---- 26) Model Comparison Summary ---------------------------------------

# Compile all model performances
all_models_summary <- data.frame(
  Model = c("Linear Regression (PC)", "KNN (PC)", "Random Forest", "XGBoost"),
  RMSE = c(
    RMSE(pred_lm, test$ecological_footprint),
    RMSE(pred_knn, test$ecological_footprint),
    perf_rf["RMSE"],
    perf_xgb["RMSE"]
  ),
  R_squared = c(
    R2(pred_lm, test$ecological_footprint),
    R2(pred_knn, test$ecological_footprint),
    perf_rf["Rsquared"],
    perf_xgb["Rsquared"]
  )
)

rownames(all_models_summary) <- NULL
print("=== Final Model Performance Comparison ===")
print(all_models_summary)

# Visualize model comparison
all_models_long <- all_models_summary %>%
  pivot_longer(cols = c(RMSE, R_squared), names_to = "Metric", values_to = "Value")

p_comparison <- ggplot(all_models_long, aes(x = reorder(Model, -Value), y = Value, fill = Model)) +
  geom_col(alpha = 0.8) +
  facet_wrap(~ Metric, scales = "free_y") +
  labs(title = "Model Performance Comparison",
       x = "Model", y = "Performance Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
  scale_fill_brewer(palette = "Set2")

print(p_comparison)

# Best model identification
best_model_idx <- which.min(all_models_summary$RMSE)
best_model_name <- all_models_summary$Model[best_model_idx]
best_rmse <- all_models_summary$RMSE[best_model_idx]
best_r2 <- all_models_summary$R_squared[best_model_idx]

cat(sprintf("\n=== Best Performing Model ===\n"))
cat(sprintf("Model: %s\n", best_model_name))
cat(sprintf("RMSE: %.4f\n", best_rmse))
cat(sprintf("R²: %.4f\n", best_r2))

