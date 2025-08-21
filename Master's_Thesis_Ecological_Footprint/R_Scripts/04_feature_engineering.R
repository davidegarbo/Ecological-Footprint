
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
