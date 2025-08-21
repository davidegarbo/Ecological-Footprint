
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

