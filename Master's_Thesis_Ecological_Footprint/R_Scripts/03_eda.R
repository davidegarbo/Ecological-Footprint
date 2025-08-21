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

