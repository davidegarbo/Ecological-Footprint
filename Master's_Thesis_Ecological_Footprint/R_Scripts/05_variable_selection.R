
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
  