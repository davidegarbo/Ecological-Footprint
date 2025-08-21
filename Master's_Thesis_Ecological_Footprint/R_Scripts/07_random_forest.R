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
