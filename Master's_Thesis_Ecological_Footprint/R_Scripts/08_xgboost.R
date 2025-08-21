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

