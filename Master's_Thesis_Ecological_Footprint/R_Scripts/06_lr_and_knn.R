
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

