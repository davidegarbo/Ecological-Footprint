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

