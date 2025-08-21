**Project Overview**
This repository contains the complete code and analysis for my Master's thesis investigating an alternative method to predict ecological footprint using various socio-economic and environmental indicators and advanced machine learning techniques.

**Dataset**
The analysis is based on a comprehensive dataset that includes:

Target Variable: Ecological Footprint (gha per capita)
Socio-economic Indicators: GDP per capita, HDI, Life Expectancy, Population
Energy Consumption: Renewable and non-renewable energy sources (per capita)
Environmental Metrics: CO2 emissions, Urbanization rate
Categorical Variables: Region, Income Group, Country
Time Coverage: 2-year analysis

**Data Sources**
The dataset combines multiple datasets from two sources:
- Global Footprint Network
- Our world in data

**Methodology**

1. Data Preprocessing

Missing Value Treatment: Systematic handling using regional median imputation
Outlier Detection: IQR-based outlier identification and removal
Zero Handling: Strategic conversion of zeros to NA for energy variables
Data Validation: Comprehensive quality checks and data integrity verification

2. Feature Engineering

Box-Cox Transformations: Normalization of skewed distributions
Per Capita Calculations: Energy consumption normalized by population
Composite Features:

Total renewable/non-renewable energy consumption
Renewable energy ratio
GDP per capita squared (non-linear effects)



3. Dimensionality Reduction

Principal Component Analysis (PCA): Reduction of feature space while retaining 80%+ variance
Component Interpretation: Analysis of principal component loadings
Variance Decomposition: Comprehensive scree plot analysis

4. Machine Learning Models
Implemented Algorithms:

Linear Regression (on Principal Components)
K-Nearest Neighbors (KNN) with hyperparameter tuning
Random Forest with variable importance analysis
XGBoost with comprehensive hyperparameter optimization

Model Evaluation:

Cross-Validation: 10-fold CV for robust performance estimation
Performance Metrics: RMSE, R²
Diagnostic Plots: Residual analysis, QQ plots, actual vs predicted
Feature Importance: Variable significance ranking
