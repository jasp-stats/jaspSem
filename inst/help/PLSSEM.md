# Partial Least Squares Structural Equation Modeling (PLS-SEM) in JASP

This document explains how to perform Partial Least Squares Structural Equation Modeling (PLS-SEM) in JASP using the various options provided in the user interface.

## 1. Model Setup
---
In the **Model** section, you can specify the structural model by selecting the appropriate grouping variable and setting the syntax for the model.

- **Grouping Variable**: You can select the grouping variable for multi-group analysis. The grouping variable is optional and can be left empty if not required.

## 2. Estimation Options
---
In the **Estimation** section, the following options are available:

- **Consistent Partial Least Squares**: Enables the option to use consistent PLS-SEM, which provides consistency in estimations for reflective constructs.
  
- **Inner Weighting Scheme**: Choose from the following options to calculate inner weights:
  - Path
  - Centroid
  - Factorial

- **Bias-Corrected Bootstrap**: Activate this option for bias-corrected bootstrapping, which refines the confidence intervals for the estimates.

- **Bootstrap Resampling**: You can adjust the number of bootstrap samples for more precise interval estimations. The default value is set to 5,000.

- **Missing Data Handling**: Options for managing missing data, including pairwise or listwise deletion, are available.

## 3. Output Options
---
The **Output** section allows you to customize the types of output you want to generate:

- **Path Coefficients**: Display the estimated path coefficients.
- **Weights and Loadings**: Shows both the indicator weights and loadings, offering insights into how well the indicators measure their corresponding latent variables.
- **Goodness-of-Fit Measures**: Presents different goodness-of-fit measures like SRMR or NFI.
- **Reliability Measures**: Enables output of reliability measures such as Cronbach's alpha or composite reliability for the constructs.

Additional correlation measures include:
- **Observed and Implied Indicator Correlations**
- **Observed and Implied Construct Correlations**

You can also add **construct scores** to the dataset for further analysis.

## 4. Prediction
---
The **Prediction** section includes options for predicting endogenous indicator scores using cross-validation:

- **Cross-Validation k-Folds**: Choose the number of k-folds for cross-validation, with a default value of 10.
- **Repetitions**: Specify the number of repetitions, with a default value of 10.
  
You can also select a benchmark to compare predictions against:
- **None**
- **Linear Model (LM)**
- **PLS-PM**
- **GSCA**
- **PCA**
- **MAXVAR**
- **All**

## 5. Output and Interpretation
---

### 5.1 Path Coefficients
The **path coefficients** represent the strength and direction of the relationships between the constructs. These coefficients are similar to regression weights and help in understanding the impact of one latent variable on another. You can also view the **t-values** and **p-values** to assess the significance of these paths.

### 5.2 Indicator Loadings and Weights
This section shows the **loadings** of each indicator on its associated construct, which indicates how well each observed variable measures the latent construct. Loadings close to 1 indicate a strong relationship between the indicator and its construct. **Weights** are presented in the case of formative constructs, showing the relative importance of each indicator.

### 5.3 Model Fit Indices
JASP provides several goodness-of-fit measures to evaluate how well the model fits the data:
- **SRMR (Standardized Root Mean Square Residual)**: A measure of model fit, where lower values (generally below 0.08) indicate a better fit.
- **NFI (Normed Fit Index)**: Ranges from 0 to 1, with higher values representing a better fit.

### 5.4 Reliability Measures
Reliability measures assess the internal consistency of the latent constructs:
- **Cronbach’s Alpha**: A commonly used reliability coefficient; values above 0.7 generally indicate acceptable reliability.
- **Composite Reliability (CR)**: A measure of internal consistency similar to Cronbach’s Alpha but considers different factor loadings.
- **Average Variance Extracted (AVE)**: Represents the amount of variance captured by a construct in relation to the variance due to measurement error. AVE values above 0.5 are generally considered acceptable.

### 5.5 R-Squared (R²)
The R-squared value represents the proportion of variance in the endogenous constructs explained by the model. Higher values indicate better explanatory power. An R-squared value close to 0.7 is considered substantial, while values around 0.3 are moderate.

### 5.6 Cross-Validated Prediction
If the cross-validation option is selected, the results will include predicted scores for the endogenous indicators. The k-fold cross-validation helps in assessing the predictive power of the model. You can compare the model’s predictions with benchmarks like linear regression or PLS-PM.

### 5.7 Construct Scores
You can include the estimated construct scores in the dataset for further analysis. These scores represent the latent variables in the model and can be used for additional analyses outside of SEM.

### 5.8 Bootstrapping Results
If bootstrapping is used, the output includes **bootstrap confidence intervals** for the path coefficients, loadings, and weights. These intervals help in understanding the stability of the parameter estimates.

### 5.9 Prediction Benchmarks
If benchmarks are selected, you can compare the PLS-SEM model with:
- **Linear Model** (LM)
- **Principal Component Analysis** (PCA)
- **Generalized Structured Component Analysis** (GSCA)
- **MAXVAR** (Maximum Variance method)

These benchmarks help in evaluating how well your PLS-SEM model predicts the endogenous variables compared to simpler methods.

References
-------
- Rademaker, M.E., & Schuberth, F. (2020). cSEM: Composite-Based Structural Equation Modeling. Package version: 0.4.0, https://m-e-rademaker.github.io/cSEM/.
- Benitez, J. Henseler, J., Castillo, A., & Schuberth, F. (2020). How to perform and report an impactful analysis using partial least squares: Guidelines for confirmatory and explanatory IS research. *Information & Management, 2*(57), 103-168. doi: 10.1016/j.im.2019.05.003.
- Henseler, J. (2021). *Composite-Based Structural Equation Modeling: Analyzing Latent and Emergent Variables.* New York, Guilford Press.
- Evermann, J., & Rönkkö, M. (2021). Recent developments in PLS. *Communications of the Association for Information Systems, 44.* doi: 10.17705/1CAIS.044XX
- Hair, J.F., Sarstedt, M., Ringle, C.M., & Mena, J.A. (2012). An assessment of the use of partial least squares structural equation modeling in marketing research. *Journal of the Academy of Marketing Science 40*, 414–433. doi: 10.1007/s11747-011-0261-6

### R Packages
---
- cSEM
- semTools
