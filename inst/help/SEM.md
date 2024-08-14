Structural Equation Modeling
==========================

Perform structural equation modeling (SEM) using `lavaan` (Rosseel, 2012). Go to lavaan.org for tutorials on the model syntax. See also Kline (2015).

For additional reading, see https://osf.io/xkg3j/ for an introduction to SEM in JASP, by Burger & Tanis.

Throughout this help file some terms are used synonymously, these are: 
- latent variables and factors
- observed variables, manifest variables and indicators

## Input
### Model window
Here, users specify their model here in `lavaan` syntax using the names of the variables in their data set (see https://lavaan.ugent.be/tutorial/sem.html). Multiple models can be specified and compared.

- Data: Is the input for the model a raw data matrix with observations as rows and variables in columns, or a variance-covariance matrix with variables in rows and columns. If it is a variance-covariance matrix, the sample size needs to be specified.
- Sampling weights: Users can choose a variable in their data set that weights each observation.

### Model Options
- Factor scaling: How should the metric, or scaling, of the latent variables be determined. 
  - Factor loadings: Default is by fixing the first indicator loading to 1. 
  - Factor variance: fixing the latent variables' variance to 1, 
  - Effect coding: fixing the average of loadings per latent variable to 1, 
  - None
- Include mean structure: Should observed or latent intercepts be estimated. In SEM, observed means are called intercepts and latent means are called means. To identify the model, some elements need to be fixed:
  - Fix latent means to zero: Default, observed means will be estimated freely.
  - Fix observed intercepts to zero: Latent means will be estimated freely.
  - Fix mean of manifest intercepts to zero: Latent means will be estimated freely.
- Assume factors uncorrelated: By default the factors are assumed to correlate if not specified differently in the model window. This checkbox changes that and factors are estimated to be orthogonal.

- Fix exogenous covariates: If checked, the exogenous covariates are considered fixed variables and the means, variances and covariances of these variables are fixed to their sample values. If not checked, they are considered random, and the means, variances and covariances are free parameters.
- Omit residual single indicator: If checked, the residual variance (if included) of an observed indicator is set to zero if it is the only indicator of a latent variable.
- Include residual variances: If checked, the (residual) variances of both observed and latent variables are free to-be-estimated parameters.
- Correlate exogenous latents: If checked, the covariances of exogenous latent variables are included in the model and set free.
- Correlate dependent variables: If checked, the covariances of dependent variables (both observed and latent) are included in the model and set free.
- Add thresholds: If checked, thresholds for limited (non-continuous) dependent variables are included in the model and set free.
- Add scaling parameters: If checked, response scaling parameters for limited (non-continuous) dependent variables are included in the model and set free.
- Constrain EFA blocks: If checked, the necessary constraints are imposed to make the (unrotated) exploratory factor analysis blocks identifiable: for each block, factor variances are set to 1, factor covariances are constrained to be zero, and factor loadings are constrained to follow an echelon pattern.

### Estimation Options
- Estimator: Some estimators imply options for test and standard error. If test and standard error remains as "default", they will be either standard or set by the chosen estimator. If test and standard error are specified (meaning not default) they will overwrite whatever option the estimator implies.
  - Default: The estiamtor will be determined by other options, as well as the scaling of the variables (usually becomes ML)
  - ML: Maximum Likelihood
  - GLS: Generalized Least Squares
  - WLS: Weighted Least Squares
  - ULS: Unweighted Least Squares
  - DWLS: Diagonally Weighted Least Squares
  - DLS: Distributionally Weighted Least Squares
  - PML: Pairwise maximum likelihood
  - Extensions of ML-estimators with extra effects: 
	  - MLM: classic robust se (se="robust.sem"), Satorra-Bentler test statistic (test="satorra.bentler")
	  - MLMV: classic robust se, scaled and shifted test statistic (test="scaled.shifted")
	  - MLMVS: classic robust se, mean and var adjusted Satterthwaite style test statistic (test="mean.var.adjusted")
	  - MLF: first-order standard se (information="first.order"), standard test
	  - MLR: Huber-White robust se (se="robust.huber.white"), Yuan-Bentler T2-star test statistic (test="yuan.bentler.mplus")
  - Others: 
	  - WLSM: implies DWLS with scaled test and robust se
	  - WLSMV: implies DWLS with mean and var adjusted test and robust se
	  - ULSM: implies ULS with scaled test and robust se
	  - ULSMV: implies ULS with mean-var adjusted test and robust se

- Model test: 
  - Default: The test will be determined by other options (estimator, missing, mimic), as well as the scaling of the variables (usually becomes standard)
  - Standard: Chisq test statistic
  - Satorra-Bentler: scaled test statistic
  - Yuan-Bentler: scaled test statistic
  - Yuan-Bentler Mplus: asymptotically equal to Yuan-Bentler scaled test statistic
  - Mean and variance adjusted: test statistic, also called "Satterthwaite"
  - Scaled and shifted: test statistic
  - Bootstrap (Bollen-Stine): The Bollen-Stine bootstrap is used to compute the bootstrap probability value of the (regular) test statistic.
  - Browne residual based (ADF): Browne's residual-based test statistic using ADF (asymptocially distribution free) theory is computed.
  - Browne residual based (NT): Browne's residual-based test statistic using normal theory is computed.

- Information matrix: Matrix used to compute the standard errors
  - Default: The information matrix will be determined by other options (estimator, missing, mimic), as well as the scaling of the variables (usually becomes expected)
  - Expected
  - Observed
  - First order: The information matrix is based on the outer product of the casewise scores

- Standard errors:
  - Default: The standard errors method will be determined by other options (estimator, missing, mimic), as well as the scaling of the variables (usually becomes expected)
  - Standard: Conventional standard errors are computed based on inverting the (expected, observed or first.order) information matrix
  - Robust: conventional robust standard errors are computed (also called robust.sem)
  - Robust Huber-White: Standard errors are computed based on the 'mlr' (aka pseudo ML, Huber-White) approach
  - Bootstrap: Standard errors are computed from bootstrapped model fit objects.
    - Bootstrap samples: Number of bootstrap samples to take for the standard errors

- Confidence intervals: CI width for the parameter estimates
- Standardize variables before estimation: z-standardizes all variables before estimation
- Missing data handling: How to treat missings
  - Listwise: Exclude cases that have missings
  - FIML: Use full-information maximum likelihood, only available for ML estimation, also called "ml" sometimes
  - Pairwise: In the first step, compute thresholds (for categorical variables) and means or intercepts (for continuous variables) using univariate information only. In this step, we simply ignore the missing values just like in mean(x, na.rm = TRUE). In the second step, we compute polychoric/polyserial/pearson correlations using (only) two variables at a time. Here we use pairwise deletion: we only keep those observations for which both values are observed (not-missing).
  - Two-stage: In this approach, we first estimate the sample  statistics (mean vector, variance-covariance matrix) using an EM algorithm. Then, we use these estimated sample statistics as input for a regular analysis (as if the data were complete). The standard errors and test statistics are adjusted correctly to reflect the two-step procedure. For continuous data.
  - Robust two-stage: Same as two-stage but option produces standard errors and a test statistic that are robust against non-normality. For continuous data.
  - Doubly robust: For estimator PML. 

### Output Options
- Additional fit measures: Produce a table with a variety of fit measures
- R-squared: Produces a table with the explained variance in each indicator
- Average variance extracted (AVE): The amount of variance that is captured by a construct in relation to the amount of variance due to measurement error. It is used to evaluate the convergent validity of a construct.
- Heterotrait-monotrait ratio (HTMT): Discriminant validity. Assessing the degree to which constructs are distinct from each other by comparing the correlations between different constructs (heterotrait) to the correlations within the same construct (monotrait).
- Reliability measures: Metrics used to assess the consistency and stability of a measurement instrument, such as coefficient alpha and composite(omega) reliability.
- Observed covariances: Covariances calculated directly from the observed data
- Implied covariances: Covariances predicted by the SEM model based on the estimated parameters, representing the model's expectations of how the variables should covary if the model is correct.
- Residual covariances: Differences between the observed covariances and the implied covariances, indicating how well the model fits the data.
- Standardized residuals: Covariance matrix of standardized residuals
- Mardia's coefficient: A measure of multivariate normality, including skewness and kurtosis components, used to assess whether the data follow a multivariate normal distribution.
- Standardized estimates: Should the output show standardized parameter estimates
  - All:  The standardized estimates are based on both the variances of both (continuous) observed and latent variables. 
  - Latents: The standardized estimates are on the variances of the (continuous) latent variables only.
  - Except exogenous covariates: The standardized estimates are based on both the variances of both (continuous) observed and latent variables, but not the variances of exogenous covariates.
- Path diagram: Plots the model
  - Show parameter estimates
  - Show legend
- Modification indices: Show the modification indices, that is, the possible improvement indications of the model test statistic
  - Hide low indices: Hide indices below a certain "Threshold" value that are unimportant for the improvement of the model
- Show warnings: Show warnings produced by the underlying R-package (if there are any)

### Multigroup SEM
- Grouping variable: Fit the model for mutliple groups. Variable needs to be a variable that is either nominally or ordinally scaled.
- Equality constraints: Constraints chosen here are fixed to be equal across all groups
  - Loadings, Intercepts, Residuals, Resdiaul covariances, Means (intercepts), Threshiolds, Regressions, Latent variances, Latent covariances
- Release constraints (one per line): Text input to release constraints, needs to be written in lavaan syntax, for instance, for releasing a single loading of indicator x2 on factor f, write `f=~x2`


# Sensitivity Analysis
- Run sensitivity analysis: Conduct sensitivity analysis for structural equation modeling (SEM) against a potential missing confounder. A potential missing confounder is specified as a simulated phantom variable, a latent variable without manifest indicators, such that the sensitivity of SEM can be assessed through comparing results from models with and without the phantom variable.
- Search algorithm: How to optimally sample the phantom variables
  - Ant colony optimization: Method for finding good paths through graphs
    - Number of ants: How many artificial ants are used in each iteration of the algorithm. Each ant represents a potential solution to the optimization problem
    - Size of the solution archive: The number of best solutions that are stored and maintained during the optimization process
    - Convergence rate threshold: During each iteration of the optimization algorithm, the change in the objective function or the parameters is measured. If this change is smaller than the convergence rate threshold, the algorithm is considered to have converged, and the iteration stops
  - Tabu search: Maintains a list of recently visited solutions (or moves) called the "tabu list" to prevent revisiting them and getting stuck in local optima
- Optimizer function: Function that is maximized during the optimization
  - % change mean estimate
  - SD of deviance /(divided by) old estimate
  - Change of p-value
  - Distance of p-value from alpha
  - Change of RMSEA
  - Distance of RMSEA from 0.05
- Significance level
- Maximum number of iterations
- Repeatability: Set seed to guarantee reproducible results

## Output

- Model fit
  - Model(s)
  - Group (optional)
  - AIC (optional): Akaike information criterion, the smaller the better
  - BIC (optional): Bayesian information criterion, the smaller the better
  - n(Observations): Number of observations used in fitting the model
  - n(Parameters): Total are the number of freely estimated parameters ignoring contraints, Free are the freely estimated parameters taking constraints into account
  - Baseline test (likelihood ratio): H0 user-specified and saturated model are the same, H1 models differ.
    - X^2: Chi-square distributed test statistic
    - df_ degrees of freedom of the model chi-square test
    - p: p-value for the comparison user-specified model against the saturated model. We check if the saturated model is significantly different from the user-specified model and if so, we reject the user-specified model. 
  - Difference test (likelihood ratio): H0 Models are the same, H1 models differ
    - delta(X^2): Difference in test statistic values between two or more models
    - delta(df): Difference in df between two models or more
    - p: p-value for the comparison of the two or more models. H0: Models are the same, H1: models differ. Checks if the models with fewer df (more complex) differ significantly from the models with more df (simpler). If significant, the model with more df is rejected. If not significant, the model with more df is retained

#### Additional Fit Measures (optional)
- Fit indices
  - Comparative Fit Index (CFI): How well a user-specified model fits the data compared to a null model; the closer to 1 the better.
  - Tucker-Lewis Index (TLI): Compares the fit of a specified model to a null model, adjusting for model complexity; the closer to 1 the better.
  - Bentler-Bonett Non-normed Fit Index (NNFI): Similar to TLI, it adjusts for model complexity and is less sensitive to sample size; the closer to 1 the better.
  - Bentler-Bonett Normed Fit Index (NFI): Compares the fit of a specified model to a null model without adjusting for model complexity; the closer to 1 the better.
  - Parsimony Normed Fit Index (PNFI): Adjusts the NFI for model complexity, rewarding simpler models; the closer to 1 the better.
  - Bollen's Relative Fit Index (RFI): Compares the fit of a specified model to a null model, similar to NFI but with different scaling; the closer to 1 the better.
  - Bollen's Incremental Fit Index (IFI): Measures the improvement in fit of a specified model over a null model, adjusting for degrees of freedom; the closer to 1 the better.
  - Relative Noncentrality Index (RNI): Similar to the CFI, it compares the fit of a specified model to a null model, adjusting for noncentrality; the closer to 1 the better.
  - Root mean square error of approximation (RMSEA): Measures the discrepancy of the model from the data per degree of freedom, with lower values indicating a better fit.
  - RMSEA 90% CI lower bound: The lower bound of the 90% confidence interval for the RMSEA.
  - RMSEA 90% CI upper bound: The upper bound of the 90% confidence interval for the RMSEA.
  - RMSEA p-value: The p-value associated with the RMSEA, testing the null hypothesis that the RMSEA is less than or equal to 0.05.
  - Standardized root mean square residual (SRMR): Measures the standardized difference between observed and model predicted correlations, with lower values indicating a better fit.
  - Hoelter's critical N (α = .05): The sample size at which the chi-square test would no longer be significant at the 0.05 level.
  - Hoelter's critical N (α = .01): The sample size at which the chi-square test would no longer be significant at the 0.01 level.
  - Goodness of fit index (GFI): Measures the proportion of variance accounted for by the estimated population covariance, with values closer to 1 indicating a better fit.
  - McDonald fit index (MFI): A fit index that adjusts for model complexity and sample size, with values closer to 1 indicating a better fit.
  - Expected cross validation index (ECVI): Estimates the fit of the model to a new sample of the same size, with lower values indicating a better fit.
  - Log-likelihood: The log of the likelihood function, used in model comparison.
  - Number of free parameters: The number of parameters estimated freely in the model.
  - Akaike Information Criterion (AIC): A measure of model fit that penalizes for model complexity, with lower values indicating a better fit.
  - Bayesian Information Criterion (BIC): Similar to AIC but with a stronger penalty for model complexity, with lower values indicating a better fit.
  - Sample-size adjusted Bayesian Information Criterion (SSABIC): A version of BIC adjusted for sample size, with lower values indicating a better fit.

- T-size fit indices: Equvalence testing: Testing for close fit instead of exact fit
  - T-size: (minimum tolerable size) of misspecification for the model(s): T-size versions of CFI and RMSEA
  - poor-fair limit: For CFI, below this limit the model fit becomes poor. For RMSEA, above this limit the model fit becomes poor.
  - fair-close limit: For the CFI, above this limit the model fit becomes close (good). For the RMSEA; below this limit the model fit becomes close (good)

- R-Squared (optional)
  - Explained variance in dependent variables by their predictors

- Average variance extracted (optional)
  - Amount of variance captured by a construct (latent variable)

- Heterotrait-Monotrait Ratio (optional)
  - Correlations of constructs, that is, latent variables

- Reliability (optional)
  - Reliability per latent variable: that is, how reliably measured are the indicators for that latent variable
    - total: How reliably measured is the full set of indicators
  - Coefficient alpha: The closer to 1 the better. Also called Cronbach's alpha. Best for unidimensional constructs.
  - Coefficient omega: The closer to 1 the better. The *total* value denotes omega_t.

- Mardias coefficients: 
  - Skewness: H0 multivariate skewness is equal to the normal distribution
  - Kurtosis: H0 multivariate kurtosis is equal to the normal distribution

#### Parameter Estimates
- Factor loadings (and others, the logic stays the same)
  - Group (optional)
  - Latent: Name of latent variable(s)
  - Indicator: Name of manifest variables
  - Label (optional, not visible column name): Shows the labels of equality constraints
  - Estimate: when standardized estimate is checked this becomes "Std.estimate"
  - Std.error
  - z-value: of the standard normal distribution
  - p: -value
  - X% Confidence interval: lower and upper bound

- Regression coefficients: Outcome and predictor
- Factor variances
- Factor covariances
- Residual variances: Unexplained variance in the indicators
- Intercepts (optional): Estimated means of observed and latent variables
- Total effects (optional): Summarize the regression paths per variable
- Indirect effects (optional): Regression paths through other variables
- Observed covariance matrix (optional)
- Implied covariance matrix (optional): model-implied
- Residuals covariance matrix
- Standardized residuals covariance matrix

- Modification indices (optional):
  - First three columns contain the specification that could be made in the model syntax
  - mi: modification index. How much better would the model fit. Sorted from high to low. 
  - epc: expected parameter change
  - sepc: expected parameter change for standardized estimates: lv, all, and no exogenous covariates (nox)

- Path diagram (optional)

#### Sensitivity Analysis
- Summary of sensitivity analysis:
  - Path: Which path would be affected (only regressions)
  - Original model: Summaries of the path(s) for the original model
  - Sensitivtiy model: 
    - p*: new p-value with phantom variable(s)
    - Mean, Min, and Max of the path estimate with the phantom variable(s)

- Sensitivity parameters that led to a change in significance
  - Path
  - Sensitivity parameters: The path estimates for the phantom variable(s)

- Summary of sensitivity parameters
  - Sensitivity parameter: Path between phantom variable and latent variable(s)


References
-------
- Hu, L. T., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in covariance structure analysis: Conventional criteria versus new alternatives. *Structural Equation Modeling: A Multidisciplinary Journal, 6*(1), 1-55, https://doi.org/10.1080/10705519909540118
- Katerina M. Marcoulides & Ke-Hai Yuan (2017). New ways to evaluate goodness of fit: A note on using equivalence testing to assess structural equation models. *Structural Equation Modeling: A Multidisciplinary Journal, 24*(1), 148-153, https://doi.org/10.1080/10705511.2016.1225260
- Kline, R. B. (2015). Principles and practice of structural equation modeling. *Guilford Publications*.
- Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling. *Journal of Statistical Software, 48*(2), 1-36, https://doi.org/10.18637/jss.v048.i02
- Shen, Z., & Leite, W. L. (2022). SEMsens: An R package for sensitivity analysis of structural equation models with the ant colony optimization algorithm. *Applied Psychological Measurement, 46*(2), 159-161. https://doi.org/10.1177/01466216211063233

### R Packages
---
- lavaan
- semPlot
- SEMsens
- semTools
- stats
