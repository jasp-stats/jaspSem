MIMIC models
============

Multiple-Indicators-multiple-causes (MIMIC) models with a single latent variable. It can be used to study the effects of covariates (causes) on a latent variable. MIMIC analysis in JASP is based on the excellent `lavaan` software (Rosseel, 2012). More information about `lavaan` can be found here: [lavaan.org](http://lavaan.org). 

MIMIC in JASP allows for continuous and ordinal endogenous variables and binary and continuous exogenous variables. For binary endogenous variables, recode your variable into a dummy continous variable with 0 for the first category and 1 for the second category. The linearity assumption still holds, however, i.e., SEM does not perform logistic regression.

For more information on allowed data types, see [the lavaan website](http://lavaan.ugent.be/tutorial/cat.html).

## Input
#### Indicators
These are the indicators, together defining the latent outcome. A minimum of 3 scale or ordinal variables are required for the analysis to run.

#### Predictors
These are the causes of the latent outcome. One or multiple predictor variable(s). A minimum of one predictor is required for MIMIC models. Otherwise, see Confirmatory Factor Analysis.

### Options
#### Standardized estimates
Check this to show three types of standardized estimates: where all variables are standardized, where the latent variables are standardized, and where the endogenous variables are standardized. See `lavaan` for more information.

#### Show lavaan syntax
Show the syntax needed to estimate this model using `lavaan` in `R` or the `SEM` module in `JASP`.

#### R-Squared
A table with the proportion of variance explained for each of the endogenous variables in the MIMIC model (the indicators).

#### Additional fit measures
Check this to display several different fit measures for this model, such as RMSEA, CFI, and other information criteria.

#### Confidence intervals
Here, you can select different ways of estimating the uncertainty around the parameter estimates, as well as the width of the confidence intervals. 

### Plots

#### Path plot
This option allows users to graphically display the path model being estimated by the MIMIC analysis. Optionally, the parameters can be shown in this plot. If the labels overlap, the plot can be saved as an EPS and edited in any vector editing program.

### Advanced
#### Missing value handling
How missing values are handled. By default, this is set to full information maximum likelihood (FIML), which automatically computes the estimates using all the available information -- assuming missing at random (MAR) missingness patterns. A suboptimal alternative is available in listwise deletion.

#### Emulation
Emulate the output from different SEM programs. Default none.

#### Estimator
Different choices for estimators can be found here. We suggest leaving this at `auto` for most -- if not all -- purposes.

References
==========

- Rosseel, Y. (2012). Lavaan: An R package for structural equation modeling and more. Version 0.5–12 (BETA). Journal of Statistical Software, 48(2), 1-36.
