#
# Copyright (C) 2013-2025 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# This is a generated file. Don't change it!

#' Structural Equation Modeling
#'
#' @param additionalFitMeasures, Display a table with various fit measures including CFI, TLI, RMSEA, SRMR, and information criteria.
#'    Defaults to \code{FALSE}.
#' @param alpha, Significance level for the sensitivity analysis.
#' @param averageVarianceExtracted, Display the amount of variance captured by a construct relative to measurement error. Used to evaluate convergent validity.
#'    Defaults to \code{FALSE}.
#' @param bootstrapCiType, Type of bootstrap confidence interval.
#' @param bootstrapSamples, Number of bootstrap samples to use for computing standard errors.
#' @param bootstrapSamplesBollenStine, Number of bootstrap samples for the Bollen-Stine bootstrap test.
#' @param ciLevel, Width of the confidence intervals for parameter estimates.
#' @param convergenceRateThreshold, If the change in the objective function is smaller than this threshold, the algorithm has converged.
#' @param dataType, Select whether the input is a raw data matrix or a variance-covariance matrix.
#' \itemize{
#'   \item \code{"raw"}: Use raw data with observations as rows and variables in columns.
#'   \item \code{"varianceCovariance"}: Use a variance-covariance matrix as input. Requires specifying the sample size.
#' }
#' @param dependentCorrelation, Include covariances of dependent variables (observed and latent) in the model.
#'    Defaults to \code{TRUE}.
#' @param efaConstrained, Impose constraints to make exploratory factor analysis blocks identifiable: factor variances set to 1, covariances to zero, and loadings follow an echelon pattern.
#'    Defaults to \code{TRUE}.
#' @param emulation, Emulate the output from different SEM programs.
#' @param equalIntercept, Constrain intercepts to be equal across groups.
#'    Defaults to \code{FALSE}.
#' @param equalLatentCovariance, Constrain latent variable covariances to be equal across groups.
#'    Defaults to \code{FALSE}.
#' @param equalLatentVariance, Constrain latent variable variances to be equal across groups.
#'    Defaults to \code{FALSE}.
#' @param equalLoading, Constrain factor loadings to be equal across groups.
#'    Defaults to \code{FALSE}.
#' @param equalMean, Constrain means to be equal across groups.
#'    Defaults to \code{FALSE}.
#' @param equalRegression, Constrain regression coefficients to be equal across groups.
#'    Defaults to \code{FALSE}.
#' @param equalResidual, Constrain residual variances to be equal across groups.
#'    Defaults to \code{FALSE}.
#' @param equalResidualCovariance, Constrain residual covariances to be equal across groups.
#'    Defaults to \code{FALSE}.
#' @param equalThreshold, Constrain thresholds to be equal across groups.
#'    Defaults to \code{FALSE}.
#' @param errorCalculationMethod, Method for computing standard errors. Standard uses the information matrix, Robust uses robust.sem, Robust Huber-White uses the mlr approach, and Bootstrap computes SEs from bootstrapped fits.
#' @param estimator, Choose the estimation method. Some estimators set implicit options for test and standard errors. ML-based extensions: MLM (robust SE, Satorra-Bentler test), MLMV (robust SE, scaled-shifted test), MLMVS (robust SE, Satterthwaite test), MLF (first-order SE), MLR (Huber-White SE, Yuan-Bentler test). WLS variants: WLSM/WLSMV imply DWLS with robust SE, ULSM/ULSMV imply ULS with robust SE.
#' @param exogenousCovariateConditional, Condition on the exogenous covariates when estimating the model.
#'    Defaults to \code{FALSE}.
#' @param exogenousCovariateFixed, If checked, exogenous covariates are considered fixed and their means, variances, and covariances are fixed to sample values.
#'    Defaults to \code{TRUE}.
#' @param exogenousLatentCorrelation, Include covariances of exogenous latent variables in the model.
#'    Defaults to \code{TRUE}.
#' @param factorScaling, How the metric of latent variables is determined: by fixing the first loading to 1 (Factor loadings), fixing factor variance to 1 (Factor variance), fixing average loadings to 1 (Effects coding), or none.
#' @param freeParameters, Release specific equality constraints using lavaan syntax, e.g., 'f=~x2' to release a single loading.
#' @param group, Select a nominal or ordinal variable to fit the model separately for each group.
#' @param heterotraitMonotraitRatio, Display the HTMT ratio to assess discriminant validity between constructs.
#'    Defaults to \code{FALSE}.
#' @param impliedCovariance, Display the model-implied covariance matrix based on estimated parameters.
#'    Defaults to \code{FALSE}.
#' @param informationMatrix, Matrix used to compute the standard errors: expected, observed, or first order (outer product of casewise scores).
#' @param latentInterceptFixedToZero, Fix latent means to zero; observed intercepts are estimated freely.
#'    Defaults to \code{TRUE}.
#' @param manifestInterceptFixedToZero, Fix observed intercepts to zero; latent means are estimated freely.
#'    Defaults to \code{FALSE}.
#' @param manifestMeanFixedToZero, Fix the mean of manifest intercepts to zero; latent means are estimated freely.
#'    Defaults to \code{FALSE}.
#' @param mardiasCoefficient, Display Mardia's multivariate skewness and kurtosis coefficients to assess multivariate normality.
#'    Defaults to \code{FALSE}.
#' @param maxIterations, Maximum number of iterations for the optimization algorithm.
#' @param meanStructure, Estimate observed intercepts and/or latent means. Some elements need to be fixed for identification.
#'    Defaults to \code{FALSE}.
#' @param measurementModelReliability, Treats each latent factor's common-factor variance as true-score variance, ignoring structural regressions among latent variables. Only affects coefficient ω.
#'    Defaults to \code{FALSE}.
#' @param modelTest, Choose the test statistic for evaluating model fit. If left at Default, the test is determined by the chosen estimator.
#' @param models, Specify the structural equation model using lavaan syntax. Multiple models can be specified and compared. See lavaan.org for syntax tutorials.
#' @param modificationIndex, Display modification indices showing potential improvements to the model.
#'    Defaults to \code{FALSE}.
#' @param modificationIndexHiddenLow, Hide modification indices below the specified threshold.
#'    Defaults to \code{FALSE}.
#' @param modificationIndexThreshold, Minimum value for modification indices to be displayed.
#' @param naAction, How to treat missing values. FIML uses full-information maximum likelihood. Pairwise computes correlations using available pairs. Two-stage uses EM-estimated statistics. Robust two-stage adds robustness against non-normality. Doubly robust is for PML estimation.
#' @param numberOfAnts, Number of artificial ants per iteration, each representing a potential solution.
#' @param observedCovariance, Display the observed covariance matrix calculated from the data.
#'    Defaults to \code{FALSE}.
#' @param optimizerFunction, Objective function maximized during the optimization.
#' @param orthogonal, Estimate factors as orthogonal (uncorrelated) instead of allowing them to correlate.
#'    Defaults to \code{FALSE}.
#' @param pathPlot, Display a path diagram of the model.
#'    Defaults to \code{FALSE}.
#' @param pathPlotLegend, Display a legend in the path diagram.
#'    Defaults to \code{FALSE}.
#' @param pathPlotParameter, Display parameter estimates on the path diagram.
#'    Defaults to \code{FALSE}.
#' @param pathPlotParameterStandardized, Show standardized estimates on the path diagram.
#'    Defaults to \code{FALSE}.
#' @param rSquared, Display the explained variance in each dependent variable.
#'    Defaults to \code{FALSE}.
#' @param reliability, Display reliability metrics such as coefficient alpha and composite (omega) reliability per latent variable.
#'    Defaults to \code{FALSE}.
#' @param residualCovariance, Display residual covariances (observed minus implied).
#'    Defaults to \code{FALSE}.
#' @param residualSingleIndicatorOmitted, Set the residual variance of a single indicator to zero if it is the only indicator of a latent variable.
#'    Defaults to \code{TRUE}.
#' @param residualVariance, Include residual variances of observed and latent variables as free parameters.
#'    Defaults to \code{TRUE}.
#' @param sampleSize, The number of observations the covariance matrix is based on.
#' @param samplingWeights, Select a variable from the dataset to use as sampling weights for each observation.
#' @param scalingParameter, Include response scaling parameters for limited (non-continuous) dependent variables.
#'    Defaults to \code{TRUE}.
#' @param searchAlgorithm, Algorithm for sampling phantom variables.
#' \itemize{
#'   \item \code{"antColonyOptimization"}: Use ant colony optimization to find good paths through the parameter space.
#'   \item \code{"tabuSearch"}: Use tabu search, which maintains a list of recently visited solutions to avoid local optima.
#' }
#' @param sensitivityAnalysis, Run a sensitivity analysis by adding phantom variables (latent variables without indicators) to assess the robustness of the model.
#'    Defaults to \code{FALSE}.
#' @param sizeOfSolutionArchive, Number of best solutions stored during optimization.
#' @param standardizedEstimate, Display standardized parameter estimates.
#'    Defaults to \code{FALSE}.
#' @param standardizedEstimateType, Type of standardization.
#' \itemize{
#'   \item \code{"all"}: Standardize based on variances of both observed and latent variables.
#'   \item \code{"latents"}: Standardize based on latent variable variances only.
#'   \item \code{"nox"}: Standardize based on variances of observed and latent variables, excluding exogenous covariates.
#' }
#' @param standardizedResidual, Display the standardized residual covariance matrix.
#'    Defaults to \code{FALSE}.
#' @param standardizedVariable, Z-standardize all variables before estimation.
#'    Defaults to \code{FALSE}.
#' @param threshold, Include thresholds for limited (non-continuous) dependent variables.
#'    Defaults to \code{TRUE}.
#' @param userGaveSeed, Set a seed to guarantee reproducible bootstrap results.
#'    Defaults to \code{FALSE}.
#' @param warnings, Display warnings produced by lavaan during estimation.
#'    Defaults to \code{FALSE}.
SEM <- function(
          data = NULL,
          version = "1",
          additionalFitMeasures = FALSE,
          alpha = 0.05,
          averageVarianceExtracted = FALSE,
          bootSeed = 1,
          bootstrapCiType = "percentileBiasCorrected",
          bootstrapSamples = 1000,
          bootstrapSamplesBollenStine = 1000,
          ciLevel = 0.95,
          convergenceRateThreshold = 0.1,
          dataType = "raw",
          dependentCorrelation = TRUE,
          efaConstrained = TRUE,
          emulation = "lavaan",
          equalIntercept = FALSE,
          equalLatentCovariance = FALSE,
          equalLatentVariance = FALSE,
          equalLoading = FALSE,
          equalMean = FALSE,
          equalRegression = FALSE,
          equalResidual = FALSE,
          equalResidualCovariance = FALSE,
          equalThreshold = FALSE,
          errorCalculationMethod = "default",
          estimator = "default",
          exogenousCovariateConditional = FALSE,
          exogenousCovariateFixed = TRUE,
          exogenousLatentCorrelation = TRUE,
          factorScaling = "factorLoading",
          freeParameters = list(columns = list(), model = "", modelOriginal = ""),
          group = list(types = "unknown", value = ""),
          heterotraitMonotraitRatio = FALSE,
          impliedCovariance = FALSE,
          informationMatrix = "default",
          latentInterceptFixedToZero = TRUE,
          manifestInterceptFixedToZero = FALSE,
          manifestMeanFixedToZero = FALSE,
          mardiasCoefficient = FALSE,
          maxIterations = 1000,
          meanStructure = FALSE,
          measurementModelReliability = FALSE,
          modelTest = "default",
          models = list(list(name = "Model 1", syntax = list(columns = list(), model = "", modelOriginal = ""))),
          modificationIndex = FALSE,
          modificationIndexHiddenLow = FALSE,
          modificationIndexThreshold = 10,
          naAction = "fiml",
          numberOfAnts = 10,
          observedCovariance = FALSE,
          optimizerFunction = "percentChangeMeanEstimate",
          orthogonal = FALSE,
          pathPlot = FALSE,
          pathPlotLegend = FALSE,
          pathPlotParameter = FALSE,
          pathPlotParameterStandardized = FALSE,
          plotHeight = 320,
          plotWidth = 480,
          rSquared = FALSE,
          reliability = FALSE,
          residualCovariance = FALSE,
          residualSingleIndicatorOmitted = TRUE,
          residualVariance = TRUE,
          sampleSize = 500,
          samplingWeights = list(types = "unknown", value = ""),
          scalingParameter = TRUE,
          searchAlgorithm = "antColonyOptimization",
          seed = 1,
          sensitivityAnalysis = FALSE,
          setSeed = FALSE,
          sizeOfSolutionArchive = 100,
          standardizedEstimate = FALSE,
          standardizedEstimateType = "all",
          standardizedResidual = FALSE,
          standardizedVariable = FALSE,
          threshold = TRUE,
          userGaveSeed = FALSE,
          warnings = FALSE) {

   defaultArgCalls <- formals(jaspSem::SEM)
   defaultArgs <- lapply(defaultArgCalls, eval)
   options <- as.list(match.call())[-1L]
   options <- lapply(options, eval)
   defaults <- setdiff(names(defaultArgs), names(options))
   options[defaults] <- defaultArgs[defaults]
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("bootstrapCiType", "emulation", "errorCalculationMethod", "estimator", "factorScaling", "freeParameters", "group", "informationMatrix", "modelTest", "models", "naAction", "optimizerFunction", "samplingWeights")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspSem", "SEM", "SEM.qml", options, version, TRUE))
}