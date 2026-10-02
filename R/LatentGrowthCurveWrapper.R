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

#' Latent Growth
#'
#' @param additionalFitMeasures, Display additional model fit measures
#'    Defaults to \code{FALSE}.
#' @param bootstrapCiType, Select the type of bootstrap confidence interval to compute
#' @param bootstrapSamples, Specify the number of bootstrap samples
#' @param categorical, Categorical variables acting as factors
#' @param ciLevel, Set the confidence level for the interval estimates
#' @param colorPalette, Select a color palette for the plots
#' @param covaryingLatentCurve, Include covariance between latent growth components
#'    Defaults to \code{TRUE}.
#' @param cubic, Include a cubic component in the model
#'    Defaults to \code{FALSE}.
#' @param curvePlot, Generate a curve plot of the growth trajectories
#'    Defaults to \code{FALSE}.
#' @param curvePlotCategorical, Select a categorical variable to color the curves
#' @param curvePlotMaxLines, Set the maximum number of lines to display in the curve plot
#' @param emulation, Select the software to emulate estimation behavior
#' \itemize{
#'   \item \code{"lavaan"}: Use Lavaan default estimation method
#'   \item \code{"mplus"}: Emulate Mplus estimation methods
#'   \item \code{"eqs"}: Emulate EQS estimation methods
#' }
#' @param errorCalculationMethod, Select the method for calculating standard errors and confidence intervals
#' \itemize{
#'   \item \code{"standard"}: Use standard maximum likelihood estimation for standard errors
#'   \item \code{"robust"}: Use robust estimation for standard errors
#'   \item \code{"bootstrap"}: Use bootstrap method for estimating standard errors and confidence intervals
#' }
#' @param estimator, Choose the estimator for model fitting
#' \itemize{
#'   \item \code{"default"}: Use the default estimator based on the model and data
#'   \item \code{"ml"}: Maximum Likelihood estimator
#'   \item \code{"gls"}: Generalized Least Squares estimator
#'   \item \code{"wls"}: Weighted Least Squares estimator
#'   \item \code{"uls"}: Unweighted Least Squares estimator
#'   \item \code{"dwls"}: Diagonally Weighted Least Squares estimator
#' }
#' @param impliedCovariance, Display the implied covariance matrix
#'    Defaults to \code{FALSE}.
#' @param intercept, Include an intercept in the model
#'    Defaults to \code{TRUE}.
#' @param linear, Include a linear component in the model
#'    Defaults to \code{TRUE}.
#' @param naAction, Select the method for handling missing data in estimation
#' \itemize{
#'   \item \code{"fiml"}: Use Full Information Maximum Likelihood to handle missing data
#'   \item \code{"listwise"}: Exclude cases with missing data (listwise deletion)
#' }
#' @param pathPlot, Generate a path diagram of the model
#'    Defaults to \code{FALSE}.
#' @param pathPlotMean, Display means in the model plot
#'    Defaults to \code{FALSE}.
#' @param pathPlotParameter, Display parameter estimates in the model plot
#'    Defaults to \code{FALSE}.
#' @param quadratic, Include a quadratic component in the model
#'    Defaults to \code{FALSE}.
#' @param rSquared, Display R-squared values for variables
#'    Defaults to \code{FALSE}.
#' @param regressions, Variables to use as regressors in the model
#' @param residualCovariance, Display the residual covariance matrix
#'    Defaults to \code{FALSE}.
#' @param standardizedEstimate, Include standardized estimates in the output
#'    Defaults to \code{FALSE}.
#' @param standardizedEstimateType, Select the type of standardized estimates to include
#' \itemize{
#'   \item \code{"all"}: Include all standardized estimates
#'   \item \code{"latents"}: Include standardized estimates for latent variables only
#'   \item \code{"nox"}: Exclude standardized estimates for exogenous covariates
#' }
#' @param syntax, Show the lavaan model syntax
#'    Defaults to \code{FALSE}.
#' @param timings, Variable timing
#' @param variables, Variables to include in the latent growth curve model
LatentGrowthCurve <- function(
          data = NULL,
          version = "1",
          additionalFitMeasures = FALSE,
          bootstrapCiType = "percentileBiasCorrected",
          bootstrapSamples = 1000,
          categorical = list(types = list(), value = list()),
          ciLevel = 0.95,
          colorPalette = "colorblind",
          covariates = list(types = list(), value = list()),
          covaryingLatentCurve = TRUE,
          cubic = FALSE,
          curvePlot = FALSE,
          curvePlotCategorical = list(types = "unknown", value = ""),
          curvePlotMaxLines = 150,
          dependentCorrelation = TRUE,
          emulation = "lavaan",
          errorCalculationMethod = "standard",
          estimator = "default",
          exogenousLatentCorrelation = TRUE,
          group = list(types = "unknown", value = ""),
          impliedCovariance = FALSE,
          intercept = TRUE,
          latentInterceptFixedToZero = TRUE,
          linear = TRUE,
          manifestInterceptFixedToZero = FALSE,
          misfitPlot = FALSE,
          naAction = "fiml",
          pathPlot = FALSE,
          pathPlotMean = FALSE,
          pathPlotParameter = FALSE,
          plotHeight = 320,
          plotWidth = 480,
          quadratic = FALSE,
          rSquared = FALSE,
          regressions = list(types = list(), value = list()),
          residualCovariance = FALSE,
          residualSingleIndicatorOmitted = TRUE,
          residualVariance = TRUE,
          scalingParameter = TRUE,
          standardizedEstimate = FALSE,
          standardizedEstimateType = "all",
          syntax = FALSE,
          threshold = TRUE,
          timings = list(optionKey = "variable", types = list(), value = list()),
          variables = list(types = list(), value = list())) {

   defaultArgCalls <- formals(jaspSem::LatentGrowthCurve)
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

   optionsWithFormula <- c("bootstrapCiType", "categorical", "colorPalette", "covariates", "curvePlotCategorical", "group", "regressions", "timings", "variables")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspSem", "LatentGrowthCurve", "LatentGrowthCurve.qml", options, version, TRUE))
}