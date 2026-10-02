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

#' MIMIC Model
#'
#' @param additionalFitMeasures, Display additional fit indices for evaluating model fit (e.g., CFI, TLI, RMSEA, SRMR).
#'    Defaults to \code{FALSE}.
#' @param bootstrapCiType, Select the type of bootstrap confidence interval to compute
#' @param bootstrapSamples, Specify the number of bootstrap samples
#' @param ciLevel, Set the confidence level for the interval estimates
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
#' @param fixPredictorVariancesAndCovariances, When checked, predictor variances and covariances are fixed to sample values. Uncheck to freely estimate them.
#'    Defaults to \code{FALSE}.
#' @param includePredictorCovariances, When checked, all pairwise covariances between predictors are included in the model syntax.
#'    Defaults to \code{TRUE}.
#' @param indicators, Observed indicator variables that define the latent variable in the MIMIC model.
#' @param naAction, Select the method for handling missing data in estimation
#' \itemize{
#'   \item \code{"fiml"}: Use Full Information Maximum Likelihood to handle missing data
#'   \item \code{"listwise"}: Exclude cases with missing data (listwise deletion)
#' }
#' @param pathPlot, Display a path diagram of the MIMIC model.
#'    Defaults to \code{FALSE}.
#' @param pathPlotLegend, Display a legend in the path diagram.
#'    Defaults to \code{FALSE}.
#' @param pathPlotParameter, Display parameter estimates on the path diagram.
#'    Defaults to \code{FALSE}.
#' @param predictors, Observed predictor variables (causes) that directly influence the latent variable.
#' @param rSquared, Display the proportion of variance explained for each endogenous variable.
#'    Defaults to \code{FALSE}.
#' @param standardizedEstimate, Standardize all variables (mean = 0, sd = 1) before estimation.
#'    Defaults to \code{FALSE}.
#' @param standardizedEstimateType, Type of standardization.
#' \itemize{
#'   \item \code{"all"}: Standardize based on variances of both observed and latent variables.
#'   \item \code{"latents"}: Standardize based on latent variable variances only.
#'   \item \code{"nox"}: Standardize excluding exogenous covariates.
#' }
#' @param syntax, Display the lavaan syntax used to estimate the model.
#'    Defaults to \code{FALSE}.
MIMIC <- function(
          data = NULL,
          version = "1",
          additionalFitMeasures = FALSE,
          bootstrapCiType = "percentileBiasCorrected",
          bootstrapSamples = 1000,
          ciLevel = 0.95,
          emulation = "lavaan",
          errorCalculationMethod = "standard",
          estimator = "default",
          fixPredictorVariancesAndCovariances = FALSE,
          includePredictorCovariances = TRUE,
          indicators = list(types = list(), value = list()),
          naAction = "fiml",
          pathPlot = FALSE,
          pathPlotLegend = FALSE,
          pathPlotParameter = FALSE,
          plotHeight = 320,
          plotWidth = 480,
          predictors = list(types = list(), value = list()),
          rSquared = FALSE,
          standardizedEstimate = FALSE,
          standardizedEstimateType = "all",
          syntax = FALSE) {

   defaultArgCalls <- formals(jaspSem::MIMIC)
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

   optionsWithFormula <- c("bootstrapCiType", "indicators", "predictors")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspSem", "MIMIC", "Mimic.qml", options, version, TRUE))
}