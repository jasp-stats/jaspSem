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

#' MIMIC
#'
#' @param bootstrapCiType, Select the type of bootstrap confidence interval to compute
#' @param bootstrapSamples, Specify the number of bootstrap samples
#' @param ciLevel, Set the confidence level for the interval estimates
#' @param emulation, Select the software to emulate estimation behavior
#' \itemize{
#'   \item \code{"eqs"}: Emulate EQS estimation methods
#'   \item \code{"lavaan"}: Use Lavaan default estimation method
#'   \item \code{"mplus"}: Emulate Mplus estimation methods
#' }
#' @param errorCalculationMethod, Select the method for calculating standard errors and confidence intervals
#' \itemize{
#'   \item \code{"standard"}: Use standard maximum likelihood estimation for standard errors
#'   \item \code{"bootstrap"}: Use bootstrap method for estimating standard errors and confidence intervals
#'   \item \code{"robust"}: Use robust estimation for standard errors
#' }
#' @param estimator, Choose the estimator for model fitting
#' \itemize{
#'   \item \code{"dwls"}: Diagonally Weighted Least Squares estimator
#'   \item \code{"wls"}: Weighted Least Squares estimator
#'   \item \code{"gls"}: Generalized Least Squares estimator
#'   \item \code{"ml"}: Maximum Likelihood estimator
#'   \item \code{"uls"}: Unweighted Least Squares estimator
#'   \item \code{"default"}: Use the default estimator based on the model and data
#' }
#' @param naAction, Select the method for handling missing data in estimation
#' \itemize{
#'   \item \code{"fiml"}: Use Full Information Maximum Likelihood to handle missing data
#'   \item \code{"listwise"}: Exclude cases with missing data (listwise deletion)
#' }
MIMIC <- function(
          data = NULL,
          version = "0.95",
          additionalFitMeasures = FALSE,
          bootstrapCiType = "percentileBiasCorrected",
          bootstrapSamples = 1000,
          ciLevel = 0.95,
          emulation = "lavaan",
          errorCalculationMethod = "standard",
          estimator = "default",
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

   return(jaspBase::runWrappedAnalysis("jaspSem", "MIMIC", "Mimic.qml", options, version, FALSE))
}