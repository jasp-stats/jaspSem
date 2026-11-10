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

#' Partial Least Squares SEM
#'
#' @param addConstructScores, Save estimated construct scores as new columns in the dataset.
#'    Defaults to \code{FALSE}.
#' @param additionalFitMeasures, Display additional fit indices for evaluating model fit.
#'    Defaults to \code{FALSE}.
#' @param benchmark, Select a benchmark model to compare the prediction accuracy of the PLS-SEM model.
#' \itemize{
#'   \item \code{"none"}: No benchmark comparison.
#'   \item \code{"lm"}: Compare against a linear regression model.
#'   \item \code{"GSCA"}: Compare against Generalized Structured Component Analysis.
#'   \item \code{"PCA"}: Compare against Principal Component Analysis.
#'   \item \code{"MAXVAR"}: Compare against the MAXVAR approach.
#'   \item \code{"all"}: Compare against all available benchmark models.
#' }
#' @param bootstrapSamples, Number of bootstrap resamples to draw.
#' @param ciLevel, Width of the bootstrap confidence intervals.
#' @param consistentPartialLeastSquares, Use consistent PLS (PLSc) to correct for attenuation, producing consistent estimates for common factor models.
#'    Defaults to \code{TRUE}.
#' @param convergenceCriterion, Criterion used to assess convergence of the PLS algorithm.
#' @param endogenousIndicatorPrediction, Perform k-fold cross-validation to predict endogenous indicator scores.
#'    Defaults to \code{FALSE}.
#' @param errorCalculationMethod, Select the error calculation method.
#' \itemize{
#'   \item \code{"none"}: Do not compute standard errors or confidence intervals.
#'   \item \code{"bootstrap"}: Compute standard errors and confidence intervals using bootstrap resampling.
#' }
#' @param group, Select a grouping variable to perform multi-group PLS-SEM analysis.
#' @param handlingOfInadmissibles, How to handle bootstrap samples that produce inadmissible results (e.g., Heywood cases).
#' \itemize{
#'   \item \code{"replace"}: Replace inadmissible bootstrap samples with new ones.
#'   \item \code{"ignore"}: Keep inadmissible results in the bootstrap distribution.
#'   \item \code{"drop"}: Drop inadmissible bootstrap samples, reducing the effective number of resamples.
#' }
#' @param impliedConstructCorrelation, Display the model-implied correlation matrix of the constructs.
#'    Defaults to \code{FALSE}.
#' @param impliedIndicatorCorrelation, Display the model-implied correlation matrix of the indicator variables.
#'    Defaults to \code{FALSE}.
#' @param innerWeightingScheme, Choose the scheme for estimating inner weights relating constructs to each other.
#' @param kFolds, Number of folds for cross-validation. Higher values increase computation time but reduce bias.
#' @param mardiasCoefficient, Display Mardia's multivariate skewness and kurtosis coefficients to assess multivariate normality.
#'    Defaults to \code{FALSE}.
#' @param models, Specify the PLS-SEM model using cSEM syntax. Define measurement models (e.g., 'eta1 =~ y1 + y2 + y3') and structural model (e.g., 'eta2 ~ eta1').
#' @param observedConstructCorrelation, Display the observed correlation matrix of the constructs.
#'    Defaults to \code{FALSE}.
#' @param observedIndicatorCorrelation, Display the observed correlation matrix of the indicator variables.
#'    Defaults to \code{FALSE}.
#' @param omfBootstrapSamples, Number of bootstrap samples for the overall model fit test.
#' @param omfSignificanceLevel, Significance level for the overall model fit test.
#' @param overallModelFit, Perform an overall model fit test using bootstrap-based tests.
#'    Defaults to \code{FALSE}.
#' @param rSquared, Display the proportion of variance explained for each endogenous construct.
#'    Defaults to \code{FALSE}.
#' @param reliabilityMeasures, Display reliability measures for each construct (e.g., Cronbach's alpha, composite reliability).
#'    Defaults to \code{FALSE}.
#' @param repetitions, Number of times the cross-validation is repeated to reduce variance in the prediction metrics.
#' @param saturatedStructuralModel, Use a saturated structural model as a reference for the model fit test.
#'    Defaults to \code{FALSE}.
#' @param structuralModelIgnored, Estimate the measurement model only, ignoring the structural (inner) model.
#'    Defaults to \code{FALSE}.
#' @param tolerance, Tolerance threshold for the convergence criterion. Smaller values require stricter convergence.
PLSSEM <- function(
          data = NULL,
          version = "1",
          addConstructScores = FALSE,
          additionalFitMeasures = FALSE,
          benchmark = "none",
          bootstrapSamples = 200,
          ciLevel = 0.95,
          consistentPartialLeastSquares = TRUE,
          convergenceCriterion = "absoluteDifference",
          endogenousIndicatorPrediction = FALSE,
          errorCalculationMethod = "none",
          group = list(types = "unknown", value = ""),
          handlingOfInadmissibles = "replace",
          impliedConstructCorrelation = FALSE,
          impliedIndicatorCorrelation = FALSE,
          innerWeightingScheme = "path",
          kFolds = 10,
          mardiasCoefficient = FALSE,
          models = list(list(name = "Model", syntax = list(columns = list(), model = "", modelOriginal = ""))),
          observedConstructCorrelation = FALSE,
          observedIndicatorCorrelation = FALSE,
          omfBootstrapSamples = 499,
          omfSignificanceLevel = 0.05,
          overallModelFit = FALSE,
          plotHeight = 320,
          plotWidth = 480,
          rSquared = FALSE,
          reliabilityMeasures = FALSE,
          repetitions = 10,
          saturatedStructuralModel = FALSE,
          seed = 1,
          setSeed = FALSE,
          structuralModelIgnored = FALSE,
          tolerance = 1e-05) {

   defaultArgCalls <- formals(jaspSem::PLSSEM)
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

   optionsWithFormula <- c("convergenceCriterion", "group", "innerWeightingScheme", "models")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspSem", "PLSSEM", "PLSSEM.qml", options, version, TRUE))
}