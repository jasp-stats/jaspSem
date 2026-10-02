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

#' MNLFA
#'
#' @param addGroupVariableToData, Save the group variable created by splitting the continuous moderator to the dataset.
#'    Defaults to \code{FALSE}.
#' @param checkModelFitPerGroup, Fit the measurement model separately in each group to verify that the model fits adequately before testing moderation.
#'    Defaults to \code{FALSE}.
#' @param factors, Define latent factors and assign observed indicator variables to each factor.
#' @param includeIndividualModerationsList, For each invariance level, specify which individual measurement parameters should be tested for moderation effects.
#' @param invarianceTestConfigural, Test configural invariance: same factor structure across groups but all parameters free.
#'    Defaults to \code{FALSE}.
#' @param invarianceTestCustom, Define a custom set of parameter constraints for invariance testing.
#'    Defaults to \code{FALSE}.
#' @param invarianceTestMetric, Test metric invariance: constrain factor loadings to be equal across groups.
#'    Defaults to \code{FALSE}.
#' @param invarianceTestScalar, Test scalar invariance: constrain factor loadings and intercepts to be equal across groups.
#'    Defaults to \code{FALSE}.
#' @param invarianceTestStrict, Test strict invariance: constrain factor loadings, intercepts, and residual variances to be equal across groups.
#'    Defaults to \code{FALSE}.
#' @param moderatorInteractionTerms, Include interaction terms between pairs of moderators.
#' @param moderators, Variables hypothesized to moderate the measurement model parameters. Continuous moderators can optionally include squared and cubic effects.
#' @param parameterEstimatesAlphaLevel, Significance level for flagging parameter estimates.
#' @param parameterEstimatesFactorCovariances, Display latent factor covariance estimates.
#'    Defaults to \code{FALSE}.
#' @param parameterEstimatesFactorMeans, Display latent factor mean estimates.
#'    Defaults to \code{FALSE}.
#' @param parameterEstimatesFactorVariance, Display latent factor variance estimates.
#'    Defaults to \code{FALSE}.
#' @param parameterEstimatesIntercepts, Display intercept estimates for observed indicators.
#'    Defaults to \code{FALSE}.
#' @param parameterEstimatesLoadings, Display factor loading estimates.
#'    Defaults to \code{FALSE}.
#' @param parameterEstimatesResidualVariances, Display residual variance estimates for observed indicators.
#'    Defaults to \code{FALSE}.
#' @param splitContinuousVariablesIntoGroups, Number of groups into which continuous moderator variables are split for the assumption check.
#' @param syncAnalysisBox, Click to start or synchronize the analysis. The analysis does not run automatically due to computational intensity.
#'    Defaults to \code{FALSE}.
ModeratedNonLinearFactorAnalysis <- function(
          data = NULL,
          version = "1",
          addGroupVariableToData = FALSE,
          checkModelFitPerGroup = FALSE,
          factors = list(list(indicators = list(), name = "Factor1", title = "Factor 1")),
          includeIndividualModerationsList = list(),
          indicatorPreprocessing = "none",
          invarianceTestConfigural = FALSE,
          invarianceTestCustom = FALSE,
          invarianceTestMetric = FALSE,
          invarianceTestScalar = FALSE,
          invarianceTestStrict = FALSE,
          moderatorInteractionTerms = list(),
          moderators = list(optionKey = "variable", types = list(), value = list()),
          parameterEstimatesAlphaLevel = 0.05,
          parameterEstimatesFactorCovariances = FALSE,
          parameterEstimatesFactorMeans = FALSE,
          parameterEstimatesFactorVariance = FALSE,
          parameterEstimatesIntercepts = FALSE,
          parameterEstimatesLoadings = FALSE,
          parameterEstimatesResidualVariances = FALSE,
          plotHeight = 320,
          plotModelList = list(),
          plotWidth = 480,
          showSyntax = FALSE,
          splitContinuousVariablesIntoGroups = 2,
          syncAnalysisBox = FALSE,
          warnings = FALSE) {

   defaultArgCalls <- formals(jaspSem::ModeratedNonLinearFactorAnalysis)
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

   optionsWithFormula <- c("factors", "includeIndividualModerationsList", "moderatorInteractionTerms", "moderators", "plotModelList")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspSem", "ModeratedNonLinearFactorAnalysis", "ModeratedNonLinearFactorAnalysis.qml", options, version, TRUE))
}