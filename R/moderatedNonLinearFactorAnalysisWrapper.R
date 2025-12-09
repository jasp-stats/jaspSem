

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

#' MNLFA (beta)
#'
ModeratedNonLinearFactorAnalysis <- function(
    data = NULL,
    version = "0",
    addGroupVariableToData = FALSE,
    checkFitPerGroup = FALSE,
    configuralInvariance = FALSE,
    continuousVariableSplit = 2,
    customInvariance = FALSE,
    factorCovarianceEstimates = FALSE,
    factorMeanEstimates = FALSE,
    factorVarianceEstimates = FALSE,
    factors = list(list(indicators = list(), name = "Factor1", title = "Factor 1")),
    includeIndividualModerationsList = list(),
    interceptEstimates = FALSE,
    loadingEstimates = FALSE,
    metricInvariance = FALSE,
    moderatorInteractionTerms = list(),
    moderators = list(optionKey = "variable", types = list(), value = list()),
    parameterAlphaLevel = 0.05,
    plotHeight = 320,
    plotModelList = list(),
    plotWidth = 480,
    residualVarianceEstimates = FALSE,
    scalarInvariance = FALSE,
    showSyntax = FALSE,
    strictInvariance = FALSE,
    syncAnalysisBox = FALSE) {

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
