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

#' BayesianSEM
#'
BayesianSEM <- function(
          data = NULL,
          version = "0.95",
          additionalFitMeasures = FALSE,
          ciLevel = 0.95,
          convergenceDiagnostics = FALSE,
          dataType = "raw",
          equalIntercept = FALSE,
          equalLatentCovariance = FALSE,
          equalLatentVariance = FALSE,
          equalLoading = FALSE,
          equalMean = FALSE,
          equalRegression = FALSE,
          equalResidual = FALSE,
          equalResidualCovariance = FALSE,
          equalThreshold = FALSE,
          factorScaling = "factorLoading",
          group = "",
          latentInterceptFixedToZero = TRUE,
          manifestInterceptFixedToZero = FALSE,
          mcmcBurnin = 500L,
          mcmcChains = 3L,
          mcmcSamples = 1000L,
          mcmcThin = 1L,
          meanStructure = FALSE,
          models = list(list(name = "Model 1", syntax = "")),
          orthogonal = FALSE,
          posteriorPredictivePvalue = FALSE,
          priorType = "default",
          priorLoadingParam1 = 0,
          priorLoadingParam2 = 10,
          priorRegressionParam1 = 0,
          priorRegressionParam2 = 10,
          priorObservedInterceptParam1 = 0,
          priorObservedInterceptParam2 = 32,
          priorLatentInterceptParam1 = 0,
          priorLatentInterceptParam2 = 10,
          priorThresholdParam1 = 0,
          priorThresholdParam2 = 1.5,
          priorResidualSdParam1 = 1,
          priorResidualSdParam2 = 0.5,
          priorLatentSdParam1 = 1,
          priorLatentSdParam2 = 0.5,
          priorCorrelationParam1 = 1,
          priorCorrelationParam2 = 1,
          seed = 1,
          setSeed = FALSE,
          tracePlots = FALSE,
          tracePlotsType = "all",
          warnings = FALSE) {

   defaultArgCalls <- formals(jaspSem::BayesianSEM)
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

   return(jaspBase::runWrappedAnalysis("jaspSem", "BayesianSEM", "BayesianSEM.qml", options, version, FALSE))
}
