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

#' SEM
#'
SEM <- function(
          data = NULL,
          version = "0.95",
          additionalFitMeasures = FALSE,
          alpha = 0.05,
          ave = FALSE,
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
          htmt = FALSE,
          impliedCovariance = FALSE,
          informationMatrix = "default",
          latentInterceptFixedToZero = TRUE,
          manifestInterceptFixedToZero = FALSE,
          manifestMeanFixedToZero = FALSE,
          mardiasCoefficient = FALSE,
          maxIterations = 1000,
          meanStructure = FALSE,
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

   return(jaspBase::runWrappedAnalysis("jaspSem", "SEM", "SEM.qml", options, version, FALSE))
}