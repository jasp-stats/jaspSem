#
# Copyright (C) 2013-2022 University of Amsterdam
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

# This is a generated file. Don't change it

LatentGrowthCurve <- function(
          data = NULL,
          version = "0.18.2",
          additionalFitMeasures = FALSE,
          bootstrapSamples = 1000,
          categorical = list(),
          ciLevel = 0.95,
          colorPalette = "colorblind",
          covariates = list(),
          covaryingLatentCurve = TRUE,
          cubic = FALSE,
          curvePlot = FALSE,
          curvePlotCategorical = "",
          curvePlotMaxLines = 150,
          dependentCorrelation = TRUE,
          emulation = "lavaan",
          errorCalculationMethod = "standard",
          estimator = "default",
          exogenousLatentCorrelation = TRUE,
          group = "",
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
          regressions = list(),
          residualCovariance = FALSE,
          residualSingleIndicatorOmitted = TRUE,
          residualVariance = TRUE,
          scalingParameter = TRUE,
          standardizedEstimate = FALSE,
          syntax = FALSE,
          threshold = TRUE,
          timings = list(),
          variables = list()) {

   defaultArgCalls <- formals(jaspSem::LatentGrowthCurve)
   defaultArgs <- lapply(defaultArgCalls, eval)
   options <- as.list(match.call())[-1L]
   options <- lapply(options, eval)
   defaults <- setdiff(names(defaultArgs), names(options))
   options[defaults] <- defaultArgs[defaults]
   options[["data"]] <- NULL
   options[["version"]] <- NULL

   optionsWithFormula <- c("categorical", "colorPalette", "covariates", "curvePlotCategorical", "group", "regressions", "timings", "variables")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspSem::LatentGrowthCurve", data, options, version))
}