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
          version = "0.15",
          addScalingParameters = TRUE,
          addThresholds = TRUE,
          bootstrapNumber = 1000,
          categorical = list(),
          ciWidth = 0.95,
          colorPalette = "colorblind",
          correlateDependentVariables = TRUE,
          correlateExogenousLatents = TRUE,
          covar = TRUE,
          covariates = list(),
          cubic = FALSE,
          curveplot = FALSE,
          estimator = "default",
          fixLatentInterceptsToZero = TRUE,
          fixManifestInterceptsToZero = FALSE,
          groupvar = "",
          impliedCov = FALSE,
          intercept = TRUE,
          linear = TRUE,
          mimic = "lavaan",
          misfitplot = FALSE,
          missing = "fiml",
          omitResidualSingleIndicator = TRUE,
          outputAdditionalFitMeasures = FALSE,
          pathplot = FALSE,
          plotHeight = 320,
          plotWidth = 480,
          plot_categorical = "",
          plot_max_n = 150,
          plotmeans = FALSE,
          plotpars = FALSE,
          quadratic = FALSE,
          regressions = list(),
          residCov = FALSE,
          residualVariances = TRUE,
          rsquared = FALSE,
          se = "standard",
          showSyntax = FALSE,
          std = FALSE,
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

   optionsWithFormula <- c("categorical", "colorPalette", "covariates", "groupvar", "plot_categorical", "regressions", "timings", "variables")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspSem::LatentGrowthCurve", data, options, version))
}