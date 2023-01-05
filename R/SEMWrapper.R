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

SEM <- function(
          data = NULL,
          version = "0.15",
          Data = "raw",
          SampleSize = 0,
          auto.cov.lv.x = TRUE,
          auto.cov.y = TRUE,
          auto.delta = TRUE,
          auto.efa = TRUE,
          auto.fix.single = TRUE,
          auto.th = TRUE,
          auto.var = TRUE,
          bootCItype = "bca.simple",
          ciWidth = 0.95,
          emulation = "lavaan",
          eq_intercepts = FALSE,
          eq_loadings = FALSE,
          eq_lvcovariances = FALSE,
          eq_means = FALSE,
          eq_regressions = FALSE,
          eq_residualcovariances = FALSE,
          eq_residuals = FALSE,
          eq_thresholds = FALSE,
          eq_variances = FALSE,
          errorCalculationBootstrapSamples = 1000,
          estimator = "default",
          factorStandardisation = "auto.fix.first",
          fixed.x = TRUE,
          group.partial = "",
          groupingVariable = "",
          information = "expected",
          int.lv.fixed = TRUE,
          int.ov.fixed = FALSE,
          meanstructure = FALSE,
          miHideLow = FALSE,
          miThreshold = 10,
          missing = "ml",
          models = list(list(modelName = "Model 1", syntax = list(columns = list(), model = "", modelOriginal = ""))),
          orthogonal = FALSE,
          outputAdditionalFitMeasures = FALSE,
          outputImpliedCovariances = FALSE,
          outputMardiasCoefficients = FALSE,
          outputModificationIndices = FALSE,
          outputObservedCovariances = FALSE,
          outputPathPlot = FALSE,
          outputRSquared = FALSE,
          outputResidualCovariances = FALSE,
          outputStandardizedResiduals = FALSE,
          pathPlotLegend = FALSE,
          pathPlotPar = FALSE,
          plotHeight = 320,
          plotWidth = 480,
          sampling.weights = "",
          se = "standard",
          std = FALSE,
          std.ov = FALSE,
          test = "default") {

   defaultArgCalls <- formals(jaspSem::SEM)
   defaultArgs <- lapply(defaultArgCalls, eval)
   options <- as.list(match.call())[-1L]
   options <- lapply(options, eval)
   defaults <- setdiff(names(defaultArgs), names(options))
   options[defaults] <- defaultArgs[defaults]
   options[["data"]] <- NULL
   options[["version"]] <- NULL

   optionsWithFormula <- c("bootCItype", "emulation", "estimator", "factorStandardisation", "group.partial", "groupingVariable", "information", "missing", "models", "sampling.weights", "test")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspSem::SEM", data, options, version))
}