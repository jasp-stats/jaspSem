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
#' @param categoricalIndicatorLink, Link function of the ordinal and binary indicators (indicators assigned as ordinal columns): logit (default) or probit. Available only when an ordinal indicator is assigned; continuous indicators are not affected.
#' @param checkModelFitPerGroup, Fit the measurement model separately in each group to verify that the model fits adequately before testing moderation.
#'    Defaults to \code{FALSE}.
#' @param errorCalculationMethod, Standard errors for the parameter tables, the likelihood-ratio tests and the plot bands; all three come from the same fit, so changing them does not refit the models. Huber-White sandwich (default) uses the exact information matrix. The reduced-information variant follows the convention of other MNLFA software; if its information matrix is not positive definite for a model, that model is shown without standard errors. Standard uses the inverse observed information and the ordinary likelihood-ratio test; the sandwich choices use scaled tests. Sandwich standard errors are valid for the model as fitted (its pseudo-true parameters); they do not correct bias from a wrong response model, invalid anchors or unmodelled dependence. Their calibration was studied only for ordinal, binary, continuous and mixed indicators, one or two factors, logit and probit links, and sample sizes of 500 to 2000.
#' @param factorScoreColumnPrefix, Text placed before each factor title in the saved score column names.
#' @param factorScoreModel, Select one of the enabled invariance models for the factor scores. Scores are saved only from a model that converged; otherwise a footnote under the table 'Factor Scores' says why.
#' @param factorScoreSavedToData, Save expected a posteriori factor scores from the selected model. Scores include moderated factor means and variances; posterior standard deviations do not include parameter uncertainty.
#'    Defaults to \code{FALSE}.
#' @param factors, Define latent factors and assign observed indicator variables to each factor. Scale variables are continuous indicators; ordinal variables are categorical (binary or ordinal) indicators.
#' @param includeIndividualModerationsList, For each model, tick the parameters that depend on the moderators; a ticked parameter depends on all moderator terms. The default anchor of a factor is its first indicator that loads on this factor only, otherwise its first indicator that is not already another factor's anchor; its loading and intercept are unticked by default. When a factor's default anchor changes (for example, its first indicator is removed), the loading and intercept boxes of the old and the new anchor return to their defaults. All other boxes keep their values when you rename a factor, add, remove or reorder indicators, or reorder the moderators; an indicator that you assign again gets back its earlier values. Residual variances exist only for continuous (scale) indicators. The factor correlation can depend on the moderators only with exactly two factors.
#' @param indicatorPreprocessing, Transformation of the continuous (scale) indicators before fitting, with the mean and standard deviation of the analysed cases. Ordinal indicators are never transformed.
#' \itemize{
#'   \item \code{"none"}: Use the continuous indicators as they are.
#'   \item \code{"center"}: Subtract the mean from each continuous indicator.
#'   \item \code{"standardize"}: Subtract the mean and divide by the standard deviation of each continuous indicator.
#' }
#' @param invarianceTestConfigural, Preset: loadings and intercepts (thresholds) of all indicators except the anchors, residual variances, factor variances, factor means and, with two factors, the factor correlation depend on the moderators.
#'    Defaults to \code{FALSE}.
#' @param invarianceTestCustom, Preset: nothing depends on the moderators; tick the parameters that should.
#'    Defaults to \code{FALSE}.
#' @param invarianceTestMetric, Preset: as Configural, but no loading depends on the moderators.
#'    Defaults to \code{FALSE}.
#' @param invarianceTestScalar, Preset: as Metric, but no intercept or threshold depends on the moderators.
#'    Defaults to \code{FALSE}.
#' @param invarianceTestStrict, Preset: as Scalar, but no residual variance depends on the moderators. With only ordinal indicators, Scalar and Strict are the same model.
#'    Defaults to \code{FALSE}.
#' @param moderatorInteractionTerms, Include interaction terms between pairs of moderators. An included interaction stays included when you reorder the moderators; its name follows their order.
#' @param moderators, Variables that may moderate the parameters of the measurement model. A scale moderator with two distinct values is used as coded (effects per unit); with more values it is standardized on the analysed cases (effects per standard deviation) and can also enter as a square and a cube. A nominal moderator is dummy coded against its first level. Cases with a missing moderator value are excluded from all models.
#' @param parameterEstimatesAlphaLevel, Significance level alpha of the tests; confidence intervals and plot bands have level 1 - alpha.
#' @param parameterEstimatesFactorCovariances, Display the factor correlations: the baseline on the Fisher z and the correlation scale, moderation effects on the Fisher z scale.
#'    Defaults to \code{FALSE}.
#' @param parameterEstimatesFactorMeans, Display the moderation effects on the factor means; the baseline mean is fixed at 0.
#'    Defaults to \code{FALSE}.
#' @param parameterEstimatesFactorVariance, Display the moderation effects on the factor variances as variance ratios; the baseline variance is fixed at 1.
#'    Defaults to \code{FALSE}.
#' @param parameterEstimatesIntercepts, Display the intercepts of continuous indicators and the thresholds of ordinal indicators, with their moderation effects.
#'    Defaults to \code{FALSE}.
#' @param parameterEstimatesLoadings, Display the loadings: baseline and moderation effects.
#'    Defaults to \code{FALSE}.
#' @param parameterEstimatesResidualVariances, Display the residual variances of continuous indicators; moderation effects are shown as variance ratios.
#'    Defaults to \code{FALSE}.
#' @param showSyntax, Show R code that fits each model with the MoFIA package outside JASP.
#'    Defaults to \code{FALSE}.
#' @param splitContinuousVariablesIntoGroups, Number of groups into which continuous moderator variables are split for the assumption check.
#' @param syncAnalysisBox, Fit the models. While unchecked, nothing is fitted; results of the last synchronized run stay visible as long as the model settings are unchanged.
#'    Defaults to \code{FALSE}.
#' @param warnings, List the warnings of each model fit. Identification, convergence and quadrature notes are always shown.
#'    Defaults to \code{FALSE}.
ModeratedNonLinearFactorAnalysis <- function(
          data = NULL,
          version = "1",
          addGroupVariableToData = FALSE,
          categoricalIndicatorLink = "logit",
          checkModelFitPerGroup = FALSE,
          errorCalculationMethod = "robustHuberWhite",
          factorScoreColumnPrefix = "FS_",
          factorScoreModel = "",
          factorScoreSavedToData = FALSE,
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

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("categoricalIndicatorLink", "errorCalculationMethod", "factorScoreModel", "factors", "includeIndividualModerationsList", "moderatorInteractionTerms", "moderators", "plotModelList")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspSem", "ModeratedNonLinearFactorAnalysis", "ModeratedNonLinearFactorAnalysis.qml", options, version, TRUE))
}