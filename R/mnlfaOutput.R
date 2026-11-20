#
# Copyright (C) 2013-2026 University of Amsterdam
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

# MNLFA with MoFIA: model construction and pre-check messages, fitting with one state per model, "Show syntax", and
# the legacy paths. The fit table is in mnlfaFitTable.R, the parameter tables in mnlfaParameterTables.R, the plots and
# factor scores in mnlfaPlots.R and mnlfaScores.R.

# Options that define the data and the moderator design of every MoFIA model.
.mnlfaBaseDeps <- c("factors", "moderators", "moderatorInteractionTerms", "indicatorPreprocessing")

# Options that define all enabled models together; the display of the MoFIA results depends on them.
.mnlfaMofiaModelDeps <- c(.mnlfaBaseDeps, "includeIndividualModerationsList", "invarianceTestConfigural",
                          "invarianceTestMetric", "invarianceTestScalar", "invarianceTestStrict",
                          "invarianceTestCustom")

# The "Model Specification" container also depends on the link, so that it exists only while every fit
# state it was built from is still valid (the Sync-off display relies on this).
.mnlfaMofiaDisplayDeps <- c(.mnlfaMofiaModelDeps, "categoricalIndicatorLink")

# Any indicator that is not a scale variable cannot use the legacy data handling of the fit per group, which converts
# every column to numeric; such data take the group CFA with ordered indicators.
.mnlfaHasCategoricalIndicators <- function(options) {
  types <- unlist(lapply(options[["factors"]], function(factor) factor[["indicators.types"]]), use.names = FALSE)
  return(any(types != "scale"))
}

#' The MoFIA part of the analysis
#'
#' Builds every enabled model, fits the accepted ones (only with Sync on: `fitModels`) and shows the
#' pre-check messages, the fit table, the parameter tables, the plots and the syntax. With Sync off, output of the
#' last synchronized run is rebuilt from the cached states as long as it is still valid (its container exists);
#' nothing is fitted then. Returns what the factor-score columns need (`ready` FALSE when no valid model output
#' exists); the columns themselves are written last in the run (`.mnlfaMofiaScoreColumns`).
#' @noRd
.mnlfaMofiaAnalysis <- function(jaspResults, dataset, options, fitModels) {

  # Development builds of this rework showed interim estimates tables, now replaced by the parameter tables; an
  # analysis saved with such a build must not keep them.
  if (!is.null(jaspResults[["mofiaEstimatesContainer"]]))
    jaspResults[["mofiaEstimatesContainer"]] <- NULL

  notReady <- list(ready = FALSE)
  if (!fitModels && is.null(jaspResults[["mofiaContainer"]]))
    return(notReady)

  container <- jaspResults[["mofiaContainer"]]
  if (is.null(container)) {
    container <- createJaspContainer(gettext("Model Specification"))
    container$dependOn(.mnlfaMofiaDisplayDeps)
    container$position <- 100
    jaspResults[["mofiaContainer"]] <- container
  }

  spec     <- .mnlfaReadSpec(options, dataset)
  prepared <- if (length(spec[["errors"]]) == 0) .mnlfaMofiaPreparedState(jaspResults, spec, dataset)
  errors   <- c(spec[["errors"]], prepared[["errors"]])
  if (length(errors) > 0) {
    if (is.null(container[["specificationError"]])) {
      errorHtml <- createJaspHtml(text = paste(.mnlfaHtmlEscape(errors), collapse = "<br>"),
                                  title = gettext("The model specification cannot be used"))
      errorHtml$position <- 1
      container[["specificationError"]] <- errorHtml
    }
    return(notReady)
  }

  if (length(prepared[["footnotes"]]) > 0 && is.null(container[["dataNotes"]])) {
    notes <- createJaspHtml(text = paste(.mnlfaHtmlEscape(prepared[["footnotes"]]), collapse = "<br>"),
                            title = gettext("Data"))
    notes$position <- 1
    container[["dataNotes"]] <- notes
  }

  if (length(spec[["models"]]) == 0) {
    if (is.null(container[["noModels"]]))
      container[["noModels"]] <- createJaspHtml(text = gettext("Select at least one invariance model."))
    return(notReady)
  }

  models <- lapply(stats::setNames(names(spec[["models"]]), names(spec[["models"]])),
                   function(key) .mnlfaMofiaModelState(jaspResults, spec, prepared, key))
  fits   <- .mnlfaMofiaFitStates(jaspResults, spec, models, fitModels)

  for (k in seq_along(models))
    .mnlfaMofiaModelMessages(container, models[[k]], fits[[names(models)[k]]], names(models)[k], k, options)
  .mnlfaMofiaFitTable(jaspResults, spec, prepared, models, fits, options)
  .mnlfaMofiaParameterTables(jaspResults, spec, prepared, models, fits, options)
  .mnlfaMofiaPlots(jaspResults, spec, prepared, models, fits, options)
  .mnlfaMofiaSyntaxOutput(jaspResults, spec, prepared, models, options)
  return(list(ready = TRUE, spec = spec, prepared = prepared, models = models, fits = fits))
}

#' The factor-score columns, written or removed as the last step of a run
#'
#' Creating or deleting a data column makes JASP refresh its column-name encoding while the analysis is running; names
#' decoded afterwards in the same run (the legacy fit-per-group preparation) then no longer match, which ended the
#' analysis with an error and lost its cached fits (observed in JASP Desktop 0.98.1). The columns are therefore
#' changed only after every other output of the run, as jaspFactor's CFA does. `mofia`: the result of
#' `.mnlfaMofiaAnalysis`, or NULL when the analysis has no model specification.
#' @noRd
.mnlfaMofiaScoreColumns <- function(jaspResults, mofia, options) {
  if (isTRUE(mofia[["ready"]]))
    .mnlfaMofiaFactorScores(jaspResults, mofia[["spec"]], mofia[["prepared"]], mofia[["models"]], mofia[["fits"]],
                            options)
  else
    .mnlfaScoresUnavailable(jaspResults)
  return()
}

# The prepared data, cached; they depend on the base options only.
.mnlfaMofiaPreparedState <- function(jaspResults, spec, dataset) {
  if (!is.null(jaspResults[["mofiaPreparedState"]]))
    return(jaspResults[["mofiaPreparedState"]]$object)

  prepared <- .mnlfaPrepareData(spec, dataset)
  state    <- createJaspState(prepared)
  state$dependOn(.mnlfaBaseDeps)
  jaspResults[["mofiaPreparedState"]] <- state
  return(prepared)
}

#' The state of one model: specification, pre-checks and the MoFIA model
#'
#' The state depends on the base options and the model's own switch. Its own subtree of
#' `includeIndividualModerationsList` enters through `signature`, the model's specification as read from
#' that subtree: a stale signature rebuilds the state, while changes to other models' settings leave it
#' alone (the tree holds one entry per enabled model, so a positional nested dependency would shift).
#' @noRd
.mnlfaMofiaModelState <- function(jaspResults, spec, prepared, key) {
  stateKey  <- paste0("mofiaModelState_", key)
  signature <- spec[["models"]][[key]]
  cached    <- jaspResults[[stateKey]]
  if (!is.null(cached) && identical(cached$object[["signature"]], signature))
    return(cached$object)

  m      <- spec[["models"]][[key]]
  result <- list(signature = signature, errors = m[["errors"]], warnings = character(0), footnotes = m[["footnotes"]],
                 mofiaWarnings = character(0), mofiaModel = NULL, args = NULL)
  if (length(m[["errors"]]) == 0) {
    args  <- .mnlfaModelArgs(spec, prepared, key)
    check <- .mnlfaCheckIdentification(spec, prepared, key, args)
    result[["errors"]]        <- check[["errors"]]
    result[["warnings"]]      <- check[["warnings"]]
    result[["mofiaWarnings"]] <- check[["mofiaWarnings"]]
    result[["mofiaModel"]]    <- check[["mofiaModel"]]
    result[["args"]]          <- args[names(args) != "data"]
  }
  state <- createJaspState(result)
  state$dependOn(c(.mnlfaBaseDeps, key))
  jaspResults[[stateKey]] <- state
  return(result)
}

#' The fit state of every accepted model
#'
#' One mofia() call per accepted model whose cached fit is missing or stale; refused models are never
#' fitted. A fit state depends on the base options, the model's switch and the link, plus its own
#' settings through `signature`; never on the SE choice, alpha or display options. Nothing is fitted
#' with Sync off.
#' @noRd
.mnlfaMofiaFitStates <- function(jaspResults, spec, models, fitModels) {
  fits  <- stats::setNames(vector("list", length(models)), names(models))
  toFit <- character(0)
  for (key in names(models)) {
    if (length(models[[key]][["errors"]]) > 0 || is.null(models[[key]][["mofiaModel"]]))
      next
    cached <- jaspResults[[paste0("mofiaFitState_", key)]]
    if (!is.null(cached) && identical(cached$object[["signature"]], .mnlfaFitSignature(spec, key)))
      fits[[key]] <- cached$object
    else
      toFit <- c(toFit, key)
  }
  if (length(toFit) == 0 || !fitModels)
    return(fits)

  jaspBase::startProgressbar(length(toFit), label = gettext("Fitting the MoFIA models"))
  for (key in toFit) {
    result <- .mnlfaFitModel(models[[key]][["mofiaModel"]], spec[["link"]])
    result[["signature"]] <- .mnlfaFitSignature(spec, key)
    state <- createJaspState(result)
    state$dependOn(c(.mnlfaBaseDeps, key, "categoricalIndicatorLink"))
    jaspResults[[paste0("mofiaFitState_", key)]] <- state
    fits[[key]] <- result
    jaspBase::progressbarTick()
  }
  return(fits)
}

.mnlfaFitSignature <- function(spec, key) list(model = spec[["models"]][[key]], link = spec[["link"]])

# Per model: whether it was refused, its specification footnotes, and MoFIA's other warnings (model
# construction and fitting) under "Show warnings". Refusal messages and identification warnings are shown in
# the model's row of the fit table. MoFIA's full estimation-error message, which the fit table replaces by a short
# text, is listed here under "Show warnings".
.mnlfaMofiaModelMessages <- function(container, model, fit, key, position, options) {
  htmlKey <- paste0(key, "Checks")
  if (!is.null(container[[htmlKey]]))
    return()

  refused         <- length(model[["errors"]]) > 0
  mofiaWarnings   <- unique(c(model[["mofiaWarnings"]], fit[["warnings"]]))
  estimationError <- if (.mnlfaIsEstimationError(fit)) fit[["error"]]
  lines <- c(
    if (refused)
      gettext("This model is refused and will not be fitted; the reason is shown in the table 'Model Fit (MoFIA)'.")
    else
      gettext("The model passed the identification pre-checks; its fit is shown in the table 'Model Fit (MoFIA)'."),
    model[["footnotes"]],
    if (options[["warnings"]] && length(estimationError) > 0)
      gettextf("MoFIA error: %s", estimationError),
    if (options[["warnings"]] && length(mofiaWarnings) > 0)
      gettextf("MoFIA warning: %s", mofiaWarnings))
  html <- createJaspHtml(text = paste(.mnlfaHtmlEscape(lines), collapse = "<br>"), title = .mnlfaModelLabel(key))
  html$dependOn("warnings")
  html$position <- 1 + position
  container[[htmlKey]] <- html
  return()
}

.mnlfaMofiaSyntaxOutput <- function(jaspResults, spec, prepared, models, options) {
  if (!options[["showSyntax"]] || !is.null(jaspResults[["mofiaSyntaxContainer"]]))
    return()

  syntaxContainer <- createJaspContainer(gettext("Model Syntax (MoFIA)"), initCollapsed = TRUE)
  syntaxContainer$dependOn(c(.mnlfaMofiaDisplayDeps, "showSyntax", "errorCalculationMethod"))
  syntaxContainer$position <- 103
  jaspResults[["mofiaSyntaxContainer"]] <- syntaxContainer

  # The link and SE choice shown in the code are taken from the current options.
  spec[["link"]]     <- options[["categoricalIndicatorLink"]]
  spec[["seChoice"]] <- options[["errorCalculationMethod"]]
  for (k in seq_along(spec[["models"]])) {
    key <- names(spec[["models"]])[k]
    if (length(spec[["models"]][[key]][["errors"]]) > 0) {
      # Invalid settings: no code, never a substitute model built from the readable rows.
      html <- createJaspHtml(text = gettext("No code is shown: the moderation settings of this model are invalid (see Model Specification)."),
                             title = .mnlfaModelLabel(key))
    } else {
      refused <- length(models[[key]][["errors"]]) > 0
      code    <- .mnlfaSyntax(spec, prepared, key, fit = !refused)
      # JASP's results page typesets with MathJax, which turns "\\" into "\"
      # and reads "$...$" as math; "mathjax_ignore" keeps the code exactly as generated, so it stays runnable as shown.
      html    <- createJaspHtml(text = .mnlfaHtmlEscape(code), class = "jasp-code mathjax_ignore",
                                title = .mnlfaModelLabel(key))
    }
    html$position <- k
    syntaxContainer[[paste0(key, "Syntax")]] <- html
  }
  return()
}

#' The fit per group with ordinal indicators
#'
#' The legacy data handling and grouping are kept unchanged; only the assigned ordinal indicator columns
#' keep their categorical type and are passed to lavaan::cfa as `ordered`.
#' @noRd
.mnlfaOrderedGroupFit <- function(jaspResults, dataset, options, ready) {
  if (!ready)
    return()

  factors <- options[["factors"]]
  items   <- unlist(lapply(factors, function(factor) factor[["indicators"]]), use.names = FALSE)
  types   <- unlist(lapply(factors, function(factor) factor[["indicators.types"]]), use.names = FALSE)
  ordinal <- unique(items[types == "ordinal"])

  groupData <- .mnlfaHandleData(jaspResults, dataset, options, ready)
  for (item in ordinal)
    groupData[[item]] <- dataset[[item]]
  groupOptions <- .mnlfaPreprocessOptions(options)

  .mnlfaCreateContainer(jaspResults, groupOptions)
  .mnlfaFitPerGroup(jaspResults, groupData, groupOptions, ready, ordered = ordinal)
  .mnlfaFitPerGroupTable(jaspResults, groupData, groupOptions, ready, ordered = ordinal)
  .mnlfaAddGroupingVariableToData(jaspResults, groupData, groupOptions)
  return()
}
