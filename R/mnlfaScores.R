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

# MNLFA with MoFIA: factor scores saved to the data. EAP scores of the selected model, mapped back to the data rows by
# JASP's own row mask. The columns are owned by the
# analysis: rewritten when the model or data change, removed when the option is switched off or no longer applies.

# The score output depends on the model definitions and link (through the display dependencies) and the three score
# options; never on anything that refits.
.mnlfaScoreDeps <- function() {
  c(.mnlfaMofiaDisplayDeps, "factorScoreSavedToData", "factorScoreModel", "factorScoreColumnPrefix")
}

# Column name = prefix + factor title (the default prefix "FS_" is the form's default).
.mnlfaScoreColumnNames <- function(prefix, titles) paste0(prefix, titles)

# Whether a data column of this name may be written: it does not exist or this analysis created it. A seam for
# tests; production asks JASP.
.mnlfaColumnAvailable <- function(name) {
  return(!(jaspBase:::columnExists(name) && !jaspBase:::columnIsMine(name)))
}

# Remove data columns this analysis created earlier (never a column it does not own).
.mnlfaDeleteOwnColumns <- function(names) {
  for (name in names)
    try(if (jaspBase:::columnExists(name) && jaspBase:::columnIsMine(name))
      jaspBase:::columnDelete(name), silent = TRUE)
  return()
}

#' The factor scores of one fit as data columns
#'
#' EAP scores from `MoFIA::mofia_scores()` (one row per fitted case, in the order of the data JASP passed), mapped to
#' the rows of the dataset by JASP's original row indices `rowIndices`; rows JASP excluded stay NA. MoFIA's
#' `row_index` is used only as a consistency check: it must be `seq_len(N)` of the masked data. Returns a matrix
#' with `nInput` rows and one column per factor (MoFIA factor names).
#' @noRd
.mnlfaScoreValues <- function(fit, rowIndices, nInput) {
  scores  <- MoFIA::mofia_scores(fit)
  factors <- fit[["model"]][["factors"]]
  index   <- attr(scores, "row_index")
  if (nrow(scores) != length(rowIndices) || !identical(as.integer(index), seq_len(length(rowIndices))) ||
      !all(factors %in% names(scores)))
    stop(gettext("The factor scores do not match the analysed cases."))
  out <- matrix(NA_real_, nInput, length(factors), dimnames = list(NULL, factors))
  out[rowIndices, ] <- as.matrix(scores[, factors, drop = FALSE])
  return(out)
}

# Score output that can no longer be valid (option off, nothing fitted, invalid settings): remove the columns written
# earlier and their record.
.mnlfaRemoveScoreOutput <- function(jaspResults) {
  for (key in c("mofiaFactorScoreTable", "mofiaFactorScoreColumns"))
    if (!is.null(jaspResults[[key]]))
      jaspResults[[key]] <- NULL
  previous <- jaspResults[["mofiaScoreColumnNames"]]
  if (!is.null(previous)) {
    .mnlfaDeleteOwnColumns(previous$object)
    jaspResults[["mofiaScoreColumnNames"]] <- NULL
  }
  return()
}

# Called on runs that cannot build the scores: the columns of an output that has been invalidated are removed.
.mnlfaScoresUnavailable <- function(jaspResults) {
  if (is.null(jaspResults[["mofiaFactorScoreTable"]]))
    .mnlfaRemoveScoreOutput(jaspResults)
  return()
}

#' Factor scores saved to the data
#'
#' A table "Factor Scores" lists the written columns (factor, column, cases with and without a score). No columns and
#' a footnote when no usable model is selected (refused, not fitted, or any outcome other than converged: not
#' converged, saddle point, singular, factor correlation matrix not positive definite, error). A name that exists in
#' the data without being owned by the analysis, or two factors with the same column name, give an error on this table
#' only; nothing is written then and the rest of the output stays. Columns written earlier under other names are
#' removed. Built from the cached fits; never fits.
#' @noRd
.mnlfaMofiaFactorScores <- function(jaspResults, spec, prepared, models, fits, options) {
  if (!isTRUE(options[["factorScoreSavedToData"]])) {
    .mnlfaRemoveScoreOutput(jaspResults)
    return()
  }
  if (!is.null(jaspResults[["mofiaFactorScoreTable"]]))
    return()

  table <- createJaspTable(gettext("Factor Scores"))
  table$dependOn(.mnlfaScoreDeps())
  table$position <- 102.8
  jaspResults[["mofiaFactorScoreTable"]] <- table
  table$addColumnInfo(name = "factor",  title = gettext("Factor"),                type = "string")
  table$addColumnInfo(name = "column",  title = gettext("Column"),                type = "string")
  table$addColumnInfo(name = "scored",  title = gettext("Cases with a score"),    type = "integer")
  table$addColumnInfo(name = "missing", title = gettext("Cases without a score"), type = "integer")

  previous <- jaspResults[["mofiaScoreColumnNames"]]$object
  finish   <- function(written) {
    .mnlfaDeleteOwnColumns(setdiff(previous, written))
    state <- createJaspState(written)            # no dependencies: the record of owned columns persists
    jaspResults[["mofiaScoreColumnNames"]] <- state
    return()
  }

  key    <- .mnlfaPlotValue(options[["factorScoreModel"]])
  label  <- .mnlfaModelLabel(key)
  reason <- NULL
  if (!nzchar(key) || !key %in% names(models)) {
    reason <- gettext("No factor scores were saved: select an enabled model for the factor scores.")
  } else if (length(models[[key]][["errors"]]) > 0) {
    reason <- gettextf("No factor scores were saved: model %s is refused; the reason is shown in the table 'Model Fit (MoFIA)'.", label)
  } else if (is.null(fits[[key]])) {
    reason <- gettextf("No factor scores were saved: model %s has not been fitted.", label)
  } else {
    outcome <- .mnlfaFitOutcome(fits[[key]], label)
    if (!identical(outcome[["outcome"]], "converged"))
      reason <- c(gettextf("No factor scores were saved: model %s did not converge to a usable solution.", label),
                  outcome[["error"]], outcome[["statusFootnotes"]])
  }
  if (!is.null(reason)) {
    for (note in reason)
      table$addFootnote(note)
    finish(character(0))
    return()
  }

  titles  <- spec[["factorTitles"]]
  prefix  <- .mnlfaPlotValue(options[["factorScoreColumnPrefix"]])
  columns <- .mnlfaScoreColumnNames(prefix, titles)
  clash   <- if (anyDuplicated(columns) > 0)
    gettextf("The factor scores cannot be saved: two factors would get the same column name (%s). Change the factor titles or the prefix.",
             paste(unique(columns[duplicated(columns)]), collapse = ", "))
  else if (!all(available <- vapply(columns, .mnlfaColumnAvailable, TRUE)))
    sprintf(ngettext(sum(!available),
      "The factor scores cannot be saved: the data already contain a column named %s that was not created by this analysis. Change the column name prefix.",
      "The factor scores cannot be saved: the data already contain columns named %s that were not created by this analysis. Change the column name prefix."),
      paste(sprintf("'%s'", columns[!available]), collapse = ", "))
  if (!is.null(clash)) {
    table$setError(clash)
    finish(character(0))
    return()
  }

  fit    <- fits[[key]][["fit"]]
  values <- tryCatch(.mnlfaScoreValues(fit, prepared[["rowIndices"]], prepared[["nInput"]]), error = function(e) e)
  if (inherits(values, "error")) {
    table$addFootnote(gettextf("No factor scores were saved: MoFIA could not compute them (%s).", conditionMessage(values)))
    finish(character(0))
    return()
  }

  columnContainer <- createJaspContainer()
  columnContainer$dependOn(.mnlfaScoreDeps())
  jaspResults[["mofiaFactorScoreColumns"]] <- columnContainer
  rows <- list()
  for (d in seq_along(spec[["factorNames"]])) {
    column <- columns[d]
    scores <- values[, spec[["factorNames"]][d]]
    columnContainer[[column]] <- jaspBase::createJaspColumn(column)
    columnContainer[[column]]$setScale(scores)
    rows[[d]] <- list(factor = titles[d], column = column, scored = sum(!is.na(scores)), missing = sum(is.na(scores)))
  }
  table$setData(do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE)))
  table$addFootnote(gettextf("Expected a posteriori (EAP) factor scores of model %s, saved to the data. The scores include the moderated factor means and variances; their posterior standard deviations do not include parameter uncertainty.",
                             label))
  if (prepared[["nInput"]] > length(prepared[["rowIndices"]]))
    table$addFootnote(gettext("Cases excluded from the analysis (a missing moderator value or no observed indicator) have no score."))
  finish(columns)
  return()
}
