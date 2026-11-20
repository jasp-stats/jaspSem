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

# MNLFA with MoFIA: the fit table. The information criteria, the choice of the
# compared pairs and the test selection are pure functions on fit objects; tests call them via jaspSem:::.

# The columns of the fit table, in order.
.mnlfaFitTableColumns <- function(table) {
  table$addColumnInfo(name = "model",         title = gettext("Model"),          type = "string")
  table$addColumnInfo(name = "npar",          title = gettext("n(par)"),         type = "integer")
  table$addColumnInfo(name = "minus2LL",      title = "\u22122LL",               type = "number")
  table$addColumnInfo(name = "aic",           title = gettext("AIC"),            type = "number")
  table$addColumnInfo(name = "saaic",         title = gettext("SAAIC"),          type = "number")
  table$addColumnInfo(name = "bic",           title = gettext("BIC"),            type = "number")
  table$addColumnInfo(name = "sabic",         title = gettext("SABIC"),          type = "number")
  table$addColumnInfo(name = "comparedWith",  title = gettext("Compared with"),  type = "string")
  table$addColumnInfo(name = "deltaMinus2LL", title = "\u0394(\u22122LL)",       type = "number")
  table$addColumnInfo(name = "deltaDf",       title = "\u0394df",                type = "integer")
  table$addColumnInfo(name = "scalingFactor", title = gettext("Scaling factor"), type = "number")
  table$addColumnInfo(name = "test",          title = gettext("Test"),           type = "string")
  table$addColumnInfo(name = "statistic",     title = gettext("Statistic"),      type = "number")
  table$addColumnInfo(name = "p",             title = "p",                       type = "pvalue")
  return()
}

.mnlfaFitTableDeps <- function() c(.mnlfaMofiaDisplayDeps, "errorCalculationMethod", "warnings")

# The empty fit table shown before indicators and moderators are assigned (replaces the legacy empty
# "Global Invariance Fit" table).
.mnlfaEmptyFitTable <- function(jaspResults) {
  if (!is.null(jaspResults[["mofiaFitTable"]]))
    return()
  table <- createJaspTable(gettext("Model Fit (MoFIA)"))
  table$dependOn(.mnlfaFitTableDeps())
  table$position <- 101
  jaspResults[["mofiaFitTable"]] <- table
  .mnlfaFitTableColumns(table)
  return()
}

#' The fit table
#'
#' One row per enabled model, in the order configural, metric, scalar, strict, custom. n(par) and -2LL for
#' every fitted model; information criteria only for converged and not-converged fits; refused models show their
#' refusal messages and every model its identification warnings as footnotes of its row. Comparisons: see
#' `.mnlfaComparisonPartners()` and `.mnlfaSelectTest()`. The table is rebuilt from the cached fits when the SE choice
#' or a display option changes; it never fits.
#' @noRd
.mnlfaMofiaFitTable <- function(jaspResults, spec, prepared, models, fits, options) {
  if (!is.null(jaspResults[["mofiaFitTable"]]))
    return()

  table <- createJaspTable(gettext("Model Fit (MoFIA)"))
  table$dependOn(.mnlfaFitTableDeps())
  table$position <- 101
  jaspResults[["mofiaFitTable"]] <- table
  .mnlfaFitTableColumns(table)

  seChoice <- options[["errorCalculationMethod"]]
  rows     <- list()
  notes    <- list()
  outcomes <- list()
  for (key in names(models)) {
    label <- .mnlfaModelLabel(key)
    row   <- .mnlfaEmptyFitRow(label)
    note  <- character(0)
    if (length(models[[key]][["errors"]]) > 0) {
      note <- models[[key]][["errors"]]
    } else if (is.null(fits[[key]])) {
      note <- gettext("Not fitted: check 'Start/Sync Analysis' to fit this model.")
    } else {
      outcome <- .mnlfaFitOutcome(fits[[key]], label)
      if (!is.null(outcome[["error"]])) {
        note <- c(outcome[["error"]], outcome[["footnotes"]])        # quadrature notes are kept
      } else {
        fit <- fits[[key]][["fit"]]
        row[["npar"]]     <- as.integer(fit[["npar"]])
        row[["minus2LL"]] <- -2 * fit[["loglik"]]
        if (outcome[["outcome"]] %in% c("converged", "notConverged"))
          row[c("aic", "saaic", "bic", "sabic")] <- .mnlfaInformationCriteria(row[["minus2LL"]], fit[["npar"]],
                                                                              fit[["nobs"]])
        note <- c(outcome[["footnotes"]], .mnlfaStandardErrors(fits[[key]], outcome, seChoice)[["footnotes"]])
        outcomes[[key]] <- outcome[["outcome"]]
      }
    }
    rows[[key]]  <- row
    notes[[key]] <- c(note, models[[key]][["warnings"]])
  }

  # Comparisons: converged fits only.
  converged <- names(outcomes)[vapply(outcomes, identical, TRUE, "converged")]
  partners  <- .mnlfaComparisonPartners(lapply(stats::setNames(converged, converged), function(key) {
    fit <- fits[[key]][["fit"]]
    list(npar = fit[["npar"]], names = names(fit[["coefficients"]]))
  }))
  testNotes <- list()
  for (key in names(partners)) {
    larger <- partners[[key]]
    if (is.na(larger))
      next
    fit0   <- fits[[key]][["fit"]]
    fit1   <- fits[[larger]][["fit"]]
    result <- .mnlfaSelectTest(fit0, fit1, seChoice)
    rows[[key]][["comparedWith"]]  <- .mnlfaModelLabel(larger)
    rows[[key]][["deltaMinus2LL"]] <- -2 * fit0[["loglik"]] - (-2 * fit1[["loglik"]])
    rows[[key]][["deltaDf"]]       <- as.integer(fit1[["npar"]] - fit0[["npar"]])
    rows[[key]][["scalingFactor"]] <- result[["scalingFactor"]]
    rows[[key]][["test"]]          <- result[["test"]]
    rows[[key]][["statistic"]]     <- result[["statistic"]]
    rows[[key]][["p"]]             <- result[["p"]]
    testNotes[[key]] <- .mnlfaTestFootnotes(result, .mnlfaModelLabel(key), .mnlfaModelLabel(larger),
                                            options[["warnings"]])
  }

  for (key in names(rows)) {
    table$addRows(rows[[key]], rowNames = key)
    for (note in unique(notes[[key]]))
      table$addFootnote(note, colNames = "model", rowNames = key)
    for (note in unique(testNotes[[key]]))
      table$addFootnote(note, colNames = "test", rowNames = key)
  }

  for (note in .mnlfaFitTableNotes(spec, prepared, anyComparison = any(!is.na(partners))))
    table$addFootnote(note)
  return()
}

.mnlfaEmptyFitRow <- function(label) {
  list(model = label, npar = NA_integer_, minus2LL = NA_real_, aic = NA_real_, saaic = NA_real_, bic = NA_real_,
       sabic = NA_real_, comparedWith = "", deltaMinus2LL = NA_real_, deltaDf = NA_integer_, scalingFactor = NA_real_,
       test = "", statistic = NA_real_, p = NA_real_)
}

#' Information criteria (formulas of the old `.mxSummaryFixed`)
#'
#' k = n(par), N = number of analysed cases (after the mask). SAAIC only when N > k + 1.
#' @noRd
.mnlfaInformationCriteria <- function(minus2LL, k, N) {
  aic <- minus2LL + 2 * k
  list(aic   = aic,
       saaic = if (N > k + 1) aic + 2 * k * (k + 1) / (N - k - 1) else NA_real_,
       bic   = minus2LL + k * log(N),
       sabic = minus2LL + k * log((N + 2) / 24))
}

#' The compared pairs
#'
#' `candidates`: named list (model key -> list(npar, names)) of the converged fits, in the order configural,
#' metric, scalar, strict, custom. Each model is compared with the smallest strictly larger candidate in which
#' it is nested by parameter names; ties in n(par) go to the earlier model. Returns the partner key per model
#' (NA without a partner). p-values play no part.
#' @noRd
.mnlfaComparisonPartners <- function(candidates) {
  partners <- stats::setNames(rep(NA_character_, length(candidates)), names(candidates))
  for (m in names(candidates)) {
    larger <- Filter(function(M) candidates[[M]][["npar"]] > candidates[[m]][["npar"]] &&
                       all(candidates[[m]][["names"]] %in% candidates[[M]][["names"]]), names(candidates))
    if (length(larger) > 0) {
      npar <- vapply(larger, function(M) as.numeric(candidates[[M]][["npar"]]), 0)
      partners[[m]] <- larger[which.min(npar)]       # first minimum = earliest model on ties
    }
  }
  return(partners)
}

#' The test of one nested pair
#'
#' `fit0` the smaller, `fit1` the larger model. Standard: the unscaled LR test. Sandwich choices: the
#' Satorra-Bentler (2001) scaled test; if its p-value is not finite, the 2010 version; if neither, no test
#' ("none"). Never the unscaled test with a sandwich choice. Any `mofia_lrt` error gives no test and its
#' message (`error`); MoFIA's warnings are kept (`warnings`). `lrt` is a seam for tests only.
#' Returns `branch` ("LR", "SB2001", "SB2010", "none" or "error"), `test`, `statistic`, `scalingFactor`,
#' `df`, `p`, `error` and `warnings`.
#' @noRd
.mnlfaSelectTest <- function(fit0, fit1, seChoice, lrt = MoFIA::mofia_lrt) {
  out <- list(branch = "error", test = "", statistic = NA_real_, scalingFactor = NA_real_,
              df = as.integer(fit1[["npar"]] - fit0[["npar"]]), p = NA_real_, error = NULL, warnings = character(0))
  run <- function(...) {
    value <- tryCatch(
      withCallingHandlers(lrt(fit0, fit1, ...), warning = function(w) {
        out[["warnings"]] <<- unique(c(out[["warnings"]], conditionMessage(w)))
        invokeRestart("muffleWarning")
      }),
      error = function(e) e)
    if (inherits(value, "error"))
      out[["error"]] <<- conditionMessage(value)
    return(value)
  }

  source <- .mnlfaSeSource(seChoice)
  if (seChoice == "standard") {
    r <- run(scaled = FALSE)
    if (inherits(r, "error"))
      return(out)
    out[c("branch", "test")] <- list("LR", "LR")
    out[["statistic"]] <- r[["statistic"]][1]
    out[["p"]]         <- r[["p_value"]][1]
    return(out)
  }

  r01 <- run(scaled = TRUE, information = source[["information"]], sb2010 = FALSE)
  if (inherits(r01, "error"))
    return(out)
  if (is.finite(r01[["p_value"]][1])) {
    out[c("branch", "test")] <- list("SB2001", "SB2001")
    out[["statistic"]]     <- r01[["statistic"]][1]
    out[["scalingFactor"]] <- r01[["cd"]][1]
    out[["p"]]             <- r01[["p_value"]][1]
    return(out)
  }

  r10 <- run(scaled = TRUE, information = source[["information"]], sb2010 = TRUE)
  if (inherits(r10, "error"))
    return(out)
  row <- r10[r10[["test"]] == "Satorra-Bentler (2010)", , drop = FALSE]
  if (nrow(row) == 1 && is.finite(row[["p_value"]])) {
    out[c("branch", "test")] <- list("SB2010", "SB2010")
    out[["statistic"]]     <- row[["statistic"]]
    out[["scalingFactor"]] <- row[["cd"]]
    out[["p"]]             <- row[["p_value"]]
    return(out)
  }
  out[c("branch", "test")] <- list("none", gettext("none"))
  return(out)
}

#' Footnotes of one comparison
#'
#' The SB2010 and "none" branches and a `mofia_lrt` error (translated frame with MoFIA's message); MoFIA's
#' quadrature warnings always, its other warnings with "Show warnings".
#' @noRd
.mnlfaTestFootnotes <- function(result, label, largerLabel, showWarnings) {
  notes <- switch(result[["branch"]],
    SB2010 = gettext("The 2001 scaling factor was not positive; the strictly positive 2010 version is used."),
    none   = gettext("No scaled test is available: neither the 2001 nor the 2010 scaling factor is positive."),
    error  = gettextf("The test against %1$s is not available. MoFIA reported: %2$s", largerLabel, result[["error"]]),
    character(0))
  warnings   <- result[["warnings"]]
  quadrature <- grepl("quadrature", warnings, ignore.case = TRUE)
  if (any(quadrature))
    notes <- c(notes, gettextf("Test of %1$s (fit0) against %2$s (fit1): MoFIA reported a quadrature problem: %3$s",
                               label, largerLabel, warnings[quadrature]))
  if (isTRUE(showWarnings) && any(!quadrature))
    notes <- c(notes, gettextf("Test of %1$s (fit0) against %2$s (fit1): MoFIA warning: %3$s",
                               label, largerLabel, warnings[!quadrature]))
  return(notes)
}

#' General footnotes of the fit table
#'
#' Exclusions, the moderator coding (only the sentences of the moderator classes present), the scalar = strict note
#' for categorical-only models and the meaning of the difference column.
#' @noRd
.mnlfaFitTableNotes <- function(spec, prepared, anyComparison) {
  notes    <- character(0)
  excluded <- prepared[["nExcludedModerator"]] + prepared[["nExcludedResponse"]]
  if (length(excluded) == 1 && excluded > 0)
    notes <- c(notes, sprintf(ngettext(excluded,
      "%1$d case was excluded: %2$d with a missing moderator value and %3$d without any observed indicator.",
      "%1$d cases were excluded: %2$d with a missing moderator value and %3$d without any observed indicator."),
      excluded, prepared[["nExcludedModerator"]], prepared[["nExcludedResponse"]]))

  coding <- .mnlfaModeratorCodingNote(spec, prepared)
  if (nzchar(coding))
    notes <- c(notes, coding)

  keys <- c("invarianceTestScalar", "invarianceTestStrict")
  if (length(spec[["continuous"]]) == 0 && all(keys %in% names(spec[["models"]]))) {
    fields <- c("loadings", "intercepts", "residualVariances", "variances", "means", "covariances")
    if (identical(spec[["models"]][[keys[1]]][fields], spec[["models"]][[keys[2]]][fields]))
      notes <- c(notes, gettext("With only categorical indicators, the scalar and strict models are identical."))
  }

  if (anyComparison)
    notes <- c(notes, gettext("\u0394(\u22122LL) is the difference in \u22122 log-likelihood."))
  return(notes)
}

# How the moderators enter the model, for the classes present.
.mnlfaModeratorCodingNote <- function(spec, prepared) {
  info    <- prepared[["moderatorInfo"]]
  classes <- vapply(info, function(x) x[["class"]], "")
  parts   <- character(0)
  if (any(classes == "continuous"))
    parts <- c(parts, gettext("Continuous moderators were standardized on the analysed sample; their effects are per standard deviation."))
  if (any(classes == "binary"))
    parts <- c(parts, gettext("Binary moderators are used as coded (effects per unit)."))
  if (any(classes == "nominal")) {
    references <- vapply(info[classes == "nominal"], function(x)
      gettextf("%1$s: %2$s", spec[["decoded"]][[x[["variable"]]]], x[["levels"]][1]), "")
    parts <- c(parts, gettextf("Nominal moderators are dummy-coded against the reference level (%s).",
                               paste(references, collapse = "; ")))
  }
  return(paste(parts, collapse = " "))
}
