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

# MNLFA with MoFIA: the parameter tables and the display labels of the moderator terms. Rows come from the indices of
# `fit$model$par`, labels from the model's design; names are never parsed. The row builder and the labels are pure
# functions; tests call them via jaspSem:::.

# Display switches of the parameter tables, in table order, and the table of each switch.
.mnlfaParameterSwitches <- c(parameterEstimatesLoadings          = "loadings",
                             parameterEstimatesIntercepts        = "intercepts",
                             parameterEstimatesResidualVariances = "residualVariances",
                             parameterEstimatesFactorVariance    = "factorVariances",
                             parameterEstimatesFactorMeans       = "factorMeans",
                             parameterEstimatesFactorCovariances = "factorCorrelations")

# The tables depend on the model definition and link (through the display dependencies), the SE choice, alpha and
# the switches; never on anything that refits.
.mnlfaParameterTableDeps <- function() {
  c(.mnlfaMofiaDisplayDeps, "errorCalculationMethod", "parameterEstimatesAlphaLevel", names(.mnlfaParameterSwitches))
}

#' The parameter tables
#'
#' One container "Parameter Estimates" with one container per fitted model and in it one table per ticked switch.
#' Refused and unfitted models have none (their fit-table row says why); a model whose fit failed shows the error in
#' one table.
#' Built from the cached fits; never fits.
#' @noRd
.mnlfaMofiaParameterTables <- function(jaspResults, spec, prepared, models, fits, options) {
  switches <- names(.mnlfaParameterSwitches)
  ticked   <- switches[vapply(switches, function(s) isTRUE(options[[s]]), TRUE)]
  if (length(ticked) == 0 || !is.null(jaspResults[["mofiaParameterContainer"]]))
    return()

  container <- createJaspContainer(gettext("Parameter Estimates"))
  container$dependOn(.mnlfaParameterTableDeps())
  container$position <- 102
  jaspResults[["mofiaParameterContainer"]] <- container

  alpha  <- options[["parameterEstimatesAlphaLevel"]]
  coding <- .mnlfaModeratorCodingNote(spec, prepared)
  titles <- stats::setNames(spec[["factorTitles"]], spec[["factorNames"]])
  for (k in seq_along(models)) {
    key <- names(models)[k]
    if (length(models[[key]][["errors"]]) > 0 || is.null(fits[[key]]))
      next
    label          <- .mnlfaModelLabel(key)
    modelContainer <- createJaspContainer(label)
    modelContainer$position <- k
    container[[key]] <- modelContainer

    outcome <- .mnlfaFitOutcome(fits[[key]], label)
    if (!is.null(outcome[["error"]])) {
      # A container's own error is not shown; the error goes on a table (create, attach, then set the error).
      errorTable <- createJaspTable(gettext("Parameter Estimates"))
      errorTable$position <- 1
      modelContainer[["error"]] <- errorTable
      errorTable$setError(outcome[["error"]])
      next
    }
    fit            <- fits[[key]][["fit"]]
    standardErrors <- .mnlfaStandardErrors(fits[[key]], outcome, options[["errorCalculationMethod"]])
    labels         <- .mnlfaTermLabels(fit[["model"]][["design"]], colnames(fit[["model"]][["X"]]), spec[["decoded"]])
    built          <- .mnlfaParameterRows(fit, standardErrors, alpha, labels, titles, spec[["decoded"]],
                                          prepared[["categoryLevels"]])
    modelNotes     <- c(outcome[["statusFootnotes"]], standardErrors[["footnotes"]])
    for (s in ticked)
      .mnlfaParameterTable(modelContainer, .mnlfaParameterSwitches[[s]], match(s, names(.mnlfaParameterSwitches)),
                           built, modelNotes, coding, alpha)
  }
  return()
}

# The table titles.
.mnlfaParameterTableTitle <- function(type, built) {
  switch(type,
         loadings           = gettext("Loadings"),
         intercepts         = if (!built[["hasThresholds"]]) gettext("Intercepts")
                              else if (built[["hasIntercepts"]]) gettext("Intercepts and Thresholds")
                              else gettext("Thresholds"),
         residualVariances  = gettext("Residual Variances"),
         factorVariances    = gettext("Factor Variances"),
         factorMeans        = gettext("Factor Means"),
         factorCorrelations = gettext("Factor Correlations"))
}

# One table of one model, with its footnotes (moderator coding, fit status, standard errors, table notes).
.mnlfaParameterTable <- function(modelContainer, type, position, built, modelNotes, coding, alpha) {
  table <- createJaspTable(.mnlfaParameterTableTitle(type, built))
  table$position <- position
  modelContainer[[type]] <- table

  if (type %in% c("loadings", "factorVariances", "factorMeans", "factorCorrelations"))
    table$addColumnInfo(name = "factor",  title = gettext("Factor"),    type = "string", combine = TRUE)
  if (type %in% c("loadings", "intercepts", "residualVariances"))
    table$addColumnInfo(name = "indicator", title = gettext("Indicator"), type = "string", combine = TRUE)
  ciTitle <- gettextf("%s%% CI", format(100 * (1 - alpha)))
  table$addColumnInfo(name = "effect",   title = gettext("Effect"),   type = "string")
  table$addColumnInfo(name = "estimate", title = gettext("Estimate"), type = "number")
  table$addColumnInfo(name = "se",       title = gettext("SE"),       type = "number")
  table$addColumnInfo(name = "z",        title = "z",                 type = "number")
  table$addColumnInfo(name = "p",        title = "p",                 type = "pvalue")
  table$addColumnInfo(name = "lower",    title = gettext("Lower"),    type = "number", overtitle = ciTitle)
  table$addColumnInfo(name = "upper",    title = gettext("Upper"),    type = "number", overtitle = ciTitle)

  rows <- built[["rows"]][built[["rows"]][["table"]] == type, , drop = FALSE]
  columns <- intersect(c("factor", "indicator", "effect", "estimate", "se", "z", "p", "lower", "upper"), names(rows))
  if (!type %in% c("loadings", "factorVariances", "factorMeans", "factorCorrelations"))
    columns <- setdiff(columns, "factor")
  if (!type %in% c("loadings", "intercepts", "residualVariances"))
    columns <- setdiff(columns, "indicator")
  for (i in seq_len(nrow(rows))) {
    rowName <- paste0("row", i)
    table$addRows(as.list(rows[i, columns, drop = FALSE]), rowNames = rowName)
    if (rows[["seMissing"]][i])
      table$addFootnote(gettext("The estimated sampling variance of this parameter is negative or not finite; its standard error, test and interval are not shown."),
                        colNames = "se", rowNames = rowName)
  }

  for (note in .mnlfaParameterTableNotes(type, rows, built, coding))
    table$addFootnote(note)
  for (note in unique(modelNotes))
    table$addFootnote(note)
  return()
}

# Table-level footnotes per table type.
.mnlfaParameterTableNotes <- function(type, rows, built, coding) {
  notes      <- character(0)
  moderation <- any(rows[["kind"]] %in% c("moderation", "ratio", "shift"))
  if (nrow(rows) == 0) {
    notes <- switch(type,
                    residualVariances  = gettext("Residual variances are not defined for categorical indicators; the model has no continuous indicators."),
                    factorCorrelations = gettext("The model has one factor; there are no factor correlations."),
                    gettext("The model has no parameters of this type."))
    return(notes)
  }
  if (type == "intercepts") {
    if (any(rows[["kind"]] == "shift"))
      notes <- c(notes, gettext("A positive shift lowers all thresholds of the indicator."))
    notes <- c(notes, built[["categoryNotes"]])
  }
  if (type == "residualVariances") {
    notes <- c(notes, gettext("Baseline rows show the residual variance; their z and p test a residual variance of 1 (log variance 0)."))
    if (moderation)
      notes <- c(notes, gettext("Moderation rows show multiplicative ratios: the residual variance is multiplied by the ratio per unit of the moderator term (per standard deviation for continuous moderators); z and p test a ratio of 1."))
  }
  if (type == "factorVariances") {
    notes <- c(notes, gettext("The baseline factor variance is fixed to 1 for identification."))
    if (moderation)
      notes <- c(notes, gettext("Moderation rows show multiplicative ratios: the factor variance is multiplied by the ratio per unit of the moderator term (per standard deviation for continuous moderators); z and p test a ratio of 1."))
  }
  if (type == "factorMeans")
    notes <- c(notes, gettext("The baseline factor mean is fixed to 0 for identification."))
  if (type == "factorCorrelations") {
    notes <- c(notes, gettext("Rows marked (Fisher z) are on the Fisher-z scale. The row 'Baseline (r)' shows the baseline correlation r = tanh(z) and the transformed Fisher-z interval."))
  }
  if (moderation && nzchar(coding))
    notes <- c(notes, coding)
  if (length(built[["unknownBlocks"]]) > 0)
    notes <- c(notes, gettextf("Parameters of a type this version does not display were omitted: %s.",
                               paste(built[["unknownBlocks"]], collapse = ", ")))
  return(notes)
}

#' The rows of all parameter tables of one fit
#'
#' A data frame with one row per displayed row: `table` (loadings, intercepts, residualVariances, factorVariances,
#' factorMeans, factorCorrelations), `factor`, `indicator`, `effect`, `estimate`, `se`, `z`, `p`, `lower`, `upper`
#' (unrounded; NA = empty cell), `kind` (baseline, moderation, threshold, shift, ratio, fixed, correlation),
#' `parameter` (the row index in `fit$model$par`, NA for fixed rows) and `seMissing` (standard errors available
#' for the model but not for this parameter). Linear rows: estimate, SE, z = est/SE, two-sided p, Wald CI. Variance
#' rows (residual variances, factor-variance moderation): exp(theta), SE exp(theta)*SE(theta), z and p for theta,
#' CI exp(theta -/+ q SE(theta)). Correlation baselines: the Fisher-z row and the row r = tanh(z) with the CI
#' tanh(z -/+ q SE). `labels`: display labels of the model-matrix columns (`.mnlfaTermLabels`), `titles`: factor
#' titles by MoFIA factor name, `decoded`: display names of the items, `categoryLevels`: the JASP level labels of the
#' categorical items. Also returns the footnotes on unobserved and sparse categories.
#' @noRd
.mnlfaParameterRows <- function(fit, standardErrors, alpha, labels, titles, decoded, categoryLevels) {
  model <- fit[["model"]]
  par   <- model[["par"]]
  theta <- unname(fit[["coefficients"]])
  if (length(theta) != nrow(par))
    stop(gettext("The coefficients of the MoFIA fit do not match its parameter table."))
  inference <- isTRUE(standardErrors[["available"]])
  se        <- if (inference) unname(standardErrors[["se"]]) else rep(NA_real_, length(theta))
  q         <- stats::qnorm(1 - alpha / 2)

  name     <- function(x) if (x %in% names(decoded)) decoded[[x]] else x
  item     <- function(j) name(model[["items"]][j])
  factorTitle <- function(d) { f <- model[["factors"]][d]; if (f %in% names(titles)) titles[[f]] else f }
  columns  <- colnames(model[["X"]])
  term     <- function(m) {
    cn <- columns[m]
    if (!is.null(labels) && cn %in% names(labels)) labels[[cn]] else name(cn)
  }

  out <- list()
  add <- function(table, kind, effect, i = NA_integer_, factorLabel = "", indicatorLabel = "", fixed = NA_real_) {
    row <- list(table = table, factor = factorLabel, indicator = indicatorLabel, effect = effect, estimate = NA_real_,
                se = NA_real_, z = NA_real_, p = NA_real_, lower = NA_real_, upper = NA_real_, kind = kind,
                parameter = i, seMissing = FALSE)
    if (kind == "fixed") {
      row[["estimate"]] <- fixed
    } else {
      t <- theta[i]
      s <- se[i]
      row[["seMissing"]] <- inference && !is.finite(s)
      ratio <- kind == "ratio" || (kind == "baseline" && table == "residualVariances")
      if (kind == "correlation") {
        row[["estimate"]] <- tanh(t)
        row[c("lower", "upper")] <- list(tanh(t - q * s), tanh(t + q * s))
      } else if (ratio) {
        row[["estimate"]] <- exp(t)
        row[c("se", "z", "p", "lower", "upper")] <- list(exp(t) * s, t / s, 2 * stats::pnorm(-abs(t / s)),
                                                         exp(t - q * s), exp(t + q * s))
      } else {
        row[["estimate"]] <- t
        row[c("se", "z", "p", "lower", "upper")] <- list(s, t / s, 2 * stats::pnorm(-abs(t / s)), t - q * s, t + q * s)
      }
    }
    out[[length(out) + 1]] <<- row
  }
  rowsOf <- function(block, ...) {
    keys <- list(...)
    sel  <- par[["block"]] == block
    for (k in names(keys))
      sel <- sel & !is.na(par[[k]]) & par[[k]] == keys[[k]]
    which(sel)
  }
  byMod <- function(i) i[order(par[["mod"]][i])]

  # Loadings: per factor, the items loading on it (item order): baseline, then one row per moderator term.
  for (d in seq_along(model[["factors"]]))
    for (j in seq_along(model[["items"]]))
      for (i in rowsOf("lambda0", item = j, factor = d)) {
        add("loadings", "baseline", gettext("Baseline"), i, factorTitle(d), item(j))
        for (m in byMod(rowsOf("lambda1", item = j, factor = d)))
          add("loadings", "moderation", term(par[["mod"]][m]), m, factorTitle(d), item(j))
      }

  # Intercepts and thresholds: per item.
  categoryNotes <- character(0)
  for (j in seq_along(model[["items"]])) {
    shift <- !isTRUE(model[["cont"]][[j]])
    if (!shift) {
      for (i in rowsOf("nu0", item = j))
        add("intercepts", "baseline", gettext("Baseline"), i, indicatorLabel = item(j))
    } else {
      observed <- model[["levels"]][[j]]
      declared <- categoryLevels[[model[["items"]][j]]]
      levelLabel <- function(code) {
        if (!is.null(declared) && code >= 1 && code <= length(declared)) declared[code] else as.character(code)
      }
      thresholds <- rowsOf("tau", item = j)
      for (i in thresholds[order(par[["k"]][thresholds])]) {
        k <- par[["k"]][i]
        add("intercepts", "threshold", gettextf("Threshold %1$d (%2$s | %3$s)", k, levelLabel(observed[k]),
                                                levelLabel(observed[k + 1])), i, indicatorLabel = item(j))
      }
      if (!is.null(declared)) {
        never <- setdiff(seq_along(declared), observed)
        if (length(never) > 0)
          categoryNotes <- c(categoryNotes, sprintf(ngettext(length(never),
            "Category of %1$s never observed in the analysed sample: %2$s. Its thresholds lie between the observed categories.",
            "Categories of %1$s never observed in the analysed sample: %2$s. Its thresholds lie between the observed categories."),
            item(j), paste(declared[never], collapse = ", ")))
      }
      y      <- model[["Y"]][, j]
      counts <- tabulate(y[!is.na(y)] + 1L, length(observed))
      sparse <- which(counts < 5)
      if (length(sparse) > 0)
        categoryNotes <- c(categoryNotes, sprintf(ngettext(length(sparse),
          "Category of %1$s with fewer than 5 responses (category: responses): %2$s. Its thresholds can be unstable.",
          "Categories of %1$s with fewer than 5 responses (category: responses): %2$s. Their thresholds can be unstable."),
          item(j), paste(sprintf("%s: %d", vapply(observed[sparse], levelLabel, ""), counts[sparse]), collapse = ", ")))
    }
    for (m in byMod(rowsOf("beta", item = j)))
      add("intercepts", if (shift) "shift" else "moderation",
          if (shift) gettextf("Shift: %s", term(par[["mod"]][m])) else term(par[["mod"]][m]), m, indicatorLabel = item(j))
  }

  # Residual variances (continuous items): baseline exp(omega0), moderation rows as ratios exp(omega).
  for (j in seq_along(model[["items"]]))
    for (i in rowsOf("omega0", item = j)) {
      add("residualVariances", "baseline", gettext("Baseline"), i, indicatorLabel = item(j))
      for (m in byMod(rowsOf("omega", item = j)))
        add("residualVariances", "ratio", gettextf("%s (ratio)", term(par[["mod"]][m])), m, indicatorLabel = item(j))
    }

  # Factor variances (baseline 1, fixed) and means (baseline 0, fixed).
  for (d in seq_along(model[["factors"]])) {
    add("factorVariances", "fixed", gettext("Baseline"), factorLabel = factorTitle(d), fixed = 1)
    for (m in byMod(rowsOf("delta", factor = d)))
      add("factorVariances", "ratio", gettextf("%s (ratio)", term(par[["mod"]][m])), m, factorLabel = factorTitle(d))
  }
  for (d in seq_along(model[["factors"]])) {
    add("factorMeans", "fixed", gettext("Baseline"), factorLabel = factorTitle(d), fixed = 0)
    for (m in byMod(rowsOf("gamma", factor = d)))
      add("factorMeans", "moderation", term(par[["mod"]][m]), m, factorLabel = factorTitle(d))
  }

  # Factor correlations: per pair (rows of model$pairs), Fisher z baseline, r, moderation on the Fisher-z scale.
  pairs <- model[["pairs"]]
  for (p in seq_len(NROW(pairs))) {
    pairLabel <- paste(factorTitle(pairs[p, 1]), factorTitle(pairs[p, 2]), sep = " \u2013 ")
    for (i in rowsOf("r0", pair = p)) {
      add("factorCorrelations", "baseline", gettext("Baseline (Fisher z)"), i, factorLabel = pairLabel)
      add("factorCorrelations", "correlation", gettext("Baseline (r)"), i, factorLabel = pairLabel)
    }
    for (m in byMod(rowsOf("r1", pair = p)))
      add("factorCorrelations", "moderation", gettextf("%s (Fisher z)", term(par[["mod"]][m])), m, factorLabel = pairLabel)
  }

  rows <- if (length(out) == 0) .mnlfaEmptyParameterRows() else
    do.call(rbind, lapply(out, as.data.frame, stringsAsFactors = FALSE))
  known <- c("lambda0", "lambda1", "nu0", "tau", "beta", "omega0", "omega", "gamma", "delta", "r0", "r1")
  list(rows = rows, categoryNotes = categoryNotes, unknownBlocks = unique(setdiff(par[["block"]], known)),
       hasThresholds = any(rows[["kind"]] == "threshold"), hasIntercepts = any(rows[["table"]] == "intercepts" &
                                                                                 rows[["kind"]] == "baseline"))
}

.mnlfaEmptyParameterRows <- function() {
  data.frame(table = character(0), factor = character(0), indicator = character(0), effect = character(0),
             estimate = numeric(0), se = numeric(0), z = numeric(0), p = numeric(0), lower = numeric(0),
             upper = numeric(0), kind = character(0), parameter = integer(0), seMissing = logical(0),
             stringsAsFactors = FALSE)
}

#' MoFIA's names of model-matrix columns
#'
#' MoFIA names its moderator columns after the model matrix, with ", " (a comma and any following white space)
#' written as "; ", because commas separate the indices in its parameter names (MoFIA R/model.R,
#' `.moderator_design`, MoFIA 2553b5b). The same rule is applied to JASP's own model matrices before they are matched
#' to MoFIA's columns or issue texts; displayed labels keep the original levels.
#' @noRd
.mnlfaMofiaColumnNames <- function(x) gsub(",[[:space:]]*", "; ", x)

#' Display labels of the moderator terms
#'
#' `Age`, `Age` squared and cubed (superscripts), `Gender (female)`, `Age x Gender (female)`, derived structurally from
#' a design (`terms`, `xlevels`, `contrasts`, as in `fit$model$design`). Per term: the
#' model matrix of the terms is evaluated on a grid of the level combinations of the nominal moderators of that term
#' only (other nominal moderators at their first level, numeric moderators at 1, so no product vanishes); the term's
#' columns are those with its `assign` index, its variables come from the terms' `factors` matrix, `I(x^k)` is read
#' from its call, and the level of a dummy is the one level at which the column is not zero. The grid of a term has
#' as many rows as the level combinations of its own nominal variables, never the product over all nominal
#' moderators. Never from the column names. `columns` are the model's columns (`colnames(fit$model$X)`, or the
#' prepared design's columns in MoFIA's naming): the model matrix's columns without the intercept, written with
#' `.mnlfaMofiaColumnNames`, must equal them in order. Returns the labels named by `columns`; NULL when the columns
#' differ or a column cannot be labelled (callers then show the decoded column names).
#' @noRd
.mnlfaTermLabels <- function(design, columns, decoded) {
  if (length(columns) == 0)
    return(stats::setNames(character(0), character(0)))
  out <- tryCatch(.mnlfaTermLabelsImpl(design, columns, decoded), error = function(e) NULL)
  if (is.null(out) || !identical(names(out), columns) || anyNA(out))
    return(NULL)
  return(out)
}

.mnlfaTermLabelsImpl <- function(design, columns, decoded) {
  tt      <- stats::delete.response(design[["terms"]])
  factors <- attr(tt, "factors")
  exprs   <- as.list(attr(tt, "variables"))[-1]
  xlev    <- design[["xlevels"]]
  if (is.null(factors) || nrow(factors) != length(exprs) || ncol(factors) == 0)
    return(NULL)

  name    <- function(x) if (x %in% names(decoded)) decoded[[x]] else x
  numeric <- setdiff(unique(unlist(lapply(exprs, all.vars))), names(xlev))

  variableLabel <- function(expr, values, nonZero) {
    if (is.symbol(expr)) {
      v <- as.character(expr)
      if (!v %in% names(xlev))
        return(name(v))
      level <- unique(as.character(values[[v]])[nonZero])
      if (length(level) != 1)
        return(NA_character_)
      return(gettextf("%1$s (%2$s)", name(v), level))
    }
    if (is.call(expr) && identical(expr[[1]], as.name("I")) && length(expr) == 2) {
      inner <- expr[[2]]
      if (is.call(inner) && identical(inner[[1]], as.name("^")) && is.symbol(inner[[2]]) && is.numeric(inner[[3]])) {
        power <- inner[[3]]
        suffix <- if (power == 2) "\u00B2" else if (power == 3) "\u00B3" else paste0("^", power)
        return(paste0(name(as.character(inner[[2]])), suffix))
      }
    }
    return(NA_character_)
  }

  labels <- character(0)
  keys   <- character(0)
  for (term in seq_len(ncol(factors))) {
    inTerm  <- which(factors[, term] > 0)
    nominal <- intersect(unique(unlist(lapply(exprs[inTerm], all.vars))), names(xlev))
    grid    <- if (length(nominal) > 0)
      expand.grid(xlev[nominal], stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
    else
      data.frame(row.names = 1L)
    n  <- max(1L, nrow(grid))
    nd <- data.frame(row.names = seq_len(n))
    for (v in names(xlev))
      nd[[v]] <- factor(if (v %in% nominal) grid[[v]] else rep(xlev[[v]][1], n), levels = xlev[[v]])
    for (v in numeric)
      nd[[v]] <- rep(1, n)
    mf     <- stats::model.frame(tt, nd, xlev = xlev)
    mm     <- stats::model.matrix(tt, mf, contrasts.arg = design[["contrasts"]])
    assign <- attr(mm, "assign")
    if (term == 1) {
      # The columns of the model matrix in MoFIA's naming must be the model's columns, in order.
      if (!identical(.mnlfaMofiaColumnNames(colnames(mm)[assign > 0]), columns))
        return(NULL)
    }
    for (column in which(assign == term)) {
      nonZero <- mm[, column] != 0
      parts   <- vapply(inTerm, function(r) variableLabel(exprs[[r]], nd, nonZero), "")
      labels  <- c(labels, if (anyNA(parts)) NA_character_ else paste(parts, collapse = " \u00D7 "))
      keys    <- c(keys, .mnlfaMofiaColumnNames(colnames(mm)[column]))
    }
  }
  if (!identical(keys, columns))
    return(NULL)
  return(stats::setNames(labels, columns))
}

#' The design of the prepared data, for labels before or without a MoFIA model
#'
#' The same stats functions as MoFIA (terms, model frame, model matrix with the moderators' explicit contrasts) on
#' the masked data; standardization does not change the labels. Used for the moderator terms of the
#' identification messages, which also exist when MoFIA refuses the model. The columns are written in MoFIA's naming
#' (`.mnlfaMofiaColumnNames`), so that they match the columns MoFIA lists in its issues also for nominal levels that
#' contain a comma; the labels keep the original levels.
#' @noRd
.mnlfaPreparedDesign <- function(prepared) {
  tt <- stats::terms(prepared[["formula"]])
  mf <- stats::model.frame(tt, prepared[["data"]])
  mm <- stats::model.matrix(tt, mf)
  list(terms = tt, xlevels = stats::.getXlevels(tt, mf), contrasts = attr(mm, "contrasts"),
       columns = .mnlfaMofiaColumnNames(colnames(mm)[attr(mm, "assign") > 0]))
}

# The labels of the prepared design, or NULL if they cannot be derived.
.mnlfaPreparedTermLabels <- function(prepared, decoded) {
  design <- tryCatch(.mnlfaPreparedDesign(prepared), error = function(e) NULL)
  if (is.null(design))
    return(NULL)
  return(.mnlfaTermLabels(design, design[["columns"]], decoded))
}

#' MoFIA's list of moderator columns ("a, b, c") as model-matrix columns
#'
#' The ways to write `text` as columns of `columns` joined by ", " (exact matches only). Returns the columns when
#' there is exactly one way, otherwise NULL.
#' @noRd
.mnlfaSplitColumns <- function(text, columns) {
  solve <- function(rest) {
    out <- list()
    for (cn in columns) {
      if (!nzchar(cn) || !startsWith(rest, cn))
        next
      after <- substring(rest, nchar(cn) + 1L)
      if (!nzchar(after))
        out <- c(out, list(cn))
      else if (startsWith(after, ", "))
        for (tail in solve(substring(after, 3L)))
          out <- c(out, list(c(cn, tail)))
      if (length(out) > 1)
        break
    }
    out
  }
  solutions <- if (nzchar(text)) solve(text) else list()
  if (length(solutions) != 1)
    return(NULL)
  return(solutions[[1]])
}
