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

# MNLFA with MoFIA: the parameter plots. The plotted quantity is a
# function of the moderators built from `fit$model$par` indices and a grid design built from `fit$model$design` with
# public stats functions; bands are pointwise delta-method intervals with the SE choice of the tables. The plot
# requests are read by name. Pure functions; tests call them via jaspSem:::.

# The plots depend on the model definitions and link (through the display dependencies), the SE choice, alpha and the
# plot requests; never on anything that refits.
.mnlfaPlotDeps <- function() {
  c(.mnlfaMofiaDisplayDeps, "errorCalculationMethod", "parameterEstimatesAlphaLevel", "plotModelList")
}

# ---- the plot rows of the form (rSource plotOptionsForQml) ----

#' The values of the plot rows per model and parameter type
#'
#' Per enabled model whose settings are valid, and per parameter type, the indicators (display names) or factors
#' (titles) whose parameter is moderated in the MoFIA model built from these settings (the same lists that
#' `.mnlfaModelArgs` passes); the correlation row "<title 1>:<title 2>" with exactly two factors and a moderated
#' correlation. Stable values (names and titles, never positions), so that a request survives a rebuild of the source.
#' Lists, so that a single value stays an array.
#' @noRd
.mnlfaPlotSource <- function(spec) {
  out    <- list()
  titles <- stats::setNames(spec[["factorTitles"]], spec[["factorNames"]])
  for (key in names(spec[["models"]])) {
    m <- spec[["models"]][[key]]
    if (length(m[["errors"]]) > 0)
      next
    indicators <- list()
    for (parameter in c("loadings", "intercepts", "residualVariances"))
      if (length(m[[parameter]]) > 0)
        indicators[[parameter]] <- as.list(unname(spec[["decoded"]][m[[parameter]]]))
    factors <- list()
    for (parameter in c("variances", "means"))
      if (length(m[[parameter]]) > 0)
        factors[[parameter]] <- as.list(unname(titles[m[[parameter]]]))
    if (length(spec[["factors"]]) == 2 && isTRUE(m[["covariances"]]))
      factors[["covariances"]] <- list(paste(spec[["factorTitles"]], collapse = ":"))
    entry <- list()
    if (length(indicators) > 0) entry[["indicators"]] <- indicators
    if (length(factors) > 0)    entry[["factors"]]    <- factors
    if (length(entry) > 0)
      out[[key]] <- entry
  }
  return(out)
}

# The R source of the plot rows. A source of an older build (the released module, key "plotOptionsForQml", built from
# the legacy moderation summary) is removed; the new one keeps the source id the form uses.
.mnlfaMofiaPlotOptionsForQml <- function(jaspResults, dataset, options) {
  if (!is.null(jaspResults[["plotOptionsForQml"]]))
    jaspResults[["plotOptionsForQml"]] <- NULL
  if (!is.null(jaspResults[["mofiaPlotOptionsForQml"]]))
    return()
  spec  <- .mnlfaReadSpec(options, dataset)
  value <- if (length(spec[["errors"]]) == 0) .mnlfaPlotSource(spec) else list()
  src   <- createJaspQmlSource(sourceID = "plotOptionsForQml", value = value)
  src$dependOn(.mnlfaMofiaModelDeps)
  jaspResults[["mofiaPlotOptionsForQml"]] <- src
  return()
}

# ---- reading the plot requests (by name) ----

# A dropdown or row value: a string, possibly list-wrapped (list(value = ...)).
.mnlfaPlotValue <- function(x) {
  if (is.list(x) && !is.null(x[["value"]]))
    x <- x[["value"]]
  value <- .mnlfaScalar(x)
  return(if (is.na(value)) "" else value)
}

#' The plot requests: every row of `plotModelList` with "Display plot" checked, read by name
#'
#' A list of requests with `model` (keyValue of level 1), `type` (`indicators`/`factors`), `parameter`, `value` (the
#' row's item or factor), `moderator1`, `moderator2` (dropdown values, "" when empty).
#' @noRd
.mnlfaPlotRequests <- function(plotModelList) {
  requests <- list()
  for (model in plotModelList) {
    for (typeBlock in model[["plotTypeList"]]) {
      for (parameterBlock in typeBlock[["plotParameterList"]]) {
        for (row in parameterBlock[["plotItemList"]]) {
          if (!isTRUE(.mnlfaFlag(row[["includePlot"]])))
            next
          requests[[length(requests) + 1]] <- list(
            model      = .mnlfaPlotValue(model[["keyValue"]]),
            type       = .mnlfaPlotValue(typeBlock[["keyValue"]]),
            parameter  = .mnlfaPlotValue(parameterBlock[["keyValue"]]),
            value      = .mnlfaPlotValue(row[["value"]]),
            moderator1 = .mnlfaPlotValue(row[["plotModerator1"]]),
            moderator2 = .mnlfaPlotValue(row[["plotModerator2"]]))
        }
      }
    }
  }
  return(requests)
}

#' The plotted parameter of a request: the indicator, factor or factor pair, resolved as the moderation
#' rows are (encoded name, display name, `.type` suffix; factor name, then title; the pair by names or
#' titles). Returns `list(error = ...)` when the request cannot be resolved.
#' @noRd
.mnlfaPlotTarget <- function(request, spec) {
  value <- request[["value"]]
  fail  <- function(text) list(error = text)
  if (request[["type"]] == "indicators" &&
      request[["parameter"]] %in% c("loadings", "intercepts", "residualVariances")) {
    item <- .mnlfaResolveIndicator(value, spec[["indicators"]], spec[["decoded"]])
    if (is.na(item))
      return(fail(gettextf("The plot refers to the indicator '%s', which is not assigned to any factor.", value)))
    if (request[["parameter"]] == "residualVariances" && !item %in% spec[["continuous"]])
      return(fail(gettextf("Residual variances are not defined for the categorical indicator '%s'.", spec[["decoded"]][[item]])))
    return(list(parameter = request[["parameter"]], item = item, label = spec[["decoded"]][[item]]))
  }
  if (request[["type"]] == "factors" && request[["parameter"]] %in% c("variances", "means")) {
    factor <- .mnlfaResolveFactor(value, spec[["factorNames"]], spec[["factorTitles"]])
    if (is.na(factor) && sum(spec[["factorTitles"]] == value) > 1)
      return(fail(gettextf("The plot refers to the factor title '%s', which more than one factor has. Give the factors distinct titles to plot them.", value)))
    if (is.na(factor))
      return(fail(gettextf("The plot refers to the factor '%s', which does not exist.", value)))
    return(list(parameter = request[["parameter"]], factor = factor,
                label = spec[["factorTitles"]][spec[["factorNames"]] == factor]))
  }
  if (request[["type"]] == "factors" && request[["parameter"]] == "covariances") {
    if (length(spec[["factors"]]) != 2)
      return(fail(gettext("Factor correlations can be plotted with exactly two factors; with more factors they are constant.")))
    if (!value %in% c(paste(spec[["factorTitles"]], collapse = ":"), paste(spec[["factorNames"]], collapse = ":")))
      return(fail(gettextf("The plot refers to the factor pair '%1$s', which does not match the current factors ('%2$s').",
                           value, paste(spec[["factorTitles"]], collapse = ":"))))
    return(list(parameter = "covariances", factors = spec[["factorNames"]],
                label = paste(spec[["factorTitles"]], collapse = " \u2013 ")))
  }
  return(fail(gettextf("The plot setting '%1$s' / '%2$s' is not known.", request[["type"]], request[["parameter"]])))
}

#' The moderators of a plot
#'
#' Moderator 1 on the x-axis, Moderator 2 as lines. A moderator value is resolved as the moderation rows are
#' (encoded name, display name, then a `.scale`/`.ordinal`/`.nominal` suffix on a display name, as the jaspTools bridge
#' delivers dropdown values). An interaction term "A:B" (an included interaction of the model)
#' with the other dropdown empty means A on the x-axis and lines for B; an interaction term together with a non-empty
#' other dropdown is an error. Only Moderator 2 set: it is the x-axis moderator. The same moderator in both dropdowns
#' is an error (never a silent fallback). Returns `list(x, lines, notes)` (variable names; `lines` NULL) or
#' `list(error = ...)`.
#' @noRd
.mnlfaPlotModerators <- function(moderator1, moderator2, spec) {
  variables <- spec[["moderatorVariables"]]
  decoded   <- spec[["decoded"]]
  resolve   <- function(value) {
    if (!nzchar(value))
      return(list(kind = "none"))
    hit <- .mnlfaResolveIndicator(value, variables, decoded)
    if (!is.na(hit))
      return(list(kind = "moderator", vars = hit))
    pair <- .mnlfaResolveInteraction(value, variables, decoded)
    if (identical(pair, "ambiguous"))
      return(list(kind = "error", error = gettextf("The plot moderator '%s' is ambiguous: it can refer to more than one pair of moderators.", value)))
    if (is.character(pair) && length(pair) == 2) {
      if (!any(vapply(spec[["interactions"]], function(p) setequal(p, pair), TRUE)))
        return(list(kind = "error", error = gettextf("The plot moderator '%s' is an interaction term that is not included in the model.", value)))
      return(list(kind = "interaction", vars = pair, value = value))
    }
    return(list(kind = "error", error = gettextf("The plot moderator '%s' is not a moderator of the model.", value)))
  }
  r1 <- resolve(moderator1)
  r2 <- resolve(moderator2)
  for (r in list(r1, r2))
    if (r[["kind"]] == "error")
      return(list(error = r[["error"]]))

  kinds <- c(r1[["kind"]], r2[["kind"]])
  if (all(kinds == "none"))
    return(list(error = gettext("Select a moderator for the plot.")))
  if ("interaction" %in% kinds) {
    if (!"none" %in% kinds)
      return(list(error = gettext("An interaction term can only be plotted with the other moderator field empty: it then shows its first moderator on the x-axis and lines for the second. Select either the interaction term or two moderators.")))
    r <- if (r1[["kind"]] == "interaction") r1 else r2
    # The term is written with the display names, not the delivered value (encoded names in JASP Desktop), so the
    # caption is wrapped at its displayed length.
    return(list(x = r[["vars"]][1], lines = r[["vars"]][2], notes = gettextf(
      "The interaction term %1$s is shown with %2$s on the x-axis and lines for %3$s.",
      paste(unname(decoded[r[["vars"]]]), collapse = ":"), decoded[[r[["vars"]][1]]], decoded[[r[["vars"]][2]]])))
  }
  if (r1[["kind"]] == "none")
    return(list(x = r2[["vars"]], lines = NULL, notes = character(0)))
  if (r2[["kind"]] == "none")
    return(list(x = r1[["vars"]], lines = NULL, notes = character(0)))
  if (identical(r1[["vars"]], r2[["vars"]]))
    return(list(error = gettextf("The moderator '%s' is selected twice; select a different second moderator or leave it empty.",
                                 decoded[[r1[["vars"]]]])))
  return(list(x = r1[["vars"]], lines = r2[["vars"]], notes = character(0)))
}

# ---- the grid and its design matrix ----

#' The model-matrix rows of new moderator values
#'
#' `newdata`: the moderator variables of `design$vars` on their original scale. Nominal moderators become fresh
#' factors with the design's levels (no contrasts attribute), the standardized moderators are standardized with the
#' design's mean and SD, and the model matrix of the design's terms is built with its levels and contrasts; the
#' intercept is dropped. The columns are aligned with the model's columns `columns` (`colnames(fit$model$X)`) by name
#' in MoFIA's naming (`.mnlfaMofiaColumnNames`: a nominal level with a comma is named with "; "), with an
#' exact one-to-one check; never by position alone. Attribute `termVariables`: per column, the moderator variables of
#' its term.
#' @noRd
.mnlfaDesignMatrix <- function(design, columns, newdata) {
  if (!identical(design[["type"]], "formula") || is.null(design[["terms"]]))
    stop(gettext("The moderator design of this model cannot be evaluated for plotting."))
  missingVars <- setdiff(design[["vars"]], names(newdata))
  if (length(missingVars) > 0)
    stop(sprintf(ngettext(length(missingVars), "Values of the moderator %s are missing.",
                          "Values of the moderators %s are missing."), paste(missingVars, collapse = ", ")))
  nd <- as.data.frame(newdata[design[["vars"]]], stringsAsFactors = FALSE)
  for (v in intersect(names(design[["xlevels"]]), names(nd)))
    nd[[v]] <- factor(as.character(nd[[v]]), levels = design[["xlevels"]][[v]])
  for (v in names(design[["scaling"]]))
    nd[[v]] <- (nd[[v]] - design[["scaling"]][[v]][["mean"]]) / design[["scaling"]][[v]][["sd"]]
  tt <- stats::delete.response(design[["terms"]])
  mf <- stats::model.frame(tt, nd, xlev = design[["xlevels"]], na.action = stats::na.fail)
  X  <- stats::model.matrix(tt, mf, contrasts.arg = design[["contrasts"]])
  assign <- attr(X, "assign")
  keep   <- colnames(X) != "(Intercept)"
  X      <- X[, keep, drop = FALSE]
  assign <- assign[keep]
  names  <- .mnlfaMofiaColumnNames(colnames(X))
  index  <- match(columns, names)
  if (length(names) != length(columns) || anyDuplicated(names) || anyNA(index))
    stop(gettext("The moderator columns of the plot grid do not match the model's columns."))
  X <- X[, index, drop = FALSE]
  colnames(X) <- columns
  attr(X, "assign") <- NULL
  factors <- attr(tt, "factors")
  exprs   <- as.list(attr(tt, "variables"))[-1]
  attr(X, "termVariables") <- lapply(assign[index], function(term)
    unique(unlist(lapply(exprs[factors[, term] > 0], all.vars))))
  return(X)
}

#' The grid of a plot
#'
#' The x-axis moderator on its original scale: a continuous moderator at 100 points over its observed range in the
#' analysed sample, a binary one at its two values, a nominal one at its levels. The line moderator (optional) at its
#' two values, its levels, or the mean - 1 SD, mean and mean + 1 SD (continuous). Every other moderator at its mean
#' (continuous), at 0 (binary) or at its reference level (nominal). Values and statistics come from the design MoFIA
#' recorded on the analysed sample (`design$info`, `design$xlevels`). Returns `newdata` (original scale), the
#' displayed x and line values, whether x is discrete, and the note on the fixed moderators.
#' @noRd
.mnlfaPlotGrid <- function(design, xVar, lineVar, decoded) {
  info <- design[["info"]]
  name <- function(v) if (v %in% names(decoded)) decoded[[v]] else v
  format3 <- function(x) format(signif(x, 4))
  for (v in c(xVar, lineVar))
    if (is.null(info[[v]]))
      stop(gettextf("The moderator '%s' is not part of the model's design.", name(v)))

  axisValues <- function(v) {
    i <- info[[v]]
    switch(i[["type"]],
           continuous = seq(i[["range"]][1], i[["range"]][2], length.out = 100),
           binary     = i[["values"]],
           factor     = design[["xlevels"]][[v]],
           stop(gettextf("The moderator '%s' has a type that cannot be plotted.", name(v))))
  }
  lineValues <- function(v) {
    i <- info[[v]]
    switch(i[["type"]],
           continuous = i[["mean"]] + c(-1, 0, 1) * i[["sd"]],
           binary     = i[["values"]],
           factor     = design[["xlevels"]][[v]],
           stop(gettextf("The moderator '%s' has a type that cannot be plotted.", name(v))))
  }
  lineLabels <- function(v, values) {
    if (info[[v]][["type"]] == "continuous")
      return(c(gettextf("Mean \u2212 1 SD (%s)", format3(values[1])), gettextf("Mean (%s)", format3(values[2])),
               gettextf("Mean + 1 SD (%s)", format3(values[3]))))
    return(as.character(values))
  }

  xValues <- axisValues(xVar)
  lValues <- if (is.null(lineVar)) NA else lineValues(lineVar)
  n       <- length(xValues) * length(lValues)
  newdata <- list()
  fixed   <- character(0)
  for (v in design[["vars"]]) {
    i <- info[[v]]
    if (identical(v, xVar)) {
      newdata[[v]] <- rep(xValues, times = length(lValues))
    } else if (identical(v, lineVar)) {
      newdata[[v]] <- rep(lValues, each = length(xValues))
    } else if (i[["type"]] == "continuous") {
      newdata[[v]] <- rep(i[["mean"]], n)
      fixed <- c(fixed, gettextf("%1$s at its mean (%2$s)", name(v), format3(i[["mean"]])))
    } else if (i[["type"]] == "binary") {
      newdata[[v]] <- rep(0, n)
      fixed <- c(fixed, gettextf("%s = 0", name(v)))
    } else {
      newdata[[v]] <- rep(design[["xlevels"]][[v]][1], n)
      fixed <- c(fixed, gettextf("%1$s at its reference level (%2$s)", name(v), design[["xlevels"]][[v]][1]))
    }
  }
  newdata <- as.data.frame(newdata, stringsAsFactors = FALSE, optional = TRUE)
  list(newdata    = newdata,
       xValue     = rep(xValues, times = length(lValues)),
       xDiscrete  = info[[xVar]][["type"]] != "continuous",
       xLevels    = as.character(xValues),
       line       = if (is.null(lineVar)) rep("", n) else rep(lineLabels(lineVar, lValues), each = length(xValues)),
       lineValue  = if (is.null(lineVar)) rep(NA, n) else rep(lValues, each = length(xValues)),
       lineLevels = if (is.null(lineVar)) character(0) else lineLabels(lineVar, lValues),
       fixedNote  = if (length(fixed) > 0)
         gettextf("Other moderators: %s.", paste(fixed, collapse = "; ")) else character(0))
}

# ---- the plotted quantity ----

# The JASP level labels of the categories around threshold k of item j (as in the tables).
.mnlfaPlotThresholdLabel <- function(model, j, k, categoryLevels) {
  observed <- model[["levels"]][[j]]
  declared <- categoryLevels[[model[["items"]][j]]]
  label    <- function(code) {
    if (!is.null(declared) && code >= 1 && code <= length(declared)) declared[code] else as.character(code)
  }
  return(gettextf("Threshold %1$d (%2$s | %3$s)", k, label(observed[k]), label(observed[k + 1])))
}

#' The curves of a plotted parameter
#'
#' Each curve: `label` (unique display label; factor titles that more than one factor has get the factor's position),
#' `base` (row of `fit$model$par` with coefficient 1, NA for the fixed factor mean and variance),
#' `moderation` (rows whose coefficient is multiplied by the model-matrix column `par$mod`), `sign` (of the
#' moderation part) and the `transform` of the linear predictor. Loadings: lambda0 + x'lambda1, one curve per factor
#' of a cross-loading item; continuous intercepts nu0 + x'beta; categorical intercepts the effective thresholds
#' tau_k - x'beta, one curve per threshold; residual variances exp(omega0 + x'omega); factor variances
#' exp(x'delta); factor means x'gamma; correlations tanh(zeta0 + x'zeta1). From the indices of `fit$model$par`,
#' never from parameter names.
#' @noRd
.mnlfaPlotCurves <- function(fit, target, titles, categoryLevels) {
  model <- fit[["model"]]
  par   <- model[["par"]]
  rowsOf <- function(block, ...) {
    keys <- list(...)
    sel  <- par[["block"]] == block
    for (k in names(keys))
      sel <- sel & !is.na(par[[k]]) & par[[k]] == keys[[k]]
    which(sel)
  }
  curve <- function(label, base, moderation, sign = 1, transform = "identity")
    list(label = label, base = base, moderation = moderation, sign = sign, transform = transform)
  factorTitle <- function(d) { f <- model[["factors"]][d]; if (f %in% names(titles)) titles[[f]] else f }
  allTitles   <- vapply(seq_along(model[["factors"]]), factorTitle, "")
  factorLabel <- function(d) {
    if (sum(allTitles == allTitles[d]) > 1) gettextf("%1$s (factor %2$d)", allTitles[d], d) else allTitles[d]
  }

  parameter <- target[["parameter"]]
  if (parameter %in% c("loadings", "intercepts", "residualVariances")) {
    j <- match(target[["item"]], model[["items"]])
    if (is.na(j))
      stop(gettext("The indicator is not part of this model."))
  }
  curves <- list()
  if (parameter == "loadings") {
    for (d in seq_along(model[["factors"]])) {
      base <- rowsOf("lambda0", item = j, factor = d)
      if (length(base) == 1)
        curves[[length(curves) + 1]] <- curve(factorLabel(d), base, rowsOf("lambda1", item = j, factor = d))
    }
  } else if (parameter == "intercepts") {
    if (isTRUE(model[["cont"]][[j]])) {
      curves[[1]] <- curve("", rowsOf("nu0", item = j), rowsOf("beta", item = j))
    } else {
      thresholds <- rowsOf("tau", item = j)
      for (i in thresholds[order(par[["k"]][thresholds])])
        curves[[length(curves) + 1]] <- curve(.mnlfaPlotThresholdLabel(model, j, par[["k"]][i], categoryLevels), i,
                                              rowsOf("beta", item = j), sign = -1)
    }
  } else if (parameter == "residualVariances") {
    curves[[1]] <- curve("", rowsOf("omega0", item = j), rowsOf("omega", item = j), transform = "exp")
  } else if (parameter %in% c("variances", "means")) {
    d <- match(target[["factor"]], model[["factors"]])
    if (is.na(d))
      stop(gettext("The factor is not part of this model."))
    curves[[1]] <- if (parameter == "variances")
      curve("", NA_integer_, rowsOf("delta", factor = d), transform = "exp")
    else
      curve("", NA_integer_, rowsOf("gamma", factor = d))
  } else if (parameter == "covariances") {
    d     <- match(target[["factors"]], model[["factors"]])
    pairs <- model[["pairs"]]
    p     <- which(apply(pairs, 1, function(row) setequal(row, d)))
    if (anyNA(d) || length(p) != 1)
      stop(gettext("The factor pair is not part of this model."))
    curves[[1]] <- curve("", rowsOf("r0", pair = p), rowsOf("r1", pair = p), transform = "tanh")
  }
  if (length(curves) == 0 || any(vapply(curves, function(cv) length(cv[["base"]]) != 1, TRUE)))
    stop(gettext("The plotted parameter was not found in the model's parameters."))
  labels <- vapply(curves, `[[`, "", "label")
  if (anyDuplicated(labels) > 0)                       # never merge two curves under one label
    for (k in seq_along(curves))
      curves[[k]][["label"]] <- gettextf("%1$s (%2$d)", labels[k], k)
  return(curves)
}

#' The data of one plot (a seam for tests)
#'
#' For every curve and grid point: the linear predictor eta = g'theta (g: 1 for the baseline parameter, the signed
#' model-matrix value for each moderation parameter), its delta-method SE sqrt(g'Vg) with `V` the covariance matrix of
#' the selected SE choice (NULL: no band), and the displayed estimate f(eta) with the pointwise band
#' f(eta -/+ q SE), q = qnorm(1 - alpha/2), f the curve's transform (identity, exp, tanh). Returns the data frame
#' (`curveId` (the curve's index, its identity), `curve` (its unique label), `line`, `x` (numeric x value or the level),
#' `xValue`, `lineValue`, `eta`, `seEta`, `estimate`, `lower`, `upper`) and the plot's descriptions.
#' @noRd
.mnlfaPlotData <- function(fit, target, moderators, V, alpha, titles, decoded, categoryLevels) {
  model  <- fit[["model"]]
  par    <- model[["par"]]
  theta  <- unname(fit[["coefficients"]])
  if (length(theta) != nrow(par))
    stop(gettext("The coefficients of the MoFIA fit do not match its parameter table."))
  curves <- .mnlfaPlotCurves(fit, target, titles, categoryLevels)
  grid   <- .mnlfaPlotGrid(model[["design"]], moderators[["x"]], moderators[["lines"]], decoded)
  X      <- .mnlfaDesignMatrix(model[["design"]], colnames(model[["X"]]), grid[["newdata"]])
  q      <- stats::qnorm(1 - alpha / 2)
  f      <- function(transform, x) switch(transform, identity = x, exp = exp(x), tanh = tanh(x))

  rows <- list()
  badVariance <- FALSE
  for (id in seq_along(curves)) {
    cv    <- curves[[id]]
    index <- c(if (!is.na(cv[["base"]])) cv[["base"]], cv[["moderation"]])
    G     <- matrix(0, nrow(X), length(index))
    if (!is.na(cv[["base"]]))
      G[, 1] <- 1
    offset <- if (is.na(cv[["base"]])) 0 else 1
    for (k in seq_along(cv[["moderation"]]))
      G[, offset + k] <- cv[["sign"]] * X[, par[["mod"]][cv[["moderation"]][k]]]
    eta <- if (length(index) > 0) as.vector(G %*% theta[index]) else rep(0, nrow(X))
    se  <- rep(NA_real_, nrow(X))
    if (!is.null(V) && length(index) > 0) {
      Vs       <- V[index, index, drop = FALSE]
      variance <- rowSums((G %*% Vs) * G)
      ok       <- is.finite(variance) & variance >= 0
      se[ok]   <- sqrt(variance[ok])
      badVariance <- badVariance || any(!ok)
    } else if (!is.null(V)) {
      se <- rep(0, nrow(X))
    }
    rows[[length(rows) + 1]] <- data.frame(
      curveId   = id,
      curve     = cv[["label"]],
      line      = grid[["line"]],
      x         = if (grid[["xDiscrete"]]) as.character(grid[["xValue"]]) else grid[["xValue"]],
      xValue    = grid[["xValue"]],
      lineValue = grid[["lineValue"]],
      eta       = eta,
      seEta     = se,
      estimate  = f(cv[["transform"]], eta),
      lower     = f(cv[["transform"]], eta - q * se),
      upper     = f(cv[["transform"]], eta + q * se),
      stringsAsFactors = FALSE)
  }
  data <- do.call(rbind, rows)

  # The moderator terms that vary along the plot (display labels) and whether the parameter is moderated at all.
  moderation <- unique(unlist(lapply(curves, function(cv) par[["mod"]][cv[["moderation"]]]), use.names = FALSE))
  plotted    <- c(moderators[["x"]], moderators[["lines"]])
  varying    <- moderation[vapply(moderation, function(m) any(attr(X, "termVariables")[[m]] %in% plotted), TRUE)]
  columns    <- colnames(model[["X"]])
  labels     <- .mnlfaTermLabels(model[["design"]], columns, decoded)
  termLabel  <- function(m) if (!is.null(labels)) labels[[columns[m]]] else columns[m]
  list(data = data, curves = vapply(curves, `[[`, "", "label"), transform = curves[[1]][["transform"]],
       xDiscrete = grid[["xDiscrete"]], xLevels = grid[["xLevels"]], lineLevels = grid[["lineLevels"]],
       fixedNote = grid[["fixedNote"]], moderated = length(moderation) > 0,
       terms = vapply(sort(varying), termLabel, ""), badVariance = badVariance)
}

# ---- building the plots ----

# The axis title of each plotted quantity (the parameter and its scale).
.mnlfaPlotAxisTitle <- function(parameter, categorical, link) {
  switch(parameter,
         loadings          = gettext("Loading"),
         intercepts        = if (categorical) gettextf("Effective threshold (%s scale)", link) else gettext("Intercept"),
         residualVariances = gettext("Residual variance"),
         variances         = gettext("Factor variance"),
         means             = gettext("Factor mean"),
         covariances       = gettext("Factor correlation (r)"))
}

# The plot title: the parameter and the indicator, factor or pair.
.mnlfaPlotTitle <- function(request, label) {
  what <- switch(request[["parameter"]],
                 loadings          = gettext("Loadings"),
                 intercepts        = gettext("Intercepts"),
                 residualVariances = gettext("Residual variances"),
                 variances         = gettext("Factor variances"),
                 means             = gettext("Factor means"),
                 covariances       = gettext("Factor correlation"),
                 request[["parameter"]])
  return(gettextf("%1$s: %2$s", what, label))
}

#' The parameter plots
#'
#' One container "Parameter Plots" with one container per model and in it one plot per request with "Display plot"
#' checked, in the order of the form. A request that cannot be drawn shows its reason as the plot's error; a model
#' without inference shows its curve without a band and the reason in the caption. Built from the
#' cached fits; never fits.
#' @noRd
.mnlfaMofiaPlots <- function(jaspResults, spec, prepared, models, fits, options) {
  requests <- .mnlfaPlotRequests(options[["plotModelList"]])
  if (length(requests) == 0 || !is.null(jaspResults[["mofiaPlotContainer"]]))
    return()

  container <- createJaspContainer(gettext("Parameter Plots"))
  container$dependOn(.mnlfaPlotDeps())
  container$position <- 102.5
  jaspResults[["mofiaPlotContainer"]] <- container

  for (i in seq_along(requests)) {
    request <- requests[[i]]
    key     <- request[["model"]]
    modelContainer <- container[[key]]
    if (is.null(modelContainer)) {
      modelContainer <- createJaspContainer(.mnlfaModelLabel(key))
      position <- match(key, .mnlfaModelKeys)
      modelContainer$position <- if (is.na(position)) length(.mnlfaModelKeys) + 1 else position
      container[[key]] <- modelContainer
    }
    # Any unexpected failure while building a plot stays on this plot; the rest of the output is built.
    # Failures when the plot object is drawn are caught by jaspResults, which sets the plot's error.
    built <- tryCatch(.mnlfaPlotBuild(request, spec, prepared, models, fits, options), error = function(e)
      list(title = .mnlfaPlotTitle(request, request[["value"]]), width = 480, height = 360,
           error = gettextf("The plot could not be drawn: %s", conditionMessage(e))))
    plot  <- createJaspPlot(title = built[["title"]], width = built[["width"]], height = built[["height"]])
    plot$position <- i
    modelContainer[[paste0("plot", i)]] <- plot
    if (!is.null(built[["error"]]))
      plot$setError(built[["error"]])
    else
      plot$plotObject <- built[["plot"]]
  }
  return()
}

# One plot: its title, size and either a ggplot or an error message.
.mnlfaPlotBuild <- function(request, spec, prepared, models, fits, options) {
  out    <- list(title  = .mnlfaPlotTitle(request, request[["value"]]), width = 480, height = 360, error = NULL,
                 plot   = NULL)
  fail   <- function(text) utils::modifyList(out, list(error = text))
  key    <- request[["model"]]
  label  <- .mnlfaModelLabel(key)
  target <- .mnlfaPlotTarget(request, spec)
  if (!is.null(target[["error"]]))
    return(fail(target[["error"]]))
  out[["title"]] <- .mnlfaPlotTitle(request, target[["label"]])

  if (!key %in% names(models))
    return(fail(gettextf("Model %s is not enabled.", label)))
  if (length(models[[key]][["errors"]]) > 0)
    return(fail(gettextf("Model %s is refused and has no estimates; the reason is shown in the table 'Model Fit (MoFIA)'.",
                         label)))
  if (is.null(fits[[key]]))
    return(fail(gettextf("Model %s has not been fitted. Check 'Sync Analysis'.", label)))
  outcome <- .mnlfaFitOutcome(fits[[key]], label)
  if (!is.null(outcome[["error"]]))
    return(fail(outcome[["error"]]))
  if (!isTRUE(outcome[["estimates"]]))
    return(fail(gettextf("Model %s has no estimates.", label)))

  moderators <- .mnlfaPlotModerators(request[["moderator1"]], request[["moderator2"]], spec)
  if (!is.null(moderators[["error"]]))
    return(fail(moderators[["error"]]))

  fit            <- fits[[key]][["fit"]]
  alpha          <- options[["parameterEstimatesAlphaLevel"]]
  standardErrors <- .mnlfaStandardErrors(fits[[key]], outcome, options[["errorCalculationMethod"]])
  band           <- isTRUE(standardErrors[["available"]])
  titles         <- stats::setNames(spec[["factorTitles"]], spec[["factorNames"]])
  data <- tryCatch(.mnlfaPlotData(fit, target, moderators, if (band) standardErrors[["vcov"]], alpha, titles,
                                  spec[["decoded"]], prepared[["categoryLevels"]]),
                   error = function(e) e)
  if (inherits(data, "error"))
    return(fail(gettextf("The plot could not be drawn: %s", conditionMessage(data))))

  notes <- character(0)
  if (band) {
    notes <- c(notes, gettextf("Pointwise %s%% confidence bands; they are not invariance tests.", format(100 * (1 - alpha))))
    if (data[["transform"]] == "exp")
      notes <- c(notes, gettext("The band is computed on the log scale and exponentiated."))
    if (data[["transform"]] == "tanh")
      notes <- c(notes, gettext("The band is computed on the Fisher-z scale and transformed to the correlation scale."))
    if (data[["badVariance"]])
      notes <- c(notes, gettext("Where the estimated sampling variance of the curve is negative or not finite, no band is shown."))
  } else {
    # Without inference: the curve without a band, with the model's reason (the same texts as the tables)
    notes <- c(notes, gettext("The curve is shown without a confidence band."), outcome[["statusFootnotes"]],
               standardErrors[["footnotes"]])
  }
  if (!data[["moderated"]])
    notes <- c(notes, gettextf("This parameter is not moderated in model %s; the curve is constant.", label))
  else if (length(data[["terms"]]) > 0)
    notes <- c(notes, sprintf(ngettext(length(data[["terms"]]), "The curve includes the moderator term %s.",
                                       "The curve includes the moderator terms %s."),
                              paste(data[["terms"]], collapse = ", ")))
  notes <- c(notes, moderators[["notes"]], data[["fixedNote"]])

  item        <- match(target[["item"]], fit[["model"]][["items"]])
  categorical <- target[["parameter"]] == "intercepts" && !isTRUE(fit[["model"]][["cont"]][[item]])
  name        <- function(v) spec[["decoded"]][[v]]
  legend      <- length(data[["curves"]]) > 1 || !is.null(moderators[["lines"]])
  panels      <- if (length(data[["curves"]]) > 1) length(data[["lineLevels"]]) else 0L
  columns     <- min(3L, max(1L, panels))
  caption     <- unlist(lapply(notes, strwrap, width = if (legend) 90 else 70))
  out[["width"]]  <- if (panels > 0) 280 * columns + 200 else if (legend) 600 else 480
  out[["height"]] <- (if (panels > 0) 260 * ceiling(panels / columns) + 80 else 320) + 18 * length(caption)
  out[["plot"]]   <- .mnlfaPlotObject(
    data,
    xTitle     = name(moderators[["x"]]),
    yTitle     = .mnlfaPlotAxisTitle(target[["parameter"]], categorical, fit[["link"]]),
    curveTitle = if (target[["parameter"]] == "loadings") gettext("Factor") else gettext("Threshold"),
    lineTitle  = if (!is.null(moderators[["lines"]])) name(moderators[["lines"]]) else "",
    band       = band,
    caption    = caption)
  return(out)
}

#' The ggplot of one plot
#'
#' Continuous x: lines with ribbons; binary or nominal x: points with error bars. Several curves (factors of a
#' cross-loading item, thresholds) are distinguished by colour; with a Moderator 2 the plot then has one panel per
#' Moderator 2 value, so that every value stays identifiable with or without bands and for any number of levels.
#' With one curve the lines of Moderator 2 are distinguished by colour. The curves are grouped by
#' their index, never by their label. `caption`: the plot's notes, wrapped, one element per line.
#' @noRd
.mnlfaPlotObject <- function(data, xTitle, yTitle, curveTitle, lineTitle, band, caption) {
  df       <- data[["data"]]
  curves   <- length(data[["curves"]]) > 1
  lines    <- length(data[["lineLevels"]]) > 0
  panels   <- curves && lines
  df$curve <- factor(df$curve, levels = data[["curves"]])
  df$line  <- factor(df$line, levels = if (lines) data[["lineLevels"]] else "")
  df$group <- interaction(df$curveId, df$line, drop = TRUE)
  if (data[["xDiscrete"]])
    df$x <- factor(df$x, levels = data[["xLevels"]])
  if (panels)
    df$panel <- factor(paste0(lineTitle, ": ", df$line), levels = paste0(lineTitle, ": ", data[["lineLevels"]]))
  colourBy <- if (curves) "curve" else if (lines) "line" else NULL

  aesthetics <- if (!is.null(colourBy))
    ggplot2::aes(x = .data[["x"]], y = .data[["estimate"]], group = .data[["group"]], colour = .data[[colourBy]],
                 fill = .data[[colourBy]])
  else
    ggplot2::aes(x = .data[["x"]], y = .data[["estimate"]], group = .data[["group"]])

  yRange <- range(c(df$estimate, if (band) c(df$lower, df$upper)), na.rm = TRUE)
  gg     <- ggplot2::ggplot(df, aesthetics)
  if (data[["xDiscrete"]]) {
    dodge <- ggplot2::position_dodge(width = if (curves || lines) 0.4 else 0)
    if (band)
      gg <- gg + ggplot2::geom_errorbar(ggplot2::aes(ymin = .data[["lower"]], ymax = .data[["upper"]]), width = 0.2,
                                        position = dodge, na.rm = TRUE)
    gg <- gg + ggplot2::geom_point(size = 3, position = dodge) + ggplot2::scale_x_discrete(drop = FALSE)
  } else {
    if (band)
      gg <- gg + ggplot2::geom_ribbon(ggplot2::aes(ymin = .data[["lower"]], ymax = .data[["upper"]]), alpha = 0.2,
                                      colour = NA, na.rm = TRUE)
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(range(df$x))
    gg <- gg + ggplot2::geom_line(linewidth = 1) +
      ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(c(xBreaks, df$x)))
  }
  if (panels)
    gg <- gg + ggplot2::facet_wrap(ggplot2::vars(.data[["panel"]]), ncol = min(3L, length(data[["lineLevels"]])))
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(yRange)
  gg <- gg +
    ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(c(yBreaks, yRange))) +
    ggplot2::labs(x = xTitle, y = yTitle, caption = paste(caption, collapse = "\n")) +
    (if (!is.null(colourBy)) ggplot2::labs(colour = if (curves) curveTitle else lineTitle,
                                           fill = if (curves) curveTitle else lineTitle)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = if (curves || lines) "right" else "none") +
    ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0, size = 11))
  return(gg)
}
