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

# MNLFA with MoFIA: from the specification to mofia_model().

#' The exact argument list of mofia_model() for one enabled model
#'
#' All six moderation arguments are always explicit (MoFIA's defaults for mean_mod and var_mod
#' are TRUE). Names are the internal (encoded) column names.
#' @noRd
.mnlfaModelArgs <- function(spec, prepared, model) {
  m <- spec[["models"]][[model]]
  factorList <- function(factors) {
    if (length(factors) == 0)
      return(list())
    return(stats::setNames(as.list(rep(TRUE, length(factors))), factors))
  }
  list(
    data           = prepared[["data"]],
    items          = spec[["indicators"]],
    loadings       = stats::setNames(lapply(spec[["factors"]], `[[`, "indicators"), spec[["factorNames"]]),
    moderators     = prepared[["formula"]],
    standardize    = TRUE,
    continuous     = spec[["continuous"]],
    loading_mod    = m[["loadings"]],
    threshold_mod  = m[["intercepts"]],
    resvar_mod     = m[["residualVariances"]],
    mean_mod       = factorList(m[["means"]]),
    var_mod        = factorList(m[["variances"]]),
    cor_mod        = length(spec[["factors"]]) == 2 && isTRUE(m[["covariances"]]),
    identification = "variance",
    cov_param      = "fisherz",
    sparse         = 5
  )
}

#' Build the MoFIA model and collect its conditions
#'
#' MoFIA's identification diagnostics arrive as typed conditions (classes
#' `mofia_identification_error` / `mofia_identification_warning`, data frame `$issues`; MoFIA
#' 2f7556a). The condition objects are kept whole (class, message, issues) and translated by the
#' caller; other warnings are kept verbatim for "Show warnings". `constructor` is a seam for
#' tests of malformed conditions; production always uses `MoFIA::mofia_model`.
#' @noRd
.mnlfaBuildMofiaModel <- function(args, constructor = MoFIA::mofia_model) {
  identificationWarnings <- list()
  otherWarnings          <- character(0)
  result <- tryCatch(
    withCallingHandlers(
      do.call(constructor, args),
      mofia_identification_warning = function(w) {
        identificationWarnings[[length(identificationWarnings) + 1]] <<- w
        invokeRestart("muffleWarning")
      },
      warning = function(w) {
        otherWarnings <<- c(otherWarnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    mofia_identification_error = function(e) e,
    error                      = function(e) e
  )
  out <- list(model = NULL, identificationError = NULL, identificationWarnings = identificationWarnings,
              error = NULL, warnings = otherWarnings)
  if (inherits(result, "mofia_identification_error"))
    out[["identificationError"]] <- result
  else if (inherits(result, "error"))
    out[["error"]] <- result
  else
    out[["model"]] <- result
  return(out)
}

#' Identification pre-checks of one model
#'
#' JASP's structural checks (fewer than two indicators; the distinct-anchor check) plus the
#' per-factor mean/variance classification read from MoFIA's typed conditions. Returns translated
#' errors (the model is refused) and warnings, the built MoFIA model and other MoFIA warnings.
#' @noRd
.mnlfaCheckIdentification <- function(spec, prepared, model, args = .mnlfaModelArgs(spec, prepared, model),
                                      constructor = MoFIA::mofia_model) {

  m      <- spec[["models"]][[model]]
  label  <- m[["label"]]
  titles <- stats::setNames(spec[["factorTitles"]], spec[["factorNames"]])
  out    <- list(errors = character(0), warnings = character(0), mofiaModel = NULL, mofiaWarnings = character(0))

  for (factor in spec[["factors"]])
    if (length(unique(factor[["indicators"]])) < 2)
      out[["errors"]] <- c(out[["errors"]], gettextf("Factor '%s' needs at least two indicators.", factor[["title"]]))
  if (length(out[["errors"]]) > 0)
    return(out)

  built <- .mnlfaBuildMofiaModel(args, constructor)
  out[["mofiaWarnings"]] <- built[["warnings"]]

  if (!is.null(built[["error"]])) {
    out[["errors"]] <- gettextf("Model %1$s could not be set up. MoFIA reported: %2$s", label,
                                conditionMessage(built[["error"]]))
    return(out)
  }

  # Every typed condition is translated; a typed error always refuses the model, and a typed warning
  # always gives at least one warning, also when its $issues are missing or unusable.
  conditions <- c(if (!is.null(built[["identificationError"]])) list(built[["identificationError"]]),
                  built[["identificationWarnings"]])
  reported   <- list(means = character(0), variances = character(0))
  # The moderator terms are named with their display labels (as in the parameter tables): from the MoFIA model's design
  # and columns when MoFIA built the model, otherwise (a refused model) from the prepared design.
  labels     <- if (length(conditions) > 0) {
    if (!is.null(built[["model"]]))
      .mnlfaTermLabels(built[["model"]][["design"]], colnames(built[["model"]][["X"]]), spec[["decoded"]])
    else
      .mnlfaPreparedTermLabels(prepared, spec[["decoded"]])
  }
  for (condition in conditions) {
    fromError  <- inherits(condition, "mofia_identification_error")
    translated <- .mnlfaIdentificationMessages(condition[["issues"]], m, titles, spec, fromError = fromError,
                                               conditionText = conditionMessage(condition), labels = labels)
    if (fromError && length(translated[["errors"]]) == 0)
      translated[["errors"]] <- gettextf("Model %1$s is not identified. MoFIA reported: %2$s", label,
                                         conditionMessage(condition))
    if (!fromError && length(translated[["warnings"]]) + length(translated[["errors"]]) == 0)
      translated[["warnings"]] <- gettextf("Model %1$s: MoFIA reported an identification problem: %2$s", label,
                                           conditionMessage(condition))
    out[["errors"]]   <- c(out[["errors"]], translated[["errors"]])
    out[["warnings"]] <- c(out[["warnings"]], translated[["warnings"]])
    reported[["means"]]     <- c(reported[["means"]], translated[["reported"]][["means"]])
    reported[["variances"]] <- c(reported[["variances"]], translated[["reported"]][["variances"]])
  }

  anchors <- .mnlfaDistinctAnchorMessages(spec, model, reported = reported)
  out[["errors"]]   <- unique(c(out[["errors"]], anchors[["errors"]]))
  out[["warnings"]] <- unique(c(out[["warnings"]], anchors[["warnings"]]))

  if (!is.null(built[["model"]])) {
    # JASP applies the mask itself: MoFIA must have kept every row it received.
    if (!identical(as.integer(built[["model"]][["cases"]][["index"]]), seq_len(nrow(args[["data"]]))))
      out[["errors"]] <- c(out[["errors"]], gettextf(
        "Model %s: MoFIA excluded cases that the analysis had kept. Please report this problem.", label))
    out[["mofiaModel"]] <- built[["model"]]
  }
  return(out)
}

# Translate MoFIA's $issues rows (columns factor, parameter, status; never the message text, except
# inside the generic frame for rows that cannot be classified). `labels`: display labels of the model-matrix columns.
.mnlfaIdentificationMessages <- function(issues, m, titles, spec, fromError, conditionText = NULL, labels = NULL) {
  label    <- m[["label"]]
  errors   <- character(0)
  warnings <- character(0)
  reported <- list(means = character(0), variances = character(0))

  usable <- is.data.frame(issues) && all(c("factor", "parameter", "status") %in% names(issues))
  if (!usable)
    return(list(errors = errors, warnings = warnings, reported = reported))

  field <- function(column, i) {
    if (!column %in% names(issues)) return(NA_character_)
    x <- issues[[column]][i]
    if (length(x) != 1 || is.na(x)) return(NA_character_)
    return(as.character(x))
  }

  for (i in seq_len(nrow(issues))) {
    factor    <- field("factor", i)
    parameter <- field("parameter", i)
    status    <- field("status", i)
    title     <- if (!is.na(factor) && factor %in% names(titles)) titles[[factor]] else NA_character_
    known     <- !is.na(title) && !is.na(parameter) && !is.na(status) && (
      (parameter == "mean"     && status %in% c("not_identified", "identified_by_loading_moderation")) ||
      (parameter == "variance" && status %in% c("not_identified", "identified_by_functional_form")))

    if (!known) {
      message <- field("message", i)
      if (is.na(message)) message <- conditionText
      text <- gettextf("Model %1$s: MoFIA reported an identification problem: %2$s", label, message)
      isError <- identical(status, "not_identified") ||
        (fromError && !(status %in% c("identified_by_loading_moderation", "identified_by_functional_form")))
      if (isError) errors <- c(errors, text) else warnings <- c(warnings, text)
      next
    }

    # Every text names the moderator terms of its issue row, with their display labels.
    terms <- .mnlfaIssueTerms(field("moderators", i), spec[["decoded"]], labels)
    if (parameter == "mean") {
      reported[["means"]] <- c(reported[["means"]], factor)
      if (status == "not_identified") {
        text <- gettextf("Model %1$s is not identified: the mean of factor '%2$s' and the intercepts or thresholds of all its indicators depend on the moderators (%3$s), and a shift of the mean is offset exactly by shifts of the intercepts or thresholds. Keep at least one anchor indicator (leave its intercepts unchecked) or uncheck the factor mean.",
                         label, title, terms)
        onFactor <- spec[["factors"]][[match(factor, spec[["factorNames"]])]][["indicators"]]
        if (any(onFactor %in% m[["loadings"]]))
          text <- paste(text, gettext("Moderating the loadings does not help here, because their products with these moderators are absorbed by the intercepts or thresholds."))
        errors <- c(errors, text)
      } else {
        warnings <- c(warnings, gettextf("Model %1$s: the mean of factor '%2$s' and the intercepts or thresholds of all its indicators depend on the moderators (%3$s). The mean is identified only through the moderated loadings (only if those effects are not zero). Consider keeping an anchor indicator.",
                                         label, title, terms))
      }
    } else {
      reported[["variances"]] <- c(reported[["variances"]], factor)
      if (status == "not_identified")
        errors <- c(errors, gettextf("Model %1$s is not identified: the variance of factor '%2$s' and the loadings of all its indicators depend on the binary moderator (%3$s), and a larger factor variance is offset exactly by smaller loadings. Keep at least one anchor indicator (leave its loadings unchecked) or uncheck the factor variance.",
                                     label, title, terms))
      else
        warnings <- c(warnings, gettextf("Model %1$s: the variance of factor '%2$s' and the loadings of all its indicators depend on the moderators (%3$s). The variance is identified only through the functional form of the moderation (if at all). Consider keeping an anchor indicator.",
                                         label, title, terms))
    }
  }
  return(list(errors = unique(errors), warnings = unique(warnings), reported = reported))
}

# The moderator terms of one MoFIA issue row for display. MoFIA lists its model-matrix columns (comma separated);
# when the list consists exactly of the model's columns (`names(labels)`), their display labels are shown.
# Otherwise MoFIA's text with the encoded variable names replaced by their display names (longest first). Display
# only; the classification never depends on it.
.mnlfaIssueTerms <- function(terms, decoded, labels = NULL) {
  if (is.na(terms) || !nzchar(terms))
    return(gettext("moderator terms not reported"))
  columns <- if (!is.null(labels)) .mnlfaSplitColumns(terms, names(labels))
  if (!is.null(columns))
    return(paste(labels[columns], collapse = ", "))
  for (encoded in names(decoded)[order(-nchar(names(decoded)))])
    terms <- gsub(encoded, decoded[[encoded]], terms, fixed = TRUE)
  return(terms)
}

# Distinct-anchor check: can every factor whose mean (variance) is moderated have
# its own anchor indicator with unmoderated intercept (loading)? The deficient set is split into its
# connected groups (factors linked through shared eligible anchors); each group is judged on its own,
# so a moderated loading on an unrelated factor cannot soften the verdict for another group.
# Factors already reported by a per-factor rule for the same parameter are reported only there.
.mnlfaDistinctAnchorMessages <- function(spec, model, reported) {
  m        <- spec[["models"]][[model]]
  label    <- m[["label"]]
  errors   <- character(0)
  warnings <- character(0)
  indicatorsOf <- stats::setNames(lapply(spec[["factors"]], `[[`, "indicators"), spec[["factorNames"]])

  checks <- list(
    list(factors = m[["means"]],     free = setdiff(spec[["indicators"]], m[["intercepts"]]),
         reported = reported[["means"]],     what = gettext("means"),     anchorPar = gettext("intercepts"),
         isMeans = TRUE),
    list(factors = m[["variances"]], free = setdiff(spec[["indicators"]], m[["loadings"]]),
         reported = reported[["variances"]], what = gettext("variances"), anchorPar = gettext("loadings"),
         isMeans = FALSE))

  for (check in checks) {
    if (length(check[["factors"]]) == 0)
      next
    eligible <- stats::setNames(lapply(check[["factors"]], function(f) intersect(indicatorsOf[[f]], check[["free"]])),
                                check[["factors"]])
    owner   <- .mnlfaMaxMatching(eligible)
    deficit <- .mnlfaDeficientSet(eligible, owner)
    for (group in .mnlfaAnchorGroups(deficit, eligible)) {
      report <- spec[["factorNames"]][spec[["factorNames"]] %in% setdiff(group, check[["reported"]])]
      if (length(report) == 0)
        next
      sharedAnchors <- spec[["indicators"]][spec[["indicators"]] %in% unlist(eligible[group], use.names = FALSE)]
      loadingMod    <- any(unique(unlist(indicatorsOf[group], use.names = FALSE)) %in% m[["loadings"]])
      reportTitles  <- spec[["factorTitles"]][match(report, spec[["factorNames"]])]
      text <- gettextf("Model %1$s: the factors %2$s share their anchor indicators (%3$s), so their %4$s cannot all be separated. Give one of these factors an additional anchor (leave the %5$s of another of its indicators unchecked) or uncheck the %4$s of one of them.",
                       label,
                       paste(sprintf("'%s'", reportTitles), collapse = ", "),
                       paste(spec[["decoded"]][sharedAnchors], collapse = ", "),
                       check[["what"]], check[["anchorPar"]])
      if (check[["isMeans"]] && !loadingMod)
        errors <- c(errors, text)
      else
        warnings <- c(warnings, text)
    }
  }
  return(list(errors = errors, warnings = warnings))
}

# Connected groups of the deficient set: factors are linked when they share an eligible indicator.
# Each group contains an unmatched factor, so each group is deficient on its own.
.mnlfaAnchorGroups <- function(deficit, eligible) {
  groups <- list()
  remaining <- deficit
  while (length(remaining) > 0) {
    group <- remaining[1]
    repeat {
      anchors <- unique(unlist(eligible[group], use.names = FALSE))
      linked  <- remaining[vapply(remaining, function(f) any(eligible[[f]] %in% anchors), TRUE)]
      added   <- setdiff(linked, group)
      if (length(added) == 0) break
      group <- c(group, added)
    }
    groups[[length(groups) + 1]] <- group
    remaining <- setdiff(remaining, group)
  }
  return(groups)
}

# Maximum bipartite matching of factors to indicators (augmenting paths).
.mnlfaMaxMatching <- function(eligible) {     # named list: factor -> eligible indicator names
  owner <- character(0)                       # names: indicators; values: matched factor
  augment <- function(f, seen) {
    for (j in eligible[[f]]) {
      if (j %in% seen$v) next
      seen$v <- c(seen$v, j)
      if (!(j %in% names(owner)) || augment(owner[[j]], seen)) {
        owner[[j]] <<- f
        return(TRUE)
      }
    }
    FALSE
  }
  for (f in names(eligible)) {
    seen <- new.env()
    seen$v <- character(0)
    augment(f, seen)
  }
  owner
}

# The deficient set: unmatched factors and, repeatedly, the owners of indicators they could use.
.mnlfaDeficientSet <- function(eligible, owner) {
  S <- setdiff(names(eligible), unname(owner))
  repeat {
    nb  <- unique(unlist(eligible[S], use.names = FALSE))
    add <- setdiff(unname(owner[intersect(nb, names(owner))]), S)
    if (!length(add)) break
    S <- c(S, add)
  }
  S
}
