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

# MNLFA with MoFIA: data preparation. Works on the raw typed dataset, before the
# legacy numeric conversion; returns errors instead of stopping.

#' Prepare the data for mofia_model()
#'
#' Returns the masked data (indicators: numeric, categorical as level-order integer codes;
#' moderator variables: numeric or factors with explicit treatment contrasts), the original row
#' indices of the analysed cases, exclusion counts, the moderator formula and footnotes.
#' @noRd
.mnlfaPrepareData <- function(spec, dataset) {

  out <- list(errors = character(0), footnotes = character(0))
  decoded  <- spec[["decoded"]]
  items    <- spec[["indicators"]]
  modVars  <- spec[["moderatorVariables"]]

  missingColumns <- setdiff(c(items, modVars), colnames(dataset))
  if (length(missingColumns) > 0) {
    out[["errors"]] <- sprintf(ngettext(length(missingColumns), "The data do not contain the variable %s.",
                                        "The data do not contain the variables %s."),
                               paste(decoded[missingColumns], collapse = ", "))
    return(out)
  }

  n       <- nrow(dataset)
  columns <- list()
  categoryLevels <- list()

  # ---- indicators: type from the assignment; the column must agree ----
  for (item in items) {
    x <- dataset[[item]]
    if (spec[["indicatorTypes"]][[item]] == "scale") {
      if (!is.numeric(x)) {
        out[["errors"]] <- c(out[["errors"]], gettextf(
          "Indicator '%s' is assigned as a continuous (scale) variable, but its column is not numeric.", decoded[[item]]))
        next
      }
      x <- as.numeric(x)
      x[is.nan(x)] <- NA
    } else {
      if (!is.factor(x)) {
        out[["errors"]] <- c(out[["errors"]], gettextf(
          "Indicator '%s' is assigned as an ordinal variable, but its column is not categorical.", decoded[[item]]))
        next
      }
      categoryLevels[[item]] <- levels(x)
      x <- as.integer(x)        # level order: level 1 -> code 1
    }
    if (all(is.na(x))) {
      out[["errors"]] <- c(out[["errors"]], gettextf("Indicator '%s' has no observed values.", decoded[[item]]))
      next
    }
    columns[[item]] <- x
  }

  # ---- moderators: scale -> numeric, nominal -> factor ----
  for (moderator in spec[["moderators"]]) {
    variable <- moderator[["variable"]]
    x <- dataset[[variable]]
    if (moderator[["type"]] == "scale") {
      if (!is.numeric(x)) {
        out[["errors"]] <- c(out[["errors"]], gettextf(
          "Moderator '%s' is a scale variable, but its column is not numeric.", decoded[[variable]]))
        next
      }
      x <- as.numeric(x)
      x[is.nan(x)] <- NA
    } else {
      if (!is.factor(x)) {
        out[["errors"]] <- c(out[["errors"]], gettextf(
          "Moderator '%s' is a nominal variable, but its column is not categorical.", decoded[[variable]]))
        next
      }
      x <- factor(as.character(x), levels = levels(x))   # unordered, with the data's level order
    }
    columns[[variable]] <- x
  }
  if (length(out[["errors"]]) > 0)
    return(out)

  data <- as.data.frame(columns[c(items, modVars)], check.names = FALSE, stringsAsFactors = FALSE)

  # ---- the analysis-row mask: one mask for every model ----
  moderatorMissing <- !stats::complete.cases(data[, modVars, drop = FALSE])
  noResponse       <- !moderatorMissing & rowSums(!is.na(data[, items, drop = FALSE])) == 0
  keep             <- !moderatorMissing & !noResponse

  out[["nInput"]]             <- n
  out[["nExcludedModerator"]] <- sum(moderatorMissing)
  out[["nExcludedResponse"]]  <- sum(noResponse)
  out[["rowIndices"]]         <- which(keep)
  if (!any(keep)) {
    out[["errors"]] <- gettext("No cases remain after excluding cases with a missing moderator value and cases without any observed indicator.")
    return(out)
  }
  if (!all(keep))
    out[["footnotes"]] <- c(out[["footnotes"]], sprintf(ngettext(sum(!keep),
      "%1$d case was excluded: %2$d with a missing moderator value and %3$d without any observed indicator.",
      "%1$d cases were excluded: %2$d with a missing moderator value and %3$d without any observed indicator."),
      sum(!keep), out[["nExcludedModerator"]], out[["nExcludedResponse"]]))
  data <- data[keep, , drop = FALSE]

  # An indicator observed only in excluded rows carries no information about its parameters.
  for (item in items)
    if (all(is.na(data[[item]])))
      out[["errors"]] <- c(out[["errors"]], gettextf(
        "Indicator '%s' has no observed values in the analysed sample.", decoded[[item]]))
  if (length(out[["errors"]]) > 0)
    return(out)

  # ---- indicator preprocessing: continuous indicators only, mean and SD of the masked sample ----
  if (spec[["preprocessing"]] != "none") {
    if (length(spec[["continuous"]]) == 0) {
      out[["footnotes"]] <- c(out[["footnotes"]], gettext(
        "Indicator preprocessing applies to continuous indicators only; no continuous indicators are assigned, so it had no effect."))
    }
    for (item in spec[["continuous"]]) {
      x  <- data[[item]]
      mu <- mean(x, na.rm = TRUE)
      if (spec[["preprocessing"]] == "center") {
        data[[item]] <- x - mu
      } else {
        s <- stats::sd(x, na.rm = TRUE)
        if (!is.finite(s) || s == 0) {
          out[["errors"]] <- c(out[["errors"]], gettextf(
            "Indicator '%s' has no variance in the analysed sample, so it cannot be z-standardized.", decoded[[item]]))
          next
        }
        data[[item]] <- (x - mu) / s
      }
    }
    if (spec[["preprocessing"]] == "standardize" && length(spec[["continuous"]]) > 0)
      out[["footnotes"]] <- c(out[["footnotes"]], gettext(
        "Continuous indicators were z-standardized on the analysed sample."))
  }

  # ---- moderator classification, levels and the formula: a two-valued scale moderator stays as coded, a nominal
  # moderator is dummy coded against its first level ----
  moderatorInfo <- list()
  baseTerms <- squareTerms <- cubeTerms <- character(0)
  for (moderator in spec[["moderators"]]) {
    variable <- moderator[["variable"]]
    name     <- decoded[[variable]]
    x        <- data[[variable]]
    info     <- list(variable = variable, type = moderator[["type"]], inputLevels = NULL, droppedLevels = character(0))
    if (moderator[["type"]] == "scale") {
      nDistinct <- length(unique(x))
      if (nDistinct < 2) {
        out[["errors"]] <- c(out[["errors"]], gettextf("The moderator '%s' is constant in the analysed sample.", name))
        next
      }
      info[["class"]] <- if (nDistinct == 2) "binary" else "continuous"
    } else {
      info[["class"]]       <- "nominal"
      info[["inputLevels"]] <- levels(x)
      x <- droplevels(x)
      info[["droppedLevels"]] <- setdiff(info[["inputLevels"]], levels(x))
      if (length(info[["droppedLevels"]]) > 0)
        out[["footnotes"]] <- c(out[["footnotes"]], sprintf(ngettext(length(info[["droppedLevels"]]),
          "A level of the moderator '%1$s' without cases in the analysed sample was dropped: %2$s.",
          "Levels of the moderator '%1$s' without cases in the analysed sample were dropped: %2$s."),
          name, paste(info[["droppedLevels"]], collapse = ", ")))
      if (nlevels(x) < 2) {
        out[["errors"]] <- c(out[["errors"]], gettextf(
          "The moderator '%s' has fewer than two levels with cases in the analysed sample.", name))
        next
      }
      stats::contrasts(x) <- stats::contr.treatment(levels(x))   # independent of options("contrasts")
      data[[variable]] <- x
      info[["levels"]] <- levels(x)
    }
    baseTerms <- c(baseTerms, variable)
    if (moderator[["square"]] || moderator[["cube"]]) {
      if (info[["class"]] == "continuous") {
        if (moderator[["square"]]) squareTerms <- c(squareTerms, variable)
        if (moderator[["cube"]])   cubeTerms   <- c(cubeTerms, variable)
      } else if (info[["class"]] == "binary") {
        out[["footnotes"]] <- c(out[["footnotes"]], gettextf(
          "The squared and cubic terms of the binary moderator '%s' were not included: they are collinear with its linear term.", name))
      } else {
        out[["footnotes"]] <- c(out[["footnotes"]], gettextf(
          "Squared and cubic terms are not defined for the nominal moderator '%s'; they were not included.", name))
      }
    }
    moderatorInfo[[variable]] <- info
  }
  if (length(out[["errors"]]) > 0)
    return(out)

  out[["formulaParts"]] <- list(base = baseTerms, square = squareTerms, cube = cubeTerms,
                                interactions = spec[["interactions"]])
  out[["formula"]]        <- .mnlfaFormula(out[["formulaParts"]], function(x) x)
  out[["data"]]           <- data
  out[["moderatorInfo"]]  <- moderatorInfo
  out[["categoryLevels"]] <- categoryLevels
  return(out)
}

# Backtick-quote a name that is not syntactic, for use in a formula. encodeString() escapes backslashes and backticks
# (and control characters), so the code parses back to the same name (also names with a backslash).
.mnlfaFormulaName <- function(x) {
  ifelse(make.names(x) == x, x, encodeString(x, quote = "`"))
}

# The moderator formula in this order: base moderators, squares, cubes, interactions.
# `rename` maps the internal (encoded) names to the names used in the formula.
.mnlfaFormulaText <- function(parts, rename) {
  q <- function(x) .mnlfaFormulaName(rename(x))
  terms <- c(q(parts[["base"]]),
             if (length(parts[["square"]])) paste0("I(", q(parts[["square"]]), "^2)"),
             if (length(parts[["cube"]]))   paste0("I(", q(parts[["cube"]]), "^3)"),
             vapply(parts[["interactions"]], function(pair) paste(q(pair), collapse = ":"), ""))
  return(paste("~", paste(terms, collapse = " + ")))
}

.mnlfaFormula <- function(parts, rename) {
  stats::as.formula(.mnlfaFormulaText(parts, rename), env = baseenv())
}
