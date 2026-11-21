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

# MNLFA with MoFIA: reading the options into a model specification.
# Pure functions without side effects; tests call them via jaspSem:::.

.mnlfaModelKeys <- c("invarianceTestConfigural", "invarianceTestMetric", "invarianceTestScalar",
                     "invarianceTestStrict", "invarianceTestCustom")

.mnlfaModelLabel <- function(key) {
  switch(key,
         invarianceTestConfigural = gettext("Configural"),
         invarianceTestMetric     = gettext("Metric"),
         invarianceTestScalar     = gettext("Scalar"),
         invarianceTestStrict     = gettext("Strict"),
         invarianceTestCustom     = gettext("Custom"),
         key)
}

.mnlfaParameterLabel <- function(parameter) {
  switch(parameter,
         loadings          = gettext("loadings"),
         intercepts        = gettext("intercepts"),
         residualVariances = gettext("residual variances"),
         variances         = gettext("factor variances"),
         means             = gettext("factor means"),
         covariances       = gettext("factor correlation"),
         parameter)
}

# A single string from an option value; option values can arrive list-wrapped.
.mnlfaScalar <- function(x) {
  x <- unlist(x, use.names = FALSE)
  if (length(x) != 1 || is.na(x))
    return(NA_character_)
  return(as.character(x))
}

# A single logical from a checkbox value; NA when it is not one.
.mnlfaFlag <- function(x) {
  x <- unlist(x, use.names = FALSE)
  if (length(x) != 1 || !is.logical(x))
    return(NA)
  return(x)
}

# The entries of a nested option list whose keyValue equals key.
.mnlfaEntriesWithKey <- function(entries, key) {
  Filter(function(entry) identical(.mnlfaScalar(entry[["keyValue"]]), key), entries)
}

#' Read the options into the model specification
#'
#' Names are kept encoded internally; `decoded` maps encoded indicator and moderator names to their
#' display names. `dataset` is not needed for reading the options; the data are checked in
#' `.mnlfaPrepareData()`. `decode` is a seam for unit tests of captured option trees.
#' @noRd
.mnlfaReadSpec <- function(options, dataset = NULL, decode = jaspBase::decodeColNames) {

  errors <- character(0)

  # ---- factors and indicators ----
  factors <- lapply(options[["factors"]], function(factor) {
    list(name       = .mnlfaScalar(factor[["name"]]),
         title      = .mnlfaScalar(factor[["title"]]),
         indicators = as.character(unlist(factor[["indicators"]], use.names = FALSE)),
         types      = as.character(unlist(factor[["indicators.types"]], use.names = FALSE)))
  })

  indicators <- character(0)
  indicatorTypes <- character(0)
  for (factor in factors) {
    if (length(factor[["types"]]) != length(factor[["indicators"]])) {
      errors <- c(errors, gettextf("The indicator types of factor '%s' do not match its indicators.", factor[["title"]]))
      next
    }
    for (k in seq_along(factor[["indicators"]])) {
      item <- factor[["indicators"]][k]
      type <- factor[["types"]][k]
      if (item %in% indicators) {
        if (indicatorTypes[[item]] != type)
          errors <- c(errors, gettextf("Indicator '%s' is assigned with different types to different factors.",
                                       decode(item)))
        next
      }
      indicators <- c(indicators, item)
      indicatorTypes[[item]] <- type
    }
  }
  badTypes <- indicators[!indicatorTypes[indicators] %in% c("scale", "ordinal")]
  for (item in badTypes)
    errors <- c(errors, gettextf("Indicator '%1$s' has type '%2$s'; only scale (continuous) and ordinal (categorical) indicators are supported.",
                                 decode(item), indicatorTypes[[item]]))

  continuous  <- indicators[indicatorTypes[indicators] == "scale"]
  categorical <- indicators[indicatorTypes[indicators] == "ordinal"]

  # ---- moderators ----
  moderatorRows  <- options[["moderators"]]
  moderatorTypes <- as.character(unlist(options[["moderators.types"]], use.names = FALSE))
  if (length(moderatorTypes) != length(moderatorRows)) {
    errors <- c(errors, gettext("The moderator types do not match the moderators."))
    moderatorTypes <- rep(NA_character_, length(moderatorRows))
  }
  moderators <- lapply(seq_along(moderatorRows), function(k) {
    row <- moderatorRows[[k]]
    list(variable = .mnlfaScalar(row[["variable"]]),
         type     = moderatorTypes[k],
         square   = .mnlfaFlag(row[["moderatorSquaredEffect"]]),
         cube     = .mnlfaFlag(row[["moderatorCubicEffect"]]))
  })
  moderatorVariables <- vapply(moderators, `[[`, "", "variable")
  for (moderator in moderators) {
    if (is.na(moderator[["type"]]) || !moderator[["type"]] %in% c("scale", "nominal"))
      errors <- c(errors, gettextf("Moderator '%s' must be a scale or nominal variable.", decode(moderator[["variable"]])))
    if (is.na(moderator[["square"]]) || is.na(moderator[["cube"]]))
      errors <- c(errors, gettextf("The squared/cubic settings of moderator '%s' are invalid.", decode(moderator[["variable"]])))
  }
  for (item in intersect(indicators, moderatorVariables))
    errors <- c(errors, gettextf("Variable '%s' cannot be both an indicator and a moderator.", decode(item)))

  allNames <- unique(c(indicators, moderatorVariables))
  decoded  <- stats::setNames(as.character(decode(allNames)), allNames)

  # ---- interactions: "A:B" resolved structurally against pairs of assigned moderators ----
  interactions <- list()
  for (row in options[["moderatorInteractionTerms"]]) {
    include <- .mnlfaFlag(row[["moderatorInteractionTermsInclude"]])
    value   <- .mnlfaScalar(row[["value"]])
    if (is.na(include)) {
      errors <- c(errors, gettextf("The setting of interaction term '%s' is invalid.", value))
      next
    }
    if (!include)
      next
    pair <- .mnlfaResolveInteraction(value, moderatorVariables, decoded)
    if (is.null(pair)) {
      errors <- c(errors, gettextf("The interaction term '%s' does not refer to two assigned moderators.", value))
    } else if (identical(pair, "ambiguous")) {
      errors <- c(errors, gettextf("The interaction term '%s' is ambiguous: it can refer to more than one pair of moderators. Rename a moderator whose name contains ':'.", value))
    } else if (any(vapply(interactions, identical, TRUE, pair))) {
      errors <- c(errors, gettextf("The interaction term '%s' is included more than once.", value))
    } else {
      interactions[[length(interactions) + 1]] <- pair
    }
  }

  spec <- list(
    factors            = factors,
    factorNames        = vapply(factors, `[[`, "", "name"),
    factorTitles       = vapply(factors, `[[`, "", "title"),
    indicators         = indicators,
    indicatorTypes     = indicatorTypes,
    continuous         = continuous,
    categorical        = categorical,
    moderators         = moderators,
    moderatorVariables = moderatorVariables,
    interactions       = interactions,
    decoded            = decoded,
    link               = options[["categoricalIndicatorLink"]],
    seChoice           = options[["errorCalculationMethod"]],
    preprocessing      = options[["indicatorPreprocessing"]],
    errors             = errors
  )
  if (is.null(spec[["link"]]) || !spec[["link"]] %in% c("logit", "probit"))
    spec[["errors"]] <- c(spec[["errors"]], gettext("The categorical indicator link must be logit or probit."))
  if (is.null(spec[["seChoice"]]) ||
      !spec[["seChoice"]] %in% c("robustHuberWhite", "robustHuberWhiteReducedInformation", "standard"))
    spec[["errors"]] <- c(spec[["errors"]], gettext("The standard error choice is invalid."))
  if (is.null(spec[["preprocessing"]]) || !spec[["preprocessing"]] %in% c("none", "center", "standardize"))
    spec[["errors"]] <- c(spec[["errors"]], gettext("The indicator preprocessing choice is invalid."))

  # ---- per enabled model: the checkbox settings, read by name ----
  enabled <- .mnlfaModelKeys[vapply(.mnlfaModelKeys, function(key) identical(options[[key]], TRUE), TRUE)]
  tree    <- options[["includeIndividualModerationsList"]]
  spec[["models"]] <- stats::setNames(lapply(enabled, function(key) {
    entries <- .mnlfaEntriesWithKey(tree, key)
    if (length(entries) != 1) {
      model <- .mnlfaEmptyModelSpec(key)
      model[["errors"]] <- if (length(entries) == 0)
        gettextf("Model %s: its moderation settings are missing.", model[["label"]])
      else
        gettextf("Model %s: its moderation settings occur more than once.", model[["label"]])
      return(model)
    }
    return(.mnlfaReadModelTree(entries[[1]], key, spec))
  }), enabled)

  return(spec)
}

.mnlfaEmptyModelSpec <- function(key) {
  list(key = key, label = .mnlfaModelLabel(key), errors = character(0), footnotes = character(0),
       loadings = character(0), intercepts = character(0), residualVariances = character(0),
       variances = character(0), means = character(0), covariances = FALSE, ignoredResiduals = character(0))
}

# "A:B" against all pairs of assigned moderators (encoded or decoded names). NULL if no pair matches,
# "ambiguous" if more than one pair does (names containing ':'); never the first match.
.mnlfaResolveInteraction <- function(value, moderatorVariables, decoded) {
  if (is.na(value) || length(moderatorVariables) < 2)
    return(NULL)
  matches <- list()
  for (i in seq_len(length(moderatorVariables) - 1)) {
    for (j in seq(i + 1, length(moderatorVariables))) {
      a <- moderatorVariables[i]
      b <- moderatorVariables[j]
      if (value %in% c(paste(a, b, sep = ":"), paste(decoded[[a]], decoded[[b]], sep = ":")))
        matches[[length(matches) + 1]] <- c(a, b)
    }
  }
  if (length(matches) == 0)
    return(NULL)
  if (length(matches) > 1)
    return("ambiguous")
  return(matches[[1]])
}

#' Resolve an item-row value to an assigned indicator: exact assigned (encoded)
#' name, exact decoded name, then a recognized type suffix on a decoded name. NA if unresolved.
#' @noRd
.mnlfaResolveIndicator <- function(value, indicators, decoded) {
  if (is.na(value))
    return(NA_character_)
  if (value %in% indicators)
    return(value)
  decodedIndicators <- decoded[indicators]
  hit <- indicators[decodedIndicators == value]
  if (length(hit) == 1)
    return(hit)
  parts <- regmatches(value, regexec("^(.*)\\.(scale|ordinal|nominal)$", value))[[1]]
  if (length(parts) == 3) {
    hit <- indicators[decodedIndicators == parts[2]]
    if (length(hit) == 1)
      return(hit)
  }
  return(NA_character_)
}

# A factor-row value against the factor names, then the titles.
.mnlfaResolveFactor <- function(value, factorNames, factorTitles) {
  if (is.na(value))
    return(NA_character_)
  if (sum(factorNames == value) == 1)
    return(value)
  hit <- factorNames[factorTitles == value]
  if (length(hit) == 1)
    return(hit)
  return(NA_character_)
}

.mnlfaReadModelTree <- function(entry, key, spec) {

  model    <- .mnlfaEmptyModelSpec(key)
  label    <- model[["label"]]
  nFactors <- length(spec[["factors"]])
  expected <- list(indicators = c("loadings", "intercepts", "residualVariances"),
                   factors    = c("variances", "means", if (nFactors == 2) "covariances"))
  addError <- function(message) model[["errors"]] <<- c(model[["errors"]], message)

  typeEntries <- entry[["moderationTypeList"]]
  typeKeys    <- vapply(typeEntries, function(x) .mnlfaScalar(x[["keyValue"]]), "")
  for (typeKey in setdiff(typeKeys, names(expected)))
    addError(gettextf("Model %1$s: unknown moderation settings '%2$s'.", label, typeKey))

  for (typeKey in names(expected)) {
    typeHits <- typeEntries[typeKeys %in% typeKey]
    if (length(typeHits) != 1) {
      addError(gettextf("Model %1$s: the %2$s settings are missing or occur more than once.", label,
                        if (typeKey == "indicators") gettext("indicator") else gettext("factor")))
      next
    }
    parameterEntries <- typeHits[[1]][["moderationParameterList"]]
    parameterKeys    <- vapply(parameterEntries, function(x) .mnlfaScalar(x[["keyValue"]]), "")

    for (k in seq_along(parameterEntries)) {
      parameter <- parameterKeys[k]
      if (parameter %in% expected[[typeKey]])
        next
      rows <- parameterEntries[[k]][["moderationItemList"]]
      if (typeKey == "factors" && identical(parameter, "covariances")) {
        # The form sends no correlation rows unless there are exactly two factors.
        if (length(rows) > 0)
          addError(gettextf("Model %s: a factor correlation setting exists, but correlations can only be moderated with exactly two factors.", label))
        next
      }
      addError(gettextf("Model %1$s: unknown moderation settings '%2$s'.", label, parameter))
    }

    for (parameter in expected[[typeKey]]) {
      hits <- parameterEntries[parameterKeys %in% parameter]
      if (length(hits) != 1) {
        addError(gettextf("Model %1$s: the settings for the %2$s are missing or occur more than once.", label,
                          .mnlfaParameterLabel(parameter)))
        next
      }
      rows <- hits[[1]][["moderationItemList"]]
      result <- if (typeKey == "indicators")
        .mnlfaReadIndicatorRows(rows, parameter, label, spec)
      else if (parameter == "covariances")
        .mnlfaReadCovarianceRows(rows, label, spec)
      else
        .mnlfaReadFactorRows(rows, parameter, label, spec)
      model[["errors"]] <- c(model[["errors"]], result[["errors"]])
      model[[parameter]] <- result[["checked"]]
      if (parameter == "residualVariances") {
        model[["ignoredResiduals"]] <- result[["ignored"]]
        for (item in result[["ignored"]])
          model[["footnotes"]] <- c(model[["footnotes"]], gettextf(
            "Residual variances are not defined for categorical indicators; the setting for %s was ignored.",
            spec[["decoded"]][[item]]))
      }
    }
  }
  return(model)
}

# Indicator rows: each eligible indicator exactly once; only stale residual-variance rows of
# assigned categorical indicators are tolerated (unchecked: no effect; checked: footnote).
.mnlfaReadIndicatorRows <- function(rows, parameter, label, spec) {
  errors   <- character(0)
  eligible <- if (parameter == "residualVariances") spec[["continuous"]] else spec[["indicators"]]
  parLabel <- .mnlfaParameterLabel(parameter)
  seen     <- character(0)
  checked  <- character(0)
  ignored  <- character(0)

  for (row in rows) {
    value <- .mnlfaScalar(row[["value"]])
    flag  <- .mnlfaFlag(row[["includeIndividualModeration"]])
    item  <- .mnlfaResolveIndicator(value, spec[["indicators"]], spec[["decoded"]])
    if (is.na(item)) {
      errors <- c(errors, gettextf("Model %1$s: the %2$s setting '%3$s' refers to an indicator that is not assigned to any factor.",
                                   label, parLabel, value))
      next
    }
    name <- spec[["decoded"]][[item]]
    if (item %in% seen) {
      errors <- c(errors, gettextf("Model %1$s: the %2$s settings contain '%3$s' more than once.", label, parLabel, name))
      next
    }
    seen <- c(seen, item)
    if (is.na(flag)) {
      errors <- c(errors, gettextf("Model %1$s: the %2$s setting of '%3$s' is not a checkbox value.", label, parLabel, name))
      next
    }
    if (!item %in% eligible) {
      if (parameter == "residualVariances" && item %in% spec[["categorical"]]) {
        if (flag)
          ignored <- c(ignored, item)
      } else {
        errors <- c(errors, gettextf("Model %1$s: the %2$s settings contain '%3$s', which is not allowed there.",
                                     label, parLabel, name))
      }
      next
    }
    if (flag)
      checked <- c(checked, item)
  }
  for (item in setdiff(eligible, seen))
    errors <- c(errors, gettextf("Model %1$s: the %2$s settings have no row for indicator '%3$s'. Settings are never filled in from defaults. Remove '%3$s' from its factor and assign it again to rebuild the settings.",
                                 label, parLabel, spec[["decoded"]][[item]]))

  order <- spec[["indicators"]]
  return(list(errors = errors, checked = order[order %in% checked], ignored = order[order %in% ignored]))
}

.mnlfaReadFactorRows <- function(rows, parameter, label, spec) {
  errors   <- character(0)
  parLabel <- .mnlfaParameterLabel(parameter)
  seen     <- character(0)
  checked  <- character(0)
  for (row in rows) {
    value  <- .mnlfaScalar(row[["value"]])
    flag   <- .mnlfaFlag(row[["includeIndividualModeration"]])
    factor <- .mnlfaResolveFactor(value, spec[["factorNames"]], spec[["factorTitles"]])
    if (is.na(factor)) {
      errors <- c(errors, gettextf("Model %1$s: the %2$s setting '%3$s' does not refer to a current factor.",
                                   label, parLabel, value))
      next
    }
    title <- spec[["factorTitles"]][spec[["factorNames"]] == factor]
    if (factor %in% seen) {
      errors <- c(errors, gettextf("Model %1$s: the %2$s settings contain '%3$s' more than once.", label, parLabel, title))
      next
    }
    seen <- c(seen, factor)
    if (is.na(flag)) {
      errors <- c(errors, gettextf("Model %1$s: the %2$s setting of '%3$s' is not a checkbox value.", label, parLabel, title))
      next
    }
    if (flag)
      checked <- c(checked, factor)
  }
  for (factor in setdiff(spec[["factorNames"]], seen))
    errors <- c(errors, gettextf("Model %1$s: the %2$s settings have no row for factor '%3$s'. Settings are never filled in from defaults.",
                                 label, parLabel, spec[["factorTitles"]][spec[["factorNames"]] == factor]))
  order <- spec[["factorNames"]]
  return(list(errors = errors, checked = order[order %in% checked]))
}

# The single correlation row of a two-factor model. The form keys it by the factor names ("Factor1:Factor2"), which a
# rename never changes; the title form "Factor 1:Factor 2" of files saved before is
# still accepted when it matches the current titles. Anything else (e.g. a stale title after a rename) is an error.
.mnlfaReadCovarianceRows <- function(rows, label, spec) {
  byName  <- paste(spec[["factorNames"]], collapse = ":")
  byTitle <- paste(spec[["factorTitles"]], collapse = ":")
  if (length(rows) != 1)
    return(list(errors = gettextf("Model %1$s: there must be exactly one factor correlation setting ('%2$s').", label, byTitle),
                checked = FALSE))
  value <- .mnlfaScalar(rows[[1]][["value"]])
  flag  <- .mnlfaFlag(rows[[1]][["includeIndividualModeration"]])
  if (is.na(value) || !(value %in% c(byName, byTitle)))
    return(list(errors = gettextf("Model %1$s: the factor correlation setting refers to '%2$s', which matches neither the current factor names ('%3$s') nor the current factor titles ('%4$s').",
                                  label, value, byName, byTitle),
                checked = FALSE))
  if (is.na(flag))
    return(list(errors = gettextf("Model %s: the factor correlation setting is not a checkbox value.", label), checked = FALSE))
  return(list(errors = character(0), checked = flag))
}
