# Copyright (C) 2013-2019 University of Amsterdam
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



ModeratedNonLinearFactorAnalysisInternal <- function(jaspResults, dataset, options, ...) {

  nIndicators  <- length(unlist(lapply(options[["factors"]], `[[`, "indicators"), use.names = FALSE))
  nModerators  <- length(options[["moderators"]])
  hasModelSpec <- nIndicators > 1 && nModerators > 0
  runAnalysis  <- options[[ "syncAnalysisBox" ]]

  ready               <- hasModelSpec
  readyForFitPerGroup <- hasModelSpec

  # Every output comes from MoFIA; the OpenMx/mxsem code is removed. Legacy
  # output and states of an analysis saved with an older build are removed.
  .mnlfaRemoveLegacyOutput(jaspResults)

  if (!hasModelSpec) {

    # An empty MoFIA fit table replaces the legacy empty "Global Invariance Fit" table.
    .mnlfaEmptyFitTable(jaspResults)

    syncText <- createJaspHtml(
      text = gettext("Specify both factor indicator variables and moderator variables to run the analysis.")
    )
    jaspResults[["syncText"]] <- syncText
    syncText$dependOn(c("factors", "moderators"))
    syncText$position <- 0.01
    .mnlfaMofiaScoreColumns(jaspResults, NULL, options)    # data columns last (see .mnlfaMofiaScoreColumns)
    return()
  }

  # The "check sync" message is shown whenever Sync is off and no MoFIA output of an earlier synchronized
  # run is still valid; valid earlier output stays visible without it.
  if (!runAnalysis && is.null(jaspResults[["mofiaContainer"]])) {
    syncText <- createJaspHtml(
      text = gettext("Check the 'Sync Analysis' box to run the analysis.")
    )
    jaspResults[["syncText"]] <- syncText
    syncText$dependOn("syncAnalysisBox")
    syncText$position <- 0.01
  }

  # The plot rows of the form list what the MoFIA models moderate.
  .mnlfaMofiaPlotOptionsForQml(jaspResults, dataset, options)

  # MoFIA reads the raw typed dataset, before the legacy preprocessing below converts every column to
  # numeric. Models are fitted only with Sync on.
  mofia <- .mnlfaMofiaAnalysis(jaspResults, dataset, options, fitModels = runAnalysis)

  # The fit per group (lavaan) keeps the legacy data handling; ordinal indicators are declared as ordered.
  if (.mnlfaHasCategoricalIndicators(options)) {
    .mnlfaOrderedGroupFit(jaspResults, dataset, options, readyForFitPerGroup)
  } else {
    groupData    <- .mnlfaHandleData(jaspResults, dataset, options, ready)
    groupOptions <- .mnlfaPreprocessOptions(options)

    .mnlfaCreateContainer(jaspResults, groupOptions)

    .mnlfaFitPerGroup(jaspResults, groupData, groupOptions, readyForFitPerGroup)
    .mnlfaFitPerGroupTable(jaspResults, groupData, groupOptions, readyForFitPerGroup)

    .mnlfaAddGroupingVariableToData(jaspResults, groupData, groupOptions)
  }

  # The factor-score columns are written or removed last: a column change refreshes JASP's column-name encoding during
  # the run (observed in JASP Desktop 0.98.1; see .mnlfaMofiaScoreColumns).
  .mnlfaMofiaScoreColumns(jaspResults, mofia, options)
  return()
}

# The legacy fit table (and its warnings block), e.g. from an analysis saved with an older build.
.mnlfaRemoveLegacyFitTable <- function(jaspResults) {
  if (!is.null(jaspResults[["fitContainer"]]))
    jaspResults[["fitContainer"]] <- NULL
  return()
}

# The legacy parameter tables (e.g. from an analysis saved with an older build).
.mnlfaRemoveLegacyParameterTables <- function(jaspResults) {
  mainContainer <- jaspResults[["mainContainer"]]
  if (!is.null(mainContainer) && !is.null(mainContainer[["globalParameterContainer"]]))
    mainContainer[["globalParameterContainer"]] <- NULL
  return()
}

# Legacy output and states of an older build (released module 0.97.1 and earlier, development builds of this rework):
# the fit table, parameter tables, plots and syntax, the plot-row source built from the legacy summary, the notice for
# categorical data and the legacy model and fit states. The fit-per-group state is kept.
.mnlfaRemoveLegacyOutput <- function(jaspResults) {
  .mnlfaRemoveLegacyFitTable(jaspResults)
  .mnlfaRemoveLegacyParameterTables(jaspResults)
  for (key in c("plotContainer", "syntaxContainer", "plotOptionsForQml", "legacyUnavailable"))
    if (!is.null(jaspResults[[key]]))
      jaspResults[[key]] <- NULL
  mainContainer <- jaspResults[["mainContainer"]]
  if (!is.null(mainContainer))
    for (key in paste0(rep(.mnlfaModelKeys, each = 2), c("State", "ModelState")))
      if (!is.null(mainContainer[[key]]))
        mainContainer[[key]] <- NULL
  return()
}

##### PREPROCESSING #####

# handle the data add interactions and higher power effects to the data
.mnlfaHandleData <- function(jaspResults, dataset, options, ready) {

  if (!is.null(jaspResults[["dataState"]])) {
    return(jaspResults[["dataState"]][["object"]])
  }

  if (!ready) return(dataset)

  # convert the whole data to numeric
  dataset <- as.data.frame(lapply(dataset, function(x) as.numeric(as.character(x))))

  # optionally center or z-standardize the indicator variables
  if (!is.null(options[["indicatorPreprocessing"]]) && options[["indicatorPreprocessing"]] != "none") {
    indicators <- unlist(lapply(options[["factors"]], `[[`, "indicators"), use.names = FALSE)
    indicators <- unique(indicators)
    indicators <- indicators[nzchar(indicators) & indicators %in% colnames(dataset)]
    for (v in indicators) {
      x <- as.numeric(dataset[[v]])
      dataset[[v]] <- switch(options[["indicatorPreprocessing"]],
                             center      = x - mean(x, na.rm = TRUE),
                             standardize = as.numeric(scale(x)),
                             x)
    }
  }

  # scale the continuous moderators
  mods <- unlist(lapply(options[["moderators"]], `[[`, "variable"), use.names = FALSE)
  mods.types <- options[["moderators.types"]]
  for (i in 1:length(mods)) {
    if (mods.types[i] != "nominal") {
      # this also forces also ordinal moderator variables to be continuous, do we want this???????????????????????????????????
      dataset[[mods[i]]] <- scale(as.numeric(as.character(dataset[[mods[i]]])))
    }
  }

  # add interactions and extra effects (pairs resolved by whole moderator names, .mnlfaGroupInteractions)
  groupInteractions <- .mnlfaGroupInteractions(options)
  for (i in seq_along(groupInteractions[["pairs"]])) {
    pair  <- groupInteractions[["pairs"]][[i]]
    tmp1  <- as.numeric(as.character(dataset[[pair[1]]])) # needed for nominal moderators
    tmp2  <- as.numeric(as.character(dataset[[pair[2]]]))
    tmpDt <- data.frame(tmp1 * tmp2)
    colnames(tmpDt) <- groupInteractions[["names"]][i]
    dataset <- cbind(dataset, tmpDt)
  }

  # add squares and cubic effects
  squares <- sapply(options[["moderators"]], function(x) x[["moderatorSquaredEffect"]])
  if (sum(squares) > 0) {
    squaredMods <- mods[squares]
    squaredTypes <- mods.types[squares]
    for (i in 1:length(squaredMods)) {
      tmp <- as.numeric(as.character(dataset[[squaredMods[i]]]))
      tmpDt <- data.frame(tmp^2)
      squaredModsName <- jaspBase::decodeColNames(squaredMods[i])
      tmpName <- paste0(squaredModsName, "_squared")
      colnames(tmpDt) <- tmpName
      dataset <- cbind(dataset, tmpDt)
    }
  }
  cubics <- sapply(options[["moderators"]], function(x) x[["moderatorCubicEffect"]])
  if (sum(cubics) > 0) {
    cubicMods <- mods[cubics]
    cubicTypes <- mods.types[cubics]
    for (i in 1:length(cubicMods)) {
      tmp <- as.numeric(as.character(dataset[[cubicMods[i]]]))
      tmpDt <- data.frame(tmp^3)
      cubicModsName <- jaspBase::decodeColNames(cubicMods[i])
      tmpName <- paste0(cubicModsName, "_cubic")
      colnames(tmpDt) <- tmpName
      dataset <- cbind(dataset, tmpDt)
    }
  }

  dataState <- dataset
  dataState <- createJaspState(dataState)
  dataState$dependOn(options = c("factors", "moderators", "moderatorInteractionTerms", "moderatorInteractionTermsInclude",
                                 "moderatorSquaredEffect", "moderatorCubicEffect", "indicatorPreprocessing"))
  jaspResults[["dataState"]] <- dataState

  return(dataset)
}

# check for empty factors and add interactions and non-linear effects to the options
.mnlfaPreprocessOptions <- function(options) {

  cleanedFactors <- lapply(options[["factors"]], function(x) {
    if (length(x[["indicators"]]) == 0) {
      return(NULL)
    } else {
      return(x)
    }
  })

  options[["factors"]] <- cleanedFactors[!sapply(cleanedFactors, is.null)]

  # scale the continuous moderators
  mods <- unlist(lapply(options[["moderators"]], `[[`, "variable"), use.names = FALSE)
  if (length(mods) == 0) return(options)
  mods.types <- options[["moderators.types"]]

  # add interactions and extra effects (pairs resolved by whole moderator names, .mnlfaGroupInteractions); a term that
  # cannot be resolved stops the fit per group with its message (options[["groupInteractionErrors"]])
  groupInteractions <- .mnlfaGroupInteractions(options)
  options[["groupInteractionErrors"]] <- groupInteractions[["errors"]]
  for (i in seq_along(groupInteractions[["pairs"]])) {
    interTypes <- mods.types[match(groupInteractions[["pairs"]][[i]], mods)]
    derived    <- list(moderatorCubicEffect = FALSE, moderatorSquaredEffect = FALSE,
                       variable = groupInteractions[["names"]][i])
    options[["moderators"]] <- append(options[["moderators"]], list(derived))
    if (sum(interTypes == "scale") > 0)
      options[["moderators.types"]] <- append(options[["moderators.types"]], "scale")
    else
      options[["moderators.types"]] <- append(options[["moderators.types"]], "nominal")
  }

  # add squares and cubic effects
  squares <- sapply(options[["moderators"]], function(x) x[["moderatorSquaredEffect"]])
  if (sum(squares) > 0) {
    squaredMods <- mods[squares]
    squaredTypes <- mods.types[squares]
    for (i in 1:length(squaredMods)) {
      squaredModsName <- jaspBase::decodeColNames(squaredMods[i])
      # check that none of the variables have already "squared" in their name
      if (grepl("_squared$", squaredModsName)) {
        .quitAnalysis(gettextf("The moderator variable '%s' already has '_squared' in its name. Please rename the variable before proceeding.", squaredModsName))
      }

      tmpName <- paste0(squaredModsName, "_squared")
      options[["moderators"]] <- append(options[["moderators"]], list(list(moderatorCubicEffect = FALSE, moderatorSquaredEffect = FALSE,
                                                                           variable = tmpName)))
      options[["moderators.types"]] <- append(options[["moderators.types"]], squaredTypes[i])
    }
  }
  cubics <- sapply(options[["moderators"]], function(x) x[["moderatorCubicEffect"]])
  if (sum(cubics) > 0) {
    cubicMods <- mods[cubics]
    cubicTypes <- mods.types[cubics]
    for (i in 1:length(cubicMods)) {
      cubicModsName <- jaspBase::decodeColNames(cubicMods[i])
      if (grepl("_cubic$", cubicModsName)) {
        .quitAnalysis(gettextf("The moderator variable '%s' already has '_cubic' in its name. Please rename the variable before proceeding.", cubicModsName))
      }
      tmpName <- paste0(cubicModsName, "_cubic")
      options[["moderators"]] <- append(options[["moderators"]], list(list(moderatorCubicEffect = FALSE, moderatorSquaredEffect = FALSE,
                                                                           variable = tmpName)))
      options[["moderators.types"]] <- append(options[["moderators.types"]], cubicTypes[i])
    }
  }

  return(options)
}

# The included interaction terms of the fit per group, resolved against pairs of assigned moderators by their whole
# names as the MoFIA specification resolves them (.mnlfaResolveInteraction): a term is never split at its ":"
# characters, so moderator names that contain ":" work. Returns the pairs (encoded names), the
# name of the derived column of each pair (as before: "<A>_x_<B>" with decoded names) and, for a term that refers to
# no pair, to several pairs or to a pair already included, the translated message of the MoFIA specification.
.mnlfaGroupInteractions <- function(options) {
  mods    <- unlist(lapply(options[["moderators"]], `[[`, "variable"), use.names = FALSE)
  decoded <- stats::setNames(as.character(jaspBase::decodeColNames(mods)), mods)
  result  <- list(pairs = list(), names = character(0), errors = character(0))
  fail    <- function(message) result[["errors"]] <<- c(result[["errors"]], message)
  for (row in options[["moderatorInteractionTerms"]]) {
    include <- .mnlfaFlag(row[["moderatorInteractionTermsInclude"]])
    value   <- .mnlfaScalar(row[["value"]])
    if (is.na(include)) {
      fail(gettextf("The setting of interaction term '%s' is invalid.", value))
      next
    }
    if (!include)
      next
    pair <- .mnlfaResolveInteraction(value, mods, decoded)
    if (is.null(pair)) {
      fail(gettextf("The interaction term '%s' does not refer to two assigned moderators.", value))
    } else if (identical(pair, "ambiguous")) {
      fail(gettextf("The interaction term '%s' is ambiguous: it can refer to more than one pair of moderators. Rename a moderator whose name contains ':'.", value))
    } else if (any(vapply(result[["pairs"]], identical, TRUE, pair))) {
      fail(gettextf("The interaction term '%s' is included more than once.", value))
    } else {
      result[["pairs"]][[length(result[["pairs"]]) + 1]] <- pair
      result[["names"]] <- c(result[["names"]], paste0(decoded[[pair[1]]], "_x_", decoded[[pair[2]]]))
    }
  }
  return(result)
}

# Create a main container for everything
.mnlfaCreateContainer <- function(jaspResults, options) {
  if (is.null(jaspResults[["mainContainer"]])) {
    jaspResults[["mainContainer"]] <- createJaspContainer()
    jaspResults[["mainContainer"]]$dependOn(options = c(
      "factors", "moderators", "moderatorInteractionTerms", "moderatorInteractionTermsInclude",
      "moderatorSquaredEffect", "moderatorCubicEffect",
      "indicatorPreprocessing"))
    jaspResults[["mainContainer"]]$position <- 3
  }
  return()
}

##### ANALYSIS #####
# test configural invariance with the CFA module
.mnlfaFitPerGroup <- function(jaspResults, dataset, options, ready, ordered = character(0)) {

  if (!ready) return()
  if (!is.null(jaspResults[["mainContainer"]][["checkModelFitPerGroupState"]])) return()
  if (!options[["checkModelFitPerGroup"]]) return()
  if (length(options[["groupInteractionErrors"]]) > 0) return()    # the table shows why (.mnlfaFitPerGroupTable)

  moderatorNames <- sapply(options[["moderators"]], function(x) x[["variable"]])
  moderatorTypes <- options[["moderators.types"]]

  nominalModerators <- jaspBase::decodeColNames(moderatorNames[moderatorTypes == "nominal"])
  nonNominalModerators <- moderatorNames[moderatorTypes != "nominal"]

  dataTmp <- dataset
  if (length(nonNominalModerators) > 0) {
    nSplits <- options[["splitContinuousVariablesIntoGroups"]]
    for (i in 1:length(nonNominalModerators)) {
      tmp <- as.numeric(as.character(dataTmp[[nonNominalModerators[i]]])) # in cases where the variable is ordinal
      modRange <- range(tmp)
      modSplits <- seq(modRange[1], modRange[2], length.out = nSplits + 1)
      dataTmp[[paste0(nonNominalModerators[i], "_nominal")]] <- cut(tmp,
                                                                   breaks = modSplits,
                                                                   labels = as.character(1:nSplits),
                                                                   include.lowest = TRUE)
    }
    nonNominalModerators <- jaspBase::decodeColNames(nonNominalModerators)
    colsToCombine <- c(nominalModerators, paste0(nonNominalModerators, "_nominal"))
  } else {
    colsToCombine <- nominalModerators
  }

  ogColnames <- colnames(dataTmp)
  colnames(dataTmp) <- jaspBase::decodeColNames(ogColnames)
  dataTmp[["addedGroupVar"]] <- NA
  for (rr in 1:nrow(dataTmp)) {
    dataTmp[["addedGroupVar"]][rr] <- paste0(colsToCombine, "_", dataTmp[rr, colsToCombine], collapse = ":")
  }

  colnames(dataTmp) <- c(ogColnames, "addedGroupVar")

  # this part is borrowed from jaspFactor
  cfaResult <- list()
  options[["seType"]] <- "default"
  cfaResult[["spec"]] <- .cfaCalcSpecs(dataTmp, options)

  # we fit a model per group so we have access to all the fit indices, which we would not have if we would use
  # the lavaan built-in group functionality
  options[["group"]] <- ""
  options[["invarianceTesting"]] <- NULL
  mod <- .optionsToCFAMod(options, dataTmp, cfaResult)[["model"]]
  groups <- unique(dataTmp[["addedGroupVar"]])
  fitArgs <- list(model         = mod,
                data            = NULL,
                se              = cfaResult[["spec"]][["se"]],
                std.lv          = FALSE,
                orthogonal      = FALSE,
                missing         = "default")
  # Ordinal indicators are declared as ordered; their columns keep their categorical type.
  if (length(ordered) > 0)
    fitArgs$ordered <- ordered

  result <- list()
  for (i in 1:length(groups)) {
    fitArgs$data <- dataTmp[dataTmp$addedGroupVar == groups[i], ]
    fit <- try(do.call(lavaan::cfa, fitArgs))
    result[[groups[i]]] <- fit
  }

  fitPerGroupResult <- createJaspState(list(result = result, addedGroupVar = dataTmp[["addedGroupVar"]]))
  fitPerGroupResult$dependOn(options = c("splitContinuousVariablesIntoGroups", "checkModelFitPerGroup"))
  jaspResults[["mainContainer"]][["checkModelFitPerGroupState"]] <- fitPerGroupResult

  return()
}

##### OUTPUT #####
.mnlfaFitPerGroupTable <- function(jaspResults, dataset, options, ready, ordered = character(0)) {

  if (!is.null(jaspResults[["groupContainer"]][["checkModelFitPerGroupTable"]])) return()
  if (!ready) return()
  if (!options[["checkModelFitPerGroup"]]) return()

  fitPerGroupTable <- createJaspTable(gettext("Fit per Group Test"))
  fitPerGroupTable$info <- gettext("CFA fit statistics computed separately for each group defined by the moderator. This checks whether the baseline factor model fits adequately within each group before testing invariance.")
  fitPerGroupTable$addColumnInfo(name = "model", title = gettext("Group Model"), type = "string")

  groupContainer <- createJaspContainer()
  groupContainer$position <- 1
  # The group fits live in mainContainer and are recomputed when its options (factors, indicator types,
  # moderators, ...) change; the table must follow them, not only the state's own options.
  # checkModelFitPerGroup is also named directly: with an unresolvable interaction term no state exists to supply it,
  # and the error table must go when the check is switched off.
  groupContainer$dependOn(optionsFromObject = list(jaspResults[["mainContainer"]],
                                                   jaspResults[["mainContainer"]][["checkModelFitPerGroupState"]]),
                          options           = "checkModelFitPerGroup")
  jaspResults[["groupContainer"]] <- groupContainer
  jaspResults[["groupContainer"]][["checkModelFitPerGroupTable"]] <- fitPerGroupTable

  # An included interaction term that the specification cannot resolve: no groups are formed.
  if (length(options[["groupInteractionErrors"]]) > 0) {
    fitPerGroupTable$setError(paste(options[["groupInteractionErrors"]], collapse = " "))
    return()
  }

  result <- jaspResults[["mainContainer"]][["checkModelFitPerGroupState"]][["object"]][["result"]]
  if (any(unlist(lapply(result, isTryError)))) {
    errs <- which(unlist(lapply(result, isTryError)))
    errmsg <- ""
    for (i in errs) {
      errmsg <- paste(errmsg, jaspBase::.extractErrorMessage(result[[i]]))
    }
    fitPerGroupTable$setError(gettextf("Error in fit per group test. Internal error message(s): %s", errmsg))
    return()
  }

  fitPerGroupTable$addColumnInfo(name = "N",      title = gettext("N"),     type = "integer")
  fitPerGroupTable$addColumnInfo(name = "chisq",  title = "\u03a7\u00b2",   type = "number")
  fitPerGroupTable$addColumnInfo(name = "df",     title = gettext("df"),    type = "integer")
  fitPerGroupTable$addColumnInfo(name = "pvalue", title = gettext("p"),     type = "pvalue")
  fitPerGroupTable$addColumnInfo(name = "rmsea",  title = gettext("RMSEA"), type = "number")
  fitPerGroupTable$addColumnInfo(name = "cfi",    title = gettext("CFI"),   type = "number")
  fitPerGroupTable$addColumnInfo(name = "srmr",   title = gettext("SRMR"),  type = "number")

  # The statistics stay as computed for continuous indicators; with ordinal indicators a footnote explains their
  # meaning.
  if (length(ordered) > 0)
    fitPerGroupTable$addFootnote(gettext("With ordinal indicators, lavaan estimates the model by WLSMV; \u03c7\u00b2 is its unscaled (DWLS) statistic, for which no p-value is computed, and RMSEA and CFI are based on it."))

  fillData <- data.frame(model = names(result))
  groupN <- unlist(lapply(result, function(x) x@SampleStats@nobs))
  fillData$N <- groupN
  fitPerGroupTable$setData(fillData)

  if (any(!unlist(lapply(result, function(x) x@optim$converged)))) {
    nonConv <- which(!unlist(lapply(result, function(x) x@optim$converged)))
    for (i in nonConv) {
      fitPerGroupTable$addFootnote(gettextf("The model for group %s did not converge.", names(result)[i]))
    }
    return()
  }

  chisqs <- unlist(lapply(result, function(x) x@test[[1]]$stat.group))
  dfs <- unlist(lapply(result, function(x) x@test[[1]]$df))
  pvalues <- unlist(lapply(result, function(x) x@test[[1]]$pvalue))
  # The footnote above says that no p-value is computed for the unscaled DWLS test; keep it so whatever the
  # lavaan version reports.
  if (length(ordered) > 0)
    pvalues <- rep(NA_real_, length(chisqs))
  fits <- sapply(result, function(x) lavaan::fitmeasures(x)[c("rmsea", "cfi", "srmr")])

  fillData$chisq <- chisqs
  fillData$df <- dfs
  fillData$pvalue <- pvalues
  fillData[, c("rmsea", "cfi", "srmr")] <- t(fits)

  fitPerGroupTable$setData(fillData)

  return()
}

##### HELPER #####
.mnlfaAddGroupingVariableToData <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["addedGroupVarContainer"]]) || !options[["addGroupVariableToData"]]) {
    return()
  }
  if (length(options[["groupInteractionErrors"]]) > 0)    # no groups were formed (.mnlfaFitPerGroup)
    return()

  container    <- createJaspContainer()
  container$dependOn(optionsFromObject = list(jaspResults[["mainContainer"]],
                                              jaspResults[["mainContainer"]][["checkModelFitPerGroupState"]]),
                     options = "addGroupVariableToData")   # as the group table

  var <- jaspResults[["mainContainer"]][["checkModelFitPerGroupState"]][["object"]][["addedGroupVar"]]

  colNameR <- gettext("addedGroupVariable")
  if (jaspBase:::columnExists(colNameR) && !jaspBase:::columnIsMine(colNameR)) {
    .quitAnalysis(gettextf("Column name %s already exists in the dataset", colNameR))
  }

  container[["addedGroupVar"]] <- jaspBase::createJaspColumn(colNameR)
  # TODO: this needs ot be fixed, see also: https://github.com/jasp-stats/INTERNAL-jasp/issues/2813
  container[["addedGroupVar"]]$setNominal(var)

  jaspResults[["addedGroupVarContainer"]] <- container

  # check if there are previous colNames that are not needed anymore and delete the cols
  oldNames <- jaspResults[["createdGroupNames"]][["object"]]
  newNames <- colNameR
  if (!is.null(oldNames)) {
    noMatch <- which(!(oldNames %in% newNames))
    if (length(noMatch) > 0) {
      for (i in 1:length(noMatch)) {
        jaspBase:::columnDelete(oldNames[noMatch[i]])
      }
    }
  }

  # save the created col names
  jaspResults[["createdGroupNames"]] <- createJaspState(newNames)

  return()

}
