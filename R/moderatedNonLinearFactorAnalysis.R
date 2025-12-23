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

  OpenMx::mxSetDefaultOptions()

  nIndicators  <- length(unlist(lapply(options[["factors"]], `[[`, "indicators"), use.names = FALSE))
  nModerators  <- length(options[["moderators"]])
  hasModelSpec <- nIndicators > 1 && nModerators > 0
  runAnalysis  <- options[[ "syncAnalysisBox" ]]

  ready               <- hasModelSpec
  readyForFitPerGroup <- hasModelSpec

  if (!hasModelSpec) {

    .mnlfaInitGlobalInvarianceFitTable(jaspResults, options)

    syncText <- createJaspHtml(
      text = gettext("Specify both factor indicator variables and moderator variables to run the analysis.")
    )
    jaspResults[["syncText"]] <- syncText
    syncText$dependOn(c("factors", "moderators"))
    syncText$position <- 0.01
    return()
  }

  # Optional: show “check sync” message only *before* the first run
  if (!runAnalysis && is.null(jaspResults[["mainContainer"]])) {
    syncText <- createJaspHtml(
      text = gettext("Check the 'Sync Analysis' box to run the analysis.")
    )
    jaspResults[["syncText"]] <- syncText
    syncText$dependOn("syncAnalysisBox")
    syncText$position <- 0.01
  }

  dataset <- .mnlfaHandleData(jaspResults, dataset, options, ready)
  options <- .mnlfaPreprocessOptions(options)

  .mnlfaCreateContainer(jaspResults, options)
  .mnlfaPlotOptionsForQml(jaspResults, options)

  .mnlfaCheckErrors(dataset, options, ready)

  .mnlfaFitPerGroup(jaspResults, dataset, options, readyForFitPerGroup)

  # .mnlfaCreateGlobalInvarianceContainer(jaspResults, options)

  if (runAnalysis) {
    .mnlfaCallGlobalInvarianceTests(jaspResults, dataset, options, ready)
    .mnlfaGlobalInvarianceFitTable(jaspResults, dataset, options, ready)
    .mnlfaGlobalInvarianceParameterTables(jaspResults, dataset, options, ready)
    .mnlfaPrintSyntax(jaspResults, dataset, options, ready)

    # plots
    .mnlfaPlot(jaspResults, dataset, options, ready)
  }

  .mnlfaFitPerGroupTable(jaspResults, dataset, options, readyForFitPerGroup)

  .mnlfaAddGroupingVariableToData(jaspResults, dataset, options)

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

  # scale the continuous moderators
  mods <- unlist(lapply(options[["moderators"]], `[[`, "variable"), use.names = FALSE)
  mods.types <- options[["moderators.types"]]
  for (i in 1:length(mods)) {
    if (mods.types[i] != "nominal") {
      # this also forces also ordinal moderator variables to be continuous, do we want this???????????????????????????????????
      dataset[[mods[i]]] <- scale(as.numeric(as.character(dataset[[mods[i]]])))
    }
  }

  # add interactions and extra effects
  includedInteractions <- lapply(options$moderatorInteractionTerms, function(x) x$value[x$moderatorInteractionTermsInclude])
  includedInteractions <- unlist(includedInteractions, use.names = FALSE)
  modsDecoded <- jaspBase::decodeColNames(mods)
  if (length(includedInteractions > 0)) {
    interMods <- strsplit(includedInteractions, ":")
    for (i in 1:length(includedInteractions)) {
      inters <- interMods[[i]]
      modsIndices <- match(inters, modsDecoded)
      interTypes <- mods.types[modsIndices]
      tmp1 <- as.numeric(as.character(dataset[[mods[modsIndices[1]]]])) # needed for nominal moderators
      tmp2 <- as.numeric(as.character(dataset[[mods[modsIndices[2]]]]))
      tmpDt <- data.frame(tmp1 * tmp2)
      tmpName <- paste0(inters[1], "_x_", inters[2])
      colnames(tmpDt) <- tmpName
      dataset <- cbind(dataset, tmpDt)
    }
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
                                 "moderatorSquaredEffect", "moderatorCubicEffect"))
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

  # add interactions and extra effects
  includedInteractions <- lapply(options$moderatorInteractionTerms, function(x) x$value[x$moderatorInteractionTermsInclude])
  includedInteractions <- unlist(includedInteractions, use.names = FALSE)
  modsDecoded <- jaspBase::decodeColNames(mods)
  if (length(includedInteractions > 0)) {
    interMods <- strsplit(includedInteractions, ":")
    for (i in 1:length(includedInteractions)) {
      inters <- interMods[[i]]
      modsIndices <- match(inters, modsDecoded)
      interTypes <- mods.types[modsIndices]
      tmpName <- paste0(inters[1], "_x_", inters[2])
      options[["moderators"]] <- append(options[["moderators"]], list(list(moderatorCubicEffect = FALSE, moderatorSquaredEffect = FALSE,
                                                                           variable = tmpName)))
      if (sum(interTypes == "scale") > 0)
        options[["moderators.types"]] <- append(options[["moderators.types"]], "scale")
      else
        options[["moderators.types"]] <- append(options[["moderators.types"]], "nominal")
    }
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


.mnlfaCheckErrors <- function(dataset, options, ready) {

  # TODO:
  # moderator names should not be "squared"
  return()
}

# Create a main container for everything
.mnlfaCreateContainer <- function(jaspResults, options) {
  if (is.null(jaspResults[["mainContainer"]])) {
    jaspResults[["mainContainer"]] <- createJaspContainer()
    jaspResults[["mainContainer"]]$dependOn(options = c(
      "factors", "moderators", "moderatorInteractionTerms", "moderatorInteractionTermsInclude",
      "moderatorSquaredEffect", "moderatorCubicEffect",
      "factorsUncorrelated", "interceptsFixedToZero", "packageMimiced", "estimator", "naAction"))
    jaspResults[["mainContainer"]]$position <- 3
  }
  return()
}

.mnlfaPlotOptionsForQml <- function(jaspResults, options) {

  if (!is.null(jaspResults[["plotOptionsForQml"]])) {
    return()
  }

  if (length(unlist(lapply(options[["factors"]], `[[`, "indicators"), use.names = FALSE)) < 1) return()

  plotOpts <- .buildModeratedVariableSummary(options[["includeIndividualModerationsList"]])

  modelOptions <- .extractIncludeIndividualModerationPaths(options[["includeIndividualModerationsList"]])
  modelOptionsPath <- unlist(modelOptions$paths, recursive = FALSE, use.names = FALSE)

  src <- createJaspQmlSource(sourceID = "plotOptionsForQml", value = plotOpts)
  src$dependOn(optionsFromObject = jaspResults[["mainContainer"]],
               options = c("invarianceTestConfigural", "invarianceTestMetric",
                           "invarianceTestScalar", "invarianceTestStrict", "invarianceTestCustom"),
               nestedOptions = modelOptionsPath)

  jaspResults[["plotOptionsForQml"]] <- src

  return()
}

##### ANALYSIS #####
# test configural invariance with the CFA module
.mnlfaFitPerGroup <- function(jaspResults, dataset, options, ready) {

  if (!ready) return()
  if (!is.null(jaspResults[["mainContainer"]][["checkModelFitPerGroupState"]])) return()
  if (!options[["checkModelFitPerGroup"]]) return()

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

.mnlfaCallGlobalInvarianceTests <- function(jaspResults, dataset, options, ready) {

  if (!ready) return()

  tests <- c(options[["invarianceTestConfigural"]], options[["invarianceTestMetric"]], options[["invarianceTestScalar"]],
             options[["invarianceTestStrict"]], options[["invarianceTestCustom"]])
  if (sum(tests) == 0) return()

  testNames <- c("invarianceTestConfigural", "invarianceTestMetric", "invarianceTestScalar", "invarianceTestStrict", "invarianceTestCustom")
  testNames <- testNames[tests]

  removeModObj <- .extractExcludedModerationItemsDf(options[["includeIndividualModerationsList"]])

  # get extra options for dependencies
  modelOptions <- .extractIncludeIndividualModerationPaths(options[["includeIndividualModerationsList"]])

  jaspBase::startProgressbar(length(testNames), label = gettext("Fitting invariance model"))
  for (i in 1:length(testNames)) {
    modelOptionsPaths <- modelOptions$paths
    modelOption <- modelOptionsPaths[[testNames[i]]]
    if (testNames[i] %in% removeModObj$modelValue) {
      removeMod <- removeModObj[removeModObj$modelValue == testNames[i], ]
      .mnlfaGlobalInvarianceTestHelper(jaspResults, dataset, options, testNames[i], modelOption, removeMod)
    } else {
      .mnlfaGlobalInvarianceTestHelper(jaspResults, dataset, options, testNames[i], modelOption)
    }
    progressbarTick()
  }

  return()
}


.mnlfaGlobalInvarianceTestHelper <- function(jaspResults, dataset, options, testName, modelOption, removeMod = NULL) {

  stateName <- paste0(testName, "State")
  if (!is.null(jaspResults[["mainContainer"]][[stateName]])) {
    return()
  }

  factorList <- lapply(options[["factors"]], function(x) x[["indicators"]])
  factorNames <- sapply(options[["factors"]], function(x) x[["name"]])
  names(factorList) <- factorNames
  moderators <- moderatorsOriginal <- sapply(options[["moderators"]], function(x) x[["variable"]])

  modelObj <- .generateSyntax(factorList, moderators, type = testName, removeMod)
  script <- mxsem::mxsem(model = modelObj$model, data = dataset, scale_loadings = FALSE, scale_latent_variances = FALSE)
  .ensureImxReportProgress()
  fit <- try(OpenMx::mxRun(script))

  fitState <- createJaspState(fit)

  fitState$dependOn(options = testName, nestedOptions = modelOption)

  jaspResults[["mainContainer"]][[stateName]] <- fitState

  # also save the model and mapping
  mapList <- modelObj$map
  modelState <- createJaspState(list(model = modelObj$model, map = mapList))
  modelState$dependOn(optionsFromObject = jaspResults[["mainContainer"]][[stateName]])
  jaspResults[["mainContainer"]][[paste0(testName, "ModelState")]] <- modelState

  return()
}



##### OUTPUT #####
.mnlfaFitPerGroupTable <- function(jaspResults, dataset, options, ready) {

  if (!is.null(jaspResults[["groupContainer"]][["checkModelFitPerGroupTable"]])) return()
  if (!ready) return()
  if (!options[["checkModelFitPerGroup"]]) return()

  fitPerGroupTable <- createJaspTable(gettext("Fit per Group Test"))
  fitPerGroupTable$addColumnInfo(name = "model", title = gettext("Group Model"), type = "string")

  groupContainer <- createJaspContainer()
  groupContainer$position <- 1
  groupContainer$dependOn(optionsFromObject = jaspResults[["mainContainer"]][["checkModelFitPerGroupState"]])
  jaspResults[["groupContainer"]] <- groupContainer
  jaspResults[["groupContainer"]][["checkModelFitPerGroupTable"]] <- fitPerGroupTable

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
  fits <- sapply(result, function(x) lavaan::fitmeasures(x)[c("rmsea", "cfi", "srmr")])

  fillData$chisq <- chisqs
  fillData$df <- dfs
  fillData$pvalue <- pvalues
  fillData[, c("rmsea", "cfi", "srmr")] <- t(fits)

  fitPerGroupTable$setData(fillData)

  return()
}

.mnlfaInitGlobalInvarianceFitTable <- function(jaspResults, options) {

  # Only create once per analysis
  if (!is.null(jaspResults[["fitContainer"]][["invFitTable"]]))
    return()

  invFitTable <- createJaspTable(gettext("Global Invariance Fit"))
  invFitTable$addColumnInfo(name = "type",  title = gettext("Type"),           type = "string")
  invFitTable$addColumnInfo(name = "N",     title = gettext("n(Par)"),  type = "integer")
  invFitTable$addColumnInfo(name = "df",     title = gettext("df"),           type = "integer")
  invFitTable$addColumnInfo(name = "Fit",     title = gettext("Fit (-2LL)"),  type = "number")
  invFitTable$addColumnInfo(name = "AIC",   title = gettext("AIC"),            type = "number")
  invFitTable$addColumnInfo(name = "SAAIC",   title = gettext("SAAIC"),            type = "number")
  invFitTable$addColumnInfo(name = "BIC",   title = gettext("BIC"),            type = "number")
  invFitTable$addColumnInfo(name = "SABIC", title = gettext("SABIC"),          type = "number")

  fitContainer <- createJaspContainer()
  fitContainer$position <- 2
  jaspResults[["fitContainer"]] <- fitContainer
  jaspResults[["fitContainer"]][["invFitTable"]] <- invFitTable
}

.mnlfaGlobalInvarianceFitTable <- function(jaspResults, dataset, options, ready) {

  if (!ready) return()

  if (is.null(jaspResults[["fitContainer"]])) {
    fitContainer <- createJaspContainer()
    fitContainer$position <- 2
    jaspResults[["fitContainer"]] <- fitContainer
  }

  invFitTable <- createJaspTable(gettext("Global Invariance Fit"))
  jaspResults[["fitContainer"]][["invFitTable"]] <- invFitTable

  invFitTable$addColumnInfo(name = "type",  title = gettext("Type"),           type = "string")
  invFitTable$addColumnInfo(name = "N",     title = gettext("n(Par)"),  type = "integer")
  invFitTable$addColumnInfo(name = "df",     title = gettext("df"),           type = "integer")
  invFitTable$addColumnInfo(name = "Fit",     title = gettext("Fit (-2LL)"),  type = "number")
  invFitTable$addColumnInfo(name = "AIC",   title = gettext("AIC"),            type = "number")
  invFitTable$addColumnInfo(name = "SAAIC",   title = gettext("SAAIC"),            type = "number")
  invFitTable$addColumnInfo(name = "BIC",   title = gettext("BIC"),            type = "number")
  invFitTable$addColumnInfo(name = "SABIC", title = gettext("SABIC"),          type = "number")

  results <- list(Configural = jaspResults[["mainContainer"]][["invarianceTestConfiguralState"]][["object"]],
                  Metric = jaspResults[["mainContainer"]][["invarianceTestMetricState"]][["object"]],
                  Scalar = jaspResults[["mainContainer"]][["invarianceTestScalarState"]][["object"]],
                  Strict = jaspResults[["mainContainer"]][["invarianceTestStrictState"]][["object"]],
                  Custom = jaspResults[["mainContainer"]][["invarianceTestCustomState"]][["object"]])

  results <- results[sapply(results, function(x) !is.null(x))]

  if (length(results) == 0) {

    if (ready) invFitTable$addFootnote(gettext("Choose one of the global invariance tests to perform the test."))
    return()
  }

  dtFill <- data.frame(type = c(), N = c(), df = c(), Fit = c(), AIC = c(), SAAIC = c(), BIC = c(), SABIC = c())
  if (length(results) > 1) {

    dtFill$diffLL <- dtFill$diffdf <- dtFill$p <- c()
    invFitTable$addColumnInfo(name = "diffLL", title = gettext("\u0394(LL)"), type = "number")
    invFitTable$addColumnInfo(name = "diffdf", title = gettext("\u0394(df)"), type = "integer")
    invFitTable$addColumnInfo(name = "p", title = gettext("p"), type = "pvalue")
  }

  dtFill <- data.frame()
  errmsg <- ""

  if (length(results) == 1L) {
    i <- 1L
    if (isTryError(results[[i]])) {
      errTmp <- jaspBase::.extractErrorMessage(results[[i]])
      errmsg <- gettextf("%1$s Error in fitting the %2$s model. Internal error message(s): %3$s",
                         errmsg, names(results)[i], errTmp)
      dtFill <- data.frame(
        type  = names(results)[i],
        N     = NA,
        df    = NA,
        Fit   = NA,
        AIC   = NA,
        SAAIC = NA,
        BIC   = NA,
        SABIC = NA
      )
    } else {
      summ <- .mxSummaryFixed(results[[i]])
      ics  <- summ$informationCriteria
      dtFill <- data.frame(
        type  = names(results)[i],
        N     = summ$estimatedParameters,
        df    = summ$degreesOfFreedom,
        Fit   = summ$Minus2LogLikelihood,
        AIC   = ics["AIC:", "par"],
        SAAIC = ics["AIC:", "sample"],
        BIC   = ics["BIC:", "par"],
        SABIC = ics["AIC:", "sample"]
      )
    }

  } else if (length(results) > 1L) {

    # precompute N (estimatedParameters) for each result
    nParams <- rep(NA_real_, length(results))
    for (i in seq_along(results)) {
      if (!isTryError(results[[i]])) {
        nParams[i] <- .mxSummaryFixed(results[[i]])$estimatedParameters
      }
    }

    # order models by N (e.g., decreasing = more parameters first)
    ord <- order(nParams, decreasing = TRUE, na.last = TRUE)

    prevGoodIdx <- NA  # index in 'results' of previous successful model in this order

    for (pos in seq_along(ord)) {
      i         <- ord[pos]
      fit       <- results[[i]]
      modelName <- names(results)[i]

      if (isTryError(fit)) {

        errTmp <- jaspBase::.extractErrorMessage(fit)
        errmsg <- gettextf("%1$s Error in fitting the %2$s model. Internal error message(s): %3$s",
                           errmsg, modelName, errTmp)

        dtAdd <- data.frame(
          type  = modelName,
          N     = NA,
          df    = NA,
          Fit   = NA,
          AIC   = NA,
          SAAIC = NA,
          BIC   = NA,
          SABIC = NA,
          diffLL = NA,
          diffdf = NA,
          p      = NA
        )

      } else {

        summ <- .mxSummaryFixed(fit)
        ics  <- summ$informationCriteria

        dtAdd <- data.frame(
          type  = modelName,
          N     = summ$estimatedParameters,
          df    = summ$degreesOfFreedom,
          Fit   = summ$Minus2LogLikelihood,
          AIC   = ics["AIC:", "par"],
          SAAIC = ics["AIC:", "sample"],
          BIC   = ics["BIC:", "par"],
          SABIC = ics["BIC:", "sample"]
        )

        if (!is.na(prevGoodIdx)) {
          comp         <- OpenMx::mxCompare(results[[prevGoodIdx]], fit)
          dtAdd$diffLL <- comp$diffLL[2]
          dtAdd$diffdf <- comp$diffdf[2]
          dtAdd$p      <- comp$p[2]
        } else {
          dtAdd$diffLL <- NA
          dtAdd$diffdf <- NA
          dtAdd$p      <- NA
        }

        prevGoodIdx <- i
      }

      dtFill <- rbind(dtFill, dtAdd)
    }
  }

  invFitTable$setData(dtFill)
  invFitTable$addFootnote(errmsg)

  return()

}


.mnlfaGlobalInvarianceParameterTables <- function(jaspResults, dataset, options, ready) {
  if (!ready) return()
  if (!is.null(jaspResults[["mainContainer"]][["globalParameterContainer"]])) return()

  results <- list(
    Configural = jaspResults[["mainContainer"]][["invarianceTestConfiguralState"]][["object"]],
    Metric     = jaspResults[["mainContainer"]][["invarianceTestMetricState"]][["object"]],
    Scalar     = jaspResults[["mainContainer"]][["invarianceTestScalarState"]][["object"]],
    Strict     = jaspResults[["mainContainer"]][["invarianceTestStrictState"]][["object"]],
    Custom     = jaspResults[["mainContainer"]][["invarianceTestCustomState"]][["object"]]
  )

  results <- results[sapply(results, function(x) !is.null(x))]
  if (length(results) == 0) return()

  globalParameterContainer <- createJaspContainer(gettext("Parameter Estimates"), initCollapsed = TRUE)
  globalParameterContainer$position <- 3

  # NEW: get nested paths for moderation include list (same trick as before)
  modelOptions <- .extractIncludeIndividualModerationPaths(options[["includeIndividualModerationsList"]])
  modelOptionsPath <- unlist(modelOptions$paths, recursive = FALSE, use.names = FALSE)

  globalParameterContainer$dependOn(
    optionsFromObject = jaspResults[["mainContainer"]],
    options = c(
      "invarianceTestConfigural", "invarianceTestMetric",
      "invarianceTestScalar", "invarianceTestStrict", "invarianceTestCustom",
      "parameterEstimatesLoadings", "parameterEstimatesIntercepts", "parameterEstimatesResidualVariances",
      "parameterEstimatesFactorVariance", "parameterEstimatesFactorMeans",
      "parameterEstimatesFactorCovariances", "parameterEstimatesAlphaLevel"
    ),
    nestedOptions = modelOptionsPath
  )

  jaspResults[["mainContainer"]][["globalParameterContainer"]] <- globalParameterContainer

  # get the mappings
  mapResults <- list(Configural = jaspResults[["mainContainer"]][["invarianceTestConfiguralModelState"]][["object"]][["map"]],
                      Metric = jaspResults[["mainContainer"]][["invarianceTestMetricModelState"]][["object"]][["map"]],
                      Scalar = jaspResults[["mainContainer"]][["invarianceTestScalarModelState"]][["object"]][["map"]],
                      Strict = jaspResults[["mainContainer"]][["invarianceTestStrictModelState"]][["object"]][["map"]],
                      Custom = jaspResults[["mainContainer"]][["invarianceTestCustomModelState"]][["object"]][["map"]])
  mapResults <- mapResults[sapply(mapResults, function(x) !is.null(x))]

  for (i in 1:length(results)) {

    if (isTryError(results[[i]])) {
      errorContainer <- createJaspContainer(names(results)[i])
      errorContainer$setError(gettextf("The %s model could not be fitted", tolower(names(results)[i])))
      globalParameterContainer[[names(results)[i]]] <- errorContainer
    } else {
      fitSummary <- .mxSummaryFixed(results[[i]])
      paramTable <- fitSummary$parameters
      globalParameterContainer[[names(results)[i]]] <- .mnlfaParameterTableHelper(paramTable = paramTable,
                                                                                  nm = names(results)[i],
                                                                                  mapResult = mapResults[[i]],
                                                                                  options)
    }

  }

  return()
}

.mnlfaParameterTableHelper <- function(paramTable, nm, mapResult, options) {

  cont <- createJaspContainer(nm, initCollapsed = TRUE)
  cont$position <- 2.1

  parNames <- paramTable[, "name"]
  parTypes <- paramTable[, "matrix"]
  newPars <- grepl("^new_parameters", parTypes)
  fTitleMapping <- lapply(options[["factors"]], function(x) c(x[["name"]], x[["title"]])) # map GUI titles with internal titles

  if (options[["parameterEstimatesLoadings"]]) {
    # Loadings
    loadPosition <- grepl("^load_", parNames)
    # special case cause when there is no moderation for loadings they are estimated but named weirdly
    loadPositionNoMod <- grepl("->", parNames, fixed = TRUE)
    allLoads <- loadPosition | loadPositionNoMod
    if (sum(loadPosition) + sum(loadPositionNoMod) > 0) { # are loadings even there
      loadTable <- createJaspTable(gettext("Loadings"))
      cont[["loadTable"]] <- loadTable
      loadTable$addColumnInfo(name = "factor", title = gettext("Factor"), type = "string", combine = TRUE)
      loadTable$addColumnInfo(name = "indicator", title = gettext("Indicator"), type = "string", combine = TRUE)
      loadTable$addColumnInfo(name = "effect", title = gettext("Effect"), type = "number")
      # loadTable$addColumnInfo(name = "param", title = gettext("Parameter"), type = "string")
      loadTable$addColumnInfo(name = "est", title = gettext("Estimate"), type = "number")
      loadTable$addColumnInfo(name = "se", title = gettext("Std. Error"), type = "number")
      loadTable$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "pvalue")
      loadTable$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number",
                           overtitle = gettextf("%s%% Confidence Interval", (1 - options$parameterEstimatesAlphaLevel) * 100))
      loadTable$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                           overtitle = gettextf("%s%% Confidence Interval", (1 - options$parameterEstimatesAlphaLevel) * 100))

      loadMap <- mapResult$loadings
      loadMap <- loadMap[loadMap$loadingParameter != "fixed_1", ]
      # Replace values in the loadingsTable based on fTitleMapping
      for (mapping in fTitleMapping) {
        loadMap$factor[loadMap$factor == mapping[1]] <- mapping[2]
      }

      subMat <- paramTable[allLoads, ]
      ciObj <- .waldCi(subMat[, "Estimate"], subMat[, "Std.Error"], options$parameterEstimatesAlphaLevel)
      # match loadMap rows to subMat rows
      idx <- match(.arrow(loadMap$loadingCoefficient), .arrow(subMat$name))

      df <- data.frame(
        factor    = loadMap$factor,
        indicator = loadMap$variable,
        effect    = sub("^data\\.", "", loadMap$moderator),
        est       = subMat$Estimate[idx],
        se        = subMat$Std.Error[idx],
        pvalue    = ciObj$pValue[idx],
        ci.lower  = ciObj$lowerBound[idx],
        ci.upper  = ciObj$upperBound[idx],
        stringsAsFactors = FALSE
      )
      loadTable$setData(df)
    }
  }

  if (options[["parameterEstimatesIntercepts"]]) {

    # Intercepts
    intPosition <- grepl("^int_", parNames)
    if (sum(intPosition) > 0) { # are intercepts even there
      intTable <- createJaspTable(gettext("Intercepts"))
      cont[["intTable"]] <- intTable
      intTable$addColumnInfo(name = "indicator", title = gettext("Indicator"), type = "string", combine = TRUE)
      intTable$addColumnInfo(name = "effect", title = gettext("Effect"), type = "number")
      # intTable$addColumnInfo(name = "param", title = gettext("Parameter"), type = "string")
      intTable$addColumnInfo(name = "est", title = gettext("Estimate"), type = "number")
      intTable$addColumnInfo(name = "se", title = gettext("Std. Error"), type = "number")
      intTable$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "pvalue")
      intTable$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number",
                              overtitle = gettextf("%s%% Confidence Interval", (1 - options$parameterEstimatesAlphaLevel) * 100))
      intTable$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                              overtitle = gettextf("%s%% Confidence Interval", (1 - options$parameterEstimatesAlphaLevel) * 100))

      intMap <- mapResult$intercepts
      # subMat contains only intercept rows from lavaan
      subMat <- paramTable[intPosition, ]

      # Match each mapping row to the correct row in subMat
      idx <- match(
        .arrow(intMap$interceptCoefficient),
        .arrow(subMat$name)
      )

      # Reorder estimates/SEs according to intMap
      est <- subMat$Estimate[idx]
      se  <- subMat$Std.Error[idx]

      # CIs in the same order
      ciObj <- .waldCi(est, se, options$parameterEstimatesAlphaLevel)

      df <- data.frame(
        indicator = intMap$variable,
        effect    = sub("^data\\.", "", intMap$moderator),
        # param  = sub("^int_", "", intMap$interceptCoefficient),  # if you want it
        est       = est,
        se        = se,
        pvalue    = ciObj$pValue,
        ci.lower  = ciObj$lowerBound,
        ci.upper  = ciObj$upperBound,
        stringsAsFactors = FALSE
      )
      intTable$setData(df)
    }
  }

  if (options[["parameterEstimatesResidualVariances"]]) {

    # residualVariances
    resPosition <- grepl("^res_", parNames)
    if (sum(resPosition) > 0) { # are residualVariances even specified
      resTable <- createJaspTable(gettext("Residual Variances"))
      cont[["resTable"]] <- resTable
      resTable$addColumnInfo(name = "indicator", title = gettext("Indicator"), type = "string", combine = TRUE)
      resTable$addColumnInfo(name = "effect", title = gettext("Effect"), type = "number")
      # resTable$addColumnInfo(name = "param", title = gettext("Parameter"), type = "string")
      resTable$addColumnInfo(name = "est", title = gettext("Estimate"), type = "number")
      resTable$addColumnInfo(name = "se", title = gettext("Std. Error"), type = "number")
      resTable$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "pvalue")
      resTable$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number",
                             overtitle = gettextf("%s%% Confidence Interval", (1 - options$parameterEstimatesAlphaLevel) * 100))
      resTable$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                             overtitle = gettextf("%s%% Confidence Interval", (1 - options$parameterEstimatesAlphaLevel) * 100))

      resMap <- mapResult$residualVariances
      subMat <- paramTable[resPosition, ]

      # Match map rows to the correct rows in subMat
      idx <- match(
        .arrow(resMap$residualCoefficient),
        .arrow(subMat$name)
      )

      # log-scale estimates and SE (as they come from lavaan)
      est_log <- subMat$Estimate[idx]
      se_log  <- subMat$Std.Error[idx]

      # Wald CI on log scale
      ciObj <- .waldCi(est_log, se_log, options$parameterEstimatesAlphaLevel)

      df <- data.frame(
        indicator = resMap$variable,
        effect    = sub("^data\\.", "", resMap$moderator),
        # param  = sub("^res_", "", resMap$residualCoefficient),
        est       = exp(est_log),            # back-transformed variance
        se        = exp(est_log) * se_log,
        pvalue    = ciObj$pValue,
        ci.lower  = exp(ciObj$lowerBound),   # CIs on variance scale
        ci.upper  = exp(ciObj$upperBound),
        stringsAsFactors = FALSE
      )
      resTable$setData(df)
    }
  }

  if (options[["parameterEstimatesFactorVariance"]]) {
    # factor Variances
    fvPosition <- grepl("^var_", parNames)
    if (sum(fvPosition) > 0) { # are factor variances even specified
      fvTable <- createJaspTable(gettext("Factor Variances"))
      cont[["fvTable"]] <- fvTable
      fvTable$addColumnInfo(name = "factor", title = gettext("Factor"), type = "string", combine = TRUE)
      fvTable$addColumnInfo(name = "effect", title = gettext("Effect"), type = "number")
      # fvTable$addColumnInfo(name = "param", title = gettext("Parameter"), type = "string")
      fvTable$addColumnInfo(name = "est", title = gettext("Estimate"), type = "number")
      fvTable$addColumnInfo(name = "se", title = gettext("Std. Error"), type = "number")
      fvTable$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "pvalue")
      fvTable$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number",
                             overtitle = gettextf("%s%% Confidence Interval", (1 - options$parameterEstimatesAlphaLevel) * 100))
      fvTable$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                             overtitle = gettextf("%s%% Confidence Interval", (1 - options$parameterEstimatesAlphaLevel) * 100))

      fvMap <- mapResult$variances
      for (mapping in fTitleMapping) {
        fvMap$factor[fvMap$factor == mapping[1]] <- mapping[2]
      }

      subMat <- paramTable[fvPosition, ]
      idx <- match(
        .arrow(fvMap$varianceCoefficient),
        .arrow(subMat$name)
      )
      est_log <- subMat$Estimate[idx]
      se_log  <- subMat$Std.Error[idx]

      ciObj_log <- .waldCi(est_log, se_log, options$parameterEstimatesAlphaLevel)

      df <- data.frame(
        factor   = fvMap$factor,
        effect   = sub("^data\\.", "", fvMap$moderator),
        est      = exp(est_log),
        se       = exp(est_log) * se_log,
        pvalue   = ciObj_log$pValue,
        ci.lower = exp(ciObj_log$lowerBound),
        ci.upper = exp(ciObj_log$upperBound),
        stringsAsFactors = FALSE
      )
      fvTable$setData(df)
    }
  }

  if (options[["parameterEstimatesFactorMeans"]]) {
    # factor Means
    fmPosition <- grepl("^mean_", parNames)
    if (sum(fmPosition) > 0) { # are factor variances even specified
      fmTable <- createJaspTable(gettext("Factor Means"))
      cont[["fmTable"]] <- fmTable
      fmTable$addColumnInfo(name = "factor", title = gettext("Factor"), type = "string", combine = TRUE)
      fmTable$addColumnInfo(name = "effect", title = gettext("Effect"), type = "number")
      # fmTable$addColumnInfo(name = "param", title = gettext("Parameter"), type = "string")
      fmTable$addColumnInfo(name = "est", title = gettext("Estimate"), type = "number")
      fmTable$addColumnInfo(name = "se", title = gettext("Std. Error"), type = "number")
      fmTable$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "pvalue")
      fmTable$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number",
                            overtitle = gettextf("%s%% Confidence Interval", (1 - options$parameterEstimatesAlphaLevel) * 100))
      fmTable$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                            overtitle = gettextf("%s%% Confidence Interval", (1 - options$parameterEstimatesAlphaLevel) * 100))

      fmMap <- mapResult$means

      for (mapping in fTitleMapping) {
        fmMap$factor[fmMap$factor == mapping[1]] <- mapping[2]
      }

      subMat <- paramTable[fmPosition, ]

      idx <- match(
        .arrow(fmMap$meanCoefficient),
        .arrow(subMat$name)
      )

      est <- subMat$Estimate[idx]
      se  <- subMat$Std.Error[idx]

      ciObj <- .waldCi(est, se, options$parameterEstimatesAlphaLevel)

      df <- data.frame(
        factor   = fmMap$factor,
        effect   = sub("^data\\.", "", fmMap$moderator),
        est      = est,
        se       = se,
        pvalue   = ciObj$pValue,
        ci.lower = ciObj$lowerBound,
        ci.upper = ciObj$upperBound,
        stringsAsFactors = FALSE
      )

      fmTable$setData(df)
    }
  }

  if (options[["parameterEstimatesFactorCovariances"]]) {
    # factor correlations
    covPosition <- grepl("^rho_", parNames)
    if (sum(covPosition) > 0) { # are factor covariances
      covTable <- createJaspTable(gettext("Factor Covariances"))
      cont[["covTable"]] <- covTable
      covTable$addColumnInfo(name = "effect", title = gettext("Effect"), type = "number")
      # covTable$addColumnInfo(name = "param", title = gettext("Parameter"), type = "string")
      covTable$addColumnInfo(name = "est", title = gettext("Estimate"), type = "number")
      covTable$addColumnInfo(name = "se", title = gettext("Std. Error"), type = "number")
      covTable$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "pvalue")
      covTable$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number",
                            overtitle = gettextf("%s%% Confidence Interval", (1 - options$parameterEstimatesAlphaLevel) * 100))
      covTable$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                            overtitle = gettextf("%s%% Confidence Interval", (1 - options$parameterEstimatesAlphaLevel) * 100))

      covMap <- mapResult$covariances
      subMat <- paramTable[covPosition, ]
      ciObj <- .waldCi(subMat[, "Estimate"], subMat[, "Std.Error"], options$parameterEstimatesAlphaLevel)
      df <- data.frame(effect = covMap$moderator,
                       # param = covMap$covarianceCoefficient,
                       est = subMat[, "Estimate"],
                       se = subMat[, "Std.Error"],
                       pvalue = ciObj$pValue,
                       ci.lower = ciObj$lowerBound,
                       ci.upper = ciObj$upperBound)
      covTable$setData(df)
    }
  }

  return(cont)
}

.mnlfaPlot <- function(jaspResults, dataset, options, ready) {

  if (!ready) return()
  if (!is.null(jaspResults[["plotContainer"]])) return()

  plotContainer <- createJaspContainer(gettext("Parameter Plots"))
  plotContainer$position <- 3
  plotContainer$dependOn(optionsFromObject = jaspResults[["mainContainer"]][["globalParameterContainer"]],
                         options = c("plotModelList", "includePlot"))
  jaspResults[["plotContainer"]] <- plotContainer

  mods <- unlist(lapply(options[["moderators"]], `[[`, "variable"), use.names = FALSE)
  mods.types <- options[["moderators.types"]]

  plotModelList <- options[["plotModelList"]]
  # only keep the elements where includePlot is true
  filteredPlots <- .extractIncludedPlotItemsDf(plotModelList)

  if (is.null(filteredPlots)) {
    return()
  }


  results <- lapply(c("invarianceTestConfiguralState", "invarianceTestMetricState",
                      "invarianceTestScalarState", "invarianceTestStrictState", "invarianceTestCustomState"),
                    function(state) jaspResults[["mainContainer"]][[state]][["object"]])
  if (all(sapply(results, is.null))) return()

  # for the filtered plots object each row is one parameter to plot:
  for (i in 1:nrow(filteredPlots)) {

    currentRow <- filteredPlots[i, ]
    modelName <- currentRow$modelName
    fit <- jaspResults[["mainContainer"]][[paste0(modelName, "State")]][["object"]]
    mapResult <- jaspResults[["mainContainer"]][[paste0(modelName, "ModelState")]][["object"]][["map"]]
    # temporary fix? the variable names in the plot qml tabview are coming from an R source and they are
    # therefor not encoded so we decode the ones in the mapping:
    mapResult <- lapply(mapResult, function(x) {
      x[["variable"]] <- jaspBase::decodeColNames(x[["variable"]])
      x
    })

    paramTable <- .mxSummaryFixed(fit)$parameters
    parameterGroup <- currentRow$parameterGroup
    # map type to corresponding prefix
    prefix <- switch(parameterGroup,
                     "loadings" = "load",
                     "intercepts" = "int",
                     "residualVariances" = "res",
                     "variances" = "var",
                     "means" = "mean",
                     "covariances" = "rho")

    parNames <- paramTable[, "name"]
    parPosition <- grepl(paste0("^", prefix, "_"), parNames)
    if (sum(parPosition) > 0) {
      subMat <- paramTable[parPosition, ]
      map <- mapResult[[parameterGroup]]
      if (currentRow$plotType == "factors") {
        # match the factor names with the factor labels
        factors <- options[["factors"]]
        fTitleMapping <- setNames(
          vapply(factors, `[[`, character(1), "title"),  # values
          vapply(factors, `[[`, character(1), "name")   # names
        )
        if (parameterGroup == "covariances") {
          subMap <- map # for factor covariances there is not matching cause the map is only created
          # if there are two factors and there are always the same parameters
        } else {
          map$factor <- ifelse(is.na(fTitleMapping[map$factor]), map$factor, fTitleMapping[map$factor])
          # currentRow value also needs the title mapping
          currentRow$value <- ifelse(is.na(fTitleMapping[currentRow$value]), currentRow$value, fTitleMapping[currentRow$value])
          subMap <- map[map$factor == currentRow$value, ]
        }

      } else {

        # something is going wrong here with the encoding and then matching
        subMap <- map[map$variable == currentRow$value, ]
      }

      currentMods <- sub("^data.", "", subMap$moderator)
      modsForPlots <- c(currentRow$plotModerator1, currentRow$plotModerator2)
      modsForPlots <- modsForPlots[modsForPlots != ""]
      modsForPlots <- gsub(":", "_x_", modsForPlots) # for interactions

      # so if there are square or cubic effects the data has those variables attached but in decoded format
      # modsForPlots has them in encoded format.
      # check for _squared and _cubic suffix in modsForPlots
      if (any(grepl("_squared$", modsForPlots))) {
        sqrMods <- modsForPlots[grepl("_squared$", modsForPlots)]
        for (sqrMod in sqrMods) {
          baseMod <- sub("_squared$", "", sqrMod)
          decMod <- jaspBase::decodeColNames(baseMod)
          modsForPlots[modsForPlots == sqrMod] <- paste0(decMod, "_squared")
        }
      }
      # same for cubic
      if (any(grepl("_cubic$", modsForPlots))) {
        cubicMods <- modsForPlots[grepl("_cubic$", modsForPlots)]
        for (cubicMod in cubicMods) {
          baseMod <- sub("_cubic$", "", cubicMod)
          decMod <- jaspBase::decodeColNames(baseMod)
          modsForPlots[modsForPlots == cubicMod] <- paste0(decMod, "_cubic")
        }
      }

      # if there is an interaction in the modsForPlots the name is encoded, but it is decoded in the data
      # grep "_x_" in mods for plots, then decode each part and reassemble
      if (any(grepl("_x_", modsForPlots))) {
        interMods <- modsForPlots[grepl("_x_", modsForPlots)]
        for (interMod in interMods) {
          parts <- unlist(strsplit(interMod, "_x_"))
          decParts <- sapply(parts, jaspBase::decodeColNames)
          modsForPlots[modsForPlots == interMod] <- paste0(decParts, collapse = "_x_")
        }
      }

      matchIndices <- match(
        .arrow(modsForPlots),
        .arrow(currentMods)
      )
      matchIndices <- matchIndices[!is.na(matchIndices)]
      coefficientColumnIndex <- grep("Coefficient", colnames(subMap))

      modEstimates <- c(
        subMat$Estimate[
          match(
            .arrow(subMap[subMap$moderator == "Baseline", coefficientColumnIndex]),
            .arrow(subMat$name)
          )
        ],
        subMat$Estimate[
          match(
            .arrow(subMap[matchIndices, coefficientColumnIndex]),
            .arrow(subMat$name)
          )
        ]
      )

      dtSub <- dataset[, modsForPlots, drop = FALSE]
      baselineTerm <- sum(modEstimates[subMap$moderator == "Baseline"])
      slopeTerms <- modEstimates[subMap$moderator != "Baseline"]
      slopeTerms <- slopeTerms[!is.na(slopeTerms)]

      if (length(slopeTerms) == 1) {
        slopeValues <- dtSub[, modsForPlots] * slopeTerms
      } else {
        slopeValues <- rowSums(sweep(dtSub, 2, slopeTerms, `*`))
      }
      # re-transform the log variances
      if (parameterGroup %in% c("residualVariances", "variances")) {
        dtSub[["estimatedValue"]] <- exp(baselineTerm + slopeValues)
      } else {
        dtSub[["estimatedValue"]] <- baselineTerm + slopeValues
      }

      if (length(modsForPlots) == 1) { # only one moderator
        if (mods.types[mods == modsForPlots] == "scale") {
          gg <- ggplot2::ggplot(data = dtSub,
                                ggplot2::aes(x = .data[[modsForPlots]],
                                             y = estimatedValue)) +
            ggplot2::ylab(gettext("Parameter Value")) +
            jaspGraphs::themeJaspRaw() +
            jaspGraphs::geom_rangeframe(size = 1.1) +
            ggplot2::geom_line() +
            ggplot2::labs(x = gsub("_x_", ":", modsForPlots))
        } else { # nominal moderator
          gg <- ggplot2::ggplot(data = dtSub,
                                ggplot2::aes(x = factor(.data[[modsForPlots]]),
                                             y = estimatedValue)) +
            ggplot2::ylab(gettext("Parameter Value")) +
            jaspGraphs::themeJaspRaw() +
            jaspGraphs::geom_rangeframe(size = 1.1) +
            ggplot2::stat_summary(fun = mean, geom = "col", fill = "gray", width = 0.5) +
            ggplot2::scale_x_discrete() +
            ggplot2::labs(x = modsForPlots)
          }

      } else { # two moderators
        modsForPlotsTypes <- mods.types[match(modsForPlots, mods)]
        modsForPlotsTypes <- modsForPlotsTypes[!is.na(modsForPlotsTypes)]
        if (length(unique(modsForPlotsTypes)) == 2) { # one moderator scale, one nominal
          gg <- ggplot2::ggplot(data = dtSub,
                                ggplot2::aes(x = .data[[modsForPlots[modsForPlotsTypes == "scale"]]],
                                             y = estimatedValue,
                                             color = factor(.data[[modsForPlots[modsForPlotsTypes == "nominal"]]]))) +
            ggplot2::ylab(gettext("Parameter Value")) +
            ggplot2::geom_point() +
            jaspGraphs::themeJaspRaw() +
            jaspGraphs::geom_rangeframe(size = 1.1) +
            ggplot2::theme(legend.position = "right") +
            ggplot2::labs(color = modsForPlots[modsForPlotsTypes == "nominal"],
                          x = gsub("_x_", ":", modsForPlots[modsForPlotsTypes == "scale"]))

        } else if (all(modsForPlotsTypes == "scale")) {
          gg <- ggplot2::ggplot(data = dtSub,
                                ggplot2::aes(x = .data[[modsForPlots[1]]],
                                             y = estimatedValue,
                                             color = .data[[modsForPlots[2]]])) +
            ggplot2::ylab(gettext("Parameter Value")) +
            ggplot2::geom_point() +
            jaspGraphs::themeJaspRaw() +
            jaspGraphs::geom_rangeframe(size = 1.1) +
            ggplot2::theme(legend.position = "right") +
            ggplot2::labs(color = modsForPlots[2]) +
            ggplot2::scale_color_gradient(low = "blue", high = "green") +
            ggplot2::labs(x = gsub("_x_", ":", modsForPlots[1]),
                          color = gsub("_x_", ":", modsForPlots[2]))

        } else if (all(modsForPlotsTypes == "nominal")) {
          gg <- ggplot2::ggplot(data = dtSub,
                                ggplot2::aes(x = factor(.data[[modsForPlots[1]]]),
                                             y = estimatedValue,
                                             color = factor(.data[[modsForPlots[2]]]))) +
            ggplot2::ylab(gettext("Parameter Value")) +
            ggplot2::geom_bar() +
            jaspGraphs::themeJaspRaw() +
            jaspGraphs::geom_rangeframe(size = 1.1) +
            ggplot2::theme(legend.position = "right") +
            ggplot2::labs(color = modsForPlots[2])
        }

      }

      plt <- createJaspPlot(gg)
      plt$title <- paste(parameterGroup, currentRow$value, sep = ": ")
      plotContainer[[paste0("plot", i)]] <- plt
    }
  }

}

.mnlfaPrintSyntax <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["syntaxContainer"]])) return()
  if (!options$showSyntax) return()
  if (!ready) return()

  syntaxContainer <- createJaspContainer(gettext("Model Syntax"), initCollapsed = TRUE)
  syntaxContainer$dependOn(options = c("showSyntax"),
                           optionsFromObject = jaspResults[["mainContainer"]][["globalParameterContainer"]])
  jaspResults[["syntaxContainer"]] <- syntaxContainer

  models <- list(Configural = jaspResults[["mainContainer"]][["invarianceTestConfiguralModelState"]][["object"]][["model"]],
                 Metric     = jaspResults[["mainContainer"]][["invarianceTestMetricModelState"]][["object"]][["model"]],
                 Scalar     = jaspResults[["mainContainer"]][["invarianceTestScalarModelState"]][["object"]][["model"]],
                 Strict     = jaspResults[["mainContainer"]][["invarianceTestStrictModelState"]][["object"]][["model"]],
                 Custom     = jaspResults[["mainContainer"]][["invarianceTestCustomModelState"]][["object"]][["model"]])
  models <- models[sapply(models, function(x) !is.null(x))]

  if (length(models) == 0) {
    modPrint <- gettext("Nothing to print.")
    syntaxContainer[["printHtml"]] <- createJaspHtml(modPrint, class = "jasp-code", position = 10)
    return()
  }

  for (i in 1:length(models)) {
    syntaxContainer[[paste0(names(models)[i], "Syntax")]] <- createJaspHtml(models[[i]], class = "jasp-code",
                                                                            title = names(models)[i])
  }

  return()
}


##### HELPER #####
.mnlfaAddGroupingVariableToData <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["addedGroupVarContainer"]]) || !options[["addGroupVariableToData"]]) {
    return()
  }

  container    <- createJaspContainer()
  container$dependOn(optionsFromObject = jaspResults[["mainContainer"]][["checkModelFitPerGroupState"]],
                     options = "addGroupVariableToData")

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


##### When creating the syntax it might make sense to use the actual factor names, or maybe not, because they might have spaces and weird symbols.
.generateSyntax <- function(factorList, moderators, type, removeMod = NULL) {

  factorNamesVars <- factorNamesMeans <- names(factorList)

  # if (type == "invarianceTestConfigural") {
  #   factorNamesVars <- factorNamesMeans <- NULL
  # } else if (type == "invarianceTestMetric") {
  #   factorNamesMeans <- NULL
  # } else if (type == "invarianceTestScalar") {
  #   rmvLoadMod <- rmvIntMod <- TRUE
  # } else if (type == "invarianceTestStrict") {
  #   rmvLoadMod <-  rmvIntMod <- rmvResMod <- TRUE
  # }

  loadingsObj <- .generateLoadings(factorList, moderators,
                                   removeModerationForVariables = removeMod$itemValue[removeMod$paramValue == "loadings"])
  loadingsBlock <- loadingsObj$loadingsBlock

  interceptsObj <- .generateIntercepts(factorList, moderators,
                                       removeModerationForVariables = removeMod$itemValue[removeMod$paramValue == "intercepts"])
  interceptsBlock <- interceptsObj$interceptsBlock

  residualsObj <- .generateResidualVariances(factorList, moderators,
                                             removeModerationForVariables = removeMod$itemValue[removeMod$paramValue == "residualVariances"])
  residualsBlock <- residualsObj$residualsBlock

  variancesObj <- .generateFactorVariances(factorList, moderators, moderate = factorNamesVars,
                                           removeAllModeration = removeMod$itemValue[removeMod$paramValue == "variances"])
  variancesBlock <- variancesObj$variancesBlock

  meansObj <- .generateFactorMeans(factorList, moderators, moderate = factorNamesMeans,
                                   removeAllModeration = removeMod$itemValue[removeMod$paramValue == "means"])
  meansBlock <- meansObj$meansBlock

  covarianceObj <- .generateFactorCovariances(factorList, moderators,
                                             removeModeration = any(removeMod$paramValue == "covariances"))

  if (length(names(factorList)) > 1) {
    covarianceObj <- .generateFactorCovariances(factorList, moderators,
                                                removeModeration = any(removeMod$paramValue == "covariances"))
  } else {
    covarianceObj <- list(covarianceBlock = "", covarianceMapDf = NULL)
  }


  covarianceBlock <- covarianceObj$covarianceBlock

  model <- paste0(loadingsBlock, interceptsBlock, residualsBlock, variancesBlock, meansBlock, covarianceBlock,
                  sep = "\n")

  map <- list(loadings = loadingsObj$loadingsMapDf,
              intercepts = interceptsObj$interceptsMapDf,
              residualVariances = residualsObj$residualsMapDf,
              variances = variancesObj$variancesMapDf,
              means = meansObj$meansMapDf,
              covariances = covarianceObj$covarianceMapDf)

  return(list(model = model, map = map))

}

.generateLoadings <- function(factorList,
                              moderators,
                              removeModeration = FALSE,
                              removeModerationFor = NULL,
                              removeModerationForVariables = c(),
                              removeSpecificModeration = list(),
                              includeIndividualModeration = NULL,
                              scaleIndicatorLoading = FALSE) {
  loadingsBlock <- gettext("# LOADINGS BLOCK \n")
  loadingsList <- list()  # Store mappings

  moderators <- paste0("data.", moderators)
  if (!is.null(includeIndividualModeration)) {
    moderators <- setdiff(moderators, paste0("data.", includeIndividualModeration))
  }

  rowIndex <- 1  # Track row index

  for (factorName in names(factorList)) {
    indicators <- factorList[[factorName]]
    loadingsBlock <- paste0(loadingsBlock, "  ", factorName, " =~ ")

    for (i in seq_along(indicators)) {
      var <- indicators[i]
      paramName <- paste0("load_", var)
      paramIntercept <- paste0(paramName, "_0")

      # Default values to avoid undefined errors
      loadExpr <- var
      moderationTerms <- ""

      # Define loading coefficients
      moderatedTerms <- moderators
      if (var %in% names(removeSpecificModeration)) {
        moderatedTerms <- setdiff(moderatedTerms, paste0("data.", removeSpecificModeration[[var]]))
      }
      paramCoefs <- if (length(moderatedTerms) > 0) {
        paste0(paramName, "_", seq_along(moderatedTerms))
      } else {
        character(0)  # Empty character vector instead of NA
      }

      if (removeModeration || (!is.null(removeModerationFor) && factorName %in% removeModerationFor) || var %in% removeModerationForVariables) {
        if (scaleIndicatorLoading && i == 1) {
          loadExpr <- paste0("1 * ", var)
          paramName <- "fixed_1"
        } else {
          loadExpr <- var
        }

        # This MUST match what JASP/lavaan puts in subMat$name
        baselineName <- paste0(factorName, "->", var)

        loadingsList[[rowIndex]] <- list(
          factor = factorName,
          variable = var,
          loadingParameter = paramName,
          loadingCoefficient = baselineName,
          moderator = gettext("Baseline")
        )
        rowIndex <- rowIndex + 1
      } else {
        # Store the intercept coefficient
        loadingsList[[rowIndex]] <- list(
          factor = factorName,
          variable = var,
          loadingParameter = paramName,
          loadingCoefficient = paramIntercept,
          moderator = gettext("Baseline")
        )
        rowIndex <- rowIndex + 1

        # Store each regression coefficient linked to its moderator
        if (length(paramCoefs) > 0) {
          for (j in seq_along(moderatedTerms)) {
            loadingsList[[rowIndex]] <- list(
              factor = factorName,
              variable = var,
              loadingParameter = paramName,
              loadingCoefficient = paramCoefs[j],
              moderator = moderatedTerms[j]
            )
            rowIndex <- rowIndex + 1
          }
        }

        # Construct the moderation expression for the model syntax
        if (length(paramCoefs) > 0) {
          moderationTerms <- paste0(
            paste0(moderatedTerms, " * ", paramCoefs),
            collapse = " + "
          )
        }
        loadExpr <- paste0("{", paramName, " := ", paramIntercept, ifelse(moderationTerms != "", paste0(" + ", moderationTerms), ""), "} * ", var)
      }

      if (i < length(indicators)) {
        loadExpr <- paste0(loadExpr, " + \n")
      }
      loadingsBlock <- paste0(loadingsBlock, loadExpr)
    }
    loadingsBlock <- paste0(loadingsBlock, "\n")
  }

  # Convert list to DataFrame
  loadingsMapDf <- do.call(rbind, lapply(loadingsList, as.data.frame))

  return(list(loadingsBlock = loadingsBlock, loadingsMapDf = loadingsMapDf))
}



.generateFactorVariances <- function(factorList,
                                     moderators,
                                     moderate = NULL,
                                     removeSpecificModeration = list(),
                                     removeAllModeration = NULL,
                                     includeIndividualModeration = NULL,
                                     scaleFactorVariance = TRUE) {

  variancesBlock <- gettext("# FACTOR VARIANCES BLOCK \n")
  variancesList <- list()

  moderators <- paste0("data.", moderators)
  if (!is.null(includeIndividualModeration)) {
    moderators <- setdiff(moderators, paste0("data.", includeIndividualModeration))
  }

  rowIndex <- 1

  for (factorName in names(factorList)) {
    removeAll <- !is.null(removeAllModeration) && factorName %in% removeAllModeration
    applyModeration <- !removeAll && !is.null(moderate) && factorName %in% moderate

    paramName <- paste0("var_", factorName)
    paramIntercept <- paste0(paramName, "_0")

    # Build moderation terms
    moderatedTerms <- if (applyModeration) {
      mods <- moderators
      if (factorName %in% names(removeSpecificModeration)) {
        mods <- setdiff(mods, paste0("data.", removeSpecificModeration[[factorName]]))
      }
      mods
    } else {
      character(0)
    }

    paramCoefs <- if (length(moderatedTerms) > 0) {
      paste0(paramName, "_", seq_along(moderatedTerms))
    } else {
      character(0)
    }

    # Build lavaan expression
    if (scaleFactorVariance) {
      if (!applyModeration) {
        # Case: no moderation, scale = TRUE → fixed variance
        variancesBlock <- paste0(variancesBlock, "  ", factorName, " ~~ 1 * ", factorName, "\n")
      } else {
        # Case: scale = TRUE, moderation active → slope-only moderation
        moderationTerms <- paste0(moderatedTerms, " * ", paramCoefs, collapse = " + ")

        variancesBlock <- paste0(variancesBlock, "  ", factorName, " ~~ {", paramName,
                                 " := exp(", moderationTerms, ")} * ", factorName, "\n")

        # Map slopes only
        for (j in seq_along(paramCoefs)) {
          variancesList[[rowIndex]] <- list(
            factor = factorName,
            varianceParameter = paramName,
            varianceCoefficient = paramCoefs[j],
            moderator = moderatedTerms[j]
          )
          rowIndex <- rowIndex + 1
        }
      }
    } else {
      # Case: scale = FALSE → always include intercept, optionally add slopes
      moderationTerms <- if (length(paramCoefs) > 0) {
        paste0(" + ", paste0(moderatedTerms, " * ", paramCoefs, collapse = " + "))
      } else {
        ""
      }

      variancesBlock <- paste0(variancesBlock, "  ", factorName, " ~~ {", paramName,
                               " := exp(", paramIntercept, moderationTerms, ")} * ", factorName, "\n")

      # Map intercept
      variancesList[[rowIndex]] <- list(
        factor = factorName,
        varianceParameter = paramName,
        varianceCoefficient = paramIntercept,
        moderator = gettext("Baseline")
      )
      rowIndex <- rowIndex + 1

      # Map slopes if any
      if (length(paramCoefs) > 0) {
        for (j in seq_along(paramCoefs)) {
          variancesList[[rowIndex]] <- list(
            factor = factorName,
            varianceParameter = paramName,
            varianceCoefficient = paramCoefs[j],
            moderator = moderatedTerms[j]
          )
          rowIndex <- rowIndex + 1
        }
      }
    }
  }

  variancesMapDf <- if (length(variancesList) > 0) {
    do.call(rbind, lapply(variancesList, as.data.frame))
  } else {
    NULL
  }

  return(list(variancesBlock = variancesBlock, variancesMapDf = variancesMapDf))
}


.generateFactorMeans <- function(factorList,
                                 moderators,
                                 moderate = NULL,
                                 removeSpecificModeration = list(),
                                 includeIndividualModeration = NULL,
                                 fixMeanZero = TRUE,
                                 removeAllModeration = NULL) {
  meansBlock <- gettext("# FACTOR MEANS BLOCK \n")
  meansList <- list()

  moderators <- paste0("data.", moderators)
  if (!is.null(includeIndividualModeration)) {
    moderators <- setdiff(moderators, paste0("data.", includeIndividualModeration))
  }

  rowIndex <- 1

  for (factorName in names(factorList)) {
    removeAll <- !is.null(removeAllModeration) && factorName %in% removeAllModeration
    applyModeration <- !removeAll && !is.null(moderate) && factorName %in% moderate

    paramName <- paste0("mean_", factorName)
    paramIntercept <- paste0(paramName, "_0")

    # Build moderated terms
    moderatedTerms <- if (applyModeration) {
      mods <- moderators
      if (factorName %in% names(removeSpecificModeration)) {
        mods <- setdiff(mods, paste0("data.", removeSpecificModeration[[factorName]]))
      }
      mods
    } else {
      character(0)
    }

    paramCoefs <- if (length(moderatedTerms) > 0) {
      paste0(paramName, "_", seq_along(moderatedTerms))
    } else {
      character(0)
    }

    # Case 1: fixMeanZero = TRUE
    if (fixMeanZero) {
      if (applyModeration && length(paramCoefs) > 0) {
        # Only slopes (no intercept)
        meanExpr <- paste0(paste0(moderatedTerms, " * ", paramCoefs), collapse = " + ")

        meansBlock <- paste0(meansBlock, "  ", factorName, " ~ {", paramName, " := ", meanExpr, "} * 1\n")

        for (j in seq_along(paramCoefs)) {
          meansList[[rowIndex]] <- list(
            factor = factorName,
            meanParameter = paramName,
            meanCoefficient = paramCoefs[j],
            moderator = moderatedTerms[j]
          )
          rowIndex <- rowIndex + 1
        }
      } else {
        # No moderation → fixed to 0
        meansBlock <- paste0(meansBlock, "  ", factorName, " ~ 0 * 1\n")
      }

    } else {
      # Case 2: fixMeanZero = FALSE → allow intercept
      if (applyModeration) {
        # Intercept + slopes
        moderationTerms <- if (length(paramCoefs) > 0) {
          paste0(" + ", paste0(moderatedTerms, " * ", paramCoefs, collapse = " + "))
        } else {
          ""
        }

        meansBlock <- paste0(meansBlock, "  ", factorName, " ~ {", paramName,
                             " := ", paramIntercept, moderationTerms, "} * 1\n")

        meansList[[rowIndex]] <- list(
          factor = factorName,
          meanParameter = paramName,
          meanCoefficient = paramIntercept,
          moderator = gettext("Baseline")
        )
        rowIndex <- rowIndex + 1

        for (j in seq_along(paramCoefs)) {
          meansList[[rowIndex]] <- list(
            factor = factorName,
            meanParameter = paramName,
            meanCoefficient = paramCoefs[j],
            moderator = moderatedTerms[j]
          )
          rowIndex <- rowIndex + 1
        }

      } else {
        # Intercept only
        meansBlock <- paste0(meansBlock, "  ", factorName, " ~ {", paramName,
                             " := ", paramIntercept, "} * 1\n")

        meansList[[rowIndex]] <- list(
          factor = factorName,
          meanParameter = paramName,
          meanCoefficient = paramIntercept,
          moderator = gettext("Baseline")
        )
        rowIndex <- rowIndex + 1
      }
    }
  }

  meansMapDf <- if (length(meansList) > 0) {
    do.call(rbind, lapply(meansList, as.data.frame))
  } else {
    NULL
  }

  return(list(meansBlock = meansBlock, meansMapDf = meansMapDf))
}


.generateIntercepts <- function(factorList,
                                moderators,
                                removeModeration = FALSE,
                                removeModerationFor = NULL,
                                removeModerationForVariables = c(),
                                removeSpecificModeration = list(),
                                includeIndividualModeration = NULL) {
  interceptsBlock <- gettext("# INTERCEPTS BLOCK \n")
  interceptsList <- list()  # Store mappings

  moderators <- paste0("data.", moderators)
  if (!is.null(includeIndividualModeration)) {
    moderators <- setdiff(moderators, paste0("data.", includeIndividualModeration))
  }

  rowIndex <- 1  # Track row index

  for (factorName in names(factorList)) {
    indicators <- factorList[[factorName]]

    for (i in seq_along(indicators)) {
      var <- indicators[i]
      paramName <- paste0("int_", var)
      paramIntercept <- paste0(paramName, "_0")

      moderatedTerms <- moderators
      if (var %in% names(removeSpecificModeration)) {
        moderatedTerms <- setdiff(moderatedTerms, paste0("data.", removeSpecificModeration[[var]]))
      }
      paramCoefs <- if (length(moderatedTerms) > 0) {
        paste0(paramName, "_", seq_along(moderatedTerms))
      } else {
        character(0)
      }

      if (removeModeration || (!is.null(removeModerationFor) && factorName %in% removeModerationFor) || var %in% removeModerationForVariables) {
        # Unmoderated intercept: lavaan parameter will be "int_<var>"
        interceptExpr <- paste0(var, " ~ ", paramName, " * 1")

        interceptsList[[rowIndex]] <- list(
          variable             = var,
          interceptParameter   = paramName,   # "int_<var>"
          interceptCoefficient = paramName,   # also "int_<var>" → matches subMat$name
          moderator            = gettext("Baseline")
        )
        rowIndex <- rowIndex + 1
      } else {
        interceptsList[[rowIndex]] <- list(
          variable = var,
          interceptParameter = paramName,
          interceptCoefficient = paramIntercept,
          moderator = gettext("Baseline")
        )
        rowIndex <- rowIndex + 1

        if (length(paramCoefs) > 0) {
          for (j in seq_along(moderatedTerms)) {
            interceptsList[[rowIndex]] <- list(
              variable = var,
              interceptParameter = paramName,
              interceptCoefficient = paramCoefs[j],
              moderator = moderatedTerms[j]
            )
            rowIndex <- rowIndex + 1
          }
        }

        moderationTerms <- if (length(paramCoefs) > 0) {
          paste0(paste0(moderatedTerms, " * ", paramCoefs), collapse = " + ")
        } else {
          ""
        }

        interceptExpr <- paste0(var, " ~ {", paramName, " := ", paramIntercept,
                                ifelse(moderationTerms != "", paste0(" + ", moderationTerms), ""),
                                "} * 1")
      }

      interceptsBlock <- paste0(interceptsBlock, "  ", interceptExpr, "\n")
    }
  }

  interceptsMapDf <- do.call(rbind, lapply(interceptsList, as.data.frame))

  return(list(interceptsBlock = interceptsBlock, interceptsMapDf = interceptsMapDf))
}



.generateResidualVariances <- function(factorList,
                                       moderators,
                                       removeModeration = FALSE,
                                       removeModerationFor = NULL,
                                       removeModerationForVariables = c(),
                                       removeSpecificModeration = list(),
                                       includeIndividualModeration = NULL) {
  residualsBlock <- gettext("# RESIDUAL VARIANCES BLOCK \n")
  residualsList <- list()  # Store mappings

  moderators <- paste0("data.", moderators)
  if (!is.null(includeIndividualModeration)) {
    moderators <- setdiff(moderators, paste0("data.", includeIndividualModeration))
  }

  rowIndex <- 1  # Track row index

  for (factorName in names(factorList)) {
    indicators <- factorList[[factorName]]

    for (i in seq_along(indicators)) {
      var <- indicators[i]
      paramName <- paste0("res_", var)
      paramIntercept <- paste0(paramName, "_0")

      moderatedTerms <- moderators
      if (var %in% names(removeSpecificModeration)) {
        moderatedTerms <- setdiff(moderatedTerms, paste0("data.", removeSpecificModeration[[var]]))
      }
      paramCoefs <- if (length(moderatedTerms) > 0) {
        paste0(paramName, "_", seq_along(moderatedTerms))
      } else {
        character(0)
      }

      if (removeModeration || (!is.null(removeModerationFor) && factorName %in% removeModerationFor) || var %in% removeModerationForVariables) {
        # Unmoderated residual: lavaan parameter is just "res_<var>"
        residualExpr <- paste0(var, " ~~ ", paramName, " * ", var)

        residualsList[[rowIndex]] <- list(
          variable             = var,
          residualParameter    = paramName,   # "res_<var>"
          residualCoefficient  = paramName,   # also "res_<var>" → matches subMat$name
          moderator            = gettext("Baseline")
        )
        rowIndex <- rowIndex + 1
      } else {
        residualsList[[rowIndex]] <- list(
          variable = var,
          residualParameter = paramName,
          residualCoefficient = paramIntercept,
          moderator = gettext("Baseline")
        )
        rowIndex <- rowIndex + 1

        if (length(paramCoefs) > 0) {
          for (j in seq_along(moderatedTerms)) {
            residualsList[[rowIndex]] <- list(
              variable = var,
              residualParameter = paramName,
              residualCoefficient = paramCoefs[j],
              moderator = moderatedTerms[j]
            )
            rowIndex <- rowIndex + 1
          }
        }

        moderationTerms <- if (length(paramCoefs) > 0) {
          paste0(paste0(moderatedTerms, " * ", paramCoefs), collapse = " + ")
        } else {
          ""
        }

        residualExpr <- paste0(var, " ~~ {", paramName, " := exp(", paramIntercept,
                               ifelse(moderationTerms != "", paste0(" + ", moderationTerms), ""),
                               ")} * ", var)
      }

      residualsBlock <- paste0(residualsBlock, "  ", residualExpr, "\n")
    }
  }

  residualsMapDf <- do.call(rbind, lapply(residualsList, as.data.frame))

  return(list(residualsBlock = residualsBlock, residualsMapDf = residualsMapDf))
}


.generateFactorCovariances <- function(factorList, moderators, removeModeration = FALSE) {
  factorNames <- names(factorList)
  numFactors <- length(factorNames)
  covarianceBlock <- gettext("# FACTOR COVARIANCES BLOCK \n")

  # Initialize an empty list to store covariance mappings
  covarianceList <- list()
  rowIndex <- 1  # Track row index for mapping

  # If there are exactly two factors, decide whether to apply moderation
  if (numFactors == 2) {
    factor1 <- factorNames[1]
    factor2 <- factorNames[2]

    if (removeModeration) {
      # Baseline-only moderation (rho := rho_0)
      covarianceBlock <- paste0(covarianceBlock, "  ", factor1, " ~~ cov * ", factor2, "\n")
      covarianceBlock <- paste0(covarianceBlock, "  !rho_0;\n")
      covarianceBlock <- paste0(covarianceBlock, "  rho := rho_0\n")
      covarianceBlock <- paste0(covarianceBlock, "  cov := (exp(2*rho) - 1)/(exp(2*rho) + 1)\n")

      covarianceList[[rowIndex]] <- list(
        covarianceParameter = "rho",
        covarianceCoefficient = "rho_0",
        moderator = gettext("Baseline")
      )
      rowIndex <- rowIndex + 1
    } else {
      # Moderated covariance case
      covarianceBlock <- paste0(covarianceBlock, "  ", factor1, " ~~ cov * ", factor2, "\n")

      # Define parameters dynamically based on the number of moderators
      numModerators <- length(moderators)
      rhParams <- paste0("!rho_", 0:numModerators, collapse = "; ")
      covarianceBlock <- paste0(covarianceBlock, "  ", rhParams, ";\n")

      # Construct the rho equation
      if (numModerators == 0) {
        rhoExpr <- "rho := rho_0"
      } else {
        rhoTerms <- paste0("rho_", 1:numModerators, " * data.", moderators, collapse = " + ")
        rhoExpr <- paste0("rho := rho_0 + ", rhoTerms)
      }
      covarianceBlock <- paste0(covarianceBlock, "  ", rhoExpr, "\n")

      # Apply the transformation for the covariance
      covarianceBlock <- paste0(covarianceBlock, "  cov := (exp(2*rho) - 1)/(exp(2*rho) + 1)\n")

      # Create the mapping for the covariance
      covarianceList[[rowIndex]] <- list(
        covarianceParameter = "rho",
        covarianceCoefficient = "rho_0",
        moderator = gettext("Baseline")
      )
      rowIndex <- rowIndex + 1

      if (numModerators > 0) {
        for (m in seq_along(moderators)) {
          covarianceList[[rowIndex]] <- list(
            covarianceParameter = "rho",
            covarianceCoefficient = paste0("rho_", m),
            moderator = moderators[m]
          )
          rowIndex <- rowIndex + 1
        }
      }
    }
  } else {
    # If more than two factors, no moderation (just simple covariance)
    for (i in 1:(numFactors - 1)) {
      for (j in (i + 1):numFactors) {
        factor1 <- factorNames[i]
        factor2 <- factorNames[j]
        covParam <- paste0("cov_", factor1, "_", factor2)

        covarianceBlock <- paste0(covarianceBlock, "  ", factor1, " ~~ ", covParam, " * ", factor2, "\n")

        covarianceList[[rowIndex]] <- list(
          covarianceParameter = covParam,
          covarianceCoefficient = "fixed",
          moderator = NA
        )
        rowIndex <- rowIndex + 1
      }
    }
  }

  # Convert list to DataFrame
  covarianceMapDf <- do.call(rbind, lapply(covarianceList, as.data.frame))

  return(list(covarianceBlock = covarianceBlock, covarianceMapDf = covarianceMapDf))
}


.waldCi <- function(estimate, se, alpha = 0.05) {
  zValue <- estimate / se
  pValue <- 2 * (1 - pnorm(abs(zValue)))
  zCrit <- qnorm(1 - alpha / 2)
  lowerBound <- estimate - zCrit * se
  upperBound <- estimate + zCrit * se

  result <- data.frame(
    lowerBound = lowerBound,
    upperBound = upperBound,
    pValue = pValue
  )

  return(result)
}

# .extractExcludedModerationItemsDf <- function(includeIndividualModerationsList) {
#   results <- list()
#
#   for (model in includeIndividualModerationsList) {
#     modelLabel <- if (!is.null(model$keyLabel)) model$keyLabel else NA
#     modelValue <- if (!is.null(model$keyValue)) model$keyValue else NA
#
#     for (typeBlock in model$moderationTypeList) {
#       modTypeLabel <- if (!is.null(typeBlock$keyLabel)) typeBlock$keyLabel else NA
#       modTypeValue <- if (!is.null(typeBlock$keyValue)) typeBlock$keyValue else NA
#
#       for (paramBlock in typeBlock$moderationParameterList) {
#         paramLabel <- if (!is.null(paramBlock$keyLabel)) paramBlock$keyLabel else NA
#         paramValue <- if (!is.null(paramBlock$keyValue)) paramBlock$keyValue else NA
#
#         for (item in paramBlock$moderationItemList) {
#           if (!isTRUE(item$includeIndividualModeration)) {
#             results[[length(results) + 1]] <- list(
#               modelLabel = modelLabel,
#               modelValue = modelValue,
#               modTypeLabel = modTypeLabel,
#               modTypeValue = modTypeValue,
#               paramLabel = paramLabel,
#               paramValue = paramValue,
#               itemValue = if (!is.null(item$value)) item$value else NA
#             )
#           }
#         }
#       }
#     }
#   }
#
#   if (length(results) == 0) return(NULL)
#   do.call(rbind, lapply(results, as.data.frame, stringsAsFactors = FALSE))
# }

.splitItemValue <- function(x) {
  if (is.list(x) && length(x) == 2) {
    list(
      itemType  = x[[1]],
      itemValue = x[[2]]
    )
  } else {
    list(
      itemType  = NA_character_,
      itemValue = x
    )
  }
}
.extractExcludedModerationItemsDf <- function(includeIndividualModerationsList) {

  results <- list()

  for (model in includeIndividualModerationsList) {
    modelLabel <- model$keyLabel %||% NA
    modelValue <- model$keyValue %||% NA

    for (typeBlock in model$moderationTypeList) {
      modTypeLabel <- typeBlock$keyLabel %||% NA
      modTypeValue <- typeBlock$keyValue %||% NA

      for (paramBlock in typeBlock$moderationParameterList) {
        paramLabel <- paramBlock$keyLabel %||% NA
        paramValue <- paramBlock$keyValue %||% NA

        for (item in paramBlock$moderationItemList) {

          include <- isTRUE(item[[1]])
          split   <- .splitItemValue(item[[2]])

          if (!include) {
            results[[length(results) + 1]] <- list(
              modelLabel   = modelLabel,
              modelValue   = modelValue,
              modTypeLabel = modTypeLabel,
              modTypeValue = modTypeValue,
              paramLabel   = paramLabel,
              paramValue   = paramValue,
              itemType     = split$itemType,
              itemValue    = split$itemValue
            )
          }
        }
      }
    }
  }

  if (length(results) == 0) return(NULL)

  do.call(
    rbind,
    lapply(results, as.data.frame, stringsAsFactors = FALSE)
  )
}

.extractIncludeIndividualModerationPaths <- function(includeIndividualModerationsList) {

  includedResults <- list()
  pathResults <- list()

  for (modelIndex in seq_along(includeIndividualModerationsList)) {
    model <- includeIndividualModerationsList[[modelIndex]]
    modelLabel <- model$keyLabel
    modelValue <- model$keyValue

    modelPaths <- list()

    for (modTypeIndex in seq_along(model$moderationTypeList)) {
      modType <- model$moderationTypeList[[modTypeIndex]]
      modTypeLabel <- modType$keyLabel
      modTypeValue <- modType$keyValue

      for (paramIndex in seq_along(modType$moderationParameterList)) {
        param <- modType$moderationParameterList[[paramIndex]]
        paramLabel <- param$keyLabel
        paramValue <- param$keyValue

        for (itemIndex in seq_along(param$moderationItemList)) {
          item <- param$moderationItemList[[itemIndex]]

          include <- isTRUE(item[[1]])
          split   <- .splitItemValue(item[[2]])

          path <- c(
            "includeIndividualModerationsList", modelIndex,
            "moderationTypeList", modTypeIndex,
            "moderationParameterList", paramIndex,
            "moderationItemList", itemIndex,
            "includeIndividualModeration"
          )

          modelPaths[[length(modelPaths) + 1]] <- path

          if (include) {
            includedResults[[length(includedResults) + 1]] <- list(
              modelLabel   = modelLabel,
              modelValue   = modelValue,
              modTypeLabel = modTypeLabel,
              modTypeValue = modTypeValue,
              paramLabel   = paramLabel,
              paramValue   = paramValue,
              itemType     = split$itemType,
              itemValue    = split$itemValue
            )
          }
        }
      }
    }

    if (!is.null(modelValue) && length(modelPaths) > 0) {
      pathResults[[modelValue]] <- modelPaths
    }
  }

  includedDf <- if (length(includedResults) > 0) {
    do.call(
      rbind,
      lapply(includedResults, as.data.frame, stringsAsFactors = FALSE)
    )
  } else {
    NULL
  }

  list(
    included = includedDf,
    paths    = pathResults
  )
}


.buildModeratedVariableSummary <- function(includeIndividualModerationsList) {
  summary <- list()

  for (model in includeIndividualModerationsList) {
    modelType <- if (!is.null(model$keyValue)) model$keyValue else "Unknown"
    moderationTypeList <- model$moderationTypeList

    for (modType in moderationTypeList) {
      modTypeName <- if (!is.null(modType$keyValue)) modType$keyValue else "Unknown"
      moderationParameterList <- modType$moderationParameterList

      for (param in moderationParameterList) {
        paramName <- if (!is.null(param$keyValue)) param$keyValue else "Unknown"
        moderationItemList <- param$moderationItemList

        for (item in moderationItemList) {
          if (isTRUE(item$includeIndividualModeration)) {
            variableName <- item$value

            summary[[modelType]][[modTypeName]][[paramName]] <-
              unique(c(summary[[modelType]][[modTypeName]][[paramName]], variableName))
          }
        }
      }
    }
  }

  return(summary)
}


.extractIncludedPlotItemsDf <- function(plotModelList) {
  results <- list()

  for (model in plotModelList) {
    modelName <- model$keyValue

    for (typeBlock in model$plotTypeList) {
      typeName <- typeBlock$keyValue

      for (param in typeBlock$plotParameterList) {
        parameterGroup <- param$keyValue

        for (item in param$plotItemList) {
          if (isTRUE(item$includePlot)) {
            results[[length(results) + 1]] <- list(
              modelName = modelName,
              plotType = typeName,
              parameterGroup = parameterGroup,
              plotModerator1 = if (is.list(item$plotModerator1)) item$plotModerator1$value else item$plotModerator1,
              plotModerator2 = if (is.list(item$plotModerator2)) item$plotModerator2$value else item$plotModerator2,
              value = if (is.list(item$value)) item$value$value else item$value
            )
          }
        }
      }
    }
  }

  if (length(results) == 0) return(NULL)
  do.call(rbind, lapply(results, as.data.frame, stringsAsFactors = FALSE))
}

.ensureImxReportProgress <- function() {
  # If imxReportProgress is not visible in the current environment,
  # but exists in the OpenMx namespace, copy it out.
  if (!exists("imxReportProgress", inherits = TRUE)) {
    if ("OpenMx" %in% loadedNamespaces() &&
        exists("imxReportProgress", where = asNamespace("OpenMx"), inherits = FALSE)) {
      fn <- get("imxReportProgress", envir = asNamespace("OpenMx"), inherits = FALSE)
      assign("imxReportProgress", fn, envir = .GlobalEnv)
    }
  }
}

.arrow <- function(x) {
  x <- as.character(x)
  x <- gsub("â†’", "->", x, fixed = TRUE)
  x <- gsub("\u2192", "->", x, fixed = TRUE)
  x
}

.mxSummaryFixed <- function(model, ...) {
  summ <- summary(model, ...)

  # Fix the parameter table names that your code matches on
  if (!is.null(summ$parameters) && "name" %in% colnames(summ$parameters)) {
    summ$parameters[, "name"] <- .arrow(summ$parameters[, "name"])
  }

  # (Optional) Fix rownames in any matrix-like outputs you later match by name
  if (!is.null(summ$SE) && !is.null(rownames(summ$SE))) {
    rownames(summ$SE) <- .arrow(rownames(summ$SE))
  }

  summ
}

