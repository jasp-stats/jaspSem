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

  sink("~/Downloads/log.txt")
  on.exit(sink(NULL))

  print("something")
  .storeOpenMxOptions(jaspResults)
  .restoreOpenMxOptions(jaspResults)

  print(options()[grep("^mx", names(options()), value = F)])

  ready <- length(unlist(lapply(options[["factors"]], `[[`, "indicators"), use.names = FALSE)) > 1 &&
    length(options[["moderators"]]) > 0

  if (!ready) {
    syncText <- createJaspHtml(text = gettext("Specify both factor indicator variables and moderator variables to run the analysis."))
    jaspResults[["syncText"]] <- syncText
    syncText$dependOn(c("factors", "moderators"))
    syncText$position <- 0.01
  }

  options <- .mnlfaPreprocessOptions(options)
  dataObj <- .mnlfaHandleData(jaspResults, dataset, options, ready)
  dataset <- dataObj$dataset
  options <- dataObj$options

  saveRDS(options, file = "~/Downloads/options.rds")
  saveRDS(dataset, file = "~/Downloads/dataset.rds")

  .mnlfaCreateContainer(jaspResults, options)
  .mnlfaCheckErrors(dataset, options, ready)

  dataTmp <- .mnlfaFitPerGroup(jaspResults, dataset, options, ready)


  # .mnlfaCreateGlobalInvarianceContainer(jaspResults, options)

  .mnlfaCallGlobalInvarianceTests(jaspResults, dataset, options, ready)

  # output
  # tables
  .mnlfaFitPerGroupTable(jaspResults, dataset, options, ready)
  .mnlfaGlobalInvarianceFitTable(jaspResults, dataset, options, ready)
  .mnlfaGlobalInvarianceParameterTables(jaspResults, dataset, options, ready)

  # plots
  .mnlfaPlot(jaspResults, dataset, options, ready)

  .mnlfaPrintSyntax(jaspResults, dataset, options, ready)

  .mnlfaAddGroupingVariableToData(jaspResults, dataTmp, options)


  return()
}

##### PREPROCESSING #####
# check for empty factors
.mnlfaPreprocessOptions <- function(options) {

  cleanedFactors <- lapply(options[["factors"]], function(x) {
    if (length(x[["indicators"]]) == 0) {
      return(NULL)
    } else {
      return(x)
    }
  })

  options[["factors"]] <- cleanedFactors[!sapply(cleanedFactors, is.null)]
  return(options)
}

# handle the data and options: add interactions and higher power effects to the data and options
.mnlfaHandleData <- function(jaspResults, dataset, options, ready) {

  if (!is.null(jaspResults[["dataState"]])) {
    return(list(dataset = jaspResults[["dataState"]][["object"]], options = options))
  }

  if (!ready) return(list(dataset = dataset, options = options))


  # convert the whole data to numeric
  dataset <- as.data.frame(lapply(dataset, function(x) as.numeric(as.character(x))))

  # scale the continuous moderators
  mods <- unlist(lapply(options[["moderators"]], `[[`, "variable"), use.names = FALSE)
  if (length(mods) == 0) return(list(dataset = dataset, options = options))
  mods.types <- options[["moderators.types"]]
  for (i in 1:length(mods)) {
    if (mods.types[i] != "nominal") {
      # this also forces also ordinal moderator variables to be numeric, do we want this???????????????????????????????????
      dataset[[mods[i]]] <- scale(as.numeric(as.character(dataset[[mods[i]]])))
    }
  }

  # add interactions and extra effects
  # already create the interaction variables, even if we dont need them.
  if (length(mods) > 1 && options[["addInteractionTerms"]]) {
    inters <- combn(mods, 2)
    interTypes <- combn(mods.types, 2)
    for (i in 1:ncol(inters)) {
      tmp1 <- as.numeric(as.character(dataset[[inters[1, i]]])) # needed for nominal moderators
      tmp2 <- as.numeric(as.character(dataset[[inters[2, i]]]))
      tmpDt <- data.frame(tmp1 * tmp2)
      interNames <- jaspBase::decodeColNames(c(inters[1, i], inters[2, i]))
      tmpName <- paste0(interNames[1], "_x_", interNames[2])
      colnames(tmpDt) <- tmpName
      dataset <- cbind(dataset, tmpDt)
      options[["moderators"]] <- append(options[["moderators"]], list(list(cubicEffect = FALSE, squaredEffect = FALSE,
                                                                           variable = tmpName)))
      if (sum(interTypes[, i] == "scale") > 0)
        options[["moderators.types"]] <- append(options[["moderators.types"]], "scale")
      else
        options[["moderators.types"]] <- append(options[["moderators.types"]], "nominal")
    }
  }

  # add squares and cubic effects
  squares <- sapply(options[["moderators"]], function(x) x[["squaredEffect"]])
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
      options[["moderators"]] <- append(options[["moderators"]], list(list(cubicEffect = FALSE, squaredEffect = FALSE,
                                                                           variable = tmpName)))
      options[["moderators.types"]] <- append(options[["moderators.types"]], squaredTypes[i])
    }
  }
  cubics <- sapply(options[["moderators"]], function(x) x[["cubicEffect"]])
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
      options[["moderators"]] <- append(options[["moderators"]], list(list(cubicEffect = FALSE, squaredEffect = FALSE,
                                                                           variable = tmpName)))
      options[["moderators.types"]] <- append(options[["moderators.types"]], cubicTypes[i])
    }
  }

  dataState <- dataset
  dataState <- createJaspState(dataState)
  dataState$dependOn(options = c("factors", "moderators", "addInteractionTerms", "squaredEffect", "cubicEffect"))
  jaspResults[["dataState"]] <- dataState

  return(list(dataset = dataset, options = options))
}

.mnlfaCheckErrors <- function(dataset, options, ready) {

  return()
}

# Create a main container for everything
.mnlfaCreateContainer <- function(jaspResults, options) {
  if (is.null(jaspResults[["mainContainer"]])) {
    jaspResults[["mainContainer"]] <- createJaspContainer()
    jaspResults[["mainContainer"]]$dependOn(options = c(
      "factors", "moderators", "addInteractionTerms", "squaredEffect", "cubicEffect",
      "meanstructure", "se", "modelIdentification", "factorsUncorrelated",
      "interceptsFixedToZero", "packageMimiced", "estimator", "naAction"))
    jaspResults[["mainContainer"]]$position <- 1
  }
  return()
}


##### ANALYSIS #####
# test configural invariance with the CFA module
.mnlfaFitPerGroup <- function(jaspResults, dataset, options, ready) {

  if (!ready) return()
  if (!is.null(jaspResults[["mainContainer"]][["fitPerGroupState"]])) return()
  if (!options[["fitPerGroup"]]) return()

  moderatorNames <- sapply(options[["moderators"]], function(x) x[["variable"]])
  moderatorTypes <- options[["moderators.types"]]

  nominalModerators <- jaspBase::decodeColNames(moderatorNames[moderatorTypes == "nominal"])
  nonNominalModerators <- moderatorNames[moderatorTypes != "nominal"]

  if (length(nonNominalModerators) > 0) {
    nSplits <- options[["continuousVariableSplit"]]
    for (i in 1:length(nonNominalModerators)) {
      tmp <- as.numeric(as.character(dataset[[nonNominalModerators[i]]])) # in cases where the variable is ordinal
      modRange <- range(tmp)
      modSplits <- seq(modRange[1], modRange[2], length.out = nSplits + 1)
      dataset[[paste0(nonNominalModerators[i], "_nominal")]] <- cut(tmp,
                                                                   breaks = modSplits,
                                                                   labels = as.character(1:nSplits),
                                                                   include.lowest = TRUE)
    }
    nonNominalModerators <- jaspBase::decodeColNames(nonNominalModerators)
    colsToCombine <- c(nominalModerators, paste0(nonNominalModerators, "_nominal"))
  } else {
    colsToCombine <- nominalModerators
  }

  ogColnames <- colnames(dataset)
  colnames(dataset) <- jaspBase::decodeColNames(colnames(dataset))
  dataset$addedGroupVar <- NA
  for (rr in 1:nrow(dataset)) {
    dataset$addedGroupVar[rr] <- paste0(colsToCombine, "_", dataset[rr, colsToCombine], collapse = ":")
  }

  colnames(dataset) <- c(ogColnames, "addedGroupVar")

  # this part is from jaspFactor
  cfaResult <- list()
  options$seType <- "default"
  cfaResult[["spec"]] <- jaspFactor:::.cfaCalcSpecs(dataset, options)

  # we fit a model per group so we have access to all the fit indices, which we would not have if we would use
  # the lavaan built-in group functionality
  options$group <- ""
  options$invarianceTesting <- NULL
  mod <- jaspFactor:::.optionsToCFAMod(options, dataset, cfaResult)$model
  groups <- unique(dataset$addedGroupVar)
  fitArgs <- list(model         = mod,
                data            = NULL,
                se              = cfaResult[["spec"]]$se,
                std.lv          = TRUE,
                orthogonal      = options$factorsUncorrelated,
                missing         = "default")

  result <- list()
  for (i in 1:length(groups)) {
    fitArgs$data <- dataset[dataset$addedGroupVar == groups[i], ]
    fit <- try(do.call(lavaan::cfa, fitArgs))
    result[[groups[i]]] <- fit
  }

  fitPerGroupResult <- createJaspState(result)
  fitPerGroupResult$dependOn(options = c("continuousVariableSplit", "fitPerGroup"))
  jaspResults[["mainContainer"]][["fitPerGroupState"]] <- fitPerGroupResult

  return(dataset)
}

.mnlfaCallGlobalInvarianceTests <- function(jaspResults, dataset, options, ready) {

  if (!ready) return()

  tests <- c(options[["configuralInvariance"]], options[["metricInvariance"]], options[["scalarInvariance"]],
             options[["strictInvariance"]])
  if (sum(tests) == 0) return()

  testNames <- c("configural", "metric", "scalar", "strict")
  testNames <- testNames[tests]

  for (i in 1:length(testNames)) {
    .mnlfaGlobalInvarianceTestHelper(jaspResults, dataset, options, testNames[i])
  }

  return()
}


.mnlfaGlobalInvarianceTestHelper <- function(jaspResults, dataset, options, testName) {

  state <- paste0(testName, "InvState")
  if (!is.null(jaspResults[["mainContainer"]][[state]])) return()

  factorList <- lapply(options[["factors"]], function(x) x[["indicators"]])
  factorNames <- sapply(options[["factors"]], function(x) x[["name"]])
  names(factorList) <- factorNames
  moderatorNames <- moderatorsOriginal <- sapply(options[["moderators"]], function(x) x[["variable"]])

  modelObj <- .generateSyntax(factorList, moderatorNames, type = testName, options)

  script <- mxsem::mxsem(model = modelObj$model, data = dataset, scale_loadings = FALSE, scale_latent_variances = FALSE)

  fit <- try(OpenMx::mxRun(script))

  fitState <- createJaspState(fit)
  fitState$dependOn(options = c(paste0(testName, "Invariance")))
  jaspResults[["mainContainer"]][[state]] <- fitState

  # also save the model and mapping
  mapList <- modelObj$map
  modelState <- createJaspState(list(model = modelObj$model, map = mapList))
  modelState$dependOn(optionsFromObject = jaspResults[["mainContainer"]][[state]])
  jaspResults[["mainContainer"]][[paste0(testName, "InvModelState")]] <- modelState

  return()
}



##### OUTPUT #####
.mnlfaFitPerGroupTable <- function(jaspResults, dataset, options, ready) {

  if (!is.null(jaspResults[["mainContainer"]][["fitPerGroupTable"]])) return()
  if (!ready) return()
  if (!options[["fitPerGroup"]]) return()

  fitPerGroupTable <- createJaspTable(gettext("Fit per Group Test"))
  fitPerGroupTable$position <- 1
  fitPerGroupTable$dependOn(optionsFromObject = jaspResults[["mainContainer"]][["fitPerGroupState"]])
  fitPerGroupTable$addColumnInfo(name = "model", title = gettext("Group Model"), type = "string")

  jaspResults[["mainContainer"]][["fitPerGroupTable"]] <- fitPerGroupTable

  result <- jaspResults[["mainContainer"]][["fitPerGroupState"]][["object"]]
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

.mnlfaGlobalInvarianceFitTable <- function(jaspResults, dataset, options, ready) {
  if (!ready) return()
  if (!is.null(jaspResults[["mainContainer"]][["invFitTable"]])) return()

  invFitTable <- createJaspTable(gettext("Global Invariance Fit"))
  invFitTable$position <- 2
  invFitTable$dependOn(optionsFromObject = jaspResults[["mainContainer"]],
                       options = c("addInteractionTerms", "configuralInvariance", "metricInvariance",
                                   "scalarInvariance", "strictInvariance",
                                   "squareEffect", "cubicEffect"))
  invFitTable$addColumnInfo(name = "type", title = gettext("Type"), type = "string")
  jaspResults[["mainContainer"]][["invFitTable"]] <- invFitTable

  results <- list(Configural = jaspResults[["mainContainer"]][["configuralInvState"]][["object"]],
                  Metric = jaspResults[["mainContainer"]][["metricInvState"]][["object"]],
                  Scalar = jaspResults[["mainContainer"]][["scalarInvState"]][["object"]],
                  Strict = jaspResults[["mainContainer"]][["strictInvState"]][["object"]])

  results <- results[sapply(results, function(x) !is.null(x))]
  saveRDS(results, "~/Downloads/results.rds")

  if (length(results) == 0) {
    invFitTable$addFootnote(gettext("Choose one of the global invariance tests to perform the test."))
    return()
  }

  invFitTable$addColumnInfo(name = "N", title = gettext("n(Parameters)"), type = "integer")
  invFitTable$addColumnInfo(name = "AIC", title = gettext("AIC"), type = "number")
  invFitTable$addColumnInfo(name = "BIC", title = gettext("BIC"), type = "number")
  invFitTable$addColumnInfo(name = "SABIC", title = gettext("SABIC"), type = "number")
  dtFill <- data.frame(type = c(), N = c(), AIC = c(), BIC = c(), SABIC = c())
  if (length(results) > 1) {

    dtFill$BF <- dtFill$diffLL <- dtFill$diffdf <- dtFill$p <- c()
    invFitTable$addColumnInfo(name = "BF", title = gettext("BF(10)"), type = "number")
    invFitTable$addColumnInfo(name = "diffLL", title = gettext("\u0394(LL)"), type = "number")
    invFitTable$addColumnInfo(name = "diffdf", title = gettext("\u0394(df)"), type = "integer")
    invFitTable$addColumnInfo(name = "p", title = gettext("p"), type = "pvalue")
  }
  errmsg <- ""
  for (i in 1:length(results)) {

    if (isTryError(results[[i]])) {
      errTmp <- jaspBase::.extractErrorMessage(results[[i]])
      errmsg <- gettextf("%1$s Error in fitting the %2$s model. Internal error message(s): %3$s",
                         errmsg, names(results)[i], errTmp)
      dtAdd <- data.frame(type = names(results)[i],
                          N = NA,
                          AIC = NA,
                          BIC = NA,
                          SABIC = NA)
    } else {
      summ <- summary(results[[i]])
      sabic <- .sabic(summ$BIC.Mx, summ$estimatedParameters, nrow(dataset))
      dtAdd <- data.frame(type = names(results)[i],
                          N = summ$estimatedParameters,
                          AIC = summ$AIC.Mx,
                          BIC = summ$BIC.Mx,
                          SABIC = sabic)
      if (length(results) > 1) {
        if (i == 1) {
          dtAdd$BF <- dtAdd$p <- dtAdd$diffdf <- dtAdd$diffLL <- NA
        } else {
          comp <- OpenMx::mxCompare(results[[i-1]], results[[i]])
          BF10 <- exp((summary(results[[i-1]])$BIC.Mx - summ$BIC.Mx) / 2)
          dtAdd$BF <- BF10
          dtAdd$diffLL <- comp$diffLL[2]
          dtAdd$diffdf <- comp$diffdf[2]
          dtAdd$p <- comp$p[2]
        }
      }
    }

    dtFill <- rbind(dtFill, dtAdd)
  }

  invFitTable$setData(dtFill)
  invFitTable$addFootnote(errmsg)

  return()

}

.mnlfaGlobalInvarianceParameterTables <- function(jaspResults, dataset, options, ready) {
  if (!ready) return()
  if (!is.null(jaspResults[["mainContainer"]][["globalParameterContainer"]])) return()

  results <- list(Configural = jaspResults[["mainContainer"]][["configuralInvState"]][["object"]],
                  Metric = jaspResults[["mainContainer"]][["metricInvState"]][["object"]],
                  Scalar = jaspResults[["mainContainer"]][["scalarInvState"]][["object"]],
                  Strict = jaspResults[["mainContainer"]][["strictInvState"]][["object"]])

  results <- results[sapply(results, function(x) !is.null(x))]

  if (length(results) == 0) return()

  globalParameterContainer <- createJaspContainer(gettext("Parameter Estimates"), initCollapsed = TRUE)
  globalParameterContainer$position <- 2
  globalParameterContainer$dependOn(optionsFromObject = jaspResults[["mainContainer"]][["invFitTable"]],
                                    options = c("loadingEstimates", "interceptEstimates", "residualVarianceEstimates",
                                            "factorVarianceEstimates", "factorMeanEstimates",
                                            "factorCovarianceEstimates", "alphaLevel"))
  jaspResults[["mainContainer"]][["globalParameterContainer"]] <- globalParameterContainer

  # get the mappings
  mapResults <- list(Configural = jaspResults[["mainContainer"]][["configuralInvModelState"]][["object"]][["map"]],
                      Metric = jaspResults[["mainContainer"]][["metricInvModelState"]][["object"]][["map"]],
                      Scalar = jaspResults[["mainContainer"]][["scalarInvModelState"]][["object"]][["map"]],
                      Strict = jaspResults[["mainContainer"]][["strictInvModelState"]][["object"]][["map"]])
  mapResults <- mapResults[sapply(mapResults, function(x) !is.null(x))]

  saveRDS(mapResults, "~/Downloads/mapResults.rds")

  for (i in 1:length(results)) {

    if (isTryError(results[[i]])) {
      errorContainer <- createJaspContainer(names(results)[i])
      errorContainer$setError(gettextf("The %s model could not be fitted", tolower(names(results)[i])))
      globalParameterContainer[[names(results)[i]]] <- errorContainer
    } else {
      fitSummary <- summary(results[[i]])
      paramTable <- fitSummary$parameters
      globalParameterContainer[[names(results)[i]]] <- .mnlfaParameterTableHelper(paramTable,
                                                                                  names(results)[i],
                                                                                  mapResults[[i]],
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

  if (options[["loadingEstimates"]]) {
    # Loadings
    loadPosition <- grepl("^load_", parNames)
    # special case cause when there is no moderation for loadings they are estimated but named weirdly
    loadPositionNoMod <- grepl("→", parNames)
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
                           overtitle = gettextf("%s%% Confidence Interval", (1 - options$alphaLevel) * 100))
      loadTable$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                           overtitle = gettextf("%s%% Confidence Interval", (1 - options$alphaLevel) * 100))

      loadMap <- mapResult$loadings
      loadMap <- loadMap[loadMap$loadingParameter != "fixed_1", ]
      # Replace values in the loadingsTable based on fTitleMapping
      for (mapping in fTitleMapping) {
        loadMap$factor[loadMap$factor == mapping[1]] <- mapping[2]
      }

      subMat <- paramTable[allLoads, ]
      ciObj <- .waldCi(subMat[, "Estimate"], subMat[, "Std.Error"], options$alphaLevel)
      df <- data.frame(factor = loadMap$factor,
                       indicator = loadMap$variable,
                       effect = sub("^data.", "", loadMap$moderator),
                       # param = sub("^load_", "", loadMap$loadingCoefficient),
                       est = subMat[, "Estimate"],
                       se = subMat[, "Std.Error"],
                       pvalue = ciObj$pValue,
                       ci.lower = ciObj$lowerBound,
                       ci.upper = ciObj$upperBound)
      loadTable$setData(df)
    }
  }

  if (options[["interceptEstimates"]]) {

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
                              overtitle = gettextf("%s%% Confidence Interval", (1 - options$alphaLevel) * 100))
      intTable$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                              overtitle = gettextf("%s%% Confidence Interval", (1 - options$alphaLevel) * 100))

      intMap <- mapResult$intercepts

      subMat <- paramTable[intPosition, ]
      ciObj <- .waldCi(subMat[, "Estimate"], subMat[, "Std.Error"], options$alphaLevel)
      df <- data.frame(indicator = intMap$variable,
                       effect = sub("^data.", "", intMap$moderator),
                       # param = sub("^int_", "", intMap$interceptCoefficient),
                       est = subMat[, "Estimate"],
                       se = subMat[, "Std.Error"],
                       pvalue = ciObj$pValue,
                       ci.lower = ciObj$lowerBound,
                       ci.upper = ciObj$upperBound)
      intTable$setData(df)
    }
  }

  if (options[["residualVarianceEstimates"]]) {

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
                             overtitle = gettextf("%s%% Confidence Interval", (1 - options$alphaLevel) * 100))
      resTable$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                             overtitle = gettextf("%s%% Confidence Interval", (1 - options$alphaLevel) * 100))

      resMap <- mapResult$residualVariances

      subMat <- paramTable[resPosition, ]
      ciObj <- .waldCi(subMat[, "Estimate"], subMat[, "Std.Error"], options$alphaLevel)
      df <- data.frame(indicator = resMap$variable,
                       effect = sub("^data.", "", resMap$moderator),
                       # param = sub("^res_", "", resMap$residualCoefficient),
                       est = subMat[, "Estimate"],
                       se = subMat[, "Std.Error"],
                       pvalue = ciObj$pValue,
                       ci.lower = ciObj$lowerBound,
                       ci.upper = ciObj$upperBound)
      resTable$setData(df)
    }
  }

  if (options[["factorVarianceEstimates"]]) {
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
                             overtitle = gettextf("%s%% Confidence Interval", (1 - options$alphaLevel) * 100))
      fvTable$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                             overtitle = gettextf("%s%% Confidence Interval", (1 - options$alphaLevel) * 100))

      fvMap <- mapResult$variances
      # Replace values in the fvTable based on fTitleMapping
      for (mapping in fTitleMapping) {
        fvMap$factor[fvMap$factor == mapping[1]] <- mapping[2]
      }

      subMat <- paramTable[fvPosition, ]
      ciObj <- .waldCi(subMat[, "Estimate"], subMat[, "Std.Error"], options$alphaLevel)
      df <- data.frame(factor = fvMap$factor,
                       effect = sub("^data.", "", fvMap$moderator),
                       # param = sub("^var_", "", fvMap$varianceCoefficient),
                       est = subMat[, "Estimate"],
                       se = subMat[, "Std.Error"],
                       pvalue = ciObj$pValue,
                       ci.lower = ciObj$lowerBound,
                       ci.upper = ciObj$upperBound)
      fvTable$setData(df)
    }
  }

  if (options[["factorMeanEstimates"]]) {
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
                            overtitle = gettextf("%s%% Confidence Interval", (1 - options$alphaLevel) * 100))
      fmTable$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                            overtitle = gettextf("%s%% Confidence Interval", (1 - options$alphaLevel) * 100))

      fmMap <- mapResult$means
      # Replace values in the fmTable based on fTitleMapping
      for (mapping in fTitleMapping) {
        fmMap$factor[fmMap$factor == mapping[1]] <- mapping[2]
      }

      subMat <- paramTable[fmPosition, ]
      ciObj <- .waldCi(subMat[, "Estimate"], subMat[, "Std.Error"], options$alphaLevel)
      df <- data.frame(factor = fmMap$factor,
                       effect = sub("^data.", "", fmMap$moderator),
                       # param = sub("^mean_", "", fmMap$meanCoefficient),
                       est = subMat[, "Estimate"],
                       se = subMat[, "Std.Error"],
                       pvalue = ciObj$pValue,
                       ci.lower = ciObj$lowerBound,
                       ci.upper = ciObj$upperBound)
      fmTable$setData(df)
    }
  }

  if (options[["factorCovarianceEstimates"]]) {
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
                            overtitle = gettextf("%s%% Confidence Interval", (1 - options$alphaLevel) * 100))
      covTable$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                            overtitle = gettextf("%s%% Confidence Interval", (1 - options$alphaLevel) * 100))

      covMap <- mapResult$covariances
      subMat <- paramTable[covPosition, ]
      ciObj <- .waldCi(subMat[, "Estimate"], subMat[, "Std.Error"], options$alphaLevel)
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
                         options = c("plotModelList"))
  jaspResults[["plotContainer"]] <- plotContainer

  mods <- unlist(lapply(options[["moderators"]], `[[`, "variable"), use.names = FALSE)
  mods.types <- options[["moderators.types"]]

  plotModelList <- options[["plotModelList"]]
  # only keep the elements where includePlot is true
  filteredPlots <- .extractIncludedPlotItemsDf(plotModelList)

  if (is.null(filteredPlots)) {
    return()
  }

  translatedNames <- .translatedElements()

  for (i in 1:nrow(filteredPlots)) {
    currentRow <- filteredPlots[i, ]
    modelName <- currentRow$modelName
    modelNameR <- translatedNames$rNames[translatedNames$guiNames == modelName]
    fit <- jaspResults[["mainContainer"]][[paste0(modelNameR, "InvState")]][["object"]]
    mapResult <- jaspResults[["mainContainer"]][[paste0(modelNameR, "InvModelState")]][["object"]][["map"]]
    paramTable <- summary(fit)$parameters
    parameterGroup <- currentRow$parameterGroup
    parameterGroupR <- translatedNames$parTableNames[translatedNames$guiNames == parameterGroup]

    parNames <- paramTable[, "name"]
    parPosition <- grepl(paste0("^", parameterGroupR, "_"), parNames)
    if (sum(parPosition) > 0) {
      subMat <- paramTable[parPosition, ]
      parName <- translatedNames$rNames[translatedNames$guiNames == parameterGroup]
      map <- mapResult[[parName]]
      subMap <- map[map$variable == currentRow$value, ]
      currentMods <- sub("^data.", "", subMap$moderator)
      modsForPlots <- c(currentRow$plotMod1, currentRow$plotMod2)
      modsForPlots <- modsForPlots[modsForPlots != ""]
      modsForPlots <- gsub(":", "_x_", modsForPlots) # for interactions
      matchIndices <- match(modsForPlots, currentMods)
      matchIndices <- matchIndices[!is.na(matchIndices)]
      coefficientColumnIndex <- grep("Coefficient", colnames(subMap))
      modEstimates <- c(subMat$Estimate[match(subMap[1, coefficientColumnIndex], subMat$name)], # Baseline
                        subMat$Estimate[match(subMap[matchIndices, coefficientColumnIndex], subMat$name)]) # the other estimates

      dtSub <- dataset[, modsForPlots, drop = FALSE]
      outValues <- modEstimates[1] + rowSums(sweep(dtSub, 2, modEstimates[-1], `*`))
      dtSub[["estimatedValue"]] <- outValues

      if (length(modsForPlots) == 1) { # only one moderator
        if (mods.types[mods == modsForPlots] == "scale") {
          gg <- ggplot2::ggplot(data = dtSub,
                                ggplot2::aes(x = .data[[modsForPlots]],
                                             y = estimatedValue)) +
            ggplot2::ylab(gettext("Parameter Value")) +
            jaspGraphs::themeJaspRaw() +
            jaspGraphs::geom_rangeframe(size = 1.1) +
            ggplot2::geom_line()
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

  ##### a lot to do still...:
  #' - special cases
  #' - do the factors separately


}

.mnlfaPrintSyntax <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["syntaxContainer"]])) return()
  if (!options$showSyntax) return()
  if (!ready) return()

  syntaxContainer <- createJaspContainer(gettext("Model Syntax"), initCollapsed = TRUE)
  syntaxContainer$dependOn(options = c("showSyntax"),
                           optionsFromObject = jaspResults[["mainContainer"]][["globalParameterContainer"]])
  jaspResults[["syntaxContainer"]] <- syntaxContainer

  models <- list(Configural = jaspResults[["mainContainer"]][["configuralInvModelState"]][["object"]][["model"]],
                 Metric = jaspResults[["mainContainer"]][["metricInvModelState"]][["object"]][["model"]],
                 Scalar = jaspResults[["mainContainer"]][["scalarInvModelState"]][["object"]][["model"]],
                 Strict = jaspResults[["mainContainer"]][["strictInvModelState"]][["object"]][["model"]])
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

  if (!is.null(jaspResults[["addedGroupVarContainer"]]) ||
      !options[["addGroupVar"]])
  {
    return()
  }

  container    <- createJaspContainer()
  container$dependOn(optionsFromObject = jaspResults[["maincontainer"]][["fitPerGroupState"]],
                     options = "addGroupVar")

  var <- dataset[["addedGroupVar"]]

  colNameR <- gettext("addedGroupVar")
  if (jaspBase:::columnExists(colNameR) && !jaspBase:::columnIsMine(colNameR)) {
    .quitAnalysis(gettextf("Column name %s already exists in the dataset", colNameR))
  }

  container[["addedGroupVar"]] <- jaspBase::createJaspColumn(colNameR)
  container[["addedGroupVar"]]$setNominal(var)

  jaspResults[["addedGroupVarContainer"]] <- container

  # check if there are previous colNames that are not needed anymore and delete the cols
  oldNames <- jaspResults[["createdColumnNames"]][["object"]]
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
  jaspResults[["createdColumnNames"]] <- createJaspState(newNames)

  return()

}

.mnlfaTranslateNames <- function(jaspResults, dataset, options, ready) {

  if (!ready) return()


  return()
}

##### HERE: When creating the syntax it might make sense to use the actual factor names, or amybe not, because they might have spaces and weird symbols.
.generateSyntax <- function(factorList, moderators, type, options) {

  rmvLoadMod <- FALSE
  rmvIntMod <- FALSE
  rmvResMod <- FALSE
  factorNamesVars <- names(factorList)
  factorNamesMeans <- names(factorList)

  if (type == "configural") {
    factorNamesVars <- NULL
    factorNamesMeans <- NULL
  }

  if (type == "metric") {
    rmvLoadMod <- TRUE
    factorNamesMeans <- NULL
  }

  if (type == "scalar") {
    rmvLoadMod <- TRUE
    rmvIntMod <- TRUE
  }

  if (type == "strict") {
    rmvLoadMod <- TRUE
    rmvIntMod <- TRUE
    rmvResMod <- TRUE
    factorNamesVars <- NULL
    factorNamesMeans <- NULL
  }


  loadingsObj <- .generateLoadings(factorList, moderators, removeModeration = rmvLoadMod)
  loadingsBlock <- loadingsObj$loadingsBlock
  interceptsObj <- .generateIntercepts(factorList, moderators, removeModeration = rmvIntMod)
  interceptsBlock <- interceptsObj$interceptsBlock
  residualsObj <- .generateResidualVariances(factorList, moderators, removeModeration = rmvResMod)
  residualsBlock <- residualsObj$residualsBlock
  variancesObj <- .generateFactorVariances(factorList, moderators, moderate = factorNamesVars)
  variancesBlock <- variancesObj$variancesBlock
  meansObj <- .generateFactorMeans(factorList, moderators, moderate = factorNamesMeans)
  meansBlock <- meansObj$meansBlock

  if (length(names(factorList)) == 2) {
    covarianceObj <-.generateFactorCovariances(factorList, moderators)
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
                              removeModerator = NULL,
                              scaleIndicatorLoading = FALSE) {
  loadingsBlock <- gettext("# LOADINGS BLOCK \n")
  loadingsList <- list()  # Store mappings

  moderators <- paste0("data.", moderators)
  if (!is.null(removeModerator)) {
    moderators <- setdiff(moderators, paste0("data.", removeModerator))
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

        loadingsList[[rowIndex]] <- list(
          factor = factorName,
          variable = var,
          loadingParameter = paramName,
          loadingCoefficient = NA,
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
                                     removeModerator = NULL,
                                     scaleFactorVariance = TRUE) {

  variancesBlock <- gettext("# FACTOR VARIANCES BLOCK \n")
  variancesList <- list()  # Store mappings of moderators only

  moderators <- paste0("data.", moderators)
  if (!is.null(removeModerator)) {
    moderators <- setdiff(moderators, paste0("data.", removeModerator))
  }

  rowIndex <- 1  # Track row index

  for (factorName in names(factorList)) {
    applyModeration <- !is.null(moderate) && factorName %in% moderate

    if (!applyModeration || (scaleFactorVariance && !applyModeration)) {
      # No moderation: scaleFactorVariance = TRUE and factor not in moderate
      variancesBlock <- paste0(variancesBlock, "  ", factorName, " ~~ 1 * ", factorName, "\n")
      next
    }

    # Build moderation terms
    moderatedTerms <- moderators
    if (factorName %in% names(removeSpecificModeration)) {
      moderatedTerms <- setdiff(moderatedTerms, paste0("data.", removeSpecificModeration[[factorName]]))
    }

    paramName <- paste0("var_", factorName)
    paramIntercept <- paste0(paramName, "_0")
    paramCoefs <- if (length(moderatedTerms) > 0) {
      paste0(paramName, "_", seq_along(moderatedTerms))
    } else {
      character(0)
    }

    # Only store intercept if needed
    if (!scaleFactorVariance) {
      variancesList[[rowIndex]] <- list(
        factor = factorName,
        varianceParameter = paramName,
        varianceCoefficient = paramIntercept,
        moderator = gettext("Baseline")
      )
      rowIndex <- rowIndex + 1
    }

    if (length(paramCoefs) > 0) {
      for (j in seq_along(moderatedTerms)) {
        variancesList[[rowIndex]] <- list(
          factor = factorName,
          varianceParameter = paramName,
          varianceCoefficient = paramCoefs[j],
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

    varianceExpression <- if (scaleFactorVariance) {
      paste0("exp(", moderationTerms, ")")
    } else {
      paste0("exp(", paramIntercept,
             ifelse(moderationTerms != "", paste0(" + ", moderationTerms), ""),
             ")")
    }

    variancesBlock <- paste0(variancesBlock, "  ", factorName, " ~~ {", paramName, " := ", varianceExpression, "} * ", factorName, "\n")
  }


  variancesMapDf <- do.call(rbind, lapply(variancesList, as.data.frame))

  return(list(variancesBlock = variancesBlock, variancesMapDf = variancesMapDf))
}


.generateFactorMeans <- function(factorList,
                                 moderators,
                                 moderate = NULL,
                                 removeSpecificModeration = list(),
                                 removeModerator = NULL,
                                 fixMeanZero = TRUE) {
  meansBlock <- gettext("# FACTOR MEANS BLOCK \n")
  meansList <- list()

  moderators <- paste0("data.", moderators)
  if (!is.null(removeModerator)) {
    moderators <- setdiff(moderators, paste0("data.", removeModerator))
  }

  rowIndex <- 1

  for (factorName in names(factorList)) {
    applyModeration <- !is.null(moderate) && factorName %in% moderate

    if (!applyModeration) {
      meansBlock <- paste0(meansBlock, "  ", factorName, " ~ 0 * 1\n")
      next
    }

    # Apply moderation
    moderatedTerms <- moderators
    if (factorName %in% names(removeSpecificModeration)) {
      moderatedTerms <- setdiff(moderatedTerms, paste0("data.", removeSpecificModeration[[factorName]]))
    }

    paramName <- paste0("mean_", factorName)
    paramIntercept <- paste0(paramName, "_0")
    paramCoefs <- if (length(moderatedTerms) > 0) {
      paste0(paramName, "_", seq_along(moderatedTerms))
    } else {
      character(0)
    }

    # Add intercept if not fixing to zero
    if (!fixMeanZero) {
      meansList[[rowIndex]] <- list(
        factor = factorName,
        meanParameter = paramName,
        meanCoefficient = paramIntercept,
        moderator = gettext("Baseline")
      )
      rowIndex <- rowIndex + 1
    }

    if (length(paramCoefs) > 0) {
      for (j in seq_along(moderatedTerms)) {
        meansList[[rowIndex]] <- list(
          factor = factorName,
          meanParameter = paramName,
          meanCoefficient = paramCoefs[j],
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

    meanExpression <- if (fixMeanZero) {
      paste0(moderationTerms)
    } else {
      paste0(paramIntercept,
             ifelse(moderationTerms != "", paste0(" + ", moderationTerms), ""))
    }

    meansBlock <- paste0(meansBlock, "  ", factorName, " ~ {", paramName, " := ", meanExpression, "} * 1\n")
  }

  meansMapDf <- do.call(rbind, lapply(meansList, as.data.frame))

  return(list(meansBlock = meansBlock, meansMapDf = meansMapDf))
}


.generateIntercepts <- function(factorList,
                                moderators,
                                removeModeration = FALSE,
                                removeModerationFor = NULL,
                                removeModerationForVariables = c(),
                                removeSpecificModeration = list(),
                                removeModerator = NULL) {
  interceptsBlock <- gettext("# INTERCEPTS BLOCK \n")
  interceptsList <- list()  # Store mappings

  moderators <- paste0("data.", moderators)
  if (!is.null(removeModerator)) {
    moderators <- setdiff(moderators, paste0("data.", removeModerator))
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
        interceptExpr <- paste0(var, " ~ ", paramName, " * 1")

        interceptsList[[rowIndex]] <- list(
          variable = var,
          interceptParameter = paramName,
          interceptCoefficient = NA,
          moderator = NA
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
                                       removeModerator = NULL) {
  residualsBlock <- gettext("# RESIDUAL VARIANCES BLOCK \n")
  residualsList <- list()  # Store mappings

  moderators <- paste0("data.", moderators)
  if (!is.null(removeModerator)) {
    moderators <- setdiff(moderators, paste0("data.", removeModerator))
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
        residualExpr <- paste0(var, " ~~ ", paramName, " * ", var)

        residualsList[[rowIndex]] <- list(
          variable = var,
          residualParameter = paramName,
          residualCoefficient = NA,
          moderator = NA
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

  if (numFactors < 2) stop("At least two factors are required for covariance estimation.")

  # If there are exactly two factors, decide whether to apply moderation
  if (numFactors == 2) {
    factor1 <- factorNames[1]
    factor2 <- factorNames[2]

    if (removeModeration) {
      # Simple covariance without moderation
      covarianceBlock <- paste0(covarianceBlock, "  ", factor1, " ~~ cov * ", factor2, "\n")

      covarianceList[[rowIndex]] <- list(
        covarianceParameter = "cov",
        covarianceCoefficient = "fixed",
        moderator = NA
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


.sabic <- function(BIC, k, N) {

  # Calculate the adjustment term
  adjustment <- k * (log((N + 2) / 24) - log(N))

  # Calculate and return the adjusted BIC
  bicStar <- BIC + adjustment
  return(bicStar)
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

.extractIncludedPlotItemsDf <- function(plotModelList) {
  results <- list()

  for (model in plotModelList) {
    modelName <- model$value
    for (typeBlock in model$plotTypeList) {
      plotType <- typeBlock$value
      for (param in typeBlock$plotParameterList) {
        parameterGroup <- param$value
        for (item in param$plotItemList) {
          if (isTRUE(item$includePlot)) {
            results[[length(results) + 1]] <- list(
              modelName = modelName,
              plotType = plotType,
              parameterGroup = parameterGroup,
              plotMod1 = item$plotMod1$value,
              plotMod2 = item$plotMod2$value,
              value = if (is.list(item$value)) item$value$value else item$value,
              value.types = if (is.list(item$value)) item$value$types else NA
            )
          }
        }
      }
    }
  }

  if (length(results) == 0) return(NULL)

  do.call(rbind, lapply(results, as.data.frame, stringsAsFactors = FALSE))
}


.translatedElements <- function() {
  # some elements in qml are to be translated but these names are transferred into R
  parTableNames <- c("configural", "metric", "scalar", "strict", "load", "int",
              "res", "var", "mean", "rho")
  rNames <- c("configural", "metric", "scalar", "strict", "loadings", "intercepts",
              "residualVariances", "variances", "means", "covariances")
  guiNames <- c(gettext("Configural"), gettext("Metric"), gettext("Scalar"), gettext("Strict"),
                gettext("Loadings"), gettext("Intercepts"), gettext("Residual variances"),
                gettext("Factor variances"), gettext("Factor means"), gettext("Factor covariances"))
  df <- data.frame(cbind(parTableNames, rNames, guiNames))
  return(df)
}

# openMx messes up the options so we store them so they are correctly loaded upon re-run
.storeOpenMxOptions <- function(jaspResults) {
  print("why not?")
  if (!is.null(jaspResults[["openMxOptions"]])) return()

  library(OpenMx)
  mxOpts <- getOption("mxOptions")
  mxOtherOpts <- options()[grep("^mx", names(options()), value = TRUE)]  # Any other mx-related options
  mxOtherOpts[["mxOptions"]] <- NULL
  optsObj <- list(mxOpts = mxOpts, mxOtherOpts = mxOtherOpts)
  optsState <- createJaspState(optsObj)
  jaspResults[["openMxOptions"]] <- optsState
  return()
}

.restoreOpenMxOptions <- function(jaspResults) {
  if (is.null(jaspResults[["openMxOptions"]])) return()

  optsObj <- jaspResults[["openMxOptions"]][["object"]]
  options(mxOptions = optsObj[["mxOpts"]])
  if (!is.null(optsObj[["mxOtherOpts"]])) {
    do.call("options", optsObj[["mxOtherOpts"]])
  }
}
