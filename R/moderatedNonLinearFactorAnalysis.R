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

  # openMx messes up the options so we store them so they are correctly loaded upon re-run
  .storeOpenMxOptions <- function(jaspResults) {
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

  .storeOpenMxOptions(jaspResults)
  .restoreOpenMxOptions(jaspResults)
#
#   print(options()[grep("^mx", names(options()), value = F)])

  ready <- length(unlist(lapply(options[["factors"]], `[[`, "indicators"), use.names = FALSE)) > 1 &&
    length(options[["moderators"]]) > 0

  if (!ready) {
    syncText <- createJaspHtml(text = gettext("Specify both factor indicator variables and moderator variables to run the analysis."))
    jaspResults[["syncText"]] <- syncText
    syncText$dependOn(c("factors", "moderators"))
    syncText$position <- 0.01
  }

  saveRDS(options, file = "~/Downloads/options.rds")
  saveRDS(dataset, file = "~/Downloads/dataset.rds")

  dataset <- .mnlfaHandleData(jaspResults, dataset, options, ready)

  .mnlfaCreateContainer(jaspResults, options)
  .mnlfaCheckErrors(dataset, options, ready)

  dataTmp <- .mnlfaFitPerGroup(jaspResults, dataset, options, ready)


  # .mnlfaCreateGlobalInvarianceContainer(jaspResults, options)

  .mnlfaCallGlobalInvarianceTests(jaspResults, dataset, options, ready)

  # output
  .mnlfaFitPerGroupTable(jaspResults, dataset, options, ready)
  .mnlfaGlobalInvarianceFitTable(jaspResults, dataset, options, ready)
  .mnlfaGlobalInvarianceParameterTables(jaspResults, dataset, options, ready)

  .mnlfaPrintSyntax(jaspResults, dataset, options, ready)

  .mnlfaAddGroupingVariableToData(jaspResults, dataTmp, options)


  return()
}

##### PREPROCESSING #####

.mnlfaHandleData <- function(jaspResults, dataset, options, ready) {

  if (!is.null(jaspResults[["dataState"]])) return(dataset)
  if (!ready) return()


  # TODO: should these all be numeric?
  # what about the moderator scaling?
  vars  <- unlist(lapply(options[["factors"]], `[[`, "indicators"), use.names = FALSE)
  # convert the dataset for the vars to numeric
  for (var in vars) {
    dataset[[var]] <- as.numeric(as.character(dataset[[var]]))
  }

  # scale the continuous moderators
  mods <- unlist(lapply(options[["moderators"]], `[[`, "variable"), use.names = FALSE)
  if (length(mods) == 0) return(dataset)
  mods.types <- options[["moderators.types"]]
  for (i in 1:length(mods)) {
    if (mods.types[i] != "nominal") {
      # this also forces also ordinal moderator variables to be numeric, do we want this???????????????????????????????????
      dataset[[mods[i]]] <- scale(as.numeric(as.character(dataset[[mods[i]]])))
    }
  }

  # add interactions and extra effects
  if (length(mods) > 0) {
    # already create the interaction variables, even if we dont need them.
    if (length(mods) > 1 && options[["addInteractionTerms"]]) {
      inters <- combn(mods, 2)
      for (i in 1:ncol(inters)) {
        tmp1 <- as.numeric(as.character(dataset[[inters[1, i]]])) # needed for nominal moderators
        tmp2 <- as.numeric(as.character(dataset[[inters[2, i]]]))
        tmpDt <- data.frame(tmp1 * tmp2)
        colnames(tmpDt) <- paste0(inters[1, i], "_x_", inters[2, i])
        dataset <- cbind(dataset, tmpDt)
      }
    }

    # add squares and cubic effects
    squares <- sapply(options[["moderators"]], function(x) x[["squaredEffect"]])
    if (sum(squares) > 0) {
      squaredMods <- mods[squares]
      for (i in 1:length(squaredMods)) {
        tmp <- as.numeric(as.character(dataset[[squaredMods[i]]]))
        tmpDt <- data.frame(tmp^2)
        colnames(tmpDt) <- paste0(squaredMods[i], "_squared")
        dataset <- cbind(dataset, tmpDt)
      }
    }
    cubics <- sapply(options[["moderators"]], function(x) x[["cubicEffect"]])
    if (sum(cubics) > 0) {
      cubicMods <- mods[cubics]
      for (i in 1:length(cubicMods)) {
        tmp <- as.numeric(as.character(dataset[[cubicMods[i]]]))
        tmpDt <- data.frame(tmp^3)
        colnames(tmpDt) <- paste0(cubicMods[i], "_cubic")
        dataset <- cbind(dataset, tmpDt)
      }
    }
  }


  dataState <- createJaspState(dataset)
  dataState$dependOn(options = c("factors", "moderators", "addInteractionTerms", "squaredEffect", "cubicEffect"))
  jaspResults[["dataState"]] <- dataState

  return(dataset)
}

.mnlfaCheckErrors <- function(dataset, options, ready) {

  return()
}

# Create a main container for everything
.mnlfaCreateContainer <- function(jaspResults, options) {
  if (is.null(jaspResults[["mainContainer"]])) {
    jaspResults[["mainContainer"]] <- createJaspContainer()
    jaspResults[["mainContainer"]]$dependOn(options = c(
      "factors", "moderators",
      "meanstructure", "se", "modelIdentification", "factorsUncorrelated",
      "interceptsFixedToZero", "packageMimiced", "estimator", "naAction"))
    jaspResults[["mainContainer"]]$position <- 1
  }
  return()
}

# # Create a container for the global invariance stuff
# .mnlfaCreateGlobalInvarianceContainer <- function(jaspResults, options) {
#   if (is.null(jaspResults[["mainContainer"]][["globalInvarianceContainer"]])) {
#     jaspResults[["mainContainer"]][["globalInvarianceContainer"]] <- createJaspContainer()
#     jaspResults[["mainContainer"]][["globalInvarianceContainer"]]$dependOn(options = c("addInteractionTerms"))
#     jaspResults[["mainContainer"]][["globalInvarianceContainer"]]$position <- 2
#   }
#   return()
# }

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
  # check for empty factors
  cleanedFactors <- lapply(options[["factors"]], function(x) {
    if (length(x[["indicators"]]) == 0) {
      return(NULL)
    } else {
      return(x)
    }
  })
  options[["factors"]] <- cleanedFactors[!sapply(cleanedFactors, is.null)]

  # this part is from jaspFactor
  cfaResult <- list()
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
                missing         = options$naAction)

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
  if (length(moderatorNames) > 0) {
    if (length(moderatorNames) > 1 && options[["addInteractionTerms"]]) {
      interactionTerms <- combn(moderatorNames, 2, paste, collapse = "_x_")
      moderatorNames <- c(moderatorNames, interactionTerms)
    }
    squares <- sapply(options[["moderators"]], function(x) x[["squaredEffect"]])
    if (sum(squares) > 0) {
      squaredMods <- moderatorsOriginal[squares]
      moderatorNames <- c(moderatorNames, paste0(squaredMods, "_squared"))
    }
    cubics <- sapply(options[["moderators"]], function(x) x[["cubicEffect"]])
    if (sum(cubics) > 0) {
      cubicMods <- moderatorsOriginal[cubics]
      moderatorNames <- c(moderatorNames, paste0(cubicMods, "_cubic"))
    }
  }

  modelObj <- .generateSyntax(factorList, moderatorNames, type = testName)

  script <- mxsem::mxsem(model = modelObj$model, data = dataset, scale_loadings = FALSE, scale_latent_variances = FALSE)

  fit <- try(OpenMx::mxRun(script))

  fitState <- createJaspState(fit)
  fitState$dependOn(options = c(paste0(testName, "Invariance"), "addInteractionTerms"))
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

  if (length(results) == 0) {
    invFitTable$addFootnote(gettext("Choose one of the global invariance tests to perform the test."))
    return()
  }

  invFitTable$addColumnInfo(name = "N", title = gettext("n(Parameters)"), type = "integer")
  invFitTable$addColumnInfo(name = "df", title = gettext("df"), type = "integer")
  invFitTable$addColumnInfo(name = "AIC", title = gettext("AIC"), type = "number")
  invFitTable$addColumnInfo(name = "BIC", title = gettext("BIC"), type = "number")
  dtFill <- data.frame(type = c(), N = c(), df = c(), AIC = c(), BIC = c())
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
                          df = NA,
                          AIC = NA,
                          BIC = NA)
    } else {
      summ <- summary(results[[i]])
      dtAdd <- data.frame(type = names(results)[i],
                          N = summ$estimatedParameters,
                          df = summ$degreesOfFreedom,
                          AIC = summ$AIC.Mx,
                          BIC = summ$BIC.Mx)
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

  globalParameterContainer <- createJaspContainer(gettext("Global Invariance Parameter Estimates"), initCollapsed = TRUE)
  globalParameterContainer$position <- 2
  globalParameterContainer$dependOn(optionsFromObject = jaspResults[["mainContainer"]][["invFitTable"]],
                                    options = c("loadingEstimates", "interceptEstimates", "residualVarianceEstimates",
                                            "factorVarianceEstimates", "factorMeanEstimates",
                                            "factorCovarianceEstimates"))
  jaspResults[["mainContainer"]][["globalParameterContainer"]] <- globalParameterContainer

  # get the mappings
  mapResults <- list(Configural = jaspResults[["mainContainer"]][["configuralInvModelState"]][["object"]][["map"]],
                      Metric = jaspResults[["mainContainer"]][["metricInvModelState"]][["object"]][["map"]],
                      Scalar = jaspResults[["mainContainer"]][["scalarInvModelState"]][["object"]][["map"]],
                      Strict = jaspResults[["mainContainer"]][["strictInvModelState"]][["object"]][["map"]])
  mapResults <- mapResults[sapply(mapResults, function(x) !is.null(x))]

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
      loadTable$addColumnInfo(name = "moderator", title = gettext("Moderator"), type = "number")
      loadTable$addColumnInfo(name = "param", title = gettext("Parameter"), type = "string")
      loadTable$addColumnInfo(name = "est", title = gettext("Estimate"), type = "number")
      loadTable$addColumnInfo(name = "se", title = gettext("Std. Error"), type = "number")

      fTitleMapping <- lapply(options[["factors"]], function(x) c(x[["name"]], x[["title"]]))
      loadMap <- mapResult$loadings
      loadMap <- loadMap[loadMap$loadingParameter != "fixed_1", ]
      # Replace values in the loadingsTable based on fTitleMapping
      for (mapping in fTitleMapping) {
        loadMap$factor[loadMap$factor == mapping[1]] <- mapping[2]
      }

      subMat <- paramTable[allLoads, ]
      df <- data.frame(factor = loadMap$factor,
                       indicator = loadMap$variable,
                       moderator = sub("^data.", "", loadMap$moderator),
                       param = sub("^load_", "", loadMap$loadingCoefficient),
                       est = subMat[, "Estimate"],
                       se = subMat[, "Std.Error"])
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
      intTable$addColumnInfo(name = "moderator", title = gettext("Moderator"), type = "number")
      intTable$addColumnInfo(name = "param", title = gettext("Parameter"), type = "string")
      intTable$addColumnInfo(name = "est", title = gettext("Estimate"), type = "number")
      intTable$addColumnInfo(name = "se", title = gettext("Std. Error"), type = "number")

      intMap <- mapResult$intercepts

      subMat <- paramTable[intPosition, ]
      df <- data.frame(indicator = intMap$variable,
                       moderator = sub("^data.", "", intMap$moderator),
                       param = sub("^int_", "", intMap$interceptCoefficient),
                       est = subMat[, "Estimate"],
                       se = subMat[, "Std.Error"])
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
      resTable$addColumnInfo(name = "moderator", title = gettext("Moderator"), type = "number")
      resTable$addColumnInfo(name = "param", title = gettext("Parameter"), type = "string")
      resTable$addColumnInfo(name = "est", title = gettext("Estimate"), type = "number")
      resTable$addColumnInfo(name = "se", title = gettext("Std. Error"), type = "number")

      resMap <- mapResult$residualVariances

      subMat <- paramTable[resPosition, ]
      df <- data.frame(indicator = resMap$variable,
                       moderator = sub("^data.", "", resMap$moderator),
                       param = sub("^res_", "", resMap$residualCoefficient),
                       est = subMat[, "Estimate"],
                       se = subMat[, "Std.Error"])
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
      fvTable$addColumnInfo(name = "moderator", title = gettext("Moderator"), type = "number")
      fvTable$addColumnInfo(name = "param", title = gettext("Parameter"), type = "string")
      fvTable$addColumnInfo(name = "est", title = gettext("Estimate"), type = "number")
      fvTable$addColumnInfo(name = "se", title = gettext("Std. Error"), type = "number")

      fTitleMapping <- lapply(options[["factors"]], function(x) c(x[["name"]], x[["title"]]))
      fvMap <- mapResult$variances
      # Replace values in the fvTable based on fTitleMapping
      for (mapping in fTitleMapping) {
        fvMap$factor[fvMap$factor == mapping[1]] <- mapping[2]
      }

      subMat <- paramTable[fvPosition, ]
      df <- data.frame(factor = fvMap$factor,
                       moderator = sub("^data.", "", fvMap$moderator),
                       param = sub("^var_", "", fvMap$varianceCoefficient),
                       est = subMat[, "Estimate"],
                       se = subMat[, "Std.Error"])
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
      fmTable$addColumnInfo(name = "moderator", title = gettext("Moderator"), type = "number")
      fmTable$addColumnInfo(name = "param", title = gettext("Parameter"), type = "string")
      fmTable$addColumnInfo(name = "est", title = gettext("Estimate"), type = "number")
      fmTable$addColumnInfo(name = "se", title = gettext("Std. Error"), type = "number")

      fTitleMapping <- lapply(options[["factors"]], function(x) c(x[["name"]], x[["title"]]))
      fmMap <- mapResult$means
      # Replace values in the fmTable based on fTitleMapping
      for (mapping in fTitleMapping) {
        fmMap$factor[fmMap$factor == mapping[1]] <- mapping[2]
      }

      subMat <- paramTable[fmPosition, ]
      df <- data.frame(factor = fmMap$factor,
                       moderator = sub("^data.", "", fmMap$moderator),
                       param = sub("^mean_", "", fmMap$meanCoefficient),
                       est = subMat[, "Estimate"],
                       se = subMat[, "Std.Error"])
      fmTable$setData(df)
    }
  }

  if (options[["factorCovarianceEstimates"]]) {
    # factor correlations
    covPosition <- grepl("^rho_", parNames)
    if (sum(covPosition) > 0) { # are factor covariances
      covTable <- createJaspTable(gettext("Factor Covariances"))
      cont[["covTable"]] <- covTable
      covTable$addColumnInfo(name = "moderator", title = gettext("Moderator"), type = "number")
      covTable$addColumnInfo(name = "param", title = gettext("Parameter"), type = "string")
      covTable$addColumnInfo(name = "est", title = gettext("Estimate"), type = "number")
      covTable$addColumnInfo(name = "se", title = gettext("Std. Error"), type = "number")

      covMap <- mapResult$covariances

      subMat <- paramTable[covPosition, ]
      df <- data.frame(moderator = covMap$moderator,
                       param = covMap$covarianceCoefficient,
                       est = subMat[, "Estimate"],
                       se = subMat[, "Std.Error"])
      covTable$setData(df)
    }
  }

  return(cont)
}

.mnlfaPrintSyntax <- function(jaspResults, dataset, options, ready, mod) {
  if (!options$showSyntax) return()
  if (!ready) return()

  syntaxContainer <- createJaspContainer(gettext("Model Syntax"), initCollapsed = TRUE)
  syntaxContainer$dependOn(options = c("showSyntax"),
                           optionsFromObject = jaspResults[["maincontainer"]][["globalParameterContainer"]])
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
.generateSyntax <- function(factorList, moderators, type) {

  rmvLoadMod <- FALSE
  rmvIntMod <- FALSE
  rmvResMod <- FALSE
  factorNamesVars <- names(factorList)
  factorNamesMeans <- names(factorList)

  if (type == "metric") {
    rmvLoadMod <- TRUE
    factorNamesVars <- NULL
  }

  if (type == "scalar") {
    rmvLoadMod <- TRUE
    rmvIntMod <- TRUE
    factorNamesVars <- NULL
    factorNamesMeans <- NULL
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
  variancesObj <- .generateFactorVariances(factorList, moderators, removeModerationFor = factorNamesVars)
  variancesBlock <- variancesObj$variancesBlock
  meansObj <- .generateFactorMeans(factorList, moderators, removeModerationFor = factorNamesMeans)
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
                              removeModerator = NULL) {
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
        if (i == 1) {
          loadExpr <- paste0("1 * ", var)  # Fix marker variable
          paramName <- "fixed_1"
        } else {
          loadExpr <- var
        }

        loadingsList[[rowIndex]] <- list(
          factor = factorName,
          variable = var,
          loadingParameter = paramName,
          loadingCoefficient = NA,
          moderator = NA
        )
        rowIndex <- rowIndex + 1
      } else {
        # Store the intercept coefficient
        loadingsList[[rowIndex]] <- list(
          factor = factorName,
          variable = var,
          loadingParameter = paramName,
          loadingCoefficient = paramIntercept,
          moderator = "Intercept"
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
                                     removeModerationFor = NULL,
                                     removeSpecificModeration = list(),
                                     removeModerator = NULL) {
  variancesBlock <- gettext("# FACTOR VARIANCES BLOCK \n")
  variancesList <- list()  # Store mappings of moderators only

  moderators <- paste0("data.", moderators)
  if (!is.null(removeModerator)) {
    moderators <- setdiff(moderators, paste0("data.", removeModerator))
  }

  rowIndex <- 1  # Track row index

  for (factorName in names(factorList)) {
    if (!is.null(removeModerationFor) && factorName %in% removeModerationFor) {
      variancesBlock <- paste0(variancesBlock, "  ", factorName, " ~~ 1 * ", factorName, "\n")
    } else {
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

      # Store mapping for intercept
      variancesList[[rowIndex]] <- list(
        factor = factorName,
        varianceParameter = paramName,
        varianceCoefficient = paramIntercept,
        moderator = "Intercept"
      )
      rowIndex <- rowIndex + 1

      # Store mapping for each moderator
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

      variancesBlock <- paste0(variancesBlock, "  ", factorName, " ~~ {", paramName, " := exp(", paramIntercept,
                               ifelse(moderationTerms != "", paste0(" + ", moderationTerms), ""),
                               ")} * ", factorName, "\n")
    }
  }

  variancesMapDf <- do.call(rbind, lapply(variancesList, as.data.frame))

  return(list(variancesBlock = variancesBlock, variancesMapDf = variancesMapDf))
}


.generateFactorMeans <- function(factorList,
                                 moderators,
                                 removeModerationFor = NULL,
                                 removeSpecificModeration = list(),
                                 removeModerator = NULL) {
  meansBlock <- gettext("# FACTOR MEANS BLOCK \n")
  meansList <- list()  # Store mappings

  moderators <- paste0("data.", moderators)
  if (!is.null(removeModerator)) {
    moderators <- setdiff(moderators, paste0("data.", removeModerator))
  }

  rowIndex <- 1  # Track row index

  for (factorName in names(factorList)) {
    if (!is.null(removeModerationFor) && factorName %in% removeModerationFor) {
      meansBlock <- paste0(meansBlock, "  ", factorName, " ~ 0 * 1\n")
    } else {
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

      # Store intercept mapping
      meansList[[rowIndex]] <- list(
        factor = factorName,
        meanParameter = paramName,
        meanCoefficient = paramIntercept,
        moderator = "Intercept"
      )
      rowIndex <- rowIndex + 1

      # Store moderator mappings
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
        paste0(
          paste0(moderatedTerms, " * ", paramCoefs),
          collapse = " + "
        )
      } else {
        ""
      }

      meansBlock <- paste0(meansBlock, "  ", factorName, " ~ {", paramName, " := ", paramIntercept,
                           ifelse(moderationTerms != "", paste0(" + ", moderationTerms), ""), "} * 1\n")
    }
  }

  # Convert list to DataFrame
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
          moderator = "Intercept"
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
          moderator = "Intercept"
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
        moderator = "Intercept"
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
