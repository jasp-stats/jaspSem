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

  ready <- length(unlist(lapply(options[["factors"]], `[[`, "indicators"), use.names = FALSE)) > 1 &&
    length(options[["moderators"]]) > 0

  .mnlfaPreprocessOptions(options)

  dataset <- .mnlfaHandleData(dataset, options, ready)

  saveRDS(options, file = "~/Downloads/options.rds")
  saveRDS(dataset, file="~/Downloads/dataset.rds")

  .mnlfaCheckErrors(dataset, options, ready)
  .mnlfaCreateContainer(jaspResults, options)

  dataTmp <- .mnlfaFitPerGroup(jaspResults, dataset, options, ready)

  .mnlfaFitPerGroupTable(jaspResults, dataset, options, ready)

  # .mnlfaCreateGlobalInvarianceContainer(jaspResults, options)

  .mnlfaConfiguralInvarianceTest(jaspResults, dataset, options, ready)
  .mnlfaGlobalInvarianceFitTable(jaspResults, dataset, options, ready)
  .mnlfaGlobalInvarianceParameterTables(jaspResults, dataset, options, ready)

  .mnlfaPrintSyntax(jaspResults, dataset, options, ready)

  .mnlfaAddGroupingVariableToData(jaspResults, dataTmp, options)


  return()
}
  ##### PREPROCESSING #####

.mnlfaPreprocessOptions <- function(options) {

  return()
}

.mnlfaHandleData <- function(dataset, options, ready) {

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

  # already create the interaction variables, even if we dont need them.
  if (length(mods) > 1) {
    inters <- combn(mods, 2)
    for (i in 1:ncol(inters)) {
      tmp1 <- as.numeric(as.character(dataset[[inters[1, i]]])) # needed for nominal moderators
      tmp2 <- as.numeric(as.character(dataset[[inters[2, i]]]))
      tmpDt <- data.frame(tmp1 * tmp2)
      colnames(tmpDt) <- paste0(inters[1, i], "_x_", inters[2, i])
      dataset <- cbind(dataset, tmpDt)
    }
  }


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
                meanstructure   = options$meanStructure,
                se              = cfaResult[["spec"]]$se,
                std.lv          = options$modelIdentification == "factorVariance",
                auto.fix.first  = options$modelIdentification == "markerVariable",
                orthogonal      = options$factorsUncorrelated,
                int.ov.free     = (options$interceptsFixedToZero == "latent" || options$interceptsFixedToZero == "meanManifest"),
                int.lv.free     = (options$interceptsFixedToZero == "manifest" || options$interceptsFixedToZero == "meanManifest"),
                effect.coding   = ifelse(options$modelIdentification == "effectsCoding", TRUE,
                                         ifelse(options$interceptsFixedToZero == "meanManifest", "intercepts", FALSE)),
                auto.fix.single = TRUE,
                auto.var        = TRUE,
                auto.cov.lv.x   = TRUE,
                auto.th         = TRUE,
                auto.delta      = TRUE,
                auto.cov.y      = TRUE,
                mimic           = options$packageMimiced,
                estimator       = options$estimator,
                missing         = options$naAction)
  result <- list()
  for (i in 1:length(groups)) {
    fitArgs$data <- dataset[dataset$addedGroupVar == groups[i], ]
    fit <- try(do.call(lavaan::lavaan, fitArgs))
    result[[groups[i]]] <- fit
  }

  fitPerGroupResult <- createJaspState(result)
  fitPerGroupResult$dependOn(options = c("continuousVariableSplit", "fitPerGroup"))
  jaspResults[["mainContainer"]][["fitPerGroupState"]] <- fitPerGroupResult

  return(dataset)
}


.mnlfaConfiguralInvarianceTest <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["mainContainer"]][["configInvState"]])) return()
  if (!ready) return()
  if (!options[["configuralInvariance"]]) return()

  factorList <- lapply(options[["factors"]], function(x) x[["indicators"]])
  factorNames <- sapply(options[["factors"]], function(x) x[["name"]])
  names(factorList) <- factorNames
  moderatorNames <- sapply(options[["moderators"]], function(x) x[["variable"]])
  if (length(moderatorNames) > 1 && options[["addInteractionTerms"]]) {
    interactionTerms <- combn(moderatorNames, 2, paste, collapse = "_x_")
    moderatorNames <- c(moderatorNames, interactionTerms)
  }

  loadingsObj <- .generateLoadings(factorList, moderatorNames)
  loadingsBlock <- loadingsObj$loadingsBlock
  interceptsBlock <- .generateIntercepts(factorList, moderatorNames)
  residualsBlock <- .generateResidualVariances(factorList, moderatorNames)
  variancesBlock <- .generateFactorVariances(factorList, moderatorNames, removeModerationFor = factorNames)
  meansBlock <- .generateFactorMeans(factorList, moderatorNames, removeModerationFor = factorNames)

  if (length(factorNames) == 2) {
    factorCovBlock <-.generateFactorCovariances(factorNames, moderatorNames)
  } else {
    factorCovBlock <- ""
  }

  model <- paste0(loadingsBlock, interceptsBlock, residualsBlock, variancesBlock, meansBlock, sep = "\n")
  script <- mxsem::mxsem(model = model, data = dataset, scale_loadings = FALSE, scale_latent_variances = FALSE)

  fit <- try(OpenMx::mxTryHard(script))
  saveRDS(fit, file = "~/Downloads/fit.rds")

  configInvState <- createJaspState(fit)
  configInvState$dependOn(options = c("configuralInvariance", "addInteractionTerms"))
  jaspResults[["mainContainer"]][["configInvState"]] <- configInvState

  # save the model to print later
  modelState <- createJaspState(model)
  modelState$dependOn(optionsFromObject = configInvState)
  jaspResults[["mainContainer"]][["configInvModelState"]] <- modelState

  # also save the mapping
  mapList <- list(loadings = loadingsObj$loadingsMap)
  mapState <- createJaspState(mapList)
  mapState$dependOn(optionsFromObject = configInvState)
  jaspResults[["mainContainer"]][["configInvMapState"]] <- mapState

  return()
}


##### OUTPUT #####
.mnlfaFitPerGroupTable <- function(jaspResults, dataset, options, ready) {

  if (!is.null(jaspResults[["mainContainer"]][["fitPerGroupTable"]])) return()
  if (!ready) return()

  fitPerGroupTable <- createJaspTable(gettext("Fit per Group Test"))
  fitPerGroupTable$position <- 1
  fitPerGroupTable$dependOn(optionsFromObject = jaspResults[["mainContainer"]][["fitPerGroupState"]])
  fitPerGroupTable$addColumnInfo(name = "model", title = gettext("Group Model"), type = "string")

  # only do this here so there is an empty table if there are no results
  if (!options[["fitPerGroup"]]) {
    fitPerGroupTable$addFootnote(gettext("Choose to fit a model per group to perform the test."))
    return()
  }

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

  jaspResults[["mainContainer"]][["fitPerGroupTable"]] <- fitPerGroupTable

  return()
}

.mnlfaGlobalInvarianceFitTable <- function(jaspResults, dataset, options, ready) {
  if (!ready) return()
  if (!is.null(jaspResults[["mainContainer"]][["invFitTable"]])) return()

  invFitTable <- createJaspTable(gettext("Global Invariance Fit"))
  invFitTable$position <- 2
  invFitTable$dependOn(optionsFromObject = jaspResults[["mainContainer"]],
                       options = c("addInteractionTerms", "configuralInvariance", "metricInvariance", "scalarInvariance", "strictInvariance"))
  invFitTable$addColumnInfo(name = "type", title = gettext("Type"), type = "string")
  jaspResults[["mainContainer"]][["invFitTable"]] <- invFitTable

  results <- list(Configural = jaspResults[["mainContainer"]][["configInvState"]][["object"]],
                  Metric = jaspResults[["mainContainer"]][["metricInvState"]][["object"]],
                  Scalar = jaspResults[["mainContainer"]][["scalarInvState"]][["object"]],
                  Strict = jaspResults[["mainContainer"]][["strictInvState"]][["object"]])

  results <- results[sapply(results, function(x) !is.null(x))]

  if (length(results) == 0) {
    invFitTable$addFootnote(gettext("Choose one of the global invariance tests to perform the test."))
    return()
  }

  invFitTable$addColumnInfo(name = "N", title = gettext("n(Parameters)"), type = "integer")
  invFitTable$addColumnInfo(name = "df", title = "df", type = "integer")
  invFitTable$addColumnInfo(name = "AIC", title = "AIC", type = "number")
  invFitTable$addColumnInfo(name = "BIC", title = "BIC", type = "number")
  dtFill <- data.frame(Type = character(), N = integer(), df = integer(), AIC = numeric(), BIC = numeric())
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
  if (!options[["parameterEstimates"]]) return()

  results <- list(Configural = jaspResults[["mainContainer"]][["configInvState"]][["object"]],
                  Metric = jaspResults[["mainContainer"]][["metricInvState"]][["object"]],
                  Scalar = jaspResults[["mainContainer"]][["scalarInvState"]][["object"]],
                  Strict = jaspResults[["mainContainer"]][["strictInvState"]][["object"]])

  results <- results[sapply(results, function(x) !is.null(x))]

  if (length(results) == 0) return()

  globalParameterContainer <- createJaspContainer(gettext("Global Invariance Parameter Estimates"), initCollapsed = TRUE)
  globalParameterContainer$position <- 2
  globalParameterContainer$dependOn(optionsFromObject = jaspResults[["mainContainer"]],
                                options = c("addInteractionTerms", "configuralInvariance", "metricInvariance", "scalarInvariance", "strictInvariance"))
  jaspResults[["mainContainer"]][["globalParameterContainer"]] <- globalParameterContainer

  # get the mappings
  mapResults <- list(Configural = jaspResults[["mainContainer"]][["configInvMapState"]][["object"]],
                      Metric = jaspResults[["mainContainer"]][["metricInvMapState"]][["object"]],
                      Scalar = jaspResults[["mainContainer"]][["scalarInvMapState"]][["object"]],
                      Strict = jaspResults[["mainContainer"]][["strictInvMapState"]][["object"]])

  for (i in 1:length(results)) {
    globalParameterContainer[[names(results)[i]]] <- .mnlfaParameterTableHelper(results[[i]],
                                                                                names(results)[i],
                                                                                mapResults[[i]],
                                                                                options)
  }

  return()
}

.mnlfaParameterTableHelper <- function(fit, nm, mapResult, options) {

  saveRDS(mapResult, file = "~/Downloads/mapResult.rds")

  cont <- createJaspContainer(nm, initCollapsed = TRUE)
  cont$position <- 2.1

  fitSummary <- summary(fit)
  paramTable <- fitSummary$parameters
  parNames <- paramTable[, "name"]

  # Loadings
  loadPosition <- grepl("^load_", parNames)

  if (sum(loadPosition) > 0) { # are loadings even specified
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
    # Replace values in the loadingsTable based on fTitleMapping
    for (mapping in fTitleMapping) {
      loadMap$factor[loadMap$factor == mapping[1]] <- mapping[2]
    }

    subMat <- paramTable[loadPosition, ]
    df <- data.frame(factor = loadMap$factor,
                     indicator = loadMap$variable,
                     moderator = sub("^data.", "", loadMap$moderator),
                     param = sub("^load_", "", subMat[, "name"]),
                     est = subMat[, "Estimate"],
                     se = subMat[, "Std.Error"])
    loadTable$setData(df)
  }

  # Intercepts
  intPosition <- grepl("^int_", parNames)
  if (sum(intPosition) > 0) { # are intercepts even specified
    intTable <- createJaspTable(gettext("Intercepts"))
    cont[["intTable"]] <- intTable
    intTable$addColumnInfo(name = "param", title = gettext("Parameter"), type = "string")
    intTable$addColumnInfo(name = "est", title = gettext("Estimate"), type = "number")
    intTable$addColumnInfo(name = "se", title = gettext("Std. Error"), type = "number")

    subMat <- paramTable[intPosition, ]
    df <- data.frame(param = subMat[, "name"],
                     est = subMat[, "Estimate"],
                     se = subMat[, "Std.Error"])
    intTable$setData(df)
  }


  return(cont)
}

.mnlfaPrintSyntax <- function(jaspResults, dataset, options, ready, mod) {
  if (!options$showSyntax) return()
  if (!ready) return()

  syntaxContainer <- createJaspContainer()
  jaspResults[["syntaxContainer"]] <- syntaxContainer

  model <- jaspResults[["mainContainer"]][["configInvModelState"]][["object"]]
  configSyntaxState <- createJaspState(model)
  if (is.null(model)) {
    modPrint <- gettext("Nothing to print.")
  } else {
    modPrint <- model
  }
  syntaxContainer[["configSyntaxState"]] <- createJaspHtml(modPrint, class = "jasp-code", position = 10, title = gettext("Model syntax"))
  syntaxContainer[["configSyntaxState"]]$dependOn(options = c("showSyntax"),
                                                  optionsFromObject = jaspResults[["maincontainer"]][["globalInvarianceContainer"]])

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
          loadingCoefficients = NA,
          moderator = NA
        )
        rowIndex <- rowIndex + 1
      } else {
        # Store the intercept coefficient
        loadingsList[[rowIndex]] <- list(
          factor = factorName,
          variable = var,
          loadingParameter = paramName,
          loadingCoefficients = paramIntercept,
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
              loadingCoefficients = paramCoefs[j],
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
  moderators <- paste0("data.", moderators)
  if (!is.null(removeModerator)) {
    moderators <- setdiff(moderators, paste0("data.", removeModerator))
  }

  for (factorName in names(factorList)) {
    if (!is.null(removeModerationFor) && factorName %in% removeModerationFor) {
      variancesBlock <- paste0(variancesBlock, "  ", factorName, " ~~ 1 * ", factorName, "\n")
    } else {
      moderatedTerms <- moderators
      if (factorName %in% names(removeSpecificModeration)) {
        moderatedTerms <- setdiff(moderatedTerms, paste0("data.", removeSpecificModeration[[factorName]]))
      }

      moderationTerms <- paste0(
        paste0(moderatedTerms, " * var_", factorName, "_", seq_along(moderatedTerms)),
        collapse = " + "
      )
      variancesBlock <- paste0(variancesBlock, "  ", factorName, " ~~ {var_", factorName, " := exp(var_", factorName, "_0 + ", moderationTerms, ")} * ", factorName, "\n")
    }
  }
  return(variancesBlock)
}

.generateFactorMeans <- function(factorList,
                                moderators,
                                removeModerationFor = NULL,
                                removeSpecificModeration = list(),
                                removeModerator = NULL) {
  meansBlock <- gettext("# FACTOR MEANS BLOCK \n")
  moderators <- paste0("data.", moderators)
  if (!is.null(removeModerator)) {
    moderators <- setdiff(moderators, paste0("data.", removeModerator))
  }

  for (factorName in names(factorList)) {
    if (!is.null(removeModerationFor) && factorName %in% removeModerationFor) {
      meansBlock <- paste0(meansBlock, "  ", factorName, " ~ 0 * 1\n")
    } else {
      moderatedTerms <- moderators
      if (factorName %in% names(removeSpecificModeration)) {
        moderatedTerms <- setdiff(moderatedTerms, paste0("data.", removeSpecificModeration[[factorName]]))
      }

      moderationTerms <- paste0(
        paste0(moderatedTerms, " * mean_", factorName, "_", seq_along(moderatedTerms)),
        collapse = " + "
      )
      meansBlock <- paste0(meansBlock, "  ", factorName, " ~ {mean_", factorName, " := mean_", factorName, "_0 + ", moderationTerms, "} * 1\n")
    }
  }
  return(meansBlock)
}

.generateIntercepts <- function(factorList,
                               moderators,
                               removeModeration = FALSE,
                               removeModerationFor = NULL,
                               removeModerationForVariables = c(),
                               removeSpecificModeration = list(),
                               removeModerator = NULL) {
  interceptsBlock <- gettext("# INTERCEPTS BLOCK \n")
  moderators <- paste0("data.", moderators)
  if (!is.null(removeModerator)) {
    moderators <- setdiff(moderators, paste0("data.", removeModerator))
  }

  for (factorName in names(factorList)) {
    indicators <- factorList[[factorName]]

    for (i in seq_along(indicators)) {
      var <- indicators[i]

      if (removeModeration || (!is.null(removeModerationFor) && factorName %in% removeModerationFor) || var %in% removeModerationForVariables) {
        interceptExpr <- paste0(var, " ~ int_", var, " * 1")
      } else {
        moderatedTerms <- moderators
        if (var %in% names(removeSpecificModeration)) {
          moderatedTerms <- setdiff(moderatedTerms, paste0("data.", removeSpecificModeration[[var]]))
        }

        moderationTerms <- paste0(
          paste0(moderatedTerms, " * int_", var, "_", seq_along(moderatedTerms)),
          collapse = " + "
        )
        interceptExpr <- paste0(var, " ~ {int_", var, " := int_", var, "_0 + ", moderationTerms, "} * 1")
      }

      interceptsBlock <- paste0(interceptsBlock, "  ", interceptExpr, "\n")
    }
  }
  return(interceptsBlock)
}


.generateResidualVariances <- function(factorList,
                                      moderators,
                                      removeModeration = FALSE,
                                      removeModerationFor = NULL,
                                      removeModerationForVariables = c(),
                                      removeSpecificModeration = list(),
                                      removeModerator = NULL) {
  residualsBlock <- gettext("# RESIDUAL VARIANCES BLOCK \n")
  moderators <- paste0("data.", moderators)
  if (!is.null(removeModerator)) {
    moderators <- setdiff(moderators, paste0("data.", removeModerator))
  }

  for (factorName in names(factorList)) {
    indicators <- factorList[[factorName]]

    for (i in seq_along(indicators)) {
      var <- indicators[i]

      if (removeModeration || (!is.null(removeModerationFor) && factorName %in% removeModerationFor) || var %in% removeModerationForVariables) {
        residualExpr <- paste0(var, " ~~ res_", var, " * ", var)
      } else {
        moderatedTerms <- moderators
        if (var %in% names(removeSpecificModeration)) {
          moderatedTerms <- setdiff(moderatedTerms, paste0("data.", removeSpecificModeration[[var]]))
        }

        moderationTerms <- paste0(
          paste0(moderatedTerms, " * res_", var, "_", seq_along(moderatedTerms)),
          collapse = " + "
        )
        residualExpr <- paste0(var, " ~~ {res_", var, " := exp(res_", var, "_0 + ", moderationTerms, ")} * ", var)
      }

      residualsBlock <- paste0(residualsBlock, "  ", residualExpr, "\n")
    }
  }
  return(residualsBlock)
}

# Function to generate covariance model syntax
.generateFactorCovariances <- function(factorNames, moderators) {
  factor1 <- factorNames[1]
  factor2 <- factorNames[2]
  covarianceBlock <- paste0("  ", factor1, " ~~ cov * ", factor2, "\n")
  covarianceBlock <- paste0(covarianceBlock, "  !r0; !r1; !r2; !r3;\n")

  rhoTerms <- paste0(
    paste0("r", seq_along(moderators), " * data.", moderators),
    collapse = " + "
  )
  covarianceBlock <- paste0(covarianceBlock, "  rho := r0 + ", rhoTerms, "\n")
  covarianceBlock <- paste0(covarianceBlock, "  cov := (exp(2*rho) - 1)/(exp(2*rho) + 1)\n")

  return(covarianceBlock)
}
