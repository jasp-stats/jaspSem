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

  .mnlfaGlobalInvarianceTest(jaspResults, dataset, options, ready)

  .mnlfaAddGroupingVariableToData(jaspResults, dataTmp, options)

  return()
}
  ##### PREPROCESSING #####

.mnlfaPreprocessOptions <- function(options) {

  return()
}

.mnlfaHandleData <- function(dataset, options, ready) {

  return(dataset)
}

.mnlfaCheckErrors <- function(dataset, options, ready) {

  return()
}

# Create a container for the results
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
  fitPerGroupResult$dependOn(options = c("continuousVariableSplit"))
  jaspResults[["mainContainer"]][["fitPerGroupState"]] <- fitPerGroupResult

  return(dataset)
}

.mnlfaFitPerGroupTable <- function(jaspResults, dataset, options, ready) {

  if (!is.null(jaspResults[["mainContainer"]][["fitPerGroupTable"]])) return()
  if (!options[["fitPerGroup"]]) return()
  if (!ready) return()

  fitPerGroupTable <- createJaspTable(gettext("Fit per Group Test"))
  fitPerGroupTable$dependOn(optionsFromObject = jaspResults[["mainContainer"]][["fitPerGroupState"]])
  jaspResults[["mainContainer"]][["fitPerGroupTable"]] <- fitPerGroupTable
  fitPerGroupTable$addColumnInfo(name = "model", title = gettext("Group Model"), type = "string")

  result <- jaspResults[["mainContainer"]][["fitPerGroupState"]][["object"]]
  if (any(unlist(lapply(result, isTryError)))) {
    errs <- which(unlist(lapply(result, isTryError)))
    errmsg <- ""
    for (i in errs) {
      errmsg <- paste(errmsg, jaspBase::.extractErrorMessage(result[[i]]))
    }
    fitPerGroupTable$setError(gettextf("Error in configural invariance test. Internal error message(s): %s", errmsg))
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

.mnlfaGlobalInvarianceTest <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["mainContainer"]][["globalInvState"]])) return()
  if (!ready) return()


}

.mnlfaPrintSyntax <- function(jaspResults, options, dataset, ready, mod) {
  if (!options$showSyntax || !is.null(jaspResults[["syntax"]])) return()

  mod <- "Some Syntax"
  jaspResults[["syntax"]] <- createJaspHtml(mod, class = "jasp-code", position = 10, title = gettext("Model syntax"))
  jaspResults[["syntax"]]$dependOn(optionsFromObject = jaspResults[["maincontainer"]])
  jaspResults[["syntax"]]$dependOn("showSyntax")

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

.mnlfaTranslateToSyntax <- function(jaspResults, dataset, options, ready) {
  if (!ready) return()
  if (!is.null(jaspResults[["mainContainer"]][["syntaxState"]])) return()



  return()
}

.generateLoadings <- function(factorList,
                             moderators,
                             removeModeration = FALSE,
                             removeModerationFor = NULL,
                             removeModerationForVariables = c(),
                             removeSpecificModeration = list(),
                             removeModerator = NULL) {
  loadingsBlock <- ""
  moderators <- paste0("data.", moderators)
  if (!is.null(removeModerator)) {
    moderators <- setdiff(moderators, paste0("data.", removeModerator))
  }

  for (factorName in names(factorList)) {
    indicators <- factorList[[factorName]]
    loadingsBlock <- paste0(loadingsBlock, "  ", factorName, " =~ ")

    for (i in seq_along(indicators)) {
      var <- indicators[i]

      if (removeModeration || (!is.null(removeModerationFor) && factorName %in% removeModerationFor) || var %in% removeModerationForVariables) {
        if (i == 1) {
          loadExpr <- paste0("1 * ", var)
        } else {
          loadExpr <- var
        }
      } else {
        moderatedTerms <- moderators
        if (var %in% names(removeSpecificModeration)) {
          moderatedTerms <- setdiff(moderatedTerms, paste0("data.", removeSpecificModeration[[var]]))
        }

        moderationTerms <- paste0(
          paste0(moderatedTerms, " * load_", var, "_", seq_along(moderatedTerms)),
          collapse = " + "
        )
        loadExpr <- paste0("{load_", var, " := load_", var, "_0 + ", moderationTerms, "} * ", var)
      }

      if (i < length(indicators)) {
        loadExpr <- paste0(loadExpr, " + \n")
      }

      loadingsBlock <- paste0(loadingsBlock, loadExpr)
    }
    loadingsBlock <- paste0(loadingsBlock, "\n")
  }

  return(loadingsBlock)
}

.generateFactorVariances <- function(factorList,
                                    moderators,
                                    removeModerationFor = NULL,
                                    removeSpecificModeration = list(),
                                    removeModerator = NULL) {
  variancesBlock <- ""
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
  meansBlock <- ""
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
  interceptsBlock <- ""
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
  residualsBlock <- ""
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


