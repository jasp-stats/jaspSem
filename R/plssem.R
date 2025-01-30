#Metrics### <- func
# Copyright (C) 2013-2020 University of Amsterdam
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

PLSSEMInternal <- function(jaspResults, dataset, options, ...) {
  jaspResults$addCitation("Rademaker ME, Schuberth F (2020). cSEM: Composite-Based Structural Equation Modeling. Package version: 0.4.0, https://m-e-rademaker.github.io/cSEM/.")

  options <- .plsSemPrepOpts(options)

  # Read data, check if ready
  dataset <- .plsSemReadData(dataset, options)
  ready   <- .plsSemIsReady(dataset, options)

  # Store in container
  modelContainer <- .plsSemModelContainer(jaspResults)

  # Check for errors
  .plsSemCheckErrors(dataset, options, ready, modelContainer)

  # Output functions
  .plsSemFitTab(modelContainer, dataset, options, ready)
  if (modelContainer$getError()) return()
  .plsSemParameters(modelContainer, dataset, options, ready)
  .plsSemRsquared(modelContainer, dataset, options, ready)
  .plsSemPrediction(modelContainer, options, ready)
  .plsSemAdditionalFits(modelContainer, dataset, options, ready)
  .semMardiasCoefficient(modelContainer, dataset, options, ready)
  .plsSemReliabilities(modelContainer, dataset, options, ready)
  .plsSemCor(modelContainer, options, ready)

  .plsAddConstructScores(jaspResults, modelContainer, options, ready)
}

.plsSemPrepOpts <- function(options) {
  #backwards compatability after changes to bouncontrollavaantextarea.cpp
  fixModel <- function(model) {
    newModel <- c(model[1], model[[2]])
    names(newModel)[names(newModel) == "model"] <- "syntax"
    return(newModel)
    }

  options[["models"]] <- lapply(options[["models"]], fixModel)

  emptymod <- vapply(options[["models"]], function(x) x[["syntax"]] == "", TRUE)
  options[["models"]] <- options[["models"]][!emptymod]
  return(options)
}

.plsSemReadData <- function(dataset, options) {
  if (!is.null(dataset)) return(dataset)

  variablesToRead <- if (options[["group"]] == "") character() else options[["group"]]
  for (model in options[["models"]])
    variablesToRead <- unique(c(variablesToRead, model[["columns"]]))

  return(.readDataSetToEnd(columns = variablesToRead, exclude.na.listwise = variablesToRead))
}

.plsSemIsReady <- function(dataset, options) {

  if (length(options[["models"]]) < 1) return(FALSE)

  for (m in options[["models"]])
    if (length(m[["columns"]]) > 0)
      return(TRUE)

  return(FALSE)
}

.plsSemCheckErrors <- function(dataset, options, ready, modelContainer) {
  if (!ready) return()

  if (ncol(dataset) > 0) {
    if (length(options[["models"]]) < 1) return(FALSE)
    usedvars <- unique(unlist(lapply(options[["models"]], function(x) {
      .semGetUsedVars(x[["syntax"]], colnames(dataset))
    })))
    .hasErrors(dataset[,usedvars],
               type = c('variance', 'infinity'), message='default', exitAnalysisIfErrors = TRUE)
  }

  # Check whether grouping variable is a grouping variable
  if (options[["group"]] != "") {
    groupfac <- factor(dataset[[options[["group"]]]])
    factab <- table(groupfac)
    if (any(factab < 3)) {
      violations <- names(table(groupfac))[table(groupfac) < 3]
      .quitAnalysis(gettextf("Grouping variable has fewer than 3 observations in group %s",
                             paste(violations, collapse = ", ")))

    }
  }
}

checkCSemModel <- function(model, availableVars) {

  # function returns informative printable string if there is an error, else ""
  if (model == "") return("Enter a model")

  # translate to base64 - function from semsimple.R
  vvars    <- availableVars
  usedvars <- vvars #.semGetUsedVars(model, vvars)
  vmodel   <- model # .semTranslateModel(model, usedvars)

  unvvars <- availableVars
  names(unvvars) <- vvars

  # Check model syntax
  parsed <- try(cSEM::parseModel(vmodel), silent = TRUE)
  if (inherits(parsed, "try-error")) {

    msg <- attr(parsed, "condition")$message

    if (msg == "NA/NaN argument") {
      return(gettext("Enter a model"))
    }
    return(stringr::str_replace_all(msg, unvvars))
  }

  # Check variable names
  if (!missing(availableVars)) {
    latents <- unique(rownames(parsed$measurement))
    modelVars <- setdiff(unique(c(rownames(parsed$measurement), colnames(parsed$measurement))), latents)
    modelVars <- modelVars[modelVars != ""]

    modelVarsInAvailableVars <- (modelVars %in% vvars)
    if (!all(modelVarsInAvailableVars)) {
      notRecognized <- modelVars[!modelVarsInAvailableVars]
      return(gettextf("Variable(s) in model syntax not recognized: %s",
                      paste(stringr::str_replace_all(notRecognized, unvvars), collapse = ", ")))
    }
  }

  # check for '~~'
  if (grepl("~~", vmodel)) {
    return(gettext("Using '~~' is not supported. Try '~' instead"))
  }

  # if checks pass, return empty string
  return("")
}


.plsSemModelContainer <- function(jaspResults) {

  if (!is.null(jaspResults[["modelContainer"]])) {
    modelContainer <- jaspResults[["modelContainer"]]
  } else {
    modelContainer <- createJaspContainer()
    modelContainer$dependOn(c("syntax", "convergenceCriterion",
                              "estimateStructural", "group", "consistentPartialLeastSquares",
                              "structuralModelIgnored", "innerWeightingScheme", "errorCalculationMethod",
                              "bootstrapSamples", "ciLevel",
                              "setSeed", "seed", "handlingOfInadmissibles", "endogenousIndicatorPrediction",
                              "kFolds", "repetitions", "benchmark", "predictedScore", "models"))
    jaspResults[["modelContainer"]] <- modelContainer
  }

  return(modelContainer)
}


.plsSemComputeResults <- function(modelContainer, dataset, options) {
  # create result list from options

  # find reusable results
  oldmodels  <- modelContainer[["models"]][["object"]]
  oldresults <- modelContainer[["results"]][["object"]]
  reuse <- match(options[["models"]], oldmodels)
  if (identical(reuse, seq_along(reuse))) return(oldresults) # reuse everything

  # create results list
  results <- vector("list", length(options[["models"]]))
  if (any(!is.na(reuse))) {
    # where possible, prefill results with old results
    results[seq_along(reuse)] <- oldresults[reuse]
  }

  # generate cSem options list
  cSemOpts <- .plsSemOptionsTocSemOptions(options, dataset)

  for (i in seq_along(results)) {

    if (!is.null(results[[i]])) next # existing model is reused

    # create options

    syntax   <- .semTranslateModel(options[["models"]][[i]][["syntax"]], dataset)
    cSemOpts[[".model"]] <- syntax
    cSemOpts[[".data"]]  <- dataset

    # fit the model

    fit <- try(do.call(cSEM::csem, cSemOpts))

    # error messages
    if (isTryError(fit)) {
      err <- .extractErrorMessage(fit)
      errmsg <- gettextf("Estimation failed Message: %s", err)
      modelContainer$setError(paste0("Error in model \"", options[["models"]][[i]][["name"]], "\" - ",
                                     .decodeVarsInMessage(names(dataset), errmsg)))
      modelContainer$dependOn("models") # add dependency so everything gets updated upon model change
      break
    }


    if(isFALSE(fit$Information$Weight_info$Convergence_status)) {
      errormsg <- gettextf("Estimation failed! Message: Model %s did not converge!", options[["models"]][[i]][["name"]])
      modelContainer$setError(errormsg)
      modelContainer$dependOn("models")
      break
    }

    # resample if bootstrap
    if (options[["errorCalculationMethod"]] == "bootstrap") {

      startProgressbar(options[["bootstrapSamples"]], "Resampling")

      # argument .user_funs in cSEM::resamplecSEMResults only accepts a function with .object as input and a vector as output; c(0,0) does not have any other function
      tickFunction <- function(.object)
      {
        progressbarTick()
        return(c(0,0))
      }
      # resample
      fit <- try(cSEM::resamplecSEMResults(.object = fit,
                                           .R = options[["bootstrapSamples"]],
                                           .user_funs = tickFunction,
                                           .resample_method = "bootstrap",
                                           .handle_inadmissibles = options[["handlingOfInadmissibles"]],
                                           .sign_change_option = "none",
                                           .seed = if (options[["setSeed"]]) options[["seed"]]))


      if (isTryError(fit)) {
        err <- .extractErrorMessage(fit)

        errmsg <- gettextf("Estimation failed Message: %s", err)
        modelContainer$setError(paste0("Error in model \"", options[["models"]][[i]][["name"]], "\" - ",
                                       .decodeVarsInMessage(names(dataset), errmsg)))
        modelContainer$dependOn("models")
        break
      }
    }

    results[[i]] <- fit

  }

  # store results in model container
  if (!modelContainer$getError()) {
    modelContainer[["results"]] <- createJaspState(results)
    modelContainer[["results"]]$dependOn(optionsFromObject = modelContainer)
    modelContainer[["models"]]  <- createJaspState(options[["models"]])
    modelContainer[["models"]]$dependOn(optionsFromObject = modelContainer)
  }

  return(results)
}


.plsSemOptionsTocSemOptions <- function(options, dataset) {
  #' mapping the QML options from JASP to cSem options

  cSemOpts <- list()

  # model features
  cSemOpts[[".approach_weights"]]            <- "PLS-PM"
  cSemOpts[[".approach_cor_robust"]]         <- "none"
  cSemOpts[[".approach_nl"]]                 <- options[["approachNonLinear"]]
  cSemOpts[[".conv_criterion"]]              <- switch(options[["convergenceCriterion"]],
                                                       "absoluteDifference" = "diff_absolute",
                                                       "squaredDifference" = "diff_squared",
                                                       "relativeDifference" = "diff_relative")
  cSemOpts[[".estimate_structural"]]         <- options[["estimateStructural"]]
  cSemOpts[[".normality"]]                   <- options[["assumeNormality"]]
  cSemOpts[[".PLS_ignore_structural_model"]] <- options[["structuralModelIgnored"]]
  cSemOpts[[".PLS_weight_scheme_inner"]]     <- options[["innerWeightingScheme"]]

  if (options[["consistentPartialLeastSquares"]]) {
    cSemOpts[".disattenuate"] <- TRUE
    cSemOpts[".PLS_approach_cf"] <- "dist_squared_euclid"

  } else {
    cSemOpts[".disattenuate"] <- FALSE
  }

  if (options[["group"]] != "")
    cSemOpts[[".id"]] <- options[["group"]]

  return(cSemOpts)
}


### output functions ###

# Model Fit Table
.plsSemFitTab <- function(modelContainer, dataset, options, ready) {
  # create model fit table
  if (!is.null(modelContainer[["fittab"]])) return()


  fittab <- createJaspTable(title = gettext("Model fit"))
  fittab$dependOn(c("models"))
  fittab$position <- 0

  # fittab$addColumnInfo(name = "Model",    title = "",                            type = "string", combine = TRUE)
  if (options[["group"]] != "")
    fittab$addColumnInfo(name = "group",  title = gettext("Group"),              type = "string" )
  fittab$addColumnInfo(name = "AIC",      title = gettext("AIC"),                type = "number" )
  fittab$addColumnInfo(name = "BIC",      title = gettext("BIC"),                type = "number" )
  fittab$addColumnInfo(name = "N",        title = gettext("n"),                  type = "integer")
  fittab$addColumnInfo(name = "Chisq",    title = "\u03C7\u00B2",       type = "number" ,
                       overtitle = gettext("Baseline test"))
  fittab$addColumnInfo(name = "Df",       title = gettext("df"),                 type = "integer",
                       overtitle = gettext("Baseline test"))
  fittab$addColumnInfo(name = "PrChisq",  title = gettext("p"),                  type = "pvalue",
                       overtitle = gettext("Baseline test"))
  if (length(options[["models"]]) > 1) {
    fittab$addColumnInfo(name = "dchisq",   title = "\u0394\u03C7\u00B2", type = "number" ,
                         overtitle = gettext("Difference test"))
    fittab$addColumnInfo(name = "ddf",      title = "\u0394df",           type = "integer",
                         overtitle = gettext("Difference test"))
    fittab$addColumnInfo(name = "dPrChisq", title = gettext("p"),                  type = "pvalue" ,
                         overtitle = gettext("Difference test"))
  }


  modelContainer[["fittab"]] <- fittab

  if (!ready) return()

  # fill model fit table
  plsSemResults <- .plsSemComputeResults(modelContainer, dataset, options)

  if (modelContainer$getError()) return()


  if (length(plsSemResults) < 2) {
    if (options[["group"]] == "") {

      msc       <- .withWarnings(.computeMSC(plsSemResults[[1]], dataset, options))

      name <- options[["models"]][[1]][["name"]]
      aic       <- msc$value$msc$AIC
      bic       <- msc$value$msc$BIC
      Ns        <- nrow(dataset)
      chisq     <- msc$value$mfm$Chi_square
      df        <- msc$value$mfm$Df
      prChisq   <- pchisq(q = chisq, df = df, lower.tail = FALSE)

    } else {

      msc       <- .withWarnings(.computeMSC(plsSemResults[[1]], dataset, options))

      name <- rep(options[["models"]][[1]][["name"]], length(plsSemResults[[1]]))
      group     <- names(plsSemResults[[1]])
      aic       <- msc$value$msc["AIC",]
      bic       <- msc$value$msc["BIC",]
      Ns        <- msc$value$Ns
      chisq     <- msc$value$mfm["Chi_square",]
      df        <- msc$value$mfm["Df",]
      prChisq   <- prChisq <- mapply(pchisq, q = chisq, df = df, lower.tail = FALSE)

    }
  } else {
      postEstimation_args <- plsSemResults
      names(postEstimation_args) <- "object" # (the first result is object, the others ...)
      name <- list()
      aic       <- list()
      bic       <- list()
      Ns        <- list()
      chisq     <- list()
      df        <- list()
      prChisq   <- list()
      group     <- list()
      rsquared  <- list()

      if (options[["group"]] == "") {

        msc <- .withWarnings(lapply(postEstimation_args, .computeMSC, dataset = dataset, options = options))

        name <- vapply(options[["models"]], getElement, name = "name", "")
        Ns        <- rep(nrow(dataset), length(plsSemResults))
        for (i in seq_along(options[["models"]])) {

          aic       <- c(aic, msc$value[[i]]$msc$AIC)
          bic       <- c(bic, msc$value[[i]]$msc$BIC)
          chisq     <- c(chisq, msc$value[[i]]$mfm$Chi_square)
          df        <- c(df, msc$value[[i]]$mfm$Df)
          prChisq   <- c(prChisq, pchisq(q = msc$value[[i]]$mfm$Chi_square, df = msc$value[[i]]$mfm$Df, lower.tail = FALSE))
        }

      } else {

        msc <- .withWarnings(lapply(postEstimation_args, .computeMSC, dataset = dataset, options = options))
        for (i in seq_along(options[["models"]])) {

          name <- c(name, rep(options[["models"]][[i]][["name"]], length(plsSemResults[[i]])))
          aic       <- c(aic,   msc$value[[i]]$msc["AIC",])
          bic       <- c(bic,   msc$value[[i]]$msc["BIC",])
          Ns        <- c(Ns,    msc$value[[i]]$Ns)
          chisq     <- c(chisq, msc$value[[i]]$mfm["Chi_square",])
          df        <- c(df,    msc$value[[i]]$mfm["Df",])
          prChisq   <- c(prChisq, mapply(pchisq, q = msc$value[[i]]$mfm["Chi_square",], df = msc$value[[i]]$mfm["Df",], lower.tail = FALSE))
          group     <- c(group, names(plsSemResults[[i]]))
        }
      }
    }

  # fittab[["Model"]]    <- name
  if (options[["group"]] != "")
    fittab[["group"]]    <- group
  fittab[["AIC"]]      <- aic
  fittab[["BIC"]]      <- bic
  fittab[["N"]]        <- Ns
  fittab[["Chisq"]]    <- chisq
  fittab[["Df"]]       <- df
  fittab[["PrChisq"]]  <- prChisq

  if (length(options[["models"]]) > 1) {
    groupLength <- length(chisq) / length(options[["models"]])
    dchisq   <- as.list(rep(NA, groupLength))
    ddf      <- as.list(rep(NA, groupLength))
    dPrChisq <- as.list(rep(NA, groupLength))
    chisq <- as.list(chisq)
    df <- as.list(df)
    for(i in 1:(length(chisq)-groupLength)) {
      dchisq     <- c(dchisq, abs(unlist(chisq[i+groupLength])- unlist(chisq[i])))
      ddf        <- c(ddf, abs(unlist(df[i+groupLength])-unlist(df[i])))
      dPrChisq   <- c(dPrChisq, pchisq(q = abs(unlist(chisq[i+groupLength])- unlist(chisq[i])),
                                       df = abs(unlist(df[i+groupLength])-unlist(df[i])),
                                       lower.tail = FALSE))
    }
    fittab[["dchisq"]] <- dchisq
    fittab[["ddf"]] <- ddf
    fittab[["dPrChisq"]] <- dPrChisq

  }


  # add warning footnotes
  if (!is.null(msc$warnings)) {
    if (!grepl(c("NaNs produced"), msc$warnings[[1]]$message))
      fittab$addFootnote(msc$warnings[[1]]$message)
  }

  # check if there are any problems with the results and give warnings
  warningmsgs <- c("Absolute standardized loading estimates are NOT all <= 1",
                   "Construct VCV is NOT positive semi-definite",
                   "Reliability estimates are NOT all <= 1",
                   "Model-implied indicator VCV is NOT positive semi-definite")

  if (options[["group"]] == "") {
    for (i in seq_along(options[["models"]])) {
      warnings <- cSEM::verify(plsSemResults[[i]])[2:5]
      msgs <- warningmsgs[warnings]

      for (j in seq_along(msgs)) {
        warningFootnote <- gettextf("WARNING! %1$s: %2$s", options[["models"]][[i]][["name"]],  msgs[j])
        fittab$addFootnote(warningFootnote)
      }
    }
  } else {
    for (i in seq_along(options[["models"]])) {
      for (j in seq_along(plsSemResults[i])) {
        warnings <- cSEM::verify(plsSemResults[[i]][[j]])[2:5]
        msgs <- warningmsgs[warnings]

        for (k in seq_along(msgs)) {
          warningFootnote <- gettextf("WARNING! %1$s, group %2$s: %3$s",
                                      options[["models"]][[i]][["name"]], names(plsSemResults[[i]])[[j]], msgs[k])
          fittab$addFootnote(warningFootnote)
        }
      }
    }
  }

  #create jasp state and store msc for additional output tables
  modSelCriteria <- createJaspState()
  modelContainer[["modSelCriteria"]] <- modSelCriteria
  modSelCriteria$dependOn(optionsFromObject = modelContainer)
  modSelCriteria$object <- msc
}

# compute model selection criteria/ fit measures
.computeMSC <- function(plsSemResults, dataset, options) {
  resultsCopy <- plsSemResults
  if (options[["group"]] == "") {

    Ns <- rep(nrow(dataset), length(plsSemResults))
    modelSelectionCriteria <- cSEM::calculateModelSelectionCriteria(resultsCopy, .only_structural = FALSE, .by_equation = FALSE)
    modelFitMeasures       <- cSEM::assess(resultsCopy)
  } else{
    Ns <- list()
    for (i in names(resultsCopy)) {
      resultsCopy[[i]]$Information$Arguments$.id <- NULL
      Ns[length(Ns)+1] <- nrow(dataset[which(dataset[[options[["group"]]]]==i),])
    }

    modelSelectionCriteria <- sapply(resultsCopy, cSEM::calculateModelSelectionCriteria, .only_structural = FALSE, .by_equation = FALSE)
    modelFitMeasures <- sapply(resultsCopy, cSEM::assess)
    }
  return(list(Ns = Ns, msc = modelSelectionCriteria, mfm = modelFitMeasures))
}

.plsSemParameters <- function(modelContainer, dataset, options, ready) {
  if (!is.null(modelContainer[["params"]])) return()

  # create container for parameter estimates
  params <- createJaspContainer(gettext("Parameter Estimates"))
  params$position <- 1
  params$dependOn(c("models", "ciLevel"))

  modelContainer[["params"]] <- params

  if (length(options[["models"]]) < 2) {
    .plsSemParameterTables(modelContainer[["results"]][["object"]][[1]], NULL, params, options, ready)
  } else {

    for (i in seq_along(options[["models"]])) {
      fit <- modelContainer[["results"]][["object"]][[i]]
      name <- options[["models"]][[i]][["name"]]
      .plsSemParameterTables(fit, name, params, options, ready)
    }
  }
}

# Parameter Estimates Tables
.plsSemParameterTables <- function(fit, name, parentContainer, options, ready) {
  if (is.null(name)) {
    pecont <- parentContainer
  } else {
    pecont <- createJaspContainer(name, initCollapsed = TRUE)
  }

  # Measurement model

  # create weights table
  weightTab <- createJaspTable(title = gettext("Weights"))

  if (options[["group"]] != "")
    weightTab$addColumnInfo(name = "group",  title = gettext("Group"),      type = "string", combine = TRUE)

  weightTab$addColumnInfo(name = "lhs",      title = gettext("Construct"),   type = "string", combine = TRUE)
  weightTab$addColumnInfo(name = "rhs",      title = gettext("Indicator"),  type = "string")
  weightTab$addColumnInfo(name = "est",      title = gettext("Estimate"),   type = "number")

  if (options[["errorCalculationMethod"]] != "none") {
    weightTab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number")
    weightTab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number")
    weightTab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "pvalue")
    weightTab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number",
                            overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
    weightTab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                            overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
  }


  pecont[["weight"]] <- weightTab

  # create loadings table
  loadingTab <- createJaspTable(title = gettext("Loadings"))

  if (options[["group"]] != "")
    loadingTab$addColumnInfo(name = "group",  title = gettext("Group"),      type = "string", combine = TRUE)

  loadingTab$addColumnInfo(name = "lhs",      title = gettext("Construct"),     type = "string", combine = TRUE)
  loadingTab$addColumnInfo(name = "rhs",      title = gettext("Indicator"),  type = "string")
  loadingTab$addColumnInfo(name = "est",      title = gettext("Estimate"),   type = "number")

  if (options[["errorCalculationMethod"]] != "none") {
    loadingTab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number")
    loadingTab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number")
    loadingTab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "pvalue")
    loadingTab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number",
                             overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
    loadingTab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                             overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
  }

  pecont[["loading"]] <- loadingTab

  # Structural Model

  #create paths table
  pathTab <- createJaspTable(title = gettext("Regression Coefficients"))

  if (options[["group"]] != "")
    pathTab$addColumnInfo(name = "group",  title = gettext("Group"),        type = "string", combine = TRUE)

  pathTab$addColumnInfo(name = "lhs",      title = gettext("Outcome"),      type = "string", combine = TRUE)
  pathTab$addColumnInfo(name = "rhs",      title = gettext("Predictor"),    type = "string")
  pathTab$addColumnInfo(name = "est",      title = gettext("Estimate"),     type = "number")

  if (options[["errorCalculationMethod"]] != "none") {
    pathTab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number")
    pathTab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number")
    pathTab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "pvalue")
    pathTab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number",
                          overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
    pathTab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                          overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
  }
  pathTab$addColumnInfo(name = "f2",       title = "f\u00B2",               type = "number")

  pecont[["path"]] <- pathTab

  # create total effects table
  totalTab <- createJaspTable(title = gettext("Total Effects"))

  if (options[["group"]] != "")
    totalTab$addColumnInfo(name = "group",  title = gettext("Group"),      type = "string", combine = TRUE)

  totalTab$addColumnInfo(name = "lhs",      title = gettext("Outcome"),     type = "string", combine = TRUE)
  totalTab$addColumnInfo(name = "rhs",      title = gettext("Predictor"),  type = "string")
  totalTab$addColumnInfo(name = "est",      title = gettext("Estimate"),   type = "number")
  if (options[["errorCalculationMethod"]] != "none") {
    totalTab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number")
    totalTab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number")
    totalTab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "pvalue")
    totalTab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number",
                           overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
    totalTab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                           overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
  }

  pecont[["total"]] <- totalTab


  if (!is.null(name)) parentContainer[[name]] <- pecont

  if (!ready) return()

  # compute parameter estimates
  if (options[["errorCalculationMethod"]] == "none") {
    summ <- cSEM::summarize(fit)
    pe <- list()
    if (options$group == "") {
      summ <- summ$Estimates

      pe[["Weight_estimates"]] <- list()
      pe[["Weight_estimates"]][["mean"]] <- summ$Weight_estimates$Estimate
      names(pe[["Weight_estimates"]][["mean"]]) <- summ$Weight_estimates$Name

      pe[["vifb"]] <-list()
      pe[["vifb"]][["mean"]] <- pe[["Weight_estimates"]][["mean"]]
      pe[["vifb"]][["mean"]][names(pe[["vifb"]][["mean"]])] <- NA
      VIFBtemp <- .plsSEMVIFBhelper(fit)
      if(!is.null(VIFBtemp)){
        weightTab$addColumnInfo(name = "vifb",      title = gettext("VIF"),   type = "number")
        pecont[["weight"]] <- weightTab
        pe[["vifb"]][["mean"]][names(VIFBtemp)] <- VIFBtemp
      }

      pe[["Loading_estimates"]] <- list()
      pe[["Loading_estimates"]][["mean"]] <- summ$Loading_estimates$Estimate
      names(pe[["Loading_estimates"]][["mean"]]) <- summ$Loading_estimates$Name

      pe[["Path_estimates"]] <- list()
      pe[["Path_estimates"]][["mean"]] <- summ$Path_estimates$Estimate
      names(pe[["Path_estimates"]][["mean"]]) <- summ$Path_estimates$Name

      pe[["vif"]] <- list()
      pe[["vif"]][["mean"]] <- pe[["Path_estimates"]][["mean"]]
      pe[["vif"]][["mean"]][names(pe[["vif"]][["mean"]])] <- NA
      VIFtemp <- .plsSEMVIFhelper(fit)
      if(!is.null(VIFtemp)){
      pathTab$addColumnInfo(name = "vif",      title = gettext("VIF")     ,     type = "number")
      pecont[["path"]] <- pathTab
      pe[["vif"]][["mean"]][names(VIFtemp)] <- VIFtemp
      }

      pe[["Total_effect"]] <- list()
      pe[["Total_effect"]][["mean"]] <- summ$Effect_estimates$Total_effect$Estimate
      names(pe[["Total_effect"]][["mean"]]) <- summ$Effect_estimates$Total_effect$Name
    } else{
      IdxViFB <- 0
      IdxViF <- 0
      for (i in names(summ)) {
        pe[[i]] <- list()
        pe[[i]][["Weight_estimates"]] <- list()
        pe[[i]][["Weight_estimates"]][["mean"]] <- summ[[i]]$Estimates$Weight_estimates$Estimate
        names(pe[[i]][["Weight_estimates"]][["mean"]]) <- summ[[i]]$Estimates$Weight_estimates$Name

        pe[[i]][["vifb"]] <-list()
        pe[[i]][["vifb"]][["mean"]] <- pe[[i]][["Weight_estimates"]][["mean"]]
        pe[[i]][["vifb"]][["mean"]][names(pe[[i]][["vifb"]][["mean"]])] <- NA
        VIFBtemp <- .plsSEMVIFBhelper(fit[[i]])
        if(!is.null(VIFBtemp)){
          pe[[i]][["vifb"]][["mean"]][names(VIFBtemp)] <- VIFBtemp

          if(IdxViFB==0){
          weightTab$addColumnInfo(name = "vifb",      title = gettext("VIF"),   type = "number")
          pecont[["weight"]] <- weightTab
          IdxViFB <- 1
          }
        }


        pe[[i]][["Loading_estimates"]] <- list()
        pe[[i]][["Loading_estimates"]][["mean"]] <- summ[[i]]$Estimates$Loading_estimates$Estimate
        names(pe[[i]][["Loading_estimates"]][["mean"]]) <- summ[[i]]$Estimates$Loading_estimates$Name

        pe[[i]][["Path_estimates"]] <- list()
        pe[[i]][["Path_estimates"]][["mean"]] <- summ[[i]]$Estimates$Path_estimates$Estimate
        names(pe[[i]][["Path_estimates"]][["mean"]]) <- summ[[i]]$Estimates$Path_estimates$Name

        pe[[i]][["vif"]] <- list()
        pe[[i]][["vif"]][["mean"]] <- pe[[i]][["Path_estimates"]][["mean"]]
        pe[[i]][["vif"]][["mean"]][names(pe[[i]][["vif"]][["mean"]])] <- NA
        VIFtemp <- .plsSEMVIFhelper(fit[[i]])
        if(!is.null(VIFtemp)){
          pe[[i]][["vif"]][["mean"]][names(VIFtemp)] <- VIFtemp

          if(IdxViF==0){
          pathTab$addColumnInfo(name = "vif",      title = gettext("VIF")     ,     type = "number")
          pecont[["path"]] <- pathTab
          IdxViF <- 1
          }
        }

        pe[[i]][["Total_effect"]] <- list()
        pe[[i]][["Total_effect"]][["mean"]] <- summ[[i]]$Estimates$Effect_estimates$Total_effect$Estimate
        names(pe[[i]][["Total_effect"]][["mean"]]) <- summ[[i]]$Estimates$Effect_estimates$Total_effect$Name
      }
    }
  } else {

    pe <- cSEM::infer(fit, .alpha = 1 - options[["ciLevel"]])

    if (options$group == "") {
      pe[["vifb"]] <-list()
      pe[["vifb"]][["mean"]] <- pe[["Weight_estimates"]][["mean"]]
      pe[["vifb"]][["mean"]][names(pe[["vifb"]][["mean"]])] <- NA
      VIFBtemp <- .plsSEMVIFBhelper(fit)
      if(!is.null(VIFBtemp)){
        weightTab$addColumnInfo(name = "vifb",      title = gettext("VIF"),   type = "number")
        pecont[["weight"]] <- weightTab
        pe[["vifb"]][["mean"]][names(VIFBtemp)] <- VIFBtemp
      }


    pe[["vif"]] <- list()
    pe[["vif"]][["mean"]] <- pe[["Path_estimates"]][["mean"]]
    pe[["vif"]][["mean"]][names(pe[["vif"]][["mean"]])] <- NA
    VIFtemp <- .plsSEMVIFhelper(fit)
    if(!is.null(VIFtemp)){
      pathTab$addColumnInfo(name = "vif",      title = gettext("VIF")     ,     type = "number")
      pecont[["path"]] <- pathTab
      pe[["vif"]][["mean"]][names(VIFtemp)] <- VIFtemp
    }
    }else{
      IdxViFB <- 0
      IdxViF <- 0
      for (i in names(pe)) {
        pe[[i]][["vifb"]] <-list()
        pe[[i]][["vifb"]][["mean"]] <- pe[[i]][["Weight_estimates"]][["mean"]]
        pe[[i]][["vifb"]][["mean"]][names(pe[[i]][["vifb"]][["mean"]])] <- NA
        VIFBtemp <- .plsSEMVIFBhelper(fit[[i]])
        if(!is.null(VIFBtemp)){
          pe[[i]][["vifb"]][["mean"]][names(VIFBtemp)] <- VIFBtemp
          if(IdxViFB==0){
            weightTab$addColumnInfo(name = "vifb",      title = gettext("VIF"),   type = "number")
            pecont[["weight"]] <- weightTab
            IdxViFB <- 1
          }
        }


        pe[[i]][["vif"]] <- list()
        pe[[i]][["vif"]][["mean"]] <- pe[[i]][["Path_estimates"]][["mean"]]
        pe[[i]][["vif"]][["mean"]][names(pe[[i]][["vif"]][["mean"]])] <- NA
        VIFtemp <- .plsSEMVIFhelper(fit[[i]])
        if(!is.null(VIFtemp)){
          pe[[i]][["vif"]][["mean"]][names(VIFtemp)] <- VIFtemp
          if(IdxViF==0){
          pathTab$addColumnInfo(name = "vif",      title = gettext("VIF")     ,     type = "number")
          pecont[["path"]] <- pathTab
          IdxViF <- 1
          }
        }
      }
    }
  }

  # fill Weights table

  if (options[["group"]] == "") {
    weightEstimates <- try(.prepareEstimates(pe, estimateType = "Weight_estimates", options = options))
    if (isTryError(weightEstimates)) {
      pecont[["weight"]] <- NULL
    }
  } else {
    weightEstimates <- try(lapply(pe, .prepareEstimates, estimateType = "Weight_estimates", options = options))
    if (isTryError(weightEstimates)) {
      pecont[["weight"]] <- NULL
    } else {
      for (i in names(weightEstimates)) {
        weightEstimates[[i]][["group"]] <- rep(i, length(weightEstimates[[i]][["rhs"]]))
      }
      weightEstimates <- as.data.frame(Reduce(function(...) merge(..., all=T), weightEstimates))
      weightEstimates <- weightEstimates[order(weightEstimates[["group"]], weightEstimates[["lhs"]]),]
    }
  }

  if (!isTryError(weightEstimates)) {

    if (options[["group"]] != "")
      weightTab[["group"]]    <- weightEstimates[["group"]]

    weightTab[["rhs"]]      <- weightEstimates[["rhs"]]
    weightTab[["lhs"]]      <- weightEstimates[["lhs"]]
    weightTab[["est"]]      <- weightEstimates[["est"]]

    if (options[["errorCalculationMethod"]] != "none") {
      weightTab[["se"]]       <- weightEstimates[["se"]]
      weightTab[["z"]]        <- weightEstimates[["zVal"]]
      weightTab[["pvalue"]]   <- weightEstimates[["pVal"]]
      weightTab[["ci.lower"]] <- weightEstimates[["ciLower"]]
      weightTab[["ci.upper"]] <- weightEstimates[["ciUpper"]]
    }

    weightTab[["vifb"]]      <- weightEstimates[["vifb"]]

  }


  # fill Loadings table

  if (options[["group"]] == "") {
    loadingEstimates <- try(.prepareEstimates(pe, estimateType = "Loading_estimates", options = options))
    if (isTryError(loadingEstimates)) {
      pecont[["loading"]] <- NULL
    }
  } else {
    loadingEstimates <- try(lapply(pe, .prepareEstimates, estimateType = "Loading_estimates", options = options))
    if (isTryError(loadingEstimates)) {
      pecont[["loading"]] <- NULL
    } else {
      for (i in names(loadingEstimates)) {
        loadingEstimates[[i]][["group"]] <- rep(i, length(loadingEstimates[[i]][["rhs"]]))
      }
      loadingEstimates <- as.data.frame(Reduce(function(...) merge(..., all=T), loadingEstimates))
      loadingEstimates <- loadingEstimates[order(loadingEstimates[["group"]], loadingEstimates[["lhs"]]),]
    }
  }

  if (!isTryError(loadingEstimates)) {

    if (options[["group"]] != "")
      loadingTab[["group"]]    <- loadingEstimates[["group"]]

    loadingTab[["rhs"]]      <- loadingEstimates[["rhs"]]
    loadingTab[["lhs"]]      <- loadingEstimates[["lhs"]]
    loadingTab[["est"]]      <- loadingEstimates[["est"]]

    if (options[["errorCalculationMethod"]] != "none") {
      loadingTab[["se"]]       <- loadingEstimates[["se"]]
      loadingTab[["z"]]        <- loadingEstimates[["zVal"]]
      loadingTab[["pvalue"]]   <- loadingEstimates[["pVal"]]
      loadingTab[["ci.lower"]] <- loadingEstimates[["ciLower"]]
      loadingTab[["ci.upper"]] <- loadingEstimates[["ciUpper"]]
    }
  }


  # fill Paths table
  f2 <- cSEM::calculatef2(fit)
  if (options[["group"]] == "") {
    pathEstimates <- try(.prepareEstimates(pe, estimateType = "Path_estimates", options = options))

    if (isTryError(pathEstimates)) {
      pecont[["path"]] <- NULL
    } else {
      f2_list <- list()
      for (i in 1:nrow(f2)) {
        f2_list <- c(f2_list, f2[i,])
      }
      pathEstimates[["f2"]] <- unlist(f2_list[f2_list != 0])
    }

  } else {
    pathEstimates <- try(lapply(pe, .prepareEstimates, estimateType = "Path_estimates", options = options))
    if (isTryError(pathEstimates)) {
      pecont[["path"]] <- NULL
    } else {
      for (i in names(pathEstimates)) {
        pathEstimates[[i]][["group"]] <- rep(i, length(pathEstimates[[i]][["rhs"]]))
        f2_list <- list()
        for (j in 1:nrow(f2[[i]])) {
          f2_list <- c(f2_list, f2[[i]][j,])
        }
        pathEstimates[[i]][["f2"]] <- unlist(f2_list[f2_list != 0])
      }

      pathEstimates <- as.data.frame(Reduce(function(...) merge(..., all=T), pathEstimates))
      pathEstimates <- pathEstimates[order(pathEstimates[["group"]], pathEstimates[["lhs"]], pathEstimates[["rhs"]]),]
    }
  }


  if (!isTryError(pathEstimates)) {

    if (options[["group"]] != "")
      pathTab[["group"]]    <- pathEstimates[["group"]]

    pathTab[["rhs"]]      <- pathEstimates[["rhs"]]
    pathTab[["lhs"]]      <- pathEstimates[["lhs"]]
    pathTab[["est"]]      <- pathEstimates[["est"]]

    pathTab[["vif"]]      <- pathEstimates[["vif"]]
    pathTab[["f2"]]       <- pathEstimates[["f2"]]

    if (options[["errorCalculationMethod"]] != "none") {
      pathTab[["se"]]       <- pathEstimates[["se"]]
      pathTab[["z"]]        <- pathEstimates[["zVal"]]
      pathTab[["pvalue"]]   <- pathEstimates[["pVal"]]
      pathTab[["ci.lower"]] <- pathEstimates[["ciLower"]]
      pathTab[["ci.upper"]] <- pathEstimates[["ciUpper"]]
    }
  }


  # fill Total effects table


  if (options[["group"]] == "") {
    totalEstimates <- try(.prepareEstimates(pe, estimateType = "Total_effect", options = options))
    if (isTryError(totalEstimates)) {
      pecont[["total"]] <- NULL
    }
  } else {
    totalEstimates <- try(lapply(pe, .prepareEstimates, estimateType = "Total_effect", options = options))
    if (isTryError(totalEstimates)) {
      pecont[["total"]] <- NULL
    } else {
      for (i in names(totalEstimates)) {
        totalEstimates[[i]][["group"]] <- rep(i, length(totalEstimates[[i]][["rhs"]]))
      }
      totalEstimates <- as.data.frame(Reduce(function(...) merge(..., all=T), totalEstimates))
      totalEstimates <- totalEstimates[order(totalEstimates[["group"]], totalEstimates[["lhs"]]),]
    }
  }

  if (!isTryError(totalEstimates)) {

    if (options[["group"]] != "")
      totalTab[["group"]]    <- totalEstimates[["group"]]

    totalTab[["rhs"]]      <- totalEstimates[["rhs"]]
    totalTab[["lhs"]]      <- totalEstimates[["lhs"]]
    totalTab[["est"]]      <- totalEstimates[["est"]]

    if (options[["errorCalculationMethod"]] != "none") {
      totalTab[["se"]]       <- totalEstimates[["se"]]
      totalTab[["z"]]        <- totalEstimates[["zVal"]]
      totalTab[["pvalue"]]   <- totalEstimates[["pVal"]]
      totalTab[["ci.lower"]] <- totalEstimates[["ciLower"]]
      totalTab[["ci.upper"]] <- totalEstimates[["ciUpper"]]
    }
  }
}

# help function to extract data for parameter tables
.prepareEstimates <- function(pe, estimateType, options) {

  operator <- ifelse(estimateType == "Weight_estimates",
                     " <~ ",
                     ifelse(estimateType == "Loading_estimates",
                            " =~ ",
                            " ~ "))

  varNamesDf <- t(data.frame(sapply(names(pe[[estimateType]]$mean), strsplit, split = operator, fixed = TRUE)))

  rhs      <- varNamesDf[,2]
  lhs      <- varNamesDf[,1]
  est      <- pe[[estimateType]]$mean


  if (options[["errorCalculationMethod"]] == "none") {
    temp=list(rhs=rhs, lhs=lhs, est=est)

    if(estimateType == "Weight_estimates"){
      temp$vifb <- pe$vifb$mean
    }

    if(estimateType == "Path_estimates"){
      temp$vif <- pe$vif$mean
    }
    return(temp)

  } else {
    se       <- pe[[estimateType]]$sd
    zVal     <- pe[[estimateType]]$mean / pe[[estimateType]]$sd
    pVal     <- pnorm(abs(pe[[estimateType]]$mean / pe[[estimateType]]$sd), lower.tail = FALSE)
    ciLower  <- pe[[estimateType]]$CI_percentile[1,]
    ciUpper  <- pe[[estimateType]]$CI_percentile[2,]

    temp<- list(rhs=rhs, lhs=lhs, est=est, se=se, zVal=zVal, pVal=pVal, ciLower=ciLower, ciUpper=ciUpper)

    if(estimateType == "Weight_estimates"){
      temp$vifb <- pe$vifb$mean
    }

    if(estimateType == "Path_estimates"){
      temp$vif <- pe$vif$mean
    }

    return(temp)
  }
}

.plsSemPrediction <- function(modelContainer, options, ready) {
  if (!options[["endogenousIndicatorPrediction"]] || !is.null(modelContainer[["predict"]])) return()

  predict <- createJaspContainer(gettext("Endogenous Indicator Prediction"))
  predict$position <- 2
  predict$dependOn(c("endogenousIndicatorPrediction", "models", "kFolds", "repetitions", "benchmark", "predictedScore"))
  modelContainer[["predict"]] <- predict

  if (length(options[["models"]]) < 2) {
    .plsSemPredictionTables(modelContainer[["results"]][["object"]][[1]], NULL, predict, modelContainer, options, ready)
  } else {
    for (i in seq_along(modelContainer[["results"]][["object"]])) {
      fit <- modelContainer[["results"]][["object"]][[i]]
      name <- options[["models"]][[i]][["name"]]
      .plsSemPredictionTables(fit, name, predict, modelContainer, options, ready)
    }
  }
}


.plsSemPredictionTables <- function(fit, name, parent, modelContainer, options, ready) {

  if (is.null(name)) {
    predictcont <- parent
  } else {
    predictcont <- createJaspContainer(name, initCollapsed = TRUE)
  }

  # Error messages

  if (options[["benchmark"]] != "none" && options[["benchmark"]] != "all") {
    benchmarks <- options[["benchmark"]]
  }
  else if (options[["benchmark"]] == "all") {
    benchmarks <- c("lm", "PLS-PM", "GSCA", "PCA", "MAXVAR")
    benchmarks <- benchmarks[benchmarks != "PLS-PM"]
  } else {
    benchmarks <- NULL
  }

  if (options[["benchmark"]] != "none" && options[["benchmark"]] != "all" && benchmarks == "PLS-PM") {
    errormsg <- gettextf("The target model uses the same weighting approach as the benchmark model, please choose another benchmark.")
    modelContainer$setError(errormsg)
    modelContainer$dependOn("benchmark")
    return()
  }
  if (options[["benchmark"]] == "all" && options[["predictedScore"]]) {
    errormsg <- gettextf("For the predicted indicator scores table(s), please select a single benchmark or 'none'.")
    modelContainer$setError(errormsg)
    modelContainer$dependOn("benchmark")
    return()
  }

  #Create metrics table
  metricstab <- createJaspTable(gettext("Prediction Metrics"))

  if (options[["group"]] != "")
    metricstab$addColumnInfo(name = "group",  title = gettext("Group"),      type = "string", combine = TRUE)

  metricstab$addColumnInfo(name = "indicator", title = gettext("Indicator"),type = "string")

  metricstab$addColumnInfo(name = "mae", title = gettext("Target MAE"), type = "number")

  if("lm" %in% benchmarks)
    metricstab$addColumnInfo(name = "maelm",     title = gettext("Linear model MAE"), type = "number")
  if("PLS-PM" %in% benchmarks)
    metricstab$addColumnInfo(name = "maePLS-PM", title = gettext("PLS-PM MAE"),       type = "number")
  if("GSCA" %in% benchmarks)
    metricstab$addColumnInfo(name = "maeGSCA",   title = gettext("GSCA MAE"),         type = "number")
  if("PCA" %in% benchmarks)
    metricstab$addColumnInfo(name = "maePCA",    title = gettext("PCA MAE"),          type = "number")
  if("MAXVAR" %in% benchmarks)
    metricstab$addColumnInfo(name = "maeMAXVAR", title = gettext("MAXVAR MAE"),       type = "number")

  metricstab$addColumnInfo(name = "rmse", title = gettext(" Target RMSE"), type = "number")

  if("lm" %in% benchmarks)
    metricstab$addColumnInfo(name = "rmselm",     title = gettext("Linear model RMSE"), type = "number")
  if("PLS-PM" %in% benchmarks)
    metricstab$addColumnInfo(name = "rmsePLS-PM", title = gettext("PLS-PM RMSE"),       type = "number")
  if("GSCA" %in% benchmarks)
    metricstab$addColumnInfo(name = "rmseGSCA",   title = gettext("GSCA RMSE"),         type = "number")
  if("PCA" %in% benchmarks)
    metricstab$addColumnInfo(name = "rmsePCA",    title = gettext("PCA RMSE"),          type = "number")
  if("MAXVAR" %in% benchmarks)
    metricstab$addColumnInfo(name = "rmseMAXVAR", title = gettext("MAXVAR RMSE"),       type = "number")

  metricstab$addColumnInfo(name = "q2", title = gettext("Target Q2 Prediction"), type = "number")

  predictcont[["metrics"]] <- metricstab

  if(!ready) return()

  # Predict indicator scores and compute metrics
  if (options[["benchmark"]] == "all") {
    startProgressbar(length(benchmarks), "Predicting")

    prediction_list <- list()
    for (i in seq_along(benchmarks)) {
      prediction <- try(cSEM::predict(fit, .handle_inadmissibles = "ignore", .benchmark = benchmarks[[i]], .cv_folds = options[["kFolds"]], .r = options[["repetitions"]]))
      if (isTryError(prediction)) {
        err <- .extractErrorMessage(prediction)
        if(grepl("attempt to set 'colnames'", err))
          err <- gettext("There are not enough observations for each k-fold, try setting 'cross-validation k-folds' to a lower number")
        if(grepl("the condition has length > 1", err))
          err <- gettext("Are all indicator variables set to 'scale'?")
        errmsg <- gettextf("Prediction failed Message: %1$s", err)
        predictcont$setError(errmsg)
        return()
      }
      progressbarTick()
      prediction_list[i] <- prediction
    }
  } else if (options[["benchmark"]] == "none") {
    prediction <- try(cSEM::predict(fit, .handle_inadmissibles = "ignore", .cv_folds = options[["kFolds"]], .r = options[["repetitions"]]))
  } else {
    prediction <- try(cSEM::predict(fit, .handle_inadmissibles = "ignore", .benchmark = benchmarks, .cv_folds = options[["kFolds"]], .r = options[["repetitions"]]))
  }

  if (isTryError(prediction)) {
    err <- .extractErrorMessage(prediction)
    if(grepl("attempt to set 'colnames'", err))
      err <- "There are not enough observations for each k-fold, try setting 'cross-validation k-folds' to a lower number"
    if(grepl("the condition has length > 1", err))
      err <- "Are all indicator variables set to 'scale'?"
    errmsg <- gettextf("Prediction failed Message: %1$s", err)
    predictcont$setError(errmsg)
    return()
  }

  # Fill Endogenous indicator prediction metrics table
  if (options[["group"]] == "") {
    if (options[["benchmark"]] == "all")
      prediction <- prediction_list[[benchmarks[1]]]
    metrics <- prediction$Prediction_metrics

    metricstab[["indicator"]]     <- metrics$Name
    metricstab[["mae"]]           <- metrics$MAE_target
    metricstab[["rmse"]]          <- metrics$RMSE_target
    metricstab[["q2"]]            <- metrics$Q2_predict

    if(options[["benchmark"]] != "none" && options[["benchmark"]] != "all") {
      metricstab[[paste0("mae", benchmarks)]] <- metrics$MAE_benchmark
      metricstab[[paste0("rmse", benchmarks)]] <- metrics$RMSE_benchmark
    }
    if (options[["benchmark"]] == "all") {
      for (i in seq_along(benchmarks)) {
        metricstab[[paste0("mae", benchmarks[[i]])]]  <- prediction_list[[benchmarks[[i]]]][["Prediction_metrics"]][["MAE_benchmark"]]
        metricstab[[paste0("rmse", benchmarks[[i]])]] <- prediction_list[[benchmarks[[i]]]][["Prediction_metrics"]][["RMSE_benchmark"]]
      }
    }
  } else {
    if (options[["benchmark"]] == "all")
      prediction <- prediction_list[[benchmarks[[1]]]]
    group_list <- list()
    for (i in names(prediction)) {
      group_i <- rep(i, length(prediction[[i]][["Prediction_metrics"]][["Name"]]))
      group_list <- c(group_list, group_i)
    }

    metricstab[["group"]]         <- group_list
    metricstab[["indicator"]]     <- unlist(lapply(prediction, function(x) x[["Prediction_metrics"]][["Name"]]))
    metricstab[["mae"]]           <- unlist(lapply(prediction, function(x) x[["Prediction_metrics"]][["MAE_target"]]))
    metricstab[["rmse"]]          <- unlist(lapply(prediction, function(x) x[["Prediction_metrics"]][["RMSE_target"]]))
    metricstab[["q2"]]            <- unlist(lapply(prediction, function(x) x[["Prediction_metrics"]][["Q2_predict"]]))

    if(options[["benchmark"]] != "none" && options[["benchmark"]] != "all") {
      metricstab[[paste0("mae", benchmarks)]] <- unlist(lapply(prediction, function(x) x[["Prediction_metrics"]][["MAE_benchmark"]]))
      metricstab[[paste0("rmse", benchmarks)]] <- unlist(lapply(prediction, function(x) x[["Prediction_metrics"]][["RMSE_benchmark"]]))
    }

    if(options[["benchmark"]] == "all") {
      for (i in seq_along(benchmarks)) {
        prediction <- prediction_list[[benchmarks[[i]]]]
        metricstab[[paste0("mae", benchmarks[[i]])]]  <- unlist(lapply(prediction, function(x) x[["Prediction_metrics"]][["MAE_benchmark"]]))
        metricstab[[paste0("rmse", benchmarks[[i]])]] <- unlist(lapply(prediction, function(x) x[["Prediction_metrics"]][["RMSE_benchmark"]]))
      }
    }
  }


  if (!is.null(name)) parent[[name]] <- predictcont

}

# Additional Fit Measures Table
.plsSemAdditionalFits <- function(modelContainer, dataset, options, ready) {
  if (!options[["additionalFitMeasures"]] || !is.null(modelContainer[["addfit"]])) return()

  mfm <- modelContainer[["modSelCriteria"]]$object$value

  # create additional fits table
  fitin <- createJaspTable(gettext("Additional Fit Measures"))
  fitin$addColumnInfo(name = "index", title = gettext("Index"), type = "string")
  if (length(options[["models"]]) < 2) {
    if(options[["group"]] == "")
      fitin$addColumnInfo(name = "value", title = gettext("Value"), type = "number")
    else {
      for (j in colnames(mfm$mfm)) {
        fitin$addColumnInfo(name = paste0("value_", j), title = gettext(j), overtitle = options[["models"]][[1]][["name"]],
                            type = "number")
      }
    }
  } else {
    if(options[["group"]] == "") {
      for (i in seq_along(options[["models"]])) {
        fitin$addColumnInfo(name = paste0("value_", i), title = options[["models"]][[i]][["name"]], type = "number")
      }
    } else {
      for (i in seq_along(options[["models"]])) {
        for (j in colnames(mfm[[i]]$mfm)) {
          fitin$addColumnInfo(name = paste0("value_",i,"_", j), title = gettext(j), overtitle = options[["models"]][[i]][["name"]],
                              type = "number")
        }
      }
    }
  }

  fitin$dependOn(c("additionalFitMeasures", "models"))
  fitin$position <- 0.5

  modelContainer[["addfit"]] <- fitin


  if (!ready || modelContainer$getError()) return()

  fitin[["index"]] <- c(
    gettext("Comparative Fit Index (CFI)"),
    gettext("Goodness of fit index (GFI)"),
    gettext("Hoelter's critical N (CN)"),
    gettext("Bollen's Incremental Fit Index (IFI)"),
    gettext("Bentler-Bonett Non-normed Fit Index (NNFI)"),
    gettext("Bentler-Bonett Normed Fit Index (NFI)"),
    gettext("Root mean square error of approximation (RMSEA)"),
    gettext("Root mean square residual covariance (RMS theta)"),
    gettext("Standardized root mean square residual (SRMR)"),
    gettext("Goodness of Fit (GoF)"),
    gettext("Geodesic distance"),
    gettext("Squared Euclidean distance"),
    gettext( "Maximum likelihood-based dinstance")


  )


  # fill additional fits table

  if (length(options[["models"]]) < 2) {

    if (options[["group"]] == "") {

      fitin[["value"]] <- list(mfm$mfm$CFI, mfm$mfm$GFI, mfm$mfm$CN, mfm$mfm$IFI, mfm$mfm$NNFI,
                               mfm$mfm$NFI, mfm$mfm$RMSEA, mfm$mfm$RMS_theta, mfm$mfm$SRMR,
                               mfm$mfm$GoF, mfm$mfm$DG, mfm$mfm$DL, mfm$mfm$DML)

    } else {
      for (j in colnames(mfm$mfm)) {
        fitin[[paste0("value_", j)]] <- list(mfm$mfm["CFI", j], mfm$mfm["GFI", j], mfm$mfm["CN", j], mfm$mfm["IFI", j],
                                             mfm$mfm["NNFI", j], mfm$mfm["NFI", j], mfm$mfm["RMSEA", j], mfm$mfm["RMS_theta", j],
                                             mfm$mfm["SRMR", j], mfm$mfm["GoF", j], mfm$mfm["DG", j], mfm$mfm["DL", j],
                                             mfm$mfm["DML", j])

      }
    }
  } else {
    if (options[["group"]] == "") {
      for (i in seq_along(options[["models"]])) {
        fitin[[paste0("value_", i)]] <- list(mfm[[i]]$mfm$CFI, mfm[[i]]$mfm$GFI, mfm[[i]]$mfm$CN, mfm[[i]]$mfm$IFI,
                                             mfm[[i]]$mfm$NNFI, mfm[[i]]$mfm$NFI, mfm[[i]]$mfm$RMSEA,
                                             mfm[[i]]$mfm$RMS_theta, mfm[[i]]$mfm$SRMR, mfm[[i]]$mfm$GoF,
                                             mfm[[i]]$mfm$DG, mfm[[i]]$mfm$DL, mfm[[i]]$mfm$DML)
      }



    } else {
      for (i in seq_along(options[["models"]])) {
        for (j in colnames(mfm[[i]]$mfm)) {
          fitin[[paste0("value_",i,"_", j)]] <- list(mfm[[i]]$mfm["CFI",j], mfm[[i]]$mfm["GFI", j], mfm[[i]]$mfm["CN", j],
                                                     mfm[[i]]$mfm["IFI", j], mfm[[i]]$mfm["NNFI", j], mfm[[i]]$mfm["NFI", j],
                                                     mfm[[i]]$mfm["RMSEA", j], mfm[[i]]$mfm["RMS_theta", j], mfm[[i]]$mfm["SRMR", j],
                                                     mfm[[i]]$mfm["GoF", j], mfm[[i]]$mfm["DG", j], mfm[[i]]$mfm["DL", j],
                                                     mfm[[i]]$mfm["DML", j])

        }
      }
    }
  }
}

# Rsquared table
.plsSemRsquared <- function(modelContainer, dataset, options, ready) {
  if (!options[["rSquared"]] || !is.null(modelContainer[["tabrsquared"]])) return()

  # create rsquared table
  tabr2 <- createJaspTable(gettext("R-Squared"))
  if (options[["group"]] != "")
    tabr2$addColumnInfo(name = "grp", title = "Group", type = "string", combine = TRUE)
  tabr2$addColumnInfo(name = "var", title = "Outcome", type = "string")
  if (length(options[["models"]]) < 2) {
    tabr2$addColumnInfo(name = "rsq", title = "R\u00B2", type = "number")
    tabr2$addColumnInfo(name = "adjustedRsq", title = "Adjusted R\u00B2", type = "number")
    } else {
    for (i in seq_along(options[["models"]])) {
      tabr2$addColumnInfo(name = paste0("rsq_", i), title = options[["models"]][[i]][["name"]],
                          overtitle = "R\u00B2", type = "number")
    }
    for (i in seq_along(options[["models"]])) {
      tabr2$addColumnInfo(name = paste0("adjustedRsq_", i), title = options[["models"]][[i]][["name"]],
                          overtitle = "Adjusted R\u00B2", type = "number")
    }
  }

  tabr2$dependOn(c("rSquared", "models"))
  tabr2$position <- 0.75

  modelContainer[["tabrsquared"]] <- tabr2

  if (!ready || modelContainer$getError()) return()

  # compute data and fill rsquared table
  mfm <- modelContainer[["modSelCriteria"]]$object$value
  if (options[["group"]] == "") {

    if (length(options[["models"]]) < 2) {

      r2                     <- mfm$mfm$R2
      tabr2[["var"]]     <- as.list(names(r2))
      tabr2[["rsq"]]         <- as.list(r2)
      tabr2[["adjustedRsq"]] <- as.list(mfm$mfm$R2_adj)
    } else {

      # determine variable names

      r2li <- list()
      adjR2li <- list()
      for (i in seq_along(options[["models"]])) {
        r2li    <- c(r2li, list(mfm[[i]]$mfm$R2))
        adjR2li <- c(adjR2li, list(mfm[[i]]$mfm$R2_adj))
      }


      # generate dfs with these names
      r2df <- data.frame("varname" = unique(unlist(lapply(r2li, names))))
      adjR2df <- data.frame("varname" = unique(unlist(lapply(adjR2li, names))))
      tabr2[["var"]] <- unique(unlist(lapply(r2li, names)))

      for (i in 1:length(r2li)) {
        # fill matching vars from model with df
        r2df[match(names(r2li[[i]]), r2df[["varname"]]), i + 1] <- r2li[[i]]
        # add column to table
        tabr2[[paste0("rsq_", i)]] <- r2df[[i + 1]]

        adjR2df[match(names(adjR2li[[i]]), adjR2df[["varname"]]), i + 1] <- adjR2li[[i]]
        # add column to table
        tabr2[[paste0("adjustedRsq_", i)]] <- adjR2df[[i + 1]]
      }
    }
  } else {
    if (length(options[["models"]]) < 2) {

      r2res                  <- mfm$mfm["R2",]
      adjR2res               <- mfm$mfm["R2_adj",]
      groups                 <- as.list(names(r2res))
      vars                   <- unlist(lapply(r2res, names))
      groupli                <- list()
      for (i in 1:length(vars)) {
        groupli <- unlist(c(groupli, rep(groups[i], length(unique(vars)))))
      }

      tabr2[["grp"]]     <- groupli
      tabr2[["var"]]     <- vars
      tabr2[["rsq"]]         <- unlist(r2res)
      tabr2[["adjustedRsq"]] <- unlist(adjR2res)

    } else {

      # here is the most difficult case with multiple groups and multiple models
      # create a list with r2 results per model. each element is a list with ngroup elements
      r2li <- list()
      adjR2li <- list()
      for (i in seq_along(options[["models"]])) {
        r2li <- c(r2li, list(mfm[[i]]$mfm["R2",]))
        adjR2li <- c(adjR2li, list(mfm[[i]]$mfm["R2_adj",]))
      }

      # now comes the difficult part: determine unique variable names in each group
      # for each group, find all variable names in each model
      unique_per_group <- lapply(seq_along(r2li[[1]]), function(grp) {

        all_names <- lapply(r2li, function(r2res) {
          # get names for each model
          names(r2res[[grp]])
        })

        # find the unique variable names
        unique(unlist(all_names))
      })


      # generate df with these names
      r2df <- data.frame(
        "grpname" = rep(names(r2li[[1]]), vapply(unique_per_group, length, 0)),
        "varname" = unlist(unique_per_group),
        stringsAsFactors = FALSE
      )

      adjR2df <- data.frame(
        "grpname" = rep(names(adjR2li[[1]]), vapply(unique_per_group, length, 0)),
        "varname" = unlist(unique_per_group),
        stringsAsFactors = FALSE
      )


      for (mod_idx in seq_along(r2li)) {
        for (grpname in names(r2li[[1]])) {
          # find correct rows in r2df for each model and group in r2li
          grp_idx <- which(r2df[["grpname"]] == grpname)
          # complex code because varnames in r2res can be in different order
          row_idx <- grp_idx[match(names(r2li[[mod_idx]][[grpname]]), r2df[grp_idx, "varname"])]
          # fill r2df with r2 results
          r2df[row_idx, mod_idx + 2] <- r2li[[mod_idx]][[grpname]]
          adjR2df[row_idx, mod_idx + 2] <- adjR2li[[mod_idx]][[grpname]]
        }
      }

      # fill jasp table with data
      tabr2[["grp"]] <- as.list(r2df[["grpname"]])
      tabr2[["var"]] <- as.list(r2df[["varname"]])
      for (i in seq_along(r2li)) tabr2[[paste0("rsq_", i)]] <- r2df[[i + 2]]
      for (i in seq_along(adjR2li)) tabr2[[paste0("adjustedRsq_", i)]] <- adjR2df[[i + 2]]
    }
  }
}

.plsSemReliabilities <- function(modelContainer, dataset, options, ready) {
  if (!options[["reliabilityMeasures"]] || !is.null(modelContainer[["tabReliability"]])) return()

  # init table
  tabrho <- createJaspTable(gettext("Reliability Measures"))
  if (options[["group"]] != "")
    tabrho$addColumnInfo(name = "grp", title = "Group",  type = "string", combine = TRUE)
  tabrho$addColumnInfo(name = "var",   title = "Latent", type = "string")
  if (length(options[["models"]]) < 2) {
    tabrho$addColumnInfo(name = "rhoT",           title = gettextf("Cronbach's %s", "\u03B1"),                type = "number")
    tabrho$addColumnInfo(name = "rhoCmm",         title = gettextf("J%1$sreskog's %2$s","\u00F6", "\u03C1" ), type = "number")
    tabrho$addColumnInfo(name = "rhoCWeightedmm", title = gettextf("Dijkstra-Henseler's %s", "\u03C1"),       type = "number")
  } else {
    for (i in seq_along(options[["models"]])) {
      tabrho$addColumnInfo(name = paste0("rhoT_", i), title = options[["models"]][[i]][["name"]],
                          overtitle = gettextf("Cronbach's %s", "\u03B1"), type = "number")
    }
    for (i in seq_along(options[["models"]])) {
      tabrho$addColumnInfo(name = paste0("rhoCmm_", i), title = options[["models"]][[i]][["name"]],
                          overtitle = gettextf("J%1$sreskog's %2$s","\u00F6", "\u03C1" ), type = "number")
    }
    for (i in seq_along(options[["models"]])) {
      tabrho$addColumnInfo(name = paste0("rhoCWeightedmm_", i), title = options[["models"]][[i]][["name"]],
                           overtitle = gettextf("Dijkstra-Henseler's %s", "\u03C1"), type = "number")
    }
  }

  tabrho$dependOn(c("reliabilityMeasures", "models"))
  tabrho$position <- 0.8

  modelContainer[["tabReliability"]] <- tabrho

  if (!ready || modelContainer$getError()) return()

  # compute data and fill table
  mfm <- modelContainer[["modSelCriteria"]]$object$value
  if (options[["group"]] == "") {

    if (length(options[["models"]]) < 2) {

      tabrho[["var"]]        <- names(mfm$mfm$Reliability[["Cronbachs_alpha"]])
      tabrho[["rhoT"]]           <- mfm$mfm$Reliability[["Cronbachs_alpha"]]
      tabrho[["rhoCmm"]]         <- mfm$mfm$Reliability[["Joereskogs_rho"]]
      tabrho[["rhoCWeightedmm"]] <- mfm$mfm$Reliability[["Dijkstra-Henselers_rho_A"]]

    } else {

      # determine variable names

      rhoTli <- list()
      rhoCmmli <- list()
      rhoCWeightedmmli <- list()

      for (i in seq_along(options[["models"]])) {
        rhoTli    <- c(rhoTli, list(mfm[[i]]$mfm$Reliability[["Cronbachs_alpha"]]))
        rhoCmmli <- c(rhoCmmli, list(mfm[[i]]$mfm$Reliability[["Joereskogs_rho"]]))
        rhoCWeightedmmli <- c(rhoCWeightedmmli, list(mfm[[i]]$mfm$Reliability[["Dijkstra-Henselers_rho_A"]]))
      }


      # generate dfs with these names
      rhoTdf <- data.frame("varname" = unique(unlist(lapply(rhoTli, names))))
      rhoCmmdf <- data.frame("varname" = unique(unlist(lapply(rhoCmmli, names))))
      rhoCWeightedmmdf <- data.frame("varname" = unique(unlist(lapply(rhoCWeightedmmli, names))))
      tabrho[["var"]] <- unique(unlist(lapply(rhoTli, names)))

      for (i in 1:length(rhoTli)) {
        # fill matching vars from model with df
        rhoTdf[match(names(rhoTli[[i]]), rhoTdf[["varname"]]), i + 1] <- rhoTli[[i]]
        # add column to table
        tabrho[[paste0("rhoT_", i)]] <- rhoTdf[[i + 1]]

        rhoCmmdf[match(names(rhoCmmli[[i]]), rhoCmmdf[["varname"]]), i + 1] <- rhoCmmli[[i]]
        # add column to table
        tabrho[[paste0("rhoCmm_", i)]] <- rhoCmmdf[[i + 1]]

        rhoCWeightedmmdf[match(names(rhoCWeightedmmli[[i]]), rhoCWeightedmmdf[["varname"]]), i + 1] <- rhoCWeightedmmli[[i]]
        # add column to table
        tabrho[[paste0("rhoCWeightedmm_", i)]] <- rhoCWeightedmmdf[[i + 1]]
      }
    }
  } else {
    if (length(options[["models"]]) < 2) {

      reliability                  <- mfm$mfm["Reliability",]
      tabrho[["grp"]]              <- rep(names(reliability), vapply(lapply(reliability, function(x) x[["Cronbachs_alpha"]]), length, 0))
      tabrho[["var"]]              <- unlist(lapply(lapply(reliability, function(x) x[["Cronbachs_alpha"]]), names))
      tabrho[["rhoT"]]             <- unlist(lapply(reliability, function(x) x[["Cronbachs_alpha"]]))
      tabrho[["rhoCmm"]]           <- unlist(lapply(reliability, function(x) x[["Joereskogs_rho"]]))
      tabrho[["rhoCWeightedmm"]]   <- unlist(lapply(reliability, function(x) x[["Dijkstra-Henselers_rho_A"]]))


    } else {

      rhoTli           <- list()
      rhoCmmli         <- list()
      rhoCWeightedmmli <- list()

      for (i in seq_along(options[["models"]])) {
        reliability      <- mfm[[i]]$mfm["Reliability",]
        rhoTli           <- c(rhoTli, list(lapply(reliability, function(x) x[["Cronbachs_alpha"]])))
        rhoCmmli         <- c(rhoCmmli, list(lapply(reliability, function(x) x[["Joereskogs_rho"]])))
        rhoCWeightedmmli <- c(rhoCWeightedmmli, list(lapply(reliability, function(x) x[["Dijkstra-Henselers_rho_A"]])))
      }

      # now comes the difficult part: determine unique variable names in each group
      # for each group, find all variable names in each model
      unique_per_group <- lapply(seq_along(rhoTli[[1]]), function(grp) {

        all_names <- lapply(rhoTli, function(rhoTres) {
          # get names for each model
          names(rhoTres[[grp]])
        })

        # find the unique variable names
        unique(unlist(all_names))
      })


      # generate df with these names
      rhoTdf <- data.frame(
        "grpname" = rep(names(rhoTli[[1]]), vapply(unique_per_group, length, 0)),
        "varname" = unlist(unique_per_group),
        stringsAsFactors = FALSE
      )

      rhoCmmdf <- data.frame(
        "grpname" = rep(names(rhoCmmli[[1]]), vapply(unique_per_group, length, 0)),
        "varname" = unlist(unique_per_group),
        stringsAsFactors = FALSE
      )

      rhoCWeightedmmdf <- data.frame(
        "grpname" = rep(names(rhoCWeightedmmli[[1]]), vapply(unique_per_group, length, 0)),
        "varname" = unlist(unique_per_group),
        stringsAsFactors = FALSE
      )


      for (mod_idx in seq_along(rhoTli)) {
        for (grpname in names(rhoTli[[1]])) {

          grp_idx <- which(rhoTdf[["grpname"]] == grpname)

          row_idx <- grp_idx[match(names(rhoTli[[mod_idx]][[grpname]]), rhoTdf[grp_idx, "varname"])]

          rhoTdf[row_idx, mod_idx + 2] <- rhoTli[[mod_idx]][[grpname]]
          rhoCmmdf[row_idx, mod_idx + 2] <- rhoCmmli[[mod_idx]][[grpname]]
          rhoCWeightedmmdf[row_idx, mod_idx + 2] <- rhoCWeightedmmli[[mod_idx]][[grpname]]
        }
      }


      tabrho[["grp"]] <- rhoTdf[["grpname"]]
      tabrho[["var"]] <- rhoTdf[["varname"]]
      for (i in seq_along(rhoTli)) tabrho[[paste0("rhoT_", i)]] <- rhoTdf[[i + 2]]
      for (i in seq_along(rhoCmmli)) tabrho[[paste0("rhoCmm_", i)]] <- rhoCmmdf[[i + 2]]
      for (i in seq_along(rhoCWeightedmmli)) tabrho[[paste0("rhoCWeightedmm_", i)]] <- rhoCWeightedmmdf[[i + 2]]
    }
  }
}

.plsSemCor <- function(modelContainer, options, ready) {
  if (!(options[["observedIndicatorCorrelation"]] || options[["impliedIndicatorCorrelation"]] ||
        options[["observedConstructCorrelation"]] || options[["impliedConstructCorrelation"]]) ||
      !is.null(modelContainer[["cors"]])) return()

  cors <- createJaspContainer(gettext("Correlation tables"))
  cors$position <- 3
  cors$dependOn(c("observedIndicatorCorrelation",
                    "impliedIndicatorCorrelation",
                    "observedConstructCorrelation",
                    "impliedConstructCorrelation",
                    "models"))

  modelContainer[["cors"]] <- cors

  if (length(options[["models"]]) < 2) {
    .plsSemCorTables(modelContainer[["results"]][["object"]][[1]], NULL, cors, options, ready)
  } else {

    for (i in seq_along(options[["models"]])) {
      fit <- modelContainer[["results"]][["object"]][[i]]
      name <- options[["models"]][[i]][["name"]]
      .plsSemCorTables(fit, name, cors, options, ready)
    }
  }
}

.plsSemCorTables <- function(fit, name, parentContainer, options, ready) {
  if (is.null(name)) {
    corcont <- parentContainer
  } else {
    corcont <- createJaspContainer(name, initCollapsed = TRUE)
  }

  if (options[["group"]] == "") {

    # without groups, these are tables

    if (options[["observedIndicatorCorrelation"]]) {
      oictab <- createJaspTable(gettext("Observed indicator correlation matrix"))
      oictab$dependOn("observedIndicatorCorrelation")
      oictab$position <- 1
      corcont[["observedInd"]] <- oictab
    }

    if (options[["impliedIndicatorCorrelation"]]) {
      iictab <- createJaspTable(gettext("Implied indicator correlation matrix"))
      iictab$dependOn("impliedIndicatorCorrelation")
      iictab$position <- 2
      corcont[["impliedInd"]] <- iictab
    }

    if (options[["observedConstructCorrelation"]]) {
      occtab <- createJaspTable(gettext("Observed construct correlation matrix"))
      occtab$dependOn("observedConstructCorrelation")
      occtab$position <- 3
      corcont[["observedCon"]] <- occtab
    }

    if (options[["impliedConstructCorrelation"]]) {
      icctab <- createJaspTable(gettext("Implied construct correlation matrix"))
      icctab$dependOn("impliedConstructCorrelation")
      icctab$position <- 4
      corcont[["impliedCon"]] <- icctab
    }

  } else {

    # with multiple groups these become containers

    if (options[["observedIndicatorCorrelation"]]) {
      oiccont <- createJaspContainer(gettext("Observed indicator correlation matrix", initCollapsed = TRUE))
      oiccont$dependOn("observedIndicatorCorrelation")
      oiccont$position <- 1
      corcont[["observedInd"]] <- oiccont
    }

    if (options[["impliedIndicatorCorrelation"]]) {
      iiccont <- createJaspContainer(gettext("Implied indicator correlation matrix", initCollapsed = TRUE))
      iiccont$dependOn("impliedIndicatorCorrelation")
      iiccont$position <- 2
      corcont[["impliedInd"]] <- iiccont
    }

    if (options[["observedConstructCorrelation"]]) {
      occcont <- createJaspContainer(gettext("Observed construct correlation matrix", initCollapsed = TRUE))
      occcont$dependOn("observedConstructCorrelation")
      occcont$position <- 3
      corcont[["observedCon"]] <- occcont
    }

    if (options[["impliedConstructCorrelation"]]) {
      icccont <- createJaspContainer(gettext("Implied construct correlation matrix", initCollapsed = TRUE))
      icccont$dependOn("impliedConstructCorrelation")
      icccont$position <- 4
      corcont[["impliedCon"]] <- icccont
    }
  }


  if (!ready) return()


  if (options[["group"]] == "") {

    # without groups, just fill the tables

    if (options[["observedIndicatorCorrelation"]]) {
      # actually compute the observed indicator correlations

      oic <- fit$Estimates$Indicator_VCV
      oic[upper.tri(oic)] <- NA

      for (i in 1:ncol(oic)) {
        if(i == 1) {
          oictab$addColumnInfo(name = "rownames", title = "", type = "string")
          oictab[["rownames"]] <- rownames(oic)
        }
        nm <- colnames(oic)[i]
        oictab$addColumnInfo(nm, title = nm, type = "number")
        oictab[[nm]] <- oic[,i]
      }
    }

    if (options[["impliedIndicatorCorrelation"]]) {
      # actually compute the implied indicator correlations
      iic <- cSEM::fit(fit, .type_vcv = "indicator")
      iic[upper.tri(iic)] <- NA

      for (i in 1:ncol(iic)) {
        if(i == 1) {
          iictab$addColumnInfo(name = "rownames", title = "", type = "string")
          iictab[["rownames"]] <- rownames(iic)
        }
        nm <- colnames(iic)[i]
        iictab$addColumnInfo(nm, title = nm, type = "number")
        iictab[[nm]] <- iic[,i]
      }
    }

    if (options[["observedConstructCorrelation"]]) {
      # actually compute the observed construct correlations
      occ <- fit$Estimates$Construct_VCV
      occ[upper.tri(occ)] <- NA

      for (i in 1:ncol(occ)) {
        if(i == 1) {
          occtab$addColumnInfo(name = "rownames", title = "", type = "string")
          occtab[["rownames"]] <- rownames(occ)
        }
        nm <- colnames(occ)[i]
        occtab$addColumnInfo(nm, title = nm, type = "number")
        occtab[[nm]] <- occ[,i]
      }
    }

    if (options[["impliedConstructCorrelation"]]) {
      # actually compute the implied construct correlations
      icc <- cSEM::fit(fit, .type_vcv = "construct")
      icc[upper.tri(icc)] <- NA

      for (i in 1:ncol(icc)) {
        if(i == 1) {
          icctab$addColumnInfo(name = "rownames", title = "", type = "string")
          icctab[["rownames"]] <- rownames(icc)
        }
        nm <- colnames(icc)[i]
        icctab$addColumnInfo(nm, title = nm, type = "number")
        icctab[[nm]] <- icc[,i]
      }
    }

  } else {

    # with groups, create tables and fill them

    # actually compute the observed indicator correlations

    if (options[["observedIndicatorCorrelation"]]) {

      groupNames <- names(fit)
      for (i in 1:length(fit)) {
        oic <- fit[[i]]$Estimates$Indicator_VCV
        oic[upper.tri(oic)] <- NA
        oiccont[[groupNames[i]]] <- createJaspTable(groupNames[i])


        for (j in 1:ncol(oic)) {
          if(j == 1) {
            oiccont[[groupNames[i]]]$addColumnInfo(name = "rownames", title = "", type = "string")
            oiccont[[groupNames[i]]][["rownames"]] <- rownames(oic)
          }
          nm <- colnames(oic)[j]
          oiccont[[groupNames[i]]]$addColumnInfo(nm, title = nm, type = "number")
          oiccont[[groupNames[i]]][[nm]] <- oic[,j]
        }
      }
    }

    # actually compute the implied indicator correlations

    if (options[["impliedIndicatorCorrelation"]]) {

      iicli <- cSEM::fit(fit, .type_vcv = "indicator")
      groupNames <- names(iicli)

      for (i in 1:length(fit)) {
        iic <- iicli[[i]]
        iic[upper.tri(iic)] <- NA
        iiccont[[groupNames[i]]] <- createJaspTable(groupNames[i])


        for (j in 1:ncol(iic)) {
          if(j == 1) {
            iiccont[[groupNames[i]]]$addColumnInfo(name = "rownames", title = "", type = "string")
            iiccont[[groupNames[i]]][["rownames"]] <- rownames(iic)
          }
          nm <- colnames(iic)[j]
          iiccont[[groupNames[i]]]$addColumnInfo(nm, title = nm, type = "number")
          iiccont[[groupNames[i]]][[nm]] <- iic[,j]
        }
      }
    }

    # actually compute the observed construct correlations

    if (options[["observedConstructCorrelation"]]) {

      groupNames <- names(fit)
      for (i in 1:length(fit)) {
        occ <- fit[[i]]$Estimates$Construct_VCV
        occ[upper.tri(occ)] <- NA
        occcont[[groupNames[i]]] <- createJaspTable(groupNames[i])


        for (j in 1:ncol(occ)) {
          if(j == 1) {
            occcont[[groupNames[i]]]$addColumnInfo(name = "rownames", title = "", type = "string")
            occcont[[groupNames[i]]][["rownames"]] <- rownames(occ)
          }
          nm <- colnames(occ)[j]
          occcont[[groupNames[i]]]$addColumnInfo(nm, title = nm, type = "number")
          occcont[[groupNames[i]]][[nm]] <- occ[,j]
        }
      }
    }

    # actually compute the implied construct correlations

    if (options[["impliedConstructCorrelation"]]) {

      iccli <- cSEM::fit(fit, .type_vcv = "construct")
      groupNames <- names(iccli)

      for (i in 1:length(fit)) {
        icc <- iccli[[i]]
        icc[upper.tri(icc)] <- NA
        icccont[[groupNames[i]]] <- createJaspTable(groupNames[i])


        for (j in 1:ncol(icc)) {
          if(j == 1) {
            icccont[[groupNames[i]]]$addColumnInfo(name = "rownames", title = "", type = "string")
            icccont[[groupNames[i]]][["rownames"]] <- rownames(icc)
          }
          nm <- colnames(icc)[j]
          icccont[[groupNames[i]]]$addColumnInfo(nm, title = nm, type = "number")
          icccont[[groupNames[i]]][[nm]] <- icc[,j]
        }
      }
    }
  }

  if (!is.null(name)) {
    parentContainer[[name]] <- corcont
  }

  return()
}


.plsAddConstructScores <- function(jaspResults, modelContainer, options, ready) {

  if (!ready ||
      !is.null(jaspResults[["addedScoresContainer"]]) ||
      modelContainer$getError() ||
      !options[["addConstructScores"]])
  {
    return()
  }

  container    <- createJaspContainer()
  container$dependOn(optionsFromObject = modelContainer, options = "addConstructScores")
  jaspResults[["addedScoresContainer"]] <- container

  models <- modelContainer[["models"]][["object"]]
  results <- modelContainer[["results"]][["object"]]

  modelNames <- sapply(models, function(x) x[["name"]])
  modelNames <- gsub(" ", "_", modelNames)
  colNamesR <- c()

  # loop over the models
  for (i in seq_len(length(results))) {

    if (options$group != "") {
      scoresList <- cSEM::getConstructScores(results[[i]])
      scores <- lapply(scoresList, function(x) x$Construct_scores)
      groupLabs <- names(scoresList)
      facNames <- colnames(scores[[1]])
      colNamesR <- paste0(rep(groupLabs, each = ncol(scores[[1]])), "_", "CS_", facNames)
    } else {
      scores <- cSEM::getConstructScores(results[[i]])$Construct_scores
      facNames <- colnames(scores)
      colNamesR <- paste0("CS_", facNames)
      scores <- list(scores)
    }

    z <- 1
    for (ll in seq_len(length(scores))) {
      for (ii in seq_len(ncol(scores[[ll]]))) {

        colNameR <- colNamesR[z]
        scoresTmp <- scores[[ll]]
        if (jaspBase:::columnExists(colNameR) && !jaspBase:::columnIsMine(colNameR)) {
          .quitAnalysis(gettextf("Column name %s already exists in the dataset", colNameR))
        }

        container[[colNameR]] <- jaspBase::createJaspColumn(colNameR)
        container[[colNameR]]$setScale(scoresTmp[, ii])

        z <- z + 1

      }
    }
  }

  jaspResults[["addedScoresContainer"]] <- container

  # check if there are previous colNames that are not needed anymore and delete the cols
  oldNames <- jaspResults[["createdColumnNames"]][["object"]]
  newNames <- colNamesR[1:z]
  if (!is.null(oldNames)) {
    noMatch <- which(!(oldNames %in% newNames))
    if (length(noMatch) > 0) {
      for (iii in 1:length(noMatch)) {
        jaspBase:::columnDelete(oldNames[noMatch[iii]])
      }
    }
  }

  # save the created col names
  jaspResults[["createdColumnNames"]] <- createJaspState(newNames)


  return()

}


.plsSEMVIFhelper <- function(fit){
  # Make VIFs into a matrix
  # Restructure the VIFs into a table.
  VIFspath <- cSEM::assess(.object = fit,.quality_criterion = 'vif')

  idx <- which(VIFspath$VIF!=0,arr.ind = T)

  if(nrow(idx)!=0){
    VIFDf <- data.frame(Relation=paste(rownames(VIFspath$VIF)[idx[,'row']],'~',colnames(VIFspath$VIF)[idx[,'col']]),
                        vif=VIFspath$VIF[cbind(rownames(VIFspath$VIF)[idx[,'row']],colnames(VIFspath$VIF)[idx[,'col']])])

    VIFvector <-setNames(VIFDf$vif, VIFDf$Relation)
  } else{
    VIFvector <- NULL
  }
  return(VIFvector)
}

.plsSEMVIFBhelper <- function(fit){
  VIFsweights <- cSEM::calculateVIFModeB(fit)

  # If there is only one weight, cSEM::calculateVIFModeB() returns NA for that VIF
  # therefore, replace NAs with 0
  VIFsweights[is.na(VIFsweights)] <- 0


  if(!is.null(VIFsweights)&sum(VIFsweights)!=0){
    idx <- which(VIFsweights!=0,arr.ind = T)

    VIFBDf <- data.frame(Relation=paste(rownames(VIFsweights)[idx[,'row']],'<~',colnames(VIFsweights)[idx[,'col']]),
                         vif=VIFsweights[cbind(rownames(VIFsweights)[idx[,'row']],colnames(VIFsweights)[idx[,'col']])])

    VIFBvector <-setNames(VIFBDf$vif, VIFBDf$Relation)

  } else{
    VIFBvector <- NULL
  }
  return(VIFBvector)

}
