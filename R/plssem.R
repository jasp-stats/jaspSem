#
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

PLSSEM <- function(jaspResults, dataset, options, ...) {
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
  .plsSemParameters(modelContainer, dataset, options, ready)
  .plsSemRsquared(modelContainer, dataset, options, ready)
  .plsSemAdditionalFits(modelContainer, dataset, options, ready)
  .plsSemMardiasCoefficient(modelContainer, dataset, options, ready)
  .plsSemReliabilities(modelContainer, dataset, options, ready)
  .plsSemCor(modelContainer, options, ready)
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

  variablesToRead <- if (options[["groupingVariable"]] == "") character() else options[["groupingVariable"]]
  for (model in options[["models"]])
    variablesToRead <- unique(c(variablesToRead, model[["columns"]]))

  return(.readDataSetToEnd(columns = variablesToRead))
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
      .plsSemGetUsedVars(x[["syntax"]], colnames(dataset))
    })))
    .hasErrors(dataset[,usedvars],
               type = c("infinity"), message='default', exitAnalysisIfErrors = TRUE)
  }

  # Check whether grouping variable is a grouping variable
  if (options[["groupingVariable"]] != "") {
    groupfac <- factor(dataset[[.v(options[["groupingVariable"]])]])
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
      return("Enter a model")
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
      return(paste("Variable(s) in model syntax not recogzed:",
                   paste(stringr::str_replace_all(notRecognized, unvvars),
                         collapse = ", ")))
    }
  }

  # if checks pass, return empty string
  return("")
}

.plsSemGetUsedVars <- function(syntax, availablevars) {
  vv <- availablevars
  findpattern <- paste0("(?<=[\\s\\+\\^\\=\\~\\<\\*\\>\\:\\%\\|\\+]|^)\\Q",
                        vv,
                        "\\E(?=[\\s\\+\\^\\=\\~\\<\\*\\>\\:\\%\\|\\+]|$)")
  return(vv[vapply(findpattern,
                   function(p) stringr::str_detect(syntax, p),
                   FUN.VALUE = TRUE,
                   USE.NAMES = FALSE)])
}

.plsSemModelContainer <- function(jaspResults) {
  if (!is.null(jaspResults[["modelContainer"]])) {
    modelContainer <- jaspResults[["modelContainer"]]
  } else {
    modelContainer <- createJaspContainer()
    modelContainer$dependOn(c("approachSecondOrder", "approachWeights", "approachCorRobust", "convergenceCriterion",
                              "estimateStructural", "groupingVariable", "approachCorrectionFactors", "disattenuate",
                              "ignoreStructuralModel", "innerWeightingScheme", "resamplingMethod", "nBootstraps", "ciWidth",
                              "setSeed", "seed", "handleInadmissibles", "Data", "signFlippingHandling"))
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

    syntax   <- .plsSemTranslateModel(options[["models"]][[i]][["syntax"]], dataset)
    cSemOpts[[".model"]] <- syntax
    cSemOpts[[".data"]]  <- dataset

    # fit the model

    fit <- try(do.call(cSEM::csem, cSemOpts))

    # error messages
    if (isTryError(fit)) {
      err <- .extractErrorMessage(fit)
      if(err == "..constant.."){
        err <- gettext("Invalid model specification. Did you pass a variable name as a string?")
      }
      errmsg <- gettextf("Estimation failed Message: %s", err)
      modelContainer$setError(paste0("Error in model \"", options[["models"]][[i]][["modelName"]], "\" - ",
                                     .decodeVarsInMessage(names(dataset), errmsg)))
      modelContainer$dependOn("models") # add dependency so everything gets updated upon model change
      break
    }


    if(isFALSE(fit$Information$Weight_info$Convergence_status)) {
      errormsg <- gettextf("Estimation failed! Message: Model %s did not converge!", options[["models"]][[i]][["modelName"]])
      modelContainer$setError(errormsg)
      modelContainer$dependOn("models")
      break
    }

    if (options[["resamplingMethod"]] != "none") {

      if(options[["resamplingMethod"]] == "bootstrap") {
        startProgressbar(options[["nBootstraps"]], "Resampling")
      } else {
        startProgressbar(nrow(dataset), "Resampling")
      }

      tickFunction <- function(.object)
      {
        progressbarTick()
        return(c(0,0))
      }

      # resample
      fit <- try(cSEM::resamplecSEMResults(.object = fit,
                                                 .R = options[["nBootstraps"]],
                                                 .user_funs = tickFunction,
                                                 .resample_method = options[["resamplingMethod"]],
                                                 .handle_inadmissibles = options[["handleInadmissibles"]],
                                                 .sign_change_option = options[["signFlippingHandling"]],
                                                 .seed = if (options[["setSeed"]]) options[["seed"]]))
      if (isTryError(fit)) {
        err <- .extractErrorMessage(fit)

        errmsg <- gettextf("Estimation failed Message: %s", err)
        modelContainer$setError(paste0("Error in model \"", options[["models"]][[i]][["modelName"]], "\" - ",
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
  cSemOpts[[".approach_weights"]]            <- options[["approachWeights"]]
  cSemOpts[[".approach_cor_robust"]]         <- options[["approachCorRobust"]]
  cSemOpts[[".approach_nl"]]                 <- options[["approachNonLinear"]]
  cSemOpts[[".conv_criterion"]]              <- options[["convergenceCriterion"]]
  cSemOpts[[".estimate_structural"]]         <- options[["estimateStructural"]]
  cSemOpts[[".normality"]]                   <- options[["assumeNormality"]]
  cSemOpts[[".PLS_ignore_structural_model"]] <- options[["ignoreStructuralModel"]]
  cSemOpts[[".PLS_weight_scheme_inner"]]     <- options[["innerWeightingScheme"]]

  if (options[["disattenuate"]])
    cSemOpts[".PLS_approach_cf"] <- options[["approachCorrectionFactors"]]

  if (options[["groupingVariable"]] != "" || length(options[["models"]]) > 1)
    cSemOpts[[".eval_plan"]] <- "multiprocess"

  if (options[["groupingVariable"]] != "")
    cSemOpts[[".id"]] <- options[["groupingVariable"]]

  return(cSemOpts)
}


.plsSemTranslateModel <- function(syntax, dataset) {
  #' translate model syntax to jasp column names syntax
  usedvars <- .plsSemGetUsedVars(syntax, colnames(dataset))

  if (length(usedvars) == 0) {
    return(syntax)
  }

  usedvars <- usedvars[order(nchar(usedvars), decreasing = TRUE)]
  with.s.quotes <- paste("\\b'", usedvars, "'\\b", sep="")
  with.d.quotes <- paste('\\b"', usedvars, '"\\b', sep="")

  new.names <- .v(usedvars)

  for (i in 1:length(usedvars)) {
    syntax <- gsub(with.d.quotes[i], new.names[i], syntax)
  }

  for (i in 1:length(usedvars)) {
    syntax <- gsub(with.s.quotes[i], new.names[i], syntax)
  }

  for (i in 1:length(usedvars)) {
    syntax <- gsub(paste0("\\b", usedvars[i], "\\b"), new.names[i], syntax)
  }

  return(syntax)
}


### output functions ###

# Model Fit Table
.plsSemFitTab <- function(modelContainer, dataset, options, ready) {
  # create model fit table
  if (!is.null(modelContainer[["fittab"]])) return()


  fittab <- createJaspTable(title = gettext("Model fit"))
  fittab$dependOn(c("models"))
  fittab$position <- 0

  fittab$addColumnInfo(name = "Model",    title = "",                            type = "string", combine = TRUE)
  if (options[["groupingVariable"]] != "")
    fittab$addColumnInfo(name = "group",  title = gettext("Group"),              type = "string" )
  fittab$addColumnInfo(name = "AIC",      title = gettext("AIC"),                type = "number" )
  fittab$addColumnInfo(name = "BIC",      title = gettext("BIC"),                type = "number" )
  fittab$addColumnInfo(name = "N",        title = gettext("n"),                  type = "integer")
  fittab$addColumnInfo(name = "Chisq",    title = gettext("&#967;&sup2;"),       type = "number" ,
                       overtitle = gettext("Baseline test"))
  fittab$addColumnInfo(name = "Df",       title = gettext("df"),                 type = "integer",
                       overtitle = gettext("Baseline test"))
  fittab$addColumnInfo(name = "PrChisq",  title = gettext("p"),                  type = "pvalue",
                       overtitle = gettext("Baseline test"))
  if (length(options[["models"]]) > 1) {
    fittab$addColumnInfo(name = "dchisq",   title = gettext("&#916;&#967;&sup2;"), type = "number" ,
                         overtitle = gettext("Difference test"))
    fittab$addColumnInfo(name = "ddf",      title = gettext("&#916;df"),           type = "integer",
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
    if (options[["groupingVariable"]] == "") {

      msc       <- .withWarnings(.computeMSC(plsSemResults[[1]], dataset, options))

      modelName <- options[["models"]][[1]][["modelName"]]
      aic       <- msc$value$msc$AIC
      bic       <- msc$value$msc$BIC
      Ns        <- nrow(dataset)
      chisq     <- msc$value$mfm$Chi_square
      df        <- msc$value$mfm$Df
      prChisq   <- pchisq(q = chisq, df = df, lower.tail = FALSE)

    } else {

      msc       <- .withWarnings(.computeMSC(plsSemResults[[1]], dataset, options))

      modelName <- rep(options[["models"]][[1]][["modelName"]], length(plsSemResults[[1]]))
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
      modelName <- list()
      aic       <- list()
      bic       <- list()
      Ns        <- list()
      chisq     <- list()
      df        <- list()
      prChisq   <- list()
      group     <- list()
      rsquared  <- list()

      if (options[["groupingVariable"]] == "") {

        msc <- .withWarnings(lapply(postEstimation_args, .computeMSC, dataset = dataset, options = options))

        modelName <- vapply(options[["models"]], getElement, name = "modelName", "")
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

          modelName <- c(modelName, rep(options[["models"]][[i]][["modelName"]], length(plsSemResults[[i]])))
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

  fittab[["Model"]]    <- modelName
  if (options[["groupingVariable"]] != "")
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
  if (!is.null(msc$warnings))
    fittab$addFootnote(msc$warnings[[1]]$message)

  # check if there are any problems with the results and give warnings
  warningmsgs <- c("Absolute standardized loading estimates are NOT all <= 1",
                   "Construct VCV is NOT positive semi-definite",
                   "Reliability estimates are NOT all <= 1",
                   "Model-implied indicator VCV is NOT positive semi-definite")

  if (options[["groupingVariable"]] == "") {
    for (i in seq_along(options[["models"]])) {
      warnings <- cSEM::verify(plsSemResults[[i]])[2:5]
      msgs <- warningmsgs[warnings]

      for (j in seq_along(msgs)) {
        warningFootnote <- paste0(gettextf("WARNING: model %s) ", options[["models"]][[i]][["modelName"]]), gettextf("%s, therefore, results may be unreliable!", msgs[j]))
        fittab$addFootnote(warningFootnote)
      }
    }
  } else {
    for (i in seq_along(options[["models"]])) {
      for (j in seq_along(plsSemResults[i])) {
        warnings <- cSEM::verify(plsSemResults[[i]][[j]])[2:5]
        msgs <- warningmsgs[warnings]

        for (k in seq_along(msgs)) {
          warningFootnote <- paste0(paste0(gettextf("WARNING: model %s, group ", options[["models"]][[i]][["modelName"]]),
                                   gettextf("%s) ", names(plsSemResults[[i]])[[j]])),
                            gettextf("%s, therefore, results may be unreliable!", msgs[k]))
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
  if (options[["groupingVariable"]] == "") {

    Ns <- rep(nrow(dataset), length(plsSemResults))
    modelSelectionCriteria <- cSEM::calculateModelSelectionCriteria(resultsCopy, .only_structural = FALSE, .by_equation = FALSE)
    modelFitMeasures       <- cSEM::assess(resultsCopy)
  } else{
    Ns <- list()
    for (i in names(resultsCopy)) {
      resultsCopy[[i]]$Information$Arguments$.id <- NULL
      Ns[length(Ns)+1] <- nrow(dataset[which(dataset[[options[["groupingVariable"]]]]==i),])
    }

    modelSelectionCriteria <- sapply(resultsCopy, cSEM::calculateModelSelectionCriteria, .only_structural = FALSE, .by_equation = FALSE)
    modelFitMeasures <- sapply(resultsCopy, cSEM::assess)
    }
  return(list(Ns = Ns, msc = modelSelectionCriteria, mfm = modelFitMeasures))
}

.plsSemParameters <- function(modelContainer, dataset, options, ready) {
  if (!is.null(modelContainer[["params"]])) return()

  # create container for parameter estimates
  params <- createJaspContainer(gettext("Parameter estimates"))
  params$position <- 1
  params$dependOn(c("models", "ciWidth"))

  modelContainer[["params"]] <- params

  if (length(options[["models"]]) < 2) {
    .plsSemParameterTables(modelContainer[["results"]][["object"]][[1]], NULL, params, options, ready)
  } else {

    for (i in seq_along(options[["models"]])) {
      fit <- modelContainer[["results"]][["object"]][[i]]
      modelname <- options[["models"]][[i]][["modelName"]]
      .plsSemParameterTables(fit, modelname, params, options, ready)
    }
  }
}

# Parameter Estimates Tables
.plsSemParameterTables <- function(fit, modelname, parentContainer, options, ready) {
  if (is.null(modelname)) {
    pecont <- parentContainer
  } else {
    pecont <- createJaspContainer(modelname, initCollapsed = TRUE)
  }

  # Measurement model

  # create weights table
  weightTab <- createJaspTable(title = gettext("Weigths"))

  if (options[["groupingVariable"]] != "")
    weightTab$addColumnInfo(name = "group",  title = gettext("Group"),      type = "string", combine = TRUE)

  weightTab$addColumnInfo(name = "lhs",      title = gettext("Latent"),   type = "string", combine = TRUE)
  weightTab$addColumnInfo(name = "rhs",      title = gettext("Indicator"),  type = "string")
  weightTab$addColumnInfo(name = "est",      title = gettext("Estimate"),   type = "number")

  if (options[["resamplingMethod"]] != "none") {
    weightTab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number")
    weightTab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number")
    weightTab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "pvalue")
    weightTab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number",
                            overtitle = gettextf("%s%% Confidence Interval", options$ciWidth * 100))
    weightTab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                            overtitle = gettextf("%s%% Confidence Interval", options$ciWidth * 100))
  }


  pecont[["weight"]] <- weightTab

  # create loadings table
  loadingTab <- createJaspTable(title = gettext("Factor Loadings"))

  if (options[["groupingVariable"]] != "")
    loadingTab$addColumnInfo(name = "group",  title = gettext("Group"),      type = "string", combine = TRUE)

  loadingTab$addColumnInfo(name = "lhs",      title = gettext("Latent"),     type = "string", combine = TRUE)
  loadingTab$addColumnInfo(name = "rhs",      title = gettext("Indicator"),  type = "string")
  loadingTab$addColumnInfo(name = "est",      title = gettext("Estimate"),   type = "number")

  if (options[["resamplingMethod"]] != "none") {
    loadingTab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number")
    loadingTab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number")
    loadingTab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "pvalue")
    loadingTab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number",
                             overtitle = gettextf("%s%% Confidence Interval", options$ciWidth * 100))
    loadingTab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                             overtitle = gettextf("%s%% Confidence Interval", options$ciWidth * 100))
  }

  pecont[["loading"]] <- loadingTab

  # Structural Model

  #create paths table
  pathTab <- createJaspTable(title = gettext("Regression Coefficients"))

  if (options[["groupingVariable"]] != "")
    pathTab$addColumnInfo(name = "group",  title = gettext("Group"),      type = "string", combine = TRUE)

  pathTab$addColumnInfo(name = "lhs",      title = gettext("Outcome"),  type = "string", combine = TRUE)
  pathTab$addColumnInfo(name = "rhs",      title = gettext("Predictor"),    type = "string")
  pathTab$addColumnInfo(name = "est",      title = gettext("Estimate"),   type = "number")
  pathTab$addColumnInfo(name = "f2",       title = gettext("f&sup2"),   type = "number")

  if (options[["resamplingMethod"]] != "none") {
    pathTab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number")
    pathTab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number")
    pathTab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "pvalue")
    pathTab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number",
                          overtitle = gettextf("%s%% Confidence Interval", options$ciWidth * 100))
    pathTab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                          overtitle = gettextf("%s%% Confidence Interval", options$ciWidth * 100))
  }

  pecont[["path"]] <- pathTab

  # create total effects table
  totalTab <- createJaspTable(title = gettext("Total effects"))

  if (options[["groupingVariable"]] != "")
    totalTab$addColumnInfo(name = "group",  title = gettext("Group"),      type = "string", combine = TRUE)

  totalTab$addColumnInfo(name = "lhs",      title = gettext("Outcome"),     type = "string", combine = TRUE)
  totalTab$addColumnInfo(name = "rhs",      title = gettext("Predictor"),  type = "string")
  totalTab$addColumnInfo(name = "est",      title = gettext("Estimate"),   type = "number")
  if (options[["resamplingMethod"]] != "none") {
    totalTab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number")
    totalTab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number")
    totalTab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "pvalue")
    totalTab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number",
                           overtitle = gettextf("%s%% Confidence Interval", options$ciWidth * 100))
    totalTab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                           overtitle = gettextf("%s%% Confidence Interval", options$ciWidth * 100))
  }

  pecont[["total"]] <- totalTab


  if (!is.null(modelname)) parentContainer[[modelname]] <- pecont

  if (!ready) return()

  # compute parameter estimates
  if (options[["resamplingMethod"]] == "none") {
    summ <- cSEM::summarize(fit)
    pe <- list()
    if (options$groupingVariable == "") {
      summ <- summ$Estimates

      pe[["Weight_estimates"]] <- list()
      pe[["Weight_estimates"]][["mean"]] <- summ$Weight_estimates$Estimate
      names(pe[["Weight_estimates"]][["mean"]]) <- summ$Weight_estimates$Name

      pe[["Loading_estimates"]] <- list()
      pe[["Loading_estimates"]][["mean"]] <- summ$Loading_estimates$Estimate
      names(pe[["Loading_estimates"]][["mean"]]) <- summ$Loading_estimates$Name

      pe[["Path_estimates"]] <- list()
      pe[["Path_estimates"]][["mean"]] <- summ$Path_estimates$Estimate
      names(pe[["Path_estimates"]][["mean"]]) <- summ$Path_estimates$Name

      pe[["Total_effect"]] <- list()
      pe[["Total_effect"]][["mean"]] <- summ$Effect_estimates$Total_effect$Estimate
      names(pe[["Total_effect"]][["mean"]]) <- summ$Effect_estimates$Total_effect$Name
    } else{
      for (i in names(summ)) {
        pe[[i]] <- list()
        pe[[i]][["Weight_estimates"]] <- list()
        pe[[i]][["Weight_estimates"]][["mean"]] <- summ[[i]]$Estimates$Weight_estimates$Estimate
        names(pe[[i]][["Weight_estimates"]][["mean"]]) <- summ[[i]]$Estimates$Weight_estimates$Name

        pe[[i]][["Loading_estimates"]] <- list()
        pe[[i]][["Loading_estimates"]][["mean"]] <- summ[[i]]$Estimates$Loading_estimates$Estimate
        names(pe[[i]][["Loading_estimates"]][["mean"]]) <- summ[[i]]$Estimates$Loading_estimates$Name

        pe[[i]][["Path_estimates"]] <- list()
        pe[[i]][["Path_estimates"]][["mean"]] <- summ[[i]]$Estimates$Path_estimates$Estimate
        names(pe[[i]][["Path_estimates"]][["mean"]]) <- summ[[i]]$Estimates$Path_estimates$Name

        pe[[i]][["Total_effect"]] <- list()
        pe[[i]][["Total_effect"]][["mean"]] <- summ[[i]]$Estimates$Effect_estimates$Total_effect$Estimate
        names(pe[[i]][["Total_effect"]][["mean"]]) <- summ[[i]]$Estimates$Effect_estimates$Total_effect$Name
      }
    }
  } else {
    pe <- cSEM::infer(fit, .alpha = 1 - options[["ciWidth"]])
  }

  # fill Weights table

  if (options[["groupingVariable"]] == "") {
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

    if (options[["groupingVariable"]] != "")
      weightTab[["group"]]    <- weightEstimates[["group"]]

    weightTab[["rhs"]]      <- weightEstimates[["rhs"]]
    weightTab[["lhs"]]      <- weightEstimates[["lhs"]]
    weightTab[["est"]]      <- weightEstimates[["est"]]

    if (options[["resamplingMethod"]] != "none") {
      weightTab[["se"]]       <- weightEstimates[["se"]]
      weightTab[["z"]]        <- weightEstimates[["zVal"]]
      weightTab[["pvalue"]]   <- weightEstimates[["pVal"]]
      weightTab[["ci.lower"]] <- weightEstimates[["ciLower"]]
      weightTab[["ci.upper"]] <- weightEstimates[["ciUpper"]]
    }
  }


  # fill Loadings table

  if (options[["groupingVariable"]] == "") {
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

    if (options[["groupingVariable"]] != "")
      loadingTab[["group"]]    <- loadingEstimates[["group"]]

    loadingTab[["rhs"]]      <- loadingEstimates[["rhs"]]
    loadingTab[["lhs"]]      <- loadingEstimates[["lhs"]]
    loadingTab[["est"]]      <- loadingEstimates[["est"]]

    if (options[["resamplingMethod"]] != "none") {
      loadingTab[["se"]]       <- loadingEstimates[["se"]]
      loadingTab[["z"]]        <- loadingEstimates[["zVal"]]
      loadingTab[["pvalue"]]   <- loadingEstimates[["pVal"]]
      loadingTab[["ci.lower"]] <- loadingEstimates[["ciLower"]]
      loadingTab[["ci.upper"]] <- loadingEstimates[["ciUpper"]]
    }
  }


  # fill Paths table
  f2 <- cSEM::calculatef2(fit)
  if (options[["groupingVariable"]] == "") {
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

    if (options[["groupingVariable"]] != "")
      pathTab[["group"]]    <- pathEstimates[["group"]]

    pathTab[["rhs"]]      <- pathEstimates[["rhs"]]
    pathTab[["lhs"]]      <- pathEstimates[["lhs"]]
    pathTab[["est"]]      <- pathEstimates[["est"]]
    pathTab[["f2"]]       <- pathEstimates[["f2"]]

    if (options[["resamplingMethod"]] != "none") {
      pathTab[["se"]]       <- pathEstimates[["se"]]
      pathTab[["z"]]        <- pathEstimates[["zVal"]]
      pathTab[["pvalue"]]   <- pathEstimates[["pVal"]]
      pathTab[["ci.lower"]] <- pathEstimates[["ciLower"]]
      pathTab[["ci.upper"]] <- pathEstimates[["ciUpper"]]
    }
  }


  # fill Total effects table


  if (options[["groupingVariable"]] == "") {
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

    if (options[["groupingVariable"]] != "")
      totalTab[["group"]]    <- totalEstimates[["group"]]

    totalTab[["rhs"]]      <- totalEstimates[["rhs"]]
    totalTab[["lhs"]]      <- totalEstimates[["lhs"]]
    totalTab[["est"]]      <- totalEstimates[["est"]]

    if (options[["resamplingMethod"]] != "none") {
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

  if (options[["resamplingMethod"]] == "none") {
    return(list(rhs=rhs, lhs=lhs, est=est))
  } else {
    se       <- pe[[estimateType]]$sd
    zVal     <- pe[[estimateType]]$mean / pe[[estimateType]]$sd
    pVal     <- pnorm(abs(pe[[estimateType]]$mean / pe[[estimateType]]$sd), lower.tail = FALSE)
    ciLower  <- pe[[estimateType]]$CI_percentile[1,]
    ciUpper  <- pe[[estimateType]]$CI_percentile[2,]

    return(list(rhs=rhs, lhs=lhs, est=est, se=se, zVal=zVal, pVal=pVal, ciLower=ciLower, ciUpper=ciUpper))
  }
}

# Additional Fit Measures Table
.plsSemAdditionalFits <- function(modelContainer, dataset, options, ready) {
  if (!options[["outputAdditionalFitMeasures"]] || !is.null(modelContainer[["addfit"]])) return()

  # create additional fits table
  fitin <- createJaspTable(gettext("Additional Fit Measures"))
  fitin$addColumnInfo(name = "index", title = gettext("Index"), type = "string")
  if (length(options[["models"]]) < 2) {
    if(options[["groupingVariable"]] == "")
      fitin$addColumnInfo(name = "value", title = gettext("Value"), type = "number", format = "sf:4;dp:3")
    else {
      for (j in names(modelContainer[["modSelCriteria"]]$object$value$mfm["CFI",])) {
        fitin$addColumnInfo(name = paste0("value_", j), title = gettext(j), overtitle = options[["models"]][[1]][["modelName"]],
                            type = "number", format = "sf:4;dp:3")
      }
    }
  } else {
    if(options[["groupingVariable"]] == "") {
      for (i in seq_along(options[["models"]])) {
        fitin$addColumnInfo(name = paste0("value_", i), title = options[["models"]][[i]][["modelName"]], type = "number",
                            format = "sf:4;dp:3")
      }
    } else {
      for (i in seq_along(options[["models"]])) {
        for (j in names(modelContainer[["modSelCriteria"]]$object$value[[i]]$mfm["CFI",])) {
          fitin$addColumnInfo(name = paste0("value_",i,"_", j), title = gettext(j), overtitle = options[["models"]][[i]][["modelName"]],
                              type = "number", format = "sf:4;dp:3")
        }
      }
    }
  }

  fitin$dependOn(c("outputAdditionalFitMeasures", "models"))
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

    if (options[["groupingVariable"]] == "") {

      fitin[["value"]] <- list(modelContainer[["modSelCriteria"]]$object$value$mfm$CFI,
                               modelContainer[["modSelCriteria"]]$object$value$mfm$GFI,
                               modelContainer[["modSelCriteria"]]$object$value$mfm$CN,
                               modelContainer[["modSelCriteria"]]$object$value$mfm$IFI,
                               modelContainer[["modSelCriteria"]]$object$value$mfm$NNFI,
                               modelContainer[["modSelCriteria"]]$object$value$mfm$NFI,
                               modelContainer[["modSelCriteria"]]$object$value$mfm$RMSEA,
                               modelContainer[["modSelCriteria"]]$object$value$mfm$RMS_theta,
                               modelContainer[["modSelCriteria"]]$object$value$mfm$SRMR,
                               modelContainer[["modSelCriteria"]]$object$value$mfm$GoF,
                               modelContainer[["modSelCriteria"]]$object$value$mfm$DG,
                               modelContainer[["modSelCriteria"]]$object$value$mfm$DL,
                               modelContainer[["modSelCriteria"]]$object$value$mfm$DML)

    } else {
      for (j in names(modelContainer[["modSelCriteria"]]$object$value$mfm["CFI",])) {
        fitin[[paste0("value_", j)]] <- list(modelContainer[["modSelCriteria"]]$object$value$mfm["CFI",j],
                                             modelContainer[["modSelCriteria"]]$object$value$mfm["GFI", j],
                                             modelContainer[["modSelCriteria"]]$object$value$mfm["CN", j],
                                             modelContainer[["modSelCriteria"]]$object$value$mfm["IFI", j],
                                             modelContainer[["modSelCriteria"]]$object$value$mfm["NNFI", j],
                                             modelContainer[["modSelCriteria"]]$object$value$mfm["NFI", j],
                                             modelContainer[["modSelCriteria"]]$object$value$mfm["RMSEA", j],
                                             modelContainer[["modSelCriteria"]]$object$value$mfm["RMS_theta", j],
                                             modelContainer[["modSelCriteria"]]$object$value$mfm["SRMR", j],
                                             modelContainer[["modSelCriteria"]]$object$value$mfm["GoF", j],
                                             modelContainer[["modSelCriteria"]]$object$value$mfm["DG", j],
                                             modelContainer[["modSelCriteria"]]$object$value$mfm["DL", j],
                                             modelContainer[["modSelCriteria"]]$object$value$mfm["DML", j])

      }
    }
  } else {
    if (options[["groupingVariable"]] == "") {
      for (i in seq_along(options[["models"]])) {
        fitin[[paste0("value_", i)]] <- list(modelContainer[["modSelCriteria"]]$object$value[[i]]$mfm$CFI,
                                             modelContainer[["modSelCriteria"]]$object$value[[i]]$mfm$GFI,
                                             modelContainer[["modSelCriteria"]]$object$value[[i]]$mfm$CN,
                                             modelContainer[["modSelCriteria"]]$object$value[[i]]$mfm$IFI,
                                             modelContainer[["modSelCriteria"]]$object$value[[i]]$mfm$NNFI,
                                             modelContainer[["modSelCriteria"]]$object$value[[i]]$mfm$NFI,
                                             modelContainer[["modSelCriteria"]]$object$value[[i]]$mfm$RMSEA,
                                             modelContainer[["modSelCriteria"]]$object$value[[i]]$mfm$RMS_theta,
                                             modelContainer[["modSelCriteria"]]$object$value[[i]]$mfm$SRMR,
                                             modelContainer[["modSelCriteria"]]$object$value[[i]]$mfm$GoF,
                                             modelContainer[["modSelCriteria"]]$object$value[[i]]$mfm$DG,
                                             modelContainer[["modSelCriteria"]]$object$value[[i]]$mfm$DL,
                                             modelContainer[["modSelCriteria"]]$object$value[[i]]$mfm$DML)
      }



    } else {
      for (i in seq_along(options[["models"]])) {
        for (j in names(modelContainer[["modSelCriteria"]]$object$value[[i]]$mfm["CFI",])) {
          fitin[[paste0("value_",i,"_", j)]] <- list(modelContainer[["modSelCriteria"]]$object$value[[i]]$mfm["CFI",j],
                                                     modelContainer[["modSelCriteria"]]$object$value[[i]]$mfm["GFI", j],
                                                     modelContainer[["modSelCriteria"]]$object$value[[i]]$mfm["CN", j],
                                                     modelContainer[["modSelCriteria"]]$object$value[[i]]$mfm["IFI", j],
                                                     modelContainer[["modSelCriteria"]]$object$value[[i]]$mfm["NNFI", j],
                                                     modelContainer[["modSelCriteria"]]$object$value[[i]]$mfm["NFI", j],
                                                     modelContainer[["modSelCriteria"]]$object$value[[i]]$mfm["RMSEA", j],
                                                     modelContainer[["modSelCriteria"]]$object$value[[i]]$mfm["RMS_theta", j],
                                                     modelContainer[["modSelCriteria"]]$object$value[[i]]$mfm["SRMR", j],
                                                     modelContainer[["modSelCriteria"]]$object$value[[i]]$mfm["GoF", j],
                                                     modelContainer[["modSelCriteria"]]$object$value[[i]]$mfm["DG", j],
                                                     modelContainer[["modSelCriteria"]]$object$value[[i]]$mfm["DL", j],
                                                     modelContainer[["modSelCriteria"]]$object$value[[i]]$mfm["DML", j])

        }
      }
    }
  }
}

# Rsquared table
.plsSemRsquared <- function(modelContainer, dataset, options, ready) {
  if (!options[["outputRSquared"]] || !is.null(modelContainer[["tabrsquared"]])) return()

  # create rsquared table
  tabr2 <- createJaspTable(gettext("R-Squared"))
  if (options[["groupingVariable"]] != "")
    tabr2$addColumnInfo(name = "__grp__", title = "Group", type = "string", combine = TRUE)
  tabr2$addColumnInfo(name = "__var__", title = "Outcome", type = "string")
  if (length(options[["models"]]) < 2) {
    tabr2$addColumnInfo(name = "rsq", title = "R\u00B2", type = "number", format = "sf:4;dp:3")
    tabr2$addColumnInfo(name = "adjustedRsq", title = "Adjusted R\u00B2", type = "number", format = "sf:4;dp:3")
    } else {
    for (i in seq_along(options[["models"]])) {
      tabr2$addColumnInfo(name = paste0("rsq_", i), title = options[["models"]][[i]][["modelName"]],
                          overtitle = "R\u00B2", type = "number", format = "sf:4;dp:3")
    }
    for (i in seq_along(options[["models"]])) {
      tabr2$addColumnInfo(name = paste0("adjustedRsq_", i), title = options[["models"]][[i]][["modelName"]],
                          overtitle = "Adjusted R\u00B2", type = "number", format = "sf:4;dp:3")
    }
  }

  tabr2$dependOn(c("outputRSquared", "models"))
  tabr2$position <- 0.75

  modelContainer[["tabrsquared"]] <- tabr2

  if (!ready || modelContainer$getError()) return()

  # compute data and fill rsquared table
  if (options[["groupingVariable"]] == "") {

    if (length(options[["models"]]) < 2) {

      r2                     <- modelContainer[["modSelCriteria"]]$object$value$mfm$R2
      tabr2[["__var__"]]     <- as.list(names(r2))
      tabr2[["rsq"]]         <- as.list(r2)
      tabr2[["adjustedRsq"]] <- as.list(modelContainer[["modSelCriteria"]]$object$value$mfm$R2_adj)
    } else {

      # determine variable names

      r2li <- list()
      adjR2li <- list()
      for (i in seq_along(options[["models"]])) {
        r2li    <- c(r2li, list(modelContainer[["modSelCriteria"]]$object$value[[i]]$mfm$R2))
        adjR2li <- c(adjR2li, list(modelContainer[["modSelCriteria"]]$object$value[[i]]$mfm$R2_adj))
      }


      # generate dfs with these names
      r2df <- data.frame("varname__" = unique(unlist(lapply(r2li, names))))
      adjR2df <- data.frame("varname__" = unique(unlist(lapply(adjR2li, names))))
      tabr2[["__var__"]] <- unique(unlist(lapply(r2li, names)))

      for (i in 1:length(r2li)) {
        # fill matching vars from model with df
        r2df[match(names(r2li[[i]]), r2df[["varname__"]]), i + 1] <- r2li[[i]]
        # add column to table
        tabr2[[paste0("rsq_", i)]] <- r2df[[i + 1]]

        adjR2df[match(names(adjR2li[[i]]), adjR2df[["varname__"]]), i + 1] <- adjR2li[[i]]
        # add column to table
        tabr2[[paste0("adjustedRsq_", i)]] <- adjR2df[[i + 1]]
      }
    }
  } else {
    if (length(options[["models"]]) < 2) {

      r2res                  <- modelContainer[["modSelCriteria"]]$object$value$mfm["R2",]
      adjR2res               <- modelContainer[["modSelCriteria"]]$object$value$mfm["R2_adj",]
      groups                 <- as.list(names(r2res))
      vars                   <- unlist(lapply(r2res, names))
      groupli                <- list()
      for (i in 1:length(vars)) {
        groupli <- unlist(c(groupli, rep(groups[i], length(unique(vars)))))
      }

      tabr2[["__grp__"]]     <- groupli
      tabr2[["__var__"]]     <- vars
      tabr2[["rsq"]]         <- unlist(r2res)
      tabr2[["adjustedRsq"]] <- unlist(adjR2res)

    } else {

      # here is the most difficult case with multiple groups and multiple models
      # create a list with r2 results per model. each element is a list with ngroup elements
      r2li <- list()
      adjR2li <- list()
      for (i in seq_along(options[["models"]])) {
        r2li <- c(r2li, list(modelContainer[["modSelCriteria"]]$object$value[[i]]$mfm["R2",]))
        adjR2li <- c(adjR2li, list(modelContainer[["modSelCriteria"]]$object$value[[i]]$mfm["R2_adj",]))
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
        "grpname__" = rep(names(r2li[[1]]), vapply(unique_per_group, length, 0)),
        "varname__" = unlist(unique_per_group),
        stringsAsFactors = FALSE
      )

      adjR2df <- data.frame(
        "grpname__" = rep(names(adjR2li[[1]]), vapply(unique_per_group, length, 0)),
        "varname__" = unlist(unique_per_group),
        stringsAsFactors = FALSE
      )


      for (mod_idx in seq_along(r2li)) {
        for (grpname in names(r2li[[1]])) {
          # find correct rows in r2df for each model and group in r2li
          grp_idx <- which(r2df[["grpname__"]] == grpname)
          # complex code because varnames in r2res can be in different order
          row_idx <- grp_idx[match(names(r2li[[mod_idx]][[grpname]]), r2df[grp_idx, "varname__"])]
          # fill r2df with r2 results
          r2df[row_idx, mod_idx + 2] <- r2li[[mod_idx]][[grpname]]
          adjR2df[row_idx, mod_idx + 2] <- adjR2li[[mod_idx]][[grpname]]
        }
      }

      # fill jasp table with data
      tabr2[["__grp__"]] <- as.list(r2df[["grpname__"]])
      tabr2[["__var__"]] <- as.list(r2df[["varname__"]])
      for (i in seq_along(r2li)) tabr2[[paste0("rsq_", i)]] <- r2df[[i + 2]]
      for (i in seq_along(adjR2li)) tabr2[[paste0("adjustedRsq_", i)]] <- adjR2df[[i + 2]]
    }
  }
}

# Mardias Coefficients table
.plsSemMardiasCoefficient <- function(modelContainer, dataset, options, ready) {
  if (!options[["outputMardiasCoefficients"]] || !is.null(modelContainer[["semMardiasTable"]])) return()

  # create mardias coefficients table
  mardiatab <- createJaspTable(title = gettext("Mardia's coefficients"))
  mardiatab$position <- .2

  mardiatab$addColumnInfo(name = "Type",        title = "",                      type = "string")
  mardiatab$addColumnInfo(name = "Coefficient", title = gettext("Coefficient"),  type = "number")
  mardiatab$addColumnInfo(name = "z",           title = gettext("z"),            type = "number")
  mardiatab$addColumnInfo(name = "Chisq",       title = gettext("&#967;&sup2;"), type = "number")
  mardiatab$addColumnInfo(name = "DF",          title = gettext("df"),           type = "integer")
  mardiatab$addColumnInfo(name = "pvalue",      title = gettext("p"),            type = "pvalue")

  mardiatab$dependOn(c("outputMardiasCoefficients", "models"))
  modelContainer[["mardiasTable"]] <- mardiatab

  if (!ready || modelContainer$getError()) return()

  # fill mardias coefficients table
  if (options[["groupingVariable"]] == "") {
    varNames <- unique(unlist(lapply(modelContainer[["results"]][["object"]], function(x) colnames(x$Information$Model$measurement))))
  } else {
    varNames <- unique(unlist(lapply(modelContainer[["results"]][["object"]], function(x) lapply(x, function(y) colnames(y$Information$Model$measurement)))))
  }

  if (length(options[["models"]]) > 1)
    mardiatab$addFootnote(
      gettext("Multivariate skewness and kurtosis calculated for observed variables from all models.")
    )


  if (!all(sapply(dataset[, varNames, drop = FALSE], is.numeric))) {
    mardiatab$setError(gettext("Not all used variables are numeric. Mardia's coefficients not available."))
    return()
  }

  mardiaSkew <- unname(semTools:::mardiaSkew(dataset[, varNames]))
  mardiaKurtosis <- unname(semTools:::mardiaKurtosis(dataset[, varNames]))
  mardiatab$addRows(
    data.frame(Type        = gettext("Skewness"),
               Coefficient = mardiaSkew[1],
               z           = NA,
               Chisq       = mardiaSkew[2],
               DF          = mardiaSkew[3],
               pvalue      = mardiaSkew[4])
  )
  mardiatab$addRows(
    data.frame(Type        = gettext("Kurtosis"),
               Coefficient = mardiaKurtosis[1],
               z           = mardiaKurtosis[2],
               Chisq       = NA,
               DF          = NA,
               pvalue      = mardiaKurtosis[3])
  )

  return()
}

.plsSemReliabilities <- function(modelContainer, dataset, options, ready) {
  if (!options[["outputReliabilityMeasures"]] || !is.null(modelContainer[["tabReliability"]])) return()

  # init table
  tabrho <- createJaspTable(gettext("Reliability Measures"))
  if (options[["groupingVariable"]] != "")
    tabrho$addColumnInfo(name = "__grp__", title = "Group", type = "string", combine = TRUE)
  tabrho$addColumnInfo(name = "__var__", title = "Latent", type = "string")
  if (length(options[["models"]]) < 2) {
    tabrho$addColumnInfo(name = "rhoT", title = "Cronbachs &#945", type = "number", format = "sf:4;dp:3")
    tabrho$addColumnInfo(name = "rhoCmm", title = "J&#246reskogs &#961", type = "number", format = "sf:4;dp:3")
    tabrho$addColumnInfo(name = "rhoCWeightedmm", title = "Dijkstra-Henselers &#961", type = "number", format = "sf:4;dp:3")
  } else {
    for (i in seq_along(options[["models"]])) {
      tabrho$addColumnInfo(name = paste0("rhoT_", i), title = options[["models"]][[i]][["modelName"]],
                          overtitle = "Cronbach's &#945", type = "number", format = "sf:4;dp:3")
    }
    for (i in seq_along(options[["models"]])) {
      tabrho$addColumnInfo(name = paste0("rhoCmm_", i), title = options[["models"]][[i]][["modelName"]],
                          overtitle = "J&#246reskog's &#961", type = "number", format = "sf:4;dp:3")
    }
    for (i in seq_along(options[["models"]])) {
      tabrho$addColumnInfo(name = paste0("rhoCWeightedmm_", i), title = options[["models"]][[i]][["modelName"]],
                           overtitle = "Dijkstra-Henseler's &#961", type = "number", format = "sf:4;dp:3")
    }
  }

  tabrho$dependOn(c("outputReliabilityMeasures", "models"))
  tabrho$position <- 0.8

  modelContainer[["tabReliability"]] <- tabrho

  if (!ready || modelContainer$getError()) return()

  # compute data and fill table
  if (options[["groupingVariable"]] == "") {

    if (length(options[["models"]]) < 2) {

      tabrho[["__var__"]]        <- names(modelContainer[["modSelCriteria"]]$object$value$mfm$Reliability[["Cronbachs_alpha"]])
      tabrho[["rhoT"]]           <- modelContainer[["modSelCriteria"]]$object$value$mfm$Reliability[["Cronbachs_alpha"]]
      tabrho[["rhoCmm"]]         <- modelContainer[["modSelCriteria"]]$object$value$mfm$Reliability[["Joereskogs_rho"]]
      tabrho[["rhoCWeightedmm"]] <- modelContainer[["modSelCriteria"]]$object$value$mfm$Reliability[["Dijkstra-Henselers_rho_A"]]

    } else {

      # determine variable names

      rhoTli <- list()
      rhoCmmli <- list()
      rhoCWeightedmmli <- list()

      for (i in seq_along(options[["models"]])) {
        rhoTli    <- c(rhoTli, list(modelContainer[["modSelCriteria"]]$object$value[[i]]$mfm$Reliability[["Cronbachs_alpha"]]))
        rhoCmmli <- c(rhoCmmli, list(modelContainer[["modSelCriteria"]]$object$value[[i]]$mfm$Reliability[["Joereskogs_rho"]]))
        rhoCWeightedmmli <- c(rhoCWeightedmmli, list(modelContainer[["modSelCriteria"]]$object$value[[i]]$mfm$Reliability[["Dijkstra-Henselers_rho_A"]]))
      }


      # generate dfs with these names
      rhoTdf <- data.frame("varname__" = unique(unlist(lapply(rhoTli, names))))
      rhoCmmdf <- data.frame("varname__" = unique(unlist(lapply(rhoCmmli, names))))
      rhoCWeightedmmdf <- data.frame("varname__" = unique(unlist(lapply(rhoCWeightedmmli, names))))
      tabrho[["__var__"]] <- unique(unlist(lapply(rhoTli, names)))

      for (i in 1:length(rhoTli)) {
        # fill matching vars from model with df
        rhoTdf[match(names(rhoTli[[i]]), rhoTdf[["varname__"]]), i + 1] <- rhoTli[[i]]
        # add column to table
        tabrho[[paste0("rhoT_", i)]] <- rhoTdf[[i + 1]]

        rhoCmmdf[match(names(rhoCmmli[[i]]), rhoCmmdf[["varname__"]]), i + 1] <- rhoCmmli[[i]]
        # add column to table
        tabrho[[paste0("rhoCmm_", i)]] <- rhoCmmdf[[i + 1]]

        rhoCWeightedmmdf[match(names(rhoCWeightedmmli[[i]]), rhoCWeightedmmdf[["varname__"]]), i + 1] <- rhoCWeightedmmli[[i]]
        # add column to table
        tabrho[[paste0("rhoCWeightedmm_", i)]] <- rhoCWeightedmmdf[[i + 1]]
      }
    }
  } else {
    if (length(options[["models"]]) < 2) {

      reliability                  <- modelContainer[["modSelCriteria"]]$object$value$mfm["Reliability",]
      tabrho[["__grp__"]]          <- rep(names(reliability), vapply(lapply(reliability, function(x) x[["Cronbachs_alpha"]]), length, 0))
      tabrho[["__var__"]]          <- unlist(lapply(lapply(reliability, function(x) x[["Cronbachs_alpha"]]), names))
      tabrho[["rhoT"]]             <- unlist(lapply(reliability, function(x) x[["Cronbachs_alpha"]]))
      tabrho[["rhoCmm"]]           <- unlist(lapply(reliability, function(x) x[["Joereskogs_rho"]]))
      tabrho[["rhoCWeightedmm"]]   <- unlist(lapply(reliability, function(x) x[["Dijkstra-Henselers_rho_A"]]))


    } else {

      rhoTli           <- list()
      rhoCmmli         <- list()
      rhoCWeightedmmli <- list()

      for (i in seq_along(options[["models"]])) {
        reliability      <- modelContainer[["modSelCriteria"]]$object$value[[i]]$mfm["Reliability",]
        rhoTli <- c(rhoTli, list(lapply(reliability, function(x) x[["Cronbachs_alpha"]])))
        rhoCmmli <- c(rhoCmmli, list(lapply(reliability, function(x) x[["Joereskogs_rho"]])))
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
        "grpname__" = rep(names(rhoTli[[1]]), vapply(unique_per_group, length, 0)),
        "varname__" = unlist(unique_per_group),
        stringsAsFactors = FALSE
      )

      rhoCmmdf <- data.frame(
        "grpname__" = rep(names(rhoCmmli[[1]]), vapply(unique_per_group, length, 0)),
        "varname__" = unlist(unique_per_group),
        stringsAsFactors = FALSE
      )

      rhoCWeightedmmdf <- data.frame(
        "grpname__" = rep(names(rhoCWeightedmmli[[1]]), vapply(unique_per_group, length, 0)),
        "varname__" = unlist(unique_per_group),
        stringsAsFactors = FALSE
      )


      for (mod_idx in seq_along(rhoTli)) {
        for (grpname in names(rhoTli[[1]])) {

          grp_idx <- which(rhoTdf[["grpname__"]] == grpname)

          row_idx <- grp_idx[match(names(rhoTli[[mod_idx]][[grpname]]), rhoTdf[grp_idx, "varname__"])]

          rhoTdf[row_idx, mod_idx + 2] <- rhoTli[[mod_idx]][[grpname]]
          rhoCmmdf[row_idx, mod_idx + 2] <- rhoCmmli[[mod_idx]][[grpname]]
          rhoCWeightedmmdf[row_idx, mod_idx + 2] <- rhoCWeightedmmli[[mod_idx]][[grpname]]
        }
      }


      tabrho[["__grp__"]] <- rhoTdf[["grpname__"]]
      tabrho[["__var__"]] <- rhoTdf[["varname__"]]
      for (i in seq_along(rhoTli)) tabrho[[paste0("rhoT_", i)]] <- rhoTdf[[i + 2]]
      for (i in seq_along(rhoCmmli)) tabrho[[paste0("rhoCmm_", i)]] <- rhoCmmdf[[i + 2]]
      for (i in seq_along(rhoCWeightedmmli)) tabrho[[paste0("rhoCWeightedmm_", i)]] <- rhoCWeightedmmdf[[i + 2]]
    }
  }
}

.plsSemCor <- function(modelContainer, options, ready) {
  if (!(options[["outputObservedIndicatorCorrelations"]] || options[["outputImpliedIndicatorCorrelations"]] ||
        options[["outputObservedConstructCorrelations"]] || options[["outputImpliedConstructCorrelations"]]) ||
      !is.null(modelContainer[["cors"]])) return()

  cors <- createJaspContainer(gettext("Correlation tables"))
  cors$position <- 3
  cors$dependOn(c("outputObservedIndicatorCorrelations",
                    "outputImpliedIndicatorCorrelations",
                    "outputObservedConstructCorrelations",
                    "outputImpliedConstructCorrelations",
                    "models"))

  modelContainer[["cors"]] <- cors

  if (length(options[["models"]]) < 2) {
    .plsSemCorTables(modelContainer[["results"]][["object"]][[1]], NULL, cors, options, ready)
  } else {

    for (i in seq_along(options[["models"]])) {
      fit <- modelContainer[["results"]][["object"]][[i]]
      modelname <- options[["models"]][[i]][["modelName"]]
      .plsSemCorTables(fit, modelname, cors, options, ready)
    }
  }
}

.plsSemCorTables <- function(fit, modelname, parentContainer, options, ready) {
  if (is.null(modelname)) {
    corcont <- parentContainer
  } else {
    corcont <- createJaspContainer(modelname, initCollapsed = TRUE)
  }

  if (options[["groupingVariable"]] == "") {

    # without groups, these are tables

    if (options[["outputObservedIndicatorCorrelations"]]) {
      oictab <- createJaspTable("Observed indicator correlation matrix")
      oictab$dependOn("outputObservedIndicatorCorrelations")
      oictab$position <- 1
      corcont[["observedInd"]] <- oictab
    }

    if (options[["outputImpliedIndicatorCorrelations"]]) {
      iictab <- createJaspTable("Implied indicator correlation matrix")
      iictab$dependOn("outputImpliedIndicatorCorrelations")
      iictab$position <- 2
      corcont[["impliedInd"]] <- iictab
    }

    if (options[["outputObservedConstructCorrelations"]]) {
      occtab <- createJaspTable("Observed construct correlation matrix")
      occtab$dependOn("outputObservedConstructCorrelations")
      occtab$position <- 3
      corcont[["observedCon"]] <- occtab
    }

    if (options[["outputImpliedConstructCorrelations"]]) {
      icctab <- createJaspTable("Implied construct correlation matrix")
      icctab$dependOn("outputImpliedConstructCorrelations")
      icctab$position <- 4
      corcont[["impliedCon"]] <- icctab
    }

  } else {

    # with multiple groups these become containers

    if (options[["outputObservedIndicatorCorrelations"]]) {
      oiccont <- createJaspContainer("Observed indicator correlation matrix", initCollapsed = TRUE)
      oiccont$dependOn("outputObservedIndicatorCorrelations")
      oiccont$position <- 1
      corcont[["observedInd"]] <- oiccont
    }

    if (options[["outputImpliedIndicatorCorrelations"]]) {
      iiccont <- createJaspContainer("Implied indicator correlation matrix", initCollapsed = TRUE)
      iiccont$dependOn("outputImpliedIndicatorCorrelations")
      iiccont$position <- 2
      corcont[["impliedInd"]] <- iiccont
    }

    if (options[["outputObservedConstructCorrelations"]]) {
      occcont <- createJaspContainer("Observed construct correlation matrix", initCollapsed = TRUE)
      occcont$dependOn("outputObservedConstructCorrelations")
      occcont$position <- 3
      corcont[["observedCon"]] <- occcont
    }

    if (options[["outputImpliedConstructCorrelations"]]) {
      icccont <- createJaspContainer("Implied construct correlation matrix", initCollapsed = TRUE)
      icccont$dependOn("outputImpliedConstructCorrelations")
      icccont$position <- 4
      corcont[["impliedCon"]] <- icccont
    }
  }


  if (!ready) return()


  if (options[["groupingVariable"]] == "") {

    # without groups, just fill the tables

    if (options[["outputObservedIndicatorCorrelations"]]) {
      # actually compute the observed indicator correlations

      oic <- fit$Estimates$Indicator_VCV
      oic[upper.tri(oic)] <- NA

      for (i in 1:ncol(oic)) {
        nm <- colnames(oic)[i]
        oictab$addColumnInfo(nm, title = nm, type = "number", format = "sf:4;dp:3;p:.001")
      }
      oictab$addRows(oic, rowNames = colnames(oic))
    }

    if (options[["outputImpliedIndicatorCorrelations"]]) {
      # actually compute the implied indicator correlations
      iic <- cSEM::fit(fit, .type_vcv = "indicator")
      iic[upper.tri(iic)] <- NA

      for (i in 1:ncol(iic)) {
        nm <- colnames(iic)[i]
        iictab$addColumnInfo(nm, title = nm, type = "number", format = "sf:4;dp:3;p:.001")
      }
      iictab$addRows(iic, rowNames = colnames(iic))
    }

    if (options[["outputObservedConstructCorrelations"]]) {
      # actually compute the observed construct correlations
      occ <- fit$Estimates$Construct_VCV
      occ[upper.tri(occ)] <- NA

      for (i in 1:ncol(occ)) {
        nm <- colnames(occ)[i]
        occtab$addColumnInfo(nm, title = nm, type = "number", format = "sf:4;dp:3;p:.001")
      }
      occtab$addRows(occ, rowNames = colnames(occ))
    }

    if (options[["outputImpliedConstructCorrelations"]]) {
      # actually compute the implied construct correlations
      icc <- cSEM::fit(fit, .type_vcv = "construct")
      icc[upper.tri(icc)] <- NA

      for (i in 1:ncol(icc)) {
        nm <- colnames(icc)[i]
        icctab$addColumnInfo(nm, title = nm, type = "number", format = "sf:4;dp:3;p:.001")
      }
      icctab$addRows(icc, rowNames = colnames(icc))
    }

  } else {

    # with groups, create tables and fill them

    # actually compute the observed indicator correlations

    if (options[["outputObservedIndicatorCorrelations"]]) {

      groupNames <- names(fit)
      for (i in 1:length(fit)) {
        oic <- fit[[i]]$Estimates$Indicator_VCV
        oic[upper.tri(oic)] <- NA
        oiccont[[groupNames[i]]] <- createJaspTable(groupNames[i])


        for (j in 1:ncol(oic)) {
          nm <- colnames(oic)[j]
          oiccont[[groupNames[i]]]$addColumnInfo(nm, title = nm, type = "number", format = "sf:4;dp:3;p:.001")
        }
        oiccont[[groupNames[i]]]$addRows(oic, rowNames = colnames(oic))
      }
    }

    # actually compute the implied indicator correlations

    if (options[["outputImpliedIndicatorCorrelations"]]) {

      iicli <- cSEM::fit(fit, .type_vcv = "indicator")
      groupNames <- names(iicli)

      for (i in 1:length(fit)) {
        iic <- iicli[[i]]
        iic[upper.tri(iic)] <- NA
        iiccont[[groupNames[i]]] <- createJaspTable(groupNames[i])


        for (j in 1:ncol(iic)) {
          nm <- colnames(iic)[j]
          iiccont[[groupNames[i]]]$addColumnInfo(nm, title = nm, type = "number", format = "sf:4;dp:3;p:.001")
        }
        iiccont[[groupNames[i]]]$addRows(iic, rowNames = colnames(iic))
      }
    }

    # actually compute the observed construct correlations

    if (options[["outputObservedConstructCorrelations"]]) {

      groupNames <- names(fit)
      for (i in 1:length(fit)) {
        occ <- fit[[i]]$Estimates$Construct_VCV
        occ[upper.tri(occ)] <- NA
        occcont[[groupNames[i]]] <- createJaspTable(groupNames[i])


        for (j in 1:ncol(occ)) {
          nm <- colnames(occ)[j]
          occcont[[groupNames[i]]]$addColumnInfo(nm, title = nm, type = "number", format = "sf:4;dp:3;p:.001")
        }
        occcont[[groupNames[i]]]$addRows(occ, rowNames = colnames(occ))
      }
    }

    # actually compute the implied construct correlations

    if (options[["outputImpliedConstructCorrelations"]]) {

      iccli <- cSEM::fit(fit, .type_vcv = "construct")
      groupNames <- names(iccli)

      for (i in 1:length(fit)) {
        icc <- iccli[[i]]
        icc[upper.tri(icc)] <- NA
        icccont[[groupNames[i]]] <- createJaspTable(groupNames[i])


        for (j in 1:ncol(icc)) {
          nm <- colnames(icc)[j]
          icccont[[groupNames[i]]]$addColumnInfo(nm, title = nm, type = "number", format = "sf:4;dp:3;p:.001")
        }
        icccont[[groupNames[i]]]$addRows(icc, rowNames = colnames(icc))
      }
    }
  }

  if (!is.null(modelname)) {
    parentContainer[[modelname]] <- corcont
  }

  return()
}




