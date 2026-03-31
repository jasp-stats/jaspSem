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
# along with this program. If not, see <http://www.gnu.org/licenses/>.
#

BayesianSEMInternal <- function(jaspResults, dataset, options, ...) {
  jaspResults$addCitation("Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2), 1-36. URL http://www.jstatsoft.org/v48/i02/")
  jaspResults$addCitation("Merkle, E. C., & Rosseel, Y. (2018). blavaan: Bayesian Structural Equation Models via Parameter Expansion. Journal of Statistical Software, 85(4), 1-30. doi:10.18637/jss.v085.i04")

  # Read dataset
  options <- .bayesiansemPrepOpts(options)

  ready      <- .bayesiansemHasRunnableModels(options)
  runAllowed <- ready && isTRUE(options[["runAnalysis"]])

  modelContainer <- .bayesiansemModelContainer(jaspResults)

  # check for errors
  .bayesiansemCheckErrors(dataset, options, runAllowed, modelContainer)

  # Compute results (before output builders, cached in modelContainer)
  modelContainer <- .bayesiansemComputeResults(jaspResults, modelContainer, dataset, options, ready, runAllowed)
  modelContainer <- .bayesiansemComputePriorResults(jaspResults, modelContainer, dataset, options, ready, runAllowed)

  # Output functions
  .bayesiansemFitTab(jaspResults, modelContainer, dataset, options, ready)
  .bayesiansemAdditionalFits(jaspResults, modelContainer, dataset, options, ready)
  .bayesiansemWarningsHtml(jaspResults, modelContainer, dataset, options, ready)
  .bayesiansemParameters(jaspResults, modelContainer, dataset, options, ready)
  .bayesiansemTracePlots(jaspResults, modelContainer, dataset, options, ready)
  .bayesiansemPriorPredictivePlots(jaspResults, modelContainer, dataset, options, ready)
}

# helper functions

.bayesiansemPrepOpts <- function(options) {
  # backwards compatibility after changes to bouncontrollavaantextarea.cpp
  fixModel <- function(model) {
    if (is.character(model[["syntax"]])) return(model)
    newModel <- c(model[1], model[[2]])
    names(newModel)[names(newModel) == "model"] <- "syntax"
    return(newModel)
  }

  options[["models"]] <- lapply(options[["models"]], fixModel)

  emptymod <- vapply(options[["models"]], function(x) x[["syntax"]] == "", TRUE)
  options[["models"]] <- options[["models"]][!emptymod]
  if (is.null(options[["runAnalysis"]]))
    options[["runAnalysis"]] <- TRUE
  return(options)
}

.bayesiansemHasRunnableModels <- function(options) {

  if (length(options[["models"]]) < 1) return(FALSE)

  for (m in options[["models"]])
    if (nchar(trimws(m[["syntax"]])) > 0)
      return(TRUE)

  return(FALSE)
}

.bayesiansemGetUsedVars <- function(syntax, availablevars) {
  vv <- availablevars
  findpattern <- paste0("(?<=[\\s\\+\\^\\=\\~\\<\\*\\>\\:\\%\\|\\+]|^)\\Q",
                        vv,
                        "\\E(?=[\\s\\+\\^\\=\\~\\<\\*\\>\\:\\%\\|\\+]|$)")
  return(vv[vapply(findpattern,
                   function(p) stringr::str_detect(syntax, p),
                   FUN.VALUE = TRUE,
                   USE.NAMES = FALSE)])
}

.bayesiansemCheckErrors <- function(dataset, options, ready, modelContainer) {
  if (!ready) return()

  if (ncol(dataset) > 0) {
    if (length(options[["models"]]) < 1) return(FALSE)
    usedvars <- unique(unlist(lapply(options[["models"]], function(x) {
      .bayesiansemGetUsedVars(x[["syntax"]], colnames(dataset))
    })))
    .hasErrors(dataset[,usedvars],
               type = c("infinity"), message='default', exitAnalysisIfErrors = TRUE)
  }

  # Check whether grouping variable is a grouping variable
  if (isTRUE(options[["group"]] != "")) {
    groupfac <- factor(dataset[[options[["group"]]]])
    factab <- table(groupfac)
    if (any(factab < 3)) {
      violations <- names(table(groupfac))[table(groupfac) < 3]
      .quitAnalysis(gettextf("Grouping variable has fewer than 3 observations in group %s",
                             paste(violations, collapse = ", ")))

    }
  }

  return()
}

# dependencies that affect the model fit (MCMC / model specification options)
.bsemModelDeps <- c("dataType", "meanStructure", "manifestInterceptFixedToZero", "latentInterceptFixedToZero",
                    "factorScaling", "orthogonal", "group",
                    "equalLoading", "equalIntercept", "equalResidual", "equalResidualCovariance",
                    "equalMean", "equalThreshold", "equalRegression", "equalLatentVariance", "equalLatentCovariance",
                    "mcmcBurnin", "mcmcSamples", "mcmcChains", "mcmcThin",
                    "setSeed", "seed",
                    "priorLoadingParam1", "priorLoadingParam2",
                    "priorRegressionParam1", "priorRegressionParam2",
                    "priorObservedInterceptParam1", "priorObservedInterceptParam2",
                    "priorLatentInterceptParam1", "priorLatentInterceptParam2",
                    "priorThresholdParam1", "priorThresholdParam2",
                    "priorResidualSdParam1", "priorResidualSdParam2",
                    "priorLatentSdParam1", "priorLatentSdParam2",
                    "priorCorrelationParam1", "priorCorrelationParam2",
                    "freeParameters")

.bayesiansemModelContainer <- function(jaspResults) {
  if (!is.null(jaspResults[["modelContainer"]])) {
    modelContainer <- jaspResults[["modelContainer"]]
  } else {
    modelContainer <- createJaspContainer()
    jaspResults[["modelContainer"]] <- modelContainer
  }

  return(modelContainer)
}

.bayesiansemResetModelContainer <- function(jaspResults) {
  modelContainer <- createJaspContainer()
  jaspResults[["modelContainer"]] <- modelContainer
  modelContainer
}

.bsemCurrentFitOptionSignature <- function(options) {
  stats::setNames(lapply(.bsemModelDeps, function(dep) options[[dep]]), .bsemModelDeps)
}

.bsemCurrentPriorFitOptionSignature <- function(options) {
  priorSampling <- .bayesiansemPriorPredictiveSamplingOptions(options)

  list(
    fitOptions = .bsemCurrentFitOptionSignature(options),
    prisamp    = TRUE,
    burnin     = priorSampling[["burnin"]],
    sample     = priorSampling[["sample"]],
    n.chains   = priorSampling[["n.chains"]]
  )
}

.bayesiansemFitGroupLabels <- function(fit, options) {
  if (!isTRUE(options[["group"]] != ""))
    return(gettext("All data"))

  labels <- tryCatch(lavaan::lavInspect(fit, "group.label"), error = function(e) character())
  if (length(labels) < 1)
    return(gettext("All data"))

  labels
}

.bayesiansemFormatEstimationError <- function(fitError, dataset, modelName, prefix) {
  err <- .extractErrorMessage(fitError)
  err <- sub("^[^:]*: ?", "", err)

  if (err == "..constant..")
    err <- gettext("Invalid model specification. Did you pass a variable name as a string?")
  if (grepl(c("no variance"), err))
    err <- gettext("One or more variables are constants or contain only missing values. ")

  errmsg <- gettextf(prefix, err)

  paste0("Error in \"", modelName, "\" - ",
         .decodeVarsInMessage(names(dataset), errmsg))
}

.bayesiansemComputeResults <- function(jaspResults, modelContainer, dataset, options, ready, runAllowed) {

  if (!ready) {
    if (isTRUE(options[["runAnalysis"]]))
      modelContainer <- .bayesiansemResetModelContainer(jaspResults)
    return(modelContainer)
  }

  if (!runAllowed)
    return(modelContainer)

  oldmodels   <- modelContainer[["bsemCachedModels"]][["object"]]
  oldresults  <- modelContainer[["bsemCachedResults"]][["object"]]
  oldwarnings <- modelContainer[["bsemCachedWarnings"]][["object"]]
  oldFitSig   <- modelContainer[["bsemCachedFitSignature"]][["object"]]
  fitOptSig   <- .bsemCurrentFitOptionSignature(options)
  fitOptsSame <- identical(fitOptSig, oldFitSig)

  # all filtered models match cache: nothing to do, display elements survive
  if (!is.null(oldresults) && fitOptsSame && .bsemModelsIdentical(options[["models"]], oldmodels))
    return(modelContainer)

  # match old models element-wise
  reuse <- if (fitOptsSame) .bsemMatchModels(options[["models"]], oldmodels) else rep(NA_integer_, length(options[["models"]]))

  # only reorder needed (subset/permutation of cached models)
  if (!anyNA(reuse) && !is.null(oldresults)) {
    modelContainer[["bsemCachedResults"]]$object  <- oldresults[reuse]
    modelContainer[["bsemCachedModels"]]$object   <- options[["models"]]
    modelContainer[["bsemCachedWarnings"]]$object <- oldwarnings[reuse]
    if (!is.null(modelContainer[["bsemCachedFitSignature"]]))
      modelContainer[["bsemCachedFitSignature"]]$object <- fitOptSig
    .bayesiansemNullDisplayElements(modelContainer)
    return(modelContainer)
  }

  # a new fit is needed; start from a fresh container to drop stale errors and fit caches
  modelContainer <- .bayesiansemResetModelContainer(jaspResults)

  # models changed: NULL display elements so builders recreate them
  .bayesiansemNullDisplayElements(modelContainer)

  # create results list, prefill from cache where possible
  results  <- vector("list", length(options[["models"]]))
  warnings <- vector("list", length(options[["models"]]))

  if (fitOptsSame && any(!is.na(reuse)) && !is.null(oldresults)) {
    for (i in which(!is.na(reuse))) {
      results[[i]]  <- oldresults[[reuse[i]]]
      warnings[[i]] <- if (!is.null(oldwarnings)) oldwarnings[[reuse[i]]] else NULL
    }
  }

  # count how many models need fitting for the progressbar
  nToFit <- sum(vapply(results, is.null, TRUE))
  if (nToFit > 0)
    startProgressbar(nToFit, label = gettext("Estimating models..."))

  # generate blavaan options list
  blavaanOptions <- .bayesiansemOptionsToBlavOptions(options, dataset)

  for (i in seq_along(results)) {
    if (!is.null(results[[i]])) next # existing model is reused

    blavaanArgs            <- blavaanOptions
    blavaanArgs[["model"]] <- .bayesiansemTranslateModel(options[["models"]][[i]][["syntax"]], dataset)

    if (options[["dataType"]] == "raw")
      blavaanArgs[["data"]] <- dataset

    # fit the model with blavaan
    # blavaan/Stan corrupts future.globals.method.default to NULL after MCMC runs;
    # reset it before each call to prevent subsequent failures
    options("future.globals.method.default" = c("ordered", "dfs"))
    fit <- try(.withWarnings(do.call(blavaan::blavaan, blavaanArgs)))

    if (isTryError(fit)) {
      modelContainer$setError(
        .bayesiansemFormatEstimationError(
          fit,
          dataset,
          options[["models"]][[i]][["name"]],
          gettextf("Estimation failed. Message: %%")
        )
      )
      break
    }

    results[[i]] <- fit$value
    warnings[i] <- ifelse(is.null(fit$warnings), list(NULL), fit$warnings)
    progressbarTick()
  }

  # store in modelContainer
  if (!modelContainer$getError()) {
    modelContainer[["bsemCachedResults"]]  <- createJaspState(results)
    modelContainer[["bsemCachedModels"]]   <- createJaspState(options[["models"]])
    modelContainer[["bsemCachedWarnings"]] <- createJaspState(warnings)
    modelContainer[["bsemCachedFitSignature"]] <- createJaspState(fitOptSig)
  }

  return(modelContainer)
}

.bayesiansemComputePriorResults <- function(jaspResults, modelContainer, dataset, options, ready, runAllowed) {

  if (!isTRUE(options[["priorPredictivePlots"]]))
    return(modelContainer)

  if (!ready)
    return(modelContainer)

  if (!runAllowed)
    return(modelContainer)

  oldmodels   <- modelContainer[["bsemCachedPriorModels"]][["object"]]
  oldresults  <- modelContainer[["bsemCachedPriorResults"]][["object"]]
  oldwarnings <- modelContainer[["bsemCachedPriorWarnings"]][["object"]]
  olderror    <- modelContainer[["bsemCachedPriorError"]][["object"]]
  oldFitSig   <- modelContainer[["bsemCachedPriorFitSignature"]][["object"]]
  fitOptSig   <- .bsemCurrentPriorFitOptionSignature(options)
  fitOptsSame <- identical(fitOptSig, oldFitSig)

  if (fitOptsSame && .bsemModelsIdentical(options[["models"]], oldmodels) &&
      (!is.null(oldresults) || !is.null(olderror)))
    return(modelContainer)

  reuse <- if (fitOptsSame) .bsemMatchModels(options[["models"]], oldmodels) else rep(NA_integer_, length(options[["models"]]))

  if (!anyNA(reuse) && !is.null(oldresults) && is.null(olderror)) {
    modelContainer[["bsemCachedPriorResults"]]$object  <- oldresults[reuse]
    modelContainer[["bsemCachedPriorModels"]]$object   <- options[["models"]]
    modelContainer[["bsemCachedPriorWarnings"]]$object <- oldwarnings[reuse]
    if (!is.null(modelContainer[["bsemCachedPriorFitSignature"]]))
      modelContainer[["bsemCachedPriorFitSignature"]]$object <- fitOptSig
    .bayesiansemNullPriorPredictiveElements(modelContainer)
    return(modelContainer)
  }

  results    <- vector("list", length(options[["models"]]))
  warnings   <- vector("list", length(options[["models"]]))
  priorError <- NULL

  if (fitOptsSame && any(!is.na(reuse)) && !is.null(oldresults)) {
    for (i in which(!is.na(reuse))) {
      results[[i]]  <- oldresults[[reuse[i]]]
      warnings[[i]] <- if (!is.null(oldwarnings)) oldwarnings[[reuse[i]]] else NULL
    }
  }

  nToFit <- sum(vapply(results, is.null, TRUE))
  if (nToFit > 0)
    startProgressbar(nToFit, label = gettext("Estimating prior predictive models..."))

  blavaanOptions <- .bayesiansemOptionsToBlavOptions(options, dataset, prisamp = TRUE)

  for (i in seq_along(results)) {
    if (!is.null(results[[i]]))
      next

    blavaanArgs            <- blavaanOptions
    blavaanArgs[["model"]] <- .bayesiansemTranslateModel(options[["models"]][[i]][["syntax"]], dataset)

    if (options[["dataType"]] == "raw")
      blavaanArgs[["data"]] <- dataset

    options("future.globals.method.default" = c("ordered", "dfs"))
    fit <- try(.withWarnings(do.call(blavaan::blavaan, blavaanArgs)))

    if (isTryError(fit)) {
      priorError <- .bayesiansemFormatEstimationError(
        fit,
        dataset,
        options[["models"]][[i]][["name"]],
        gettextf("Prior predictive estimation failed. Message: %%")
      )
      break
    }

    results[[i]]  <- fit$value
    warnings[[i]] <- ifelse(is.null(fit$warnings), list(NULL), fit$warnings)
    progressbarTick()
  }

  modelContainer[["bsemCachedPriorResults"]]        <- createJaspState(results)
  modelContainer[["bsemCachedPriorModels"]]         <- createJaspState(options[["models"]])
  modelContainer[["bsemCachedPriorWarnings"]]       <- createJaspState(warnings)
  modelContainer[["bsemCachedPriorError"]]          <- createJaspState(priorError)
  modelContainer[["bsemCachedPriorFitSignature"]]   <- createJaspState(fitOptSig)

  .bayesiansemNullPriorPredictiveElements(modelContainer)

  return(modelContainer)
}

# compare models by name + syntax only (other fields like 'value' change between invocations)
.bsemModelsIdentical <- function(newModels, oldModels) {
  if (is.null(oldModels) || length(newModels) != length(oldModels)) return(FALSE)
  for (i in seq_along(newModels)) {
    if (newModels[[i]][["name"]]   != oldModels[[i]][["name"]])   return(FALSE)
    if (newModels[[i]][["syntax"]] != oldModels[[i]][["syntax"]]) return(FALSE)
  }
  TRUE
}

.bsemMatchModels <- function(newModels, oldModels) {
  if (is.null(oldModels)) return(rep(NA_integer_, length(newModels)))
  vapply(newModels, function(m) {
    for (j in seq_along(oldModels))
      if (m[["name"]] == oldModels[[j]][["name"]] && m[["syntax"]] == oldModels[[j]][["syntax"]])
        return(j)
    NA_integer_
  }, integer(1))
}

.bayesiansemNullDisplayElements <- function(modelContainer) {
  modelContainer[["fittab"]]       <- NULL
  modelContainer[["addfit"]]       <- NULL
  modelContainer[["warningsHtml"]] <- NULL
  modelContainer[["params"]]       <- NULL
  modelContainer[["traceplots"]]   <- NULL
  modelContainer[["priorPredictivePlots"]] <- NULL
}

.bayesiansemNullPriorPredictiveElements <- function(modelContainer) {
  modelContainer[["priorPredictivePlots"]] <- NULL
}

.bayesiansemPriorPredictiveSamplingOptions <- function(options) {
  list(
    burnin   = max(0L, if (!is.null(options[["priorPredictiveBurnin"]]))  as.integer(options[["priorPredictiveBurnin"]])  else 20L),
    sample   = max(1L, if (!is.null(options[["priorPredictiveSamples"]])) as.integer(options[["priorPredictiveSamples"]]) else 50L),
    n.chains = 1L
  )
}

.bayesiansemOptionsToBlavOptions <- function(options, dataset, prisamp = FALSE) {
  #' mapping the QML options from JASP to blavaan options
  blavaanOptions <- list()

  # model features
  # blavaan always estimates mean structure internally; setting meanstructure = FALSE
  # triggers a warning ("missing argument ml forces meanstructure = TRUE").
  # Always pass TRUE to avoid the warning; the meanStructure option controls display only.
  blavaanOptions[["meanstructure"]]   <- TRUE
  blavaanOptions[["int.ov.free"]]     <- !options[["manifestInterceptFixedToZero"]]
  blavaanOptions[["int.lv.free"]]     <- !options[["latentInterceptFixedToZero"]]
  blavaanOptions[["orthogonal"]]      <- options[["orthogonal"]]
  blavaanOptions[["std.lv"]]          <- options[["factorScaling"]] == "factorVariance"
  blavaanOptions[["auto.fix.first"]]  <- options[["factorScaling"]] == "factorLoading"

  # auto-settings matching bsem() defaults (required when calling blavaan() directly)
  blavaanOptions[["auto.fix.single"]] <- TRUE
  blavaanOptions[["auto.var"]]        <- TRUE
  blavaanOptions[["auto.cov.lv.x"]]  <- TRUE
  blavaanOptions[["auto.cov.y"]]     <- TRUE
  blavaanOptions[["auto.th"]]        <- TRUE
  blavaanOptions[["auto.delta"]]     <- TRUE

  # group variable
  if (isTRUE(options[["group"]] != "")) {
    blavaanOptions[["group"]] <- options[["group"]]
  }

  # group.equal constraints
  equality_constraints <- c(
    options[["equalLoading"]],
    options[["equalIntercept"]],
    options[["equalMean"]],
    options[["equalThreshold"]],
    options[["equalRegression"]],
    options[["equalResidual"]],
    options[["equalResidualCovariance"]],
    options[["equalLatentVariance"]],
    options[["equalLatentCovariance"]]
  )
  if (isTRUE(options[["group"]] != "") && any(equality_constraints)) {
    blavaanOptions[["group.equal"]] <- c("loadings", "intercepts", "means", "thresholds", "regressions",
                                         "residuals", "residual.covariances",
                                         "lv.variances", "lv.covariances")[equality_constraints]
  }

  # group.partial (release constraints)
  if (isTRUE(options[["group"]] != "") &&
      !is.null(options[["freeParameters"]]) &&
      options[["freeParameters"]][["model"]] != "") {
    splitted <- strsplit(options[["freeParameters"]][["model"]], "[\\n,;]+", perl = TRUE)[[1]]
    blavaanOptions[["group.partial"]] <- splitted
  }

  # Bayesian-specific options for blavaan
  if (isTRUE(prisamp)) {
    priorSampling <- .bayesiansemPriorPredictiveSamplingOptions(options)
    blavaanOptions[["burnin"]]   <- priorSampling[["burnin"]]
    blavaanOptions[["sample"]]   <- priorSampling[["sample"]]
    blavaanOptions[["n.chains"]] <- priorSampling[["n.chains"]]
  } else {
    blavaanOptions[["burnin"]]   <- if (!is.null(options[["mcmcBurnin"]]))   options[["mcmcBurnin"]]   else 500L
    blavaanOptions[["sample"]]   <- if (!is.null(options[["mcmcSamples"]]))  options[["mcmcSamples"]]  else 1000L
    blavaanOptions[["n.chains"]] <- if (!is.null(options[["mcmcChains"]]))   options[["mcmcChains"]]   else 3L
  }
  blavaanOptions[["target"]]   <- "stan"
  blavaanOptions[["prisamp"]]  <- isTRUE(prisamp)

  thinVal <- if (!is.null(options[["mcmcThin"]])) options[["mcmcThin"]] else 1L
  if (!isTRUE(prisamp) && thinVal > 1L)
    blavaanOptions[["bcontrol"]] <- list(thin = thinVal)

  if (isTRUE(options[["setSeed"]])) {
    blavaanOptions[["seed"]] <- options[["seed"]]
  }

  # Prior specification
  blavaanOptions[["dp"]] <- .bayesiansemBuildDpriors(options)

  return(blavaanOptions)
}

.bayesiansemBuildDpriors <- function(options) {
  # validate scale parameters are strictly positive
  scaleOpts <- c("priorLoadingParam2", "priorRegressionParam2", "priorObservedInterceptParam2",
                 "priorLatentInterceptParam2", "priorThresholdParam2",
                 "priorResidualSdParam1", "priorResidualSdParam2",
                 "priorLatentSdParam1", "priorLatentSdParam2",
                 "priorCorrelationParam1", "priorCorrelationParam2")
  for (opt in scaleOpts) {
    if (isTRUE(options[[opt]] <= 0))
      .quitAnalysis(gettextf("Prior parameter '%s' must be strictly positive.", opt))
  }

  fmt <- function(dist, p1, p2, suffix = "")
    paste0(dist, "(", p1, ",", p2, ")", suffix)

  blavaan::dpriors(
    target = "stan",
    lambda = fmt("normal", options[["priorLoadingParam1"]],           options[["priorLoadingParam2"]]),
    beta   = fmt("normal", options[["priorRegressionParam1"]],        options[["priorRegressionParam2"]]),
    nu     = fmt("normal", options[["priorObservedInterceptParam1"]], options[["priorObservedInterceptParam2"]]),
    alpha  = fmt("normal", options[["priorLatentInterceptParam1"]],   options[["priorLatentInterceptParam2"]]),
    tau    = fmt("normal", options[["priorThresholdParam1"]],         options[["priorThresholdParam2"]]),
    theta  = fmt("gamma",  options[["priorResidualSdParam1"]],        options[["priorResidualSdParam2"]], "[sd]"),
    psi    = fmt("gamma",  options[["priorLatentSdParam1"]],          options[["priorLatentSdParam2"]],  "[sd]"),
    rho    = fmt("beta",   options[["priorCorrelationParam1"]],       options[["priorCorrelationParam2"]])
  )
}

.bayesiansemTranslateModel <- function(syntax, dataset) {
  #' translate model syntax to jasp column names syntax
  usedvars <- .bayesiansemGetUsedVars(syntax, colnames(dataset))

  if (length(usedvars) == 0) {
    return(syntax)
  }

  usedvars <- usedvars[order(nchar(usedvars), decreasing = TRUE)]
  with.s.quotes <- paste("\\b'", usedvars, "'\\b", sep="")
  with.d.quotes <- paste('\\b"', usedvars, '"\\b', sep="")

  new.names <- usedvars

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

# output functions

.bayesiansemFitTab <- function(jaspResults, modelContainer, dataset, options, ready) {
  if (!is.null(modelContainer[["fittab"]])) return()

  showLooComparison <- isTRUE(options[["looComparison"]]) && length(options[["models"]]) >= 2

  fittab <- createJaspTable(title = gettext("Model Fit"))
  fittab$dependOn(c("warnings", "posteriorPredictivePvalue", "looComparison"))
  fittab$position <- 0

  fittab$addColumnInfo(name = "Model",    title = "",                            type = "string" , combine = TRUE)
  fittab$addColumnInfo(name = "N",        title = gettext("n(Obs.)"),            type = "integer")
  fittab$addColumnInfo(name = "npar",     title = gettext("Total"),              overtitle = gettext("n(Parameters)"), type = "integer")
  fittab$addColumnInfo(name = "nfree",    title = gettext("Free"),               overtitle = gettext("n(Parameters)"), type = "integer")
  if (options[["posteriorPredictivePvalue"]])
    fittab$addColumnInfo(name = "PPP",    title = gettext("PPP"),                type = "number")
  fittab$addColumnInfo(name = "DIC",      title = gettext("DIC"),                type = "number" )
  fittab$addColumnInfo(name = "WAIC",     title = gettext("WAIC"),               type = "number" )
  fittab$addColumnInfo(name = "LOO",      title = gettext("LOO"),                type = "number" )
  if (showLooComparison) {
    fittab$addColumnInfo(name = "Rank",      title = gettext("Rank"),            type = "integer")
    fittab$addColumnInfo(name = "elpdDiff",  title = gettext("ELPD diff"),       type = "number" )
    fittab$addColumnInfo(name = "seDiff",    title = gettext("SE diff"),         type = "number" )
  }
  modelContainer[["fittab"]] <- fittab

  if (!ready) return()

  # add data to the table!
  blavaanResults <- modelContainer[["bsemCachedResults"]][["object"]]
  if (is.null(blavaanResults)) return()

  if (modelContainer$getError()) return()

  # handle the warnings
  fnote <- ""

  warns <- unlist(modelContainer[["bsemCachedWarnings"]][["object"]])
  if (length(warns) > 0 && !options[["warnings"]]) {
    fnote <- gettextf("%sFitting the model resulted in warnings. Check the 'Show warnings' box in the Output Options to see the warnings. ", fnote)
  }

  if (length(blavaanResults) == 1) {
    rownames_data <- options[["models"]][[1]][["name"]]
    Ns <- lavaan::lavInspect(blavaanResults[[1]], "ntotal")
    npar <- lavaan::lavInspect(blavaanResults[[1]], "npar")
    nfree <- npar - sum(blavaanResults[[1]]@ParTable$op == "==")
  } else {
    rownames_data <- vapply(options[["models"]], getElement, name = "name", "")
    Ns <- vapply(blavaanResults, lavaan::lavInspect, 0, what = "ntotal")
    npar <- vapply(blavaanResults, lavaan::lavInspect, 0, what = "npar")
    nfree <- npar - vapply(blavaanResults, function(x) sum(x@ParTable$op == "=="), 0L)
  }

  # Extract Bayesian fit indices
  fitMeasures <- lapply(blavaanResults, function(fit) {
    tryCatch({
      fm <- lavaan::fitMeasures(fit, c("dic", "waic", "looic"))
      list(DIC = as.numeric(fm[["dic"]]), WAIC = as.numeric(fm[["waic"]]), looic = as.numeric(fm[["looic"]]))
    }, error = function(e) {
      list(DIC = NA, WAIC = NA, looic = NA)
    })
  })

  dtFill <- data.frame(matrix(ncol = 0, nrow = length(rownames_data)))
  dtFill[["Model"]]    <- rownames_data
  if (options[["posteriorPredictivePvalue"]]) {
    # reuse cached PPP values by model matching
    oldPpp       <- modelContainer[["bsemCachedPppValues"]][["object"]]
    oldPppModels <- modelContainer[["bsemCachedPppModels"]][["object"]]
    pppReuse     <- .bsemMatchModels(options[["models"]], oldPppModels)

    pppValues <- rep(NA_real_, length(blavaanResults))
    if (any(!is.na(pppReuse)) && !is.null(oldPpp)) {
      for (j in which(!is.na(pppReuse)))
        pppValues[j] <- oldPpp[[pppReuse[j]]]
    }

    nPppToCompute <- sum(is.na(pppValues))
    if (nPppToCompute > 0)
      startProgressbar(nPppToCompute, label = gettext("Computing posterior predictive p-values..."))

    for (j in seq_along(blavaanResults)) {
      if (!is.na(pppValues[j])) next
      pppValues[j] <- tryCatch({
        options("future.globals.method.default" = c("ordered", "dfs"))
        ppmcResult <- blavaan::ppmc(blavaanResults[[j]], fit.measures = "chisq")
        as.numeric(ppmcResult@PPP[["fit.indices"]]["chisq"])
      }, error = function(e) NA_real_)
      progressbarTick()
    }

    modelContainer[["bsemCachedPppValues"]] <- createJaspState(pppValues)
    modelContainer[["bsemCachedPppValues"]]$dependOn(optionsFromObject = modelContainer)
    modelContainer[["bsemCachedPppModels"]] <- createJaspState(options[["models"]])
    modelContainer[["bsemCachedPppModels"]]$dependOn(optionsFromObject = modelContainer)

    dtFill[["PPP"]] <- pppValues
  }
  dtFill[["DIC"]]      <- sapply(fitMeasures, function(x) x$DIC)
  dtFill[["WAIC"]]     <- sapply(fitMeasures, function(x) x$WAIC)
  dtFill[["LOO"]]      <- sapply(fitMeasures, function(x) x$looic)

  if (showLooComparison && length(blavaanResults) >= 2) {
    looValues  <- dtFill[["LOO"]]
    finiteLoo  <- is.finite(looValues)
    compFailed <- FALSE

    dtFill[["Rank"]]     <- NA_integer_
    dtFill[["elpdDiff"]] <- NA_real_
    dtFill[["seDiff"]]   <- NA_real_

    if (any(finiteLoo)) {
      rankIdx <- order(looValues[finiteLoo], seq_along(looValues)[finiteLoo])
      dtFill[["Rank"]][which(finiteLoo)[rankIdx]] <- seq_along(rankIdx)

      bestIdx <- which(finiteLoo)[rankIdx[1]]

      dtFill[["elpdDiff"]][bestIdx] <- 0
      dtFill[["seDiff"]][bestIdx]   <- 0

      for (j in which(finiteLoo)) {
        if (j == bestIdx) next

        compRes <- tryCatch({
          suppressWarnings(
            capture.output(
              comparison <- blavaan::blavCompare(blavaanResults[[bestIdx]], blavaanResults[[j]])
            )
          )
          comparison$diff_loo
        }, error = function(e) NULL)

        if (is.null(compRes) || nrow(compRes) < 2) {
          compFailed <- TRUE
          next
        }

        dtFill[["elpdDiff"]][j] <- as.numeric(compRes[2, "elpd_diff"])
        dtFill[["seDiff"]][j]   <- as.numeric(compRes[2, "se_diff"])
      }

      fnote <- paste0(
        fnote,
        gettext("LOO comparison ranks models by leave-one-out predictive performance. ELPD differences are relative to the best model; more negative values indicate worse expected out-of-sample performance. ")
      )

      if (compFailed) {
        fnote <- paste0(
          fnote,
          gettext("LOO comparison could not be computed for one or more models. ")
        )
      }
    }
  }

  dtFill[["N"]]        <- Ns
  dtFill[["npar"]]     <- npar
  dtFill[["nfree"]]    <- nfree

  fittab$setData(dtFill)
  if (nchar(fnote) > 0) {
    fittab$addFootnote(message = fnote)
  }
  if (options[["posteriorPredictivePvalue"]])
    modelContainer$addCitation("Levy, R. (2011). Bayesian data-model fit assessment for structural equation modeling. Structural Equation Modeling, 18(4), 663-685. doi:10.1080/10705511.2011.607723")

}

.bayesiansemAdditionalFits <- function(jaspResults, modelContainer, dataset, options, ready) {

  if (!isTRUE(options[["additionalFitMeasures"]]) || !is.null(modelContainer[["addfit"]])) return()

  fitContainer <- createJaspContainer(gettext("Additional Fit Measures"))
  fitContainer$dependOn("additionalFitMeasures")
  fitContainer$position <- 0.5
  modelContainer[["addfit"]] <- fitContainer

  if (!ready || modelContainer$getError()) return()

  blavaanResults <- modelContainer[["bsemCachedResults"]][["object"]]
  if (is.null(blavaanResults)) return()

  # Fit baseline (null) model for incremental indices (BCFI, BTLI, BNFI)
  baselineResults <- .bayesiansemFitBaseline(modelContainer, blavaanResults, dataset, options)

  ciLevel <- if (!is.null(options[["ciLevel"]])) options[["ciLevel"]] else 0.95

  for (i in seq_along(blavaanResults)) {
    modelName <- options[["models"]][[i]][["name"]]

    fitTable <- createJaspTable(title = if (length(blavaanResults) > 1) modelName else gettext("Fit Indices"))
    fitTable$position <- i

    fitTable$addColumnInfo(name = "index",  title = gettext("Index"),   type = "string")
    fitTable$addColumnInfo(name = "eap",    title = gettext("EAP"),     type = "number", format = "sf:4;dp:3")
    fitTable$addColumnInfo(name = "median", title = gettext("Median"),  type = "number", format = "sf:4;dp:3")
    fitTable$addColumnInfo(name = "sd",     title = gettext("Post. SD"), type = "number", format = "sf:4;dp:3")
    fitTable$addColumnInfo(name = "lower",  title = gettext("Lower"),   type = "number", format = "sf:4;dp:3",
                           overtitle = gettextf("%s%% Credible interval", round(ciLevel * 100)))
    fitTable$addColumnInfo(name = "upper",  title = gettext("Upper"),   type = "number", format = "sf:4;dp:3",
                           overtitle = gettextf("%s%% Credible interval", round(ciLevel * 100)))

    fitContainer[[paste0("fitTable_", i)]] <- fitTable

    bfi <- tryCatch({
      baseline <- if (!is.null(baselineResults)) baselineResults[[i]] else NULL
      blavaan::blavFitIndices(blavaanResults[[i]],
                              baseline.model = baseline,
                              pD = "loo",
                              fit.measures = "all")
    }, error = function(e) {
      fitTable$addFootnote(gettextf("Could not compute fit indices: %s", e$message))
      NULL
    })

    if (is.null(bfi)) next

    # Extract summary
    bfiSummary <- summary(bfi, central.tendency = c("mean", "median"),
                          prob = ciLevel)

    indexLabels <- c(
      "BRMSEA"       = gettext("Bayesian RMSEA"),
      "BGammaHat"    = gettext("Bayesian Gamma Hat"),
      "adjBGammaHat" = gettext("Bayesian Adj. Gamma Hat"),
      "BMc"          = gettext("Bayesian McDonald's Centrality"),
      "BCFI"         = gettext("Bayesian CFI"),
      "BTLI"         = gettext("Bayesian TLI"),
      "BNFI"         = gettext("Bayesian NFI")
    )

    availableIndices <- rownames(bfiSummary)
    rows <- data.frame(
      index  = unname(indexLabels[availableIndices]),
      eap    = bfiSummary[, "EAP"],
      median = bfiSummary[, "Median"],
      sd     = bfiSummary[, "SD"],
      lower  = bfiSummary[, "lower"],
      upper  = bfiSummary[, "upper"],
      stringsAsFactors = FALSE
    )

    fitTable$setData(rows)

    fitTable$addFootnote(gettext("Fit indices based on the deviance evaluated at the posterior mean (Garnier-Villarreal & Jorgensen, 2020)."))
  }

  fitContainer$addCitation("Garnier-Villarreal, M., & Jorgensen, T. D. (2020). Adapting Fit Indices for Bayesian Structural Equation Modeling: Comparison to Maximum Likelihood. Psychological Methods, 25(1), 46-70.")
}

.bayesiansemFitBaseline <- function(modelContainer, blavaanResults, dataset, options) {
  #' Fit a null (baseline) model for incremental Bayesian fit indices.
  #' Returns a list of baseline fits (one per target model).

  oldBaselineModels  <- modelContainer[["bsemCachedBaselineModels"]][["object"]]
  oldBaselineResults <- modelContainer[["bsemCachedBaselineResults"]][["object"]]
  reuse <- .bsemMatchModels(options[["models"]], oldBaselineModels)

  if (!anyNA(reuse) && !is.null(oldBaselineResults))
    return(oldBaselineResults[reuse])

  baselineResults <- vector("list", length(blavaanResults))
  if (any(!is.na(reuse)) && !is.null(oldBaselineResults)) {
    for (i in which(!is.na(reuse)))
      baselineResults[[i]] <- oldBaselineResults[[reuse[i]]]
  }

  blavaanOptions <- .bayesiansemOptionsToBlavOptions(options, dataset)

  for (i in seq_along(blavaanResults)) {
    if (!is.null(baselineResults[[i]]))
      next

    ovNames <- lavaan::lavNames(blavaanResults[[i]], "ov")

    # Null model: free variances and intercepts, all covariances = 0
    nullSyntax <- paste(
      c(paste0(ovNames, " ~~ ", ovNames),
        paste0(ovNames, " ~ 1")),
      collapse = "\n"
    )

    nullArgs <- blavaanOptions
    nullArgs[["model"]] <- nullSyntax
    if (options[["dataType"]] == "raw") {
      nullArgs[["data"]] <- dataset
    }
    # Override settings unsuitable for null model
    nullArgs[["auto.cov.lv.x"]] <- FALSE
    nullArgs[["auto.cov.y"]]    <- FALSE
    nullArgs[["orthogonal"]]    <- FALSE
    nullArgs[["std.lv"]]        <- FALSE
    nullArgs[["auto.fix.first"]] <- FALSE

    nullFit <- tryCatch(
      do.call(blavaan::blavaan, nullArgs),
      error = function(e) NULL
    )

    baselineResults[[i]] <- nullFit
  }

  # Cache for reuse
  modelContainer[["bsemCachedBaselineResults"]] <- createJaspState(baselineResults)
  modelContainer[["bsemCachedBaselineResults"]]$dependOn(optionsFromObject = modelContainer)
  modelContainer[["bsemCachedBaselineModels"]]  <- createJaspState(options[["models"]])
  modelContainer[["bsemCachedBaselineModels"]]$dependOn(optionsFromObject = modelContainer)

  return(baselineResults)
}

.bayesiansemWarningsHtml <- function(jaspResults, modelContainer, dataset, options, ready) {

  if (!options[["warnings"]] || !is.null(modelContainer[["warningsHtml"]])) return()

  if (!ready || modelContainer$getError()) return()

  warnings <- modelContainer[["bsemCachedWarnings"]][["object"]]
  warns <- unlist(warnings)

  if (length(warns) > 0) {
    if (length(unique(warns)) == 1) warns <- warns[1] # all warnings across models are the same
    warns <- trimws(gsub("\\s+", " ", warns))
    warns <- paste(warns, collapse = ". ")

    warnings <- createJaspHtml(text = gettextf("<b>Warnings:</b> %s", warns))

    warnings$dependOn("warnings")
    warnings$position <- 0.1

    modelContainer[["warningsHtml"]] <- warnings
  }

  return()
}

.bayesiansemParameters <- function(jaspResults, modelContainer, dataset, options, ready) {

  if (!is.null(modelContainer[["params"]])) return()
  if (modelContainer$getError()) return()

  params <- createJaspContainer(gettext("Parameter Estimates"))
  params$position <- 1
  params$dependOn(c("ciLevel", "convergenceDiagnostics"))

  modelContainer[["params"]] <- params

  cachedResults <- modelContainer[["bsemCachedResults"]][["object"]]
  if (is.null(cachedResults) || length(cachedResults) == 0)
    return()

  if (length(options[["models"]]) < 2) {
    .bayesiansemParameterTables(cachedResults[[1]], NULL, params, options, ready, modelContainer, dataset)
  } else {
    for (i in seq_along(options[["models"]])) {
      .bayesiansemParameterTables(cachedResults[[i]], options[["models"]][[i]], params, options, ready, modelContainer, dataset)
    }
  }
}

.bayesiansemParameterTables <- function(fit, model, parentContainer, options, ready, modelContainer, dataset) {
  if (!ready) return()
  if (is.null(fit)) return()
  if (is.null(model)) {
    pecont <- parentContainer
  } else {
    pecont <- createJaspContainer(model[["name"]], initCollapsed = TRUE)
  }

  # Extract parameter estimates from blavaan fit
  # parameterTable provides est (posterior mean) and se (posterior SD)
  # CIs must be computed from MCMC samples
  paramTable <- lavaan::parameterTable(fit)

  # Ensure label column exists
  if (!"label" %in% colnames(paramTable))
    paramTable$label <- ""

  # Compute credible intervals from MCMC draws
  ciProbs <- c((1 - options[["ciLevel"]]) / 2, 1 - (1 - options[["ciLevel"]]) / 2)
  mcmcDraws <- tryCatch(blavaan::blavInspect(fit, "mcmc"), error = function(e) NULL)

  paramTable$ci.lower <- NA_real_
  paramTable$ci.upper <- NA_real_

  if (!is.null(mcmcDraws)) {
    allDraws <- do.call(rbind, mcmcDraws)
    # MCMC columns match free parameters in order via lhs+op+rhs naming
    freeIdx <- which(paramTable$free > 0)
    for (j in seq_along(freeIdx)) {
      i <- freeIdx[j]
      if (j <= ncol(allDraws)) {
        ci <- quantile(allDraws[, j], probs = ciProbs, na.rm = TRUE)
        paramTable$ci.lower[i] <- ci[1]
        paramTable$ci.upper[i] <- ci[2]
      }
    }
    # Fixed parameters: CI equals the point estimate
    fixedIdx <- which(paramTable$free == 0 & !is.na(paramTable$est))
    paramTable$ci.lower[fixedIdx] <- paramTable$est[fixedIdx]
    paramTable$ci.upper[fixedIdx] <- paramTable$est[fixedIdx]
  }

  # Extract convergence diagnostics (Rhat & ESS) per free parameter
  paramTable$rhat <- NA_real_
  paramTable$neff <- NA_real_
  if (options[["convergenceDiagnostics"]]) {
    rhatVec <- tryCatch(blavaan::blavInspect(fit, "rhat"), error = function(e) NULL)
    neffVec <- tryCatch(blavaan::blavInspect(fit, "neff"), error = function(e) NULL)
    freeIdx <- which(paramTable$free > 0)
    if (!is.null(rhatVec)) {
      for (j in seq_along(freeIdx)) {
        if (j <= length(rhatVec))
          paramTable$rhat[freeIdx[j]] <- rhatVec[j]
      }
    }
    if (!is.null(neffVec)) {
      for (j in seq_along(freeIdx)) {
        if (j <= length(neffVec))
          paramTable$neff[freeIdx[j]] <- neffVec[j]
      }
    }
  }

  # Map group numbers to group labels for multigroup models
  if (isTRUE(options[["group"]] != "")) {
    groupLabels <- lavaan::lavInspect(fit, "group.label")
    paramTable$group <- ifelse(paramTable$group == 0, "", groupLabels[paramTable$group])
  }

  # Helper: create and attach a parameter table only when it has rows
  ciOvertitle <- gettextf("%s%% Credible interval", options[["ciLevel"]] * 100)
  hasGroup <- isTRUE(options[["group"]] != "")

  .makeParamTable <- function(key, title, lhsTitle, data, rhsTitle = NULL) {
    if (nrow(data) == 0) return()
    tab <- createJaspTable(title = gettext(title))
    if (hasGroup)
      tab$addColumnInfo(name = "group",    title = gettext("Group"),       type = "string", combine = TRUE)
    tab$addColumnInfo(name = "lhs",        title = gettext(lhsTitle),      type = "string", combine = TRUE)
    if (!is.null(rhsTitle))
      tab$addColumnInfo(name = "rhs",      title = gettext(rhsTitle),      type = "string")
    tab$addColumnInfo(name = "label",      title = "",                     type = "string")
    tab$addColumnInfo(name = "prior",      title = gettext("Prior"),       type = "string")
    tab$addColumnInfo(name = "est",        title = gettext("Post. Mean"),  type = "number")
    tab$addColumnInfo(name = "se",         title = gettext("Post. SD"),    type = "number")
    tab$addColumnInfo(name = "ci.lower",   title = gettext("Lower"),       type = "number", overtitle = ciOvertitle)
    tab$addColumnInfo(name = "ci.upper",   title = gettext("Upper"),       type = "number", overtitle = ciOvertitle)
    if (options[["convergenceDiagnostics"]]) {
      tab$addColumnInfo(name = "rhat",     title = gettext("Rhat"),        type = "number", format = "sf:4;dp:3")
      tab$addColumnInfo(name = "neff",     title = gettext("ESS"),         type = "number", format = "dp:0")
      if (any(data$rhat > 1.05, na.rm = TRUE))
        tab$addFootnote(gettext("Some parameters have Rhat > 1.05, indicating potential non-convergence."))
    }
    cols <- c("lhs", "label", "prior", "est", "se", "ci.lower", "ci.upper")
    if (options[["convergenceDiagnostics"]]) cols <- c(cols, "rhat", "neff")
    if (!is.null(rhsTitle)) cols <- c("lhs", "rhs", cols[!cols %in% c("lhs")])
    if (hasGroup) cols <- c("group", cols)
    tab$setData(data[, cols])
    pecont[[key]] <- tab
  }

  latentVars <- unique(paramTable$lhs[paramTable$op == "=~"])

  # Measurement model
  ind_params <- paramTable[paramTable$op == "=~", ]
  .makeParamTable("ind", "Factor Loadings", "Latent", ind_params, rhsTitle = "Indicator")

  # Structural model
  reg_params <- paramTable[paramTable$op == "~", ]
  .makeParamTable("reg", "Regression Coefficients", "Outcome", reg_params, rhsTitle = "Predictor")

  # Latent variances
  lvar_params <- paramTable[paramTable$op == "~~" & paramTable$lhs == paramTable$rhs & paramTable$lhs %in% latentVars, ]
  .makeParamTable("lvar", "Factor Variances", "Variable", lvar_params)

  # Latent covariances
  lcov_params <- paramTable[paramTable$op == "~~" & paramTable$lhs != paramTable$rhs &
                              (paramTable$lhs %in% latentVars | paramTable$rhs %in% latentVars), ]
  if (nrow(lcov_params) > 0)
    lcov_params$lhs <- paste(lcov_params$lhs, "-", lcov_params$rhs)
  .makeParamTable("lcov", "Factor Covariances", "Variables", lcov_params)

  # Residual variances
  var_params <- paramTable[paramTable$op == "~~" & paramTable$lhs == paramTable$rhs & !paramTable$lhs %in% latentVars, ]
  .makeParamTable("var", "Residual Variances", "Variable", var_params)

  # Residual covariances
  cov_params <- paramTable[paramTable$op == "~~" & paramTable$lhs != paramTable$rhs &
                             !paramTable$lhs %in% latentVars & !paramTable$rhs %in% latentVars, ]
  if (nrow(cov_params) > 0)
    cov_params$lhs <- paste(cov_params$lhs, "-", cov_params$rhs)
  .makeParamTable("cov", "Residual Covariances", "Variables", cov_params)

  # Means/Intercepts
  if (options[["meanStructure"]]) {
    mu_params <- paramTable[paramTable$op == "~1", ]
    .makeParamTable("mu", "Intercepts", "Variable", mu_params)
  }

  if (!is.null(model)) {
    parentContainer[[model[["name"]]]] <- pecont
  }
}

# Traceplots ----

.bayesiansemTracePlots <- function(jaspResults, modelContainer, dataset, options, ready) {
  if (!options[["tracePlots"]])               return()
  if (!is.null(modelContainer[["traceplots"]])) return()
  if (!ready || modelContainer$getError())    return()

  traceCont <- createJaspContainer(gettext("Traceplots"))
  traceCont$dependOn(c("tracePlots", "tracePlotsType"))
  traceCont$position <- 2
  modelContainer[["traceplots"]] <- traceCont

  cachedResults <- modelContainer[["bsemCachedResults"]][["object"]]
  if (is.null(cachedResults)) return()

  if (length(options[["models"]]) < 2) {
    .bayesiansemTraceplotsForFit(cachedResults[[1]], NULL, traceCont, options)
  } else {
    for (i in seq_along(options[["models"]])) {
      .bayesiansemTraceplotsForFit(cachedResults[[i]], options[["models"]][[i]], traceCont, options)
    }
  }
}

.bayesiansemTraceplotsForFit <- function(fit, model, parentContainer, options) {
  if (is.null(model)) {
    container <- parentContainer
  } else {
    container <- createJaspContainer(model[["name"]], initCollapsed = TRUE)
    parentContainer[[model[["name"]]]] <- container
  }

  mcmcDraws <- tryCatch(blavaan::blavInspect(fit, "mcmc"), error = function(e) NULL)
  if (is.null(mcmcDraws)) return()

  groupLabels <- .bayesiansemFitGroupLabels(fit, options)

  if (length(groupLabels) > 1L) {
    for (groupIdx in seq_along(groupLabels)) {
      groupCont <- createJaspContainer(groupLabels[[groupIdx]])
      groupCont$position <- groupIdx
      container[[paste0("group", groupIdx)]] <- groupCont

      .bayesiansemTraceplotParameterPlots(fit, mcmcDraws, groupCont, options, groupIdx = groupIdx)
    }
  } else {
    .bayesiansemTraceplotParameterPlots(fit, mcmcDraws, container, options)
  }
}

.bayesiansemTraceplotParameterPlots <- function(fit, mcmcDraws, container, options, groupIdx = NULL) {
  paramTable    <- lavaan::parameterTable(fit)
  freeIdx       <- which(paramTable$free > 0)
  selectedType  <- options[["tracePlotsType"]]

  groups <- list(
    loadings    = list(title = "Factor Loadings",         op = "=~", filter = NULL),
    regressions = list(title = "Regression Coefficients", op = "~",  filter = NULL),
    variances   = list(title = "Variances",               op = "~~", filter = "same"),
    covariances = list(title = "Covariances",             op = "~~", filter = "diff"),
    intercepts  = list(title = "Intercepts",              op = "~1", filter = NULL),
    thresholds  = list(title = "Thresholds",              op = "|",  filter = NULL)
  )

  plotIdx <- 0
  for (key in names(groups)) {
    if (selectedType != "all" && selectedType != key)
      next

    g    <- groups[[key]]
    mask <- paramTable$op == g$op & paramTable$free > 0

    if (!is.null(groupIdx))
      mask <- mask & paramTable$group == groupIdx

    if (!is.null(g$filter)) {
      if (g$filter == "same")
        mask <- mask & paramTable$lhs == paramTable$rhs
      if (g$filter == "diff")
        mask <- mask & paramTable$lhs != paramTable$rhs
    }

    if (key == "intercepts" && !options[["meanStructure"]])
      next

    if (sum(mask) == 0)
      next

    rowIdx   <- which(mask)
    mcmcCols <- match(rowIdx, freeIdx)
    keepCols <- !is.na(mcmcCols)
    if (!any(keepCols))
      next

    rowIdx   <- rowIdx[keepCols]
    mcmcCols <- mcmcCols[keepCols]
    labels   <- paste0(paramTable$lhs[rowIdx], paramTable$op[rowIdx], paramTable$rhs[rowIdx])

    dfList <- lapply(seq_along(mcmcDraws), function(ch) {
      mat <- as.matrix(mcmcDraws[[ch]])
      data.frame(
        Chain     = factor(ch),
        Iteration = rep(seq_len(nrow(mat)), length(mcmcCols)),
        Parameter = rep(labels, each = nrow(mat)),
        Value     = as.vector(mat[, mcmcCols, drop = FALSE])
      )
    })
    df <- do.call(rbind, dfList)

    plotIdx <- plotIdx + 1
    nParams <- length(mcmcCols)
    height  <- max(320, 160 * ceiling(nParams / 2))

    plt <- createJaspPlot(
      title  = gettext(g$title),
      width  = 600,
      height = height
    )
    plt$position <- plotIdx

    plotObj <- ggplot2::ggplot(df, ggplot2::aes(x = .data[["Iteration"]], y = .data[["Value"]],
                                                color = .data[["Chain"]])) +
      ggplot2::geom_line(alpha = 0.7, linewidth = 0.3) +
      ggplot2::facet_wrap(~ Parameter, scales = "free_y", ncol = 2) +
      ggplot2::labs(x = gettext("Iteration"), y = gettext("Value")) +
      jaspGraphs::geom_rangeframe(sides = "bl") +
      jaspGraphs::themeJaspRaw(legend.position = "none") +
      jaspGraphs::scale_JASPcolor_discrete()

    plt$plotObject <- plotObj
    container[[key]] <- plt
  }
}

# Prior predictive plots ----

.bayesiansemPriorPredictivePlots <- function(jaspResults, modelContainer, dataset, options, ready) {
  if (!isTRUE(options[["priorPredictivePlots"]]))             return()
  if (!is.null(modelContainer[["priorPredictivePlots"]]))     return()
  if (!ready || modelContainer$getError())                    return()

  priorCont <- createJaspContainer(gettext("Prior Predictive Plots"))
  priorCont$dependOn(c("priorPredictivePlots", "priorPredictiveBurnin", "priorPredictiveSamples", "priorPredictiveReplicates"))
  priorCont$position <- 3
  modelContainer[["priorPredictivePlots"]] <- priorCont

  priorError <- modelContainer[["bsemCachedPriorError"]][["object"]]
  if (!is.null(priorError)) {
    priorCont$setError(priorError)
    return()
  }

  cachedResults <- modelContainer[["bsemCachedPriorResults"]][["object"]]
  if (is.null(cachedResults) || length(cachedResults) == 0)
    return()

  if (length(options[["models"]]) < 2) {
    .bayesiansemPriorPredictivePlotForFit(cachedResults[[1]], NULL, priorCont, dataset, options)
  } else {
    for (i in seq_along(options[["models"]])) {
      .bayesiansemPriorPredictivePlotForFit(cachedResults[[i]], options[["models"]][[i]], priorCont, dataset, options)
    }
  }
}

.bayesiansemPriorPredictivePlotForFit <- function(fit, model, parentContainer, dataset, options) {
  if (is.null(fit))
    return()

  if (is.null(model)) {
    container <- parentContainer
  } else {
    container <- createJaspContainer(model[["name"]], initCollapsed = TRUE)
    parentContainer[[model[["name"]]]] <- container
  }

  plotData <- tryCatch(
    .bayesiansemBuildPriorPredictiveData(fit, dataset, options),
    error = function(e) e
  )

  if (inherits(plotData, "error")) {
    plt <- createJaspPlot(title = gettext("Observed vs. Prior Predictive"), width = 650, height = 320)
    plt$position <- 1
    container[["plot"]] <- plt
    plt$setError(gettextf("Could not create prior predictive plot: %s", plotData$message))
    return()
  }

  groups <- plotData[["groups"]]
  hasGroups <- length(groups) > 1L

  if (hasGroups) {
    for (groupIdx in seq_along(groups)) {
      groupCont <- createJaspContainer(names(groups)[groupIdx])
      groupCont$position <- groupIdx
      container[[paste0("group", groupIdx)]] <- groupCont

      .bayesiansemPriorPredictiveGroupPlot(groups[[groupIdx]], groupCont)
    }
  } else {
    .bayesiansemPriorPredictiveGroupPlot(groups[[1]], container)
  }
}

.bayesiansemBuildPriorPredictiveData <- function(fit, dataset, options) {
  ovNames <- lavaan::lavNames(fit, "ov")
  if (length(ovNames) < 1)
    stop(gettext("No observed variables available for prior predictive plotting."))

  observedData <- dataset[, ovNames, drop = FALSE]
  numericMask  <- vapply(observedData, is.numeric, TRUE)
  if (!any(numericMask))
    stop(gettext("Prior predictive plots are only available for numeric observed variables."))

  ovNames         <- ovNames[numericMask]
  decodedOvNames  <- jaspBase::decodeColNames(ovNames)
  numericIdx      <- which(numericMask)
  requestedReps   <- .bayesiansemPriorPredictiveReplicateCount(fit, options)
  sampledData     <- blavaan::sampleData(fit, nrep = requestedReps)
  sampledData     <- .bayesiansemNormalizeSampledData(sampledData)
  observedByGroup <- .bayesiansemObservedDataByGroup(fit, dataset, ovNames, options)

  observedLabel <- gettext("Observed")
  priorLabel    <- gettext("Prior predictive")
  hasGroups     <- length(observedByGroup) > 1

  observedLong <- lapply(seq_along(observedByGroup), function(groupIdx) {
    obs <- observedByGroup[[groupIdx]]
    data.frame(
      Group    = names(observedByGroup)[groupIdx],
      Variable = rep(decodedOvNames, each = nrow(obs)),
      Value    = as.vector(as.matrix(obs[, ovNames, drop = FALSE])),
      Series   = observedLabel
    )
  })
  observedLong <- do.call(rbind, observedLong)

  simulatedLong <- lapply(seq_along(sampledData), function(repIdx) {
    repData <- sampledData[[repIdx]]
    repLong <- lapply(seq_along(observedByGroup), function(groupIdx) {
      simMat <- as.matrix(repData[[groupIdx]])
      if (ncol(simMat) < max(numericIdx))
        stop(gettext("Prior predictive sampled data do not match the observed variable structure."))

      data.frame(
        Group     = names(observedByGroup)[groupIdx],
        Variable  = rep(decodedOvNames, each = nrow(simMat)),
        Value     = as.vector(simMat[, numericIdx, drop = FALSE]),
        Series    = priorLabel,
        Replicate = factor(repIdx)
      )
    })
    do.call(rbind, repLong)
  })
  simulatedLong <- do.call(rbind, simulatedLong)

  groupLevels <- names(observedByGroup)

  observedCurves <- .bayesiansemPriorPredictiveDensityCurves(
    observedLong,
    splitVars      = c("Group", "Variable"),
    groupLevels    = groupLevels,
    variableLevels = decodedOvNames
  )

  simulatedCurves <- .bayesiansemPriorPredictiveDensityCurves(
    simulatedLong,
    splitVars      = c("Group", "Variable", "Replicate"),
    groupLevels    = groupLevels,
    variableLevels = decodedOvNames
  )

  if (nrow(observedCurves) < 1)
    stop(gettext("Observed data do not contain enough finite variation for prior predictive plotting."))

  if (nrow(simulatedCurves) < 1)
    stop(gettext("Prior predictive sampled data do not contain enough finite variation for plotting."))

  groupedCurves <- .bayesiansemPriorPredictiveGroupedCurves(
    observedCurves,
    simulatedCurves,
    groupLevels    = groupLevels,
    variableLevels = decodedOvNames
  )

  if (length(groupedCurves) < 1)
    stop(gettext("Prior predictive sampled data do not contain enough finite variation for plotting."))

  list(
    groups    = groupedCurves,
    hasGroups = hasGroups
  )
}

.bayesiansemPriorPredictiveReplicateCount <- function(fit, options) {
  requested <- if (!is.null(options[["priorPredictiveReplicates"]])) as.integer(options[["priorPredictiveReplicates"]]) else 50L
  requested <- max(1L, requested)

  mcmcDraws <- tryCatch(blavaan::blavInspect(fit, "mcmc"), error = function(e) NULL)
  available <- if (is.null(mcmcDraws)) requested else sum(vapply(mcmcDraws, nrow, integer(1)))

  max(1L, min(requested, available))
}

.bayesiansemNormalizeSampledData <- function(sampledData) {
  if (length(sampledData) < 1)
    return(sampledData)

  if (is.matrix(sampledData[[1]]) || is.data.frame(sampledData[[1]]))
    return(lapply(sampledData, function(x) list(x)))

  sampledData
}

.bayesiansemPriorPredictiveDensityCurves <- function(data, splitVars, groupLevels, variableLevels) {
  if (nrow(data) < 1)
    return(data.frame())

  splitIdx <- split(
    seq_len(nrow(data)),
    interaction(data[, splitVars, drop = FALSE], drop = TRUE, lex.order = TRUE)
  )

  curves <- lapply(splitIdx, function(idx) {
    slice <- data[idx, , drop = FALSE]
    dens  <- .bayesiansemDensityCurveForVector(slice[["Value"]])

    if (is.null(dens))
      return(NULL)

    out <- data.frame(
      Group    = slice[["Group"]][1],
      Variable = slice[["Variable"]][1],
      Value    = dens[["x"]],
      Density  = dens[["y"]],
      Series   = slice[["Series"]][1],
      Curve    = paste(slice[1, splitVars, drop = TRUE], collapse = "__")
    )

    if ("Replicate" %in% names(slice))
      out[["Replicate"]] <- slice[["Replicate"]][1]

    out
  })

  curves <- Filter(Negate(is.null), curves)
  if (length(curves) < 1)
    return(data.frame())

  curves <- do.call(rbind, curves)
  curves[["Group"]]    <- factor(curves[["Group"]], levels = groupLevels)
  curves[["Variable"]] <- factor(curves[["Variable"]], levels = variableLevels)

  curves
}

.bayesiansemDensityCurveForVector <- function(values) {
  values <- values[is.finite(values)]

  if (length(values) < 2 || length(unique(values)) < 2)
    return(NULL)

  tryCatch(stats::density(values), error = function(e) NULL)
}

.bayesiansemPriorPredictiveGroupedCurves <- function(observedCurves, simulatedCurves, groupLevels, variableLevels) {
  groupedCurves <- stats::setNames(lapply(groupLevels, function(groupLabel) {
    vars <- lapply(variableLevels, function(variableLabel) {
      observedVar <- observedCurves[as.character(observedCurves[["Group"]]) == groupLabel &
                                      as.character(observedCurves[["Variable"]]) == variableLabel, , drop = FALSE]
      simulatedVar <- simulatedCurves[as.character(simulatedCurves[["Group"]]) == groupLabel &
                                        as.character(simulatedCurves[["Variable"]]) == variableLabel, , drop = FALSE]

      if (nrow(observedVar) < 1 || nrow(simulatedVar) < 1)
        return(NULL)

      list(
        variableLabel   = variableLabel,
        observedCurves  = observedVar,
        simulatedCurves = simulatedVar
      )
    })

    vars <- Filter(Negate(is.null), vars)
    if (length(vars) < 1)
      return(NULL)

    stats::setNames(vars, vapply(vars, function(x) x[["variableLabel"]], character(1)))
  }), groupLevels)

  groupedCurves <- Filter(Negate(is.null), groupedCurves)
  if (length(groupedCurves) < 1)
    return(groupedCurves)

  groupedCurves
}

.bayesiansemPriorPredictiveGroupPlot <- function(groupData, container) {
  priorLabel    <- gettext("Prior predictive")
  observedLabel <- gettext("Observed")
  nVars <- length(groupData)

  observedCurves  <- do.call(rbind, lapply(groupData, function(x) x[["observedCurves"]]))
  simulatedCurves <- do.call(rbind, lapply(groupData, function(x) x[["simulatedCurves"]]))
  variableLevels  <- vapply(groupData, function(x) x[["variableLabel"]], character(1))

  observedCurves[["Variable"]]  <- factor(as.character(observedCurves[["Variable"]]), levels = variableLevels)
  simulatedCurves[["Variable"]] <- factor(as.character(simulatedCurves[["Variable"]]), levels = variableLevels)

  plt <- createJaspPlot(
    title  = "",
    width  = 650,
    height = max(320, 180 * ceiling(nVars / 2))
  )
  plt$position <- 1

  plotObj <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data      = simulatedCurves,
      mapping   = ggplot2::aes(x = .data[["Value"]],
                               y = .data[["Density"]],
                               group = .data[["Curve"]],
                               color = .data[["Series"]]),
      alpha     = 0.15,
      linewidth = 0.3,
      na.rm     = TRUE
    ) +
    ggplot2::geom_line(
      data      = observedCurves,
      mapping   = ggplot2::aes(x = .data[["Value"]],
                               y = .data[["Density"]],
                               group = .data[["Curve"]],
                               color = .data[["Series"]]),
      linewidth = 0.9,
      na.rm     = TRUE
    ) +
    ggplot2::facet_wrap(~ Variable, scales = "free", ncol = 2, strip.position = "bottom") +
    ggplot2::labs(
      x     = "",
      y     = gettext("Density"),
      color = ""
    ) +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::scale_color_manual(
      values = stats::setNames(c("#4C78A8", "#D55E00"), c(priorLabel, observedLabel))
    ) +
    jaspGraphs::geom_rangeframe(sides = "bl") +
    jaspGraphs::themeJaspRaw() +
    ggplot2::theme(
      axis.text.y      = ggplot2::element_blank(),
      axis.ticks.y     = ggplot2::element_blank(),
      axis.line.y      = ggplot2::element_line(),
      strip.text.x = ggplot2::element_text(size = 14),
      strip.placement  = "outside"
    )

  plt$plotObject <- plotObj
  container[["plot"]] <- plt
}

.bayesiansemObservedDataByGroup <- function(fit, dataset, ovNames, options) {
  if (!isTRUE(options[["group"]] != "")) {
    observed <- list(dataset[, ovNames, drop = FALSE])
    names(observed) <- gettext("All data")
    return(observed)
  }

  groupLabels <- lavaan::lavInspect(fit, "group.label")
  groupFac    <- factor(as.character(dataset[[options[["group"]]]]), levels = groupLabels)
  observed    <- split(dataset[, ovNames, drop = FALSE], groupFac, drop = FALSE)
  observed
}
