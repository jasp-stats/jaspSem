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

  ready   <- .bayesiansemIsReady(dataset, options)

  modelContainer <- .bayesiansemModelContainer(jaspResults)

  # check for errors
  .bayesiansemCheckErrors(dataset, options, ready, modelContainer)

  # Output functions
  .bayesiansemFitTab(jaspResults, modelContainer, dataset, options, ready)
  .bayesiansemAdditionalFits(modelContainer, dataset, options, ready)
  .bayesiansemWarningsHtml(modelContainer, dataset, options, ready)
  .bayesiansemParameters(modelContainer, dataset, options, ready)
}

# helper functions
.bayesiansemPrepOpts <- function(options) {

  # Handle both current format (syntax is a plain string from TextArea) and
  # old ListBase format (syntax is a list with "model" key)
  fixModel <- function(model) {
    if (is.character(model[["syntax"]])) return(model)
    newModel <- c(model[1], model[[2]])
    names(newModel)[names(newModel) == "model"] <- "syntax"
    return(newModel)
  }

  options[["models"]] <- lapply(options[["models"]], fixModel)

  emptymod <- vapply(options[["models"]], function(x) x[["syntax"]] == "", TRUE)
  options[["models"]] <- options[["models"]][!emptymod]
  return(options)
}

.bayesiansemIsReady <- function(dataset, options) {

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

.bayesiansemModelContainer <- function(jaspResults) {
  if (!is.null(jaspResults[["modelContainer"]])) {
    modelContainer <- jaspResults[["modelContainer"]]
  } else {
    modelContainer <- createJaspContainer()
    modelContainer$dependOn(c("models", "meanStructure", "manifestInterceptFixedToZero", "latentInterceptFixedToZero",
                              "factorScaling", "orthogonal", "group",
                              "equalLoading", "equalIntercept", "equalResidual", "equalResidualCovariance",
                              "equalMean", "equalThreshold", "equalRegression", "equalLatentVariance", "equalLatentCovariance",
                              "mcmcBurnin", "mcmcSamples", "mcmcChains", "mcmcThin",
                              "userGaveSeed", "bootSeed"))
    jaspResults[["modelContainer"]] <- modelContainer
  }

  return(modelContainer)
}

.bayesiansemComputeResults <- function(modelContainer, dataset, options) {

  # find reusable results
  oldmodels  <- modelContainer[["models"]][["object"]]
  oldresults <- modelContainer[["results"]][["object"]]
  oldwarnings <- modelContainer[["warnings"]][["object"]]
  reuse <- match(options[["models"]], oldmodels)

  if (identical(reuse, seq_along(reuse))) return(oldresults) # reuse everything

  # create results list
  results <- vector("list", length(options[["models"]]))
  warnings <- vector("list", length(options[["models"]]))

  if (any(!is.na(reuse))) {
    # where possible, prefill results with old results
    results[seq_along(reuse)] <- oldresults[reuse]
    warnings[seq_along(reuse)] <- ifelse(is.null(oldwarnings[reuse]), list(NULL), oldwarnings[reuse])
  }

  # generate blavaan options list
  blavaanOptions <- .bayesiansemOptionsToBlavOptions(options, dataset)

  for (i in seq_along(results)) {
    if (!is.null(results[[i]])) next # existing model is reused

    # create options
    blavaanArgs <- blavaanOptions
    originalSyntax <- .bayesiansemTranslateModel(options[["models"]][[i]][["syntax"]], dataset)
    blavaanArgs[["model"]] <- originalSyntax

    if (options[["dataType"]] == "raw") {
      blavaanArgs[["data"]] <- dataset
    }

    # fit the model with blavaan
    fit <- try(.withWarnings(do.call(blavaan::blavaan, blavaanArgs)))

    if (isTryError(fit)) {
      err <- .extractErrorMessage(fit)
      err <- sub("^[^:]*: ?", "", err)
      if (err == "..constant..")
        err <- gettext("Invalid model specification. Did you pass a variable name as a string?")
      if (grepl(c("no variance"), err))
        err <- gettext("One or more variables are constants or contain only missing values. ")

      errmsg <- gettextf("Estimation failed. Message: %s", err)

      modelContainer$setError(paste0("Error in \"", options[["models"]][[i]][["name"]], "\" - ",
                                     .decodeVarsInMessage(names(dataset), errmsg)))
      modelContainer$dependOn("models")
      break
    }

    results[[i]] <- fit$value
    warnings[i] <- ifelse(is.null(fit$warnings), list(NULL), fit$warnings)
  }

  # store in model container
  if (!modelContainer$getError()) {
    modelContainer[["results"]] <- createJaspState(results)
    modelContainer[["results"]]$dependOn(optionsFromObject = modelContainer)
    modelContainer[["models"]]  <- createJaspState(options[["models"]])
    modelContainer[["models"]]$dependOn(optionsFromObject = modelContainer)
    modelContainer[["warnings"]] <- createJaspState(warnings)
    modelContainer[["warnings"]]$dependOn(optionsFromObject = modelContainer)
  }

  return(results)
}

.bayesiansemOptionsToBlavOptions <- function(options, dataset) {
  #' mapping the QML options from JASP to blavaan options
  blavaanOptions <- list()

  # model features
  blavaanOptions[["meanstructure"]]   <- options[["meanStructure"]]
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

  # Bayesian-specific options for blavaan
  blavaanOptions[["burnin"]]   <- if (!is.null(options[["mcmcBurnin"]]))   options[["mcmcBurnin"]]   else 500L
  blavaanOptions[["sample"]]   <- if (!is.null(options[["mcmcSamples"]])) options[["mcmcSamples"]] else 1000L
  blavaanOptions[["n.chains"]] <- if (!is.null(options[["mcmcChains"]]))  options[["mcmcChains"]]  else 3L
  blavaanOptions[["target"]]   <- "stan"

  thinVal <- if (!is.null(options[["mcmcThin"]])) options[["mcmcThin"]] else 1L
  if (thinVal > 1L)
    blavaanOptions[["bcontrol"]] <- list(thin = thinVal)

  if (options[["userGaveSeed"]]) {
    blavaanOptions[["seed"]] <- options[["bootSeed"]]
  }

  return(blavaanOptions)
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

  fittab <- createJaspTable(title = gettext("Model fit"))
  fittab$dependOn(c("models", "warnings"))
  fittab$position <- 0

  fittab$addColumnInfo(name = "Model",    title = "",                            type = "string" , combine = TRUE)

  if (isTRUE(options[["group"]] != "")) {
    fittab$addColumnInfo(name = "group",    title = gettext("Group"),              type = "string" )
  }
  fittab$addColumnInfo(name = "DIC",      title = gettext("DIC"),                type = "number" )
  fittab$addColumnInfo(name = "WAIC",     title = gettext("WAIC"),               type = "number" )
  fittab$addColumnInfo(name = "LOO",      title = gettext("LOO"),                type = "number" )
  fittab$addColumnInfo(name = "N",        title = gettext("n(Observations)"),    type = "integer")
  fittab$addColumnInfo(name = "npar",     title = gettext("Total"),              overtitle = gettext("n(Parameters)"), type = "integer")

  modelContainer[["fittab"]] <- fittab

  if (!ready) return()

  # add data to the table!
  blavaanResults <- .bayesiansemComputeResults(modelContainer, dataset, options)

  if (modelContainer$getError()) return()

  # handle the warnings
  fnote <- ""

  warns <- unlist(modelContainer[["warnings"]][["object"]])
  if (length(warns) > 0 && !options[["warnings"]]) {
    fnote <- gettextf("%sFitting the model resulted in warnings. Check the 'Show warnings' box in the Output Options to see the warnings. ", fnote)
  }

  if (length(blavaanResults) == 1) {
    rownames_data <- options[["models"]][[1]][["name"]]
    Ns <- lavaan::lavInspect(blavaanResults[[1]], "ntotal")
    npar <- lavaan::lavInspect(blavaanResults[[1]], "npar")
  } else {
    rownames_data <- vapply(options[["models"]], getElement, name = "name", "")
    Ns <- vapply(blavaanResults, lavaan::lavInspect, 0, what = "ntotal")
    npar <- vapply(blavaanResults, lavaan::lavInspect, 0, what = "npar")
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
  dtFill[["DIC"]]      <- sapply(fitMeasures, function(x) x$DIC)
  dtFill[["WAIC"]]     <- sapply(fitMeasures, function(x) x$WAIC)
  dtFill[["LOO"]]      <- sapply(fitMeasures, function(x) x$looic)
  dtFill[["N"]]        <- Ns
  dtFill[["npar"]]     <- npar

  fittab$setData(dtFill)
  if (nchar(fnote) > 0) {
    fittab$addFootnote(message = fnote)
  }

}

.bayesiansemAdditionalFits <- function(modelContainer, dataset, options, ready) {

  if (!isTRUE(options[["additionalFitMeasures"]]) || !is.null(modelContainer[["addfit"]])) return()

  fitContainer <- createJaspContainer(gettext("Additional Fit Measures"))
  fitContainer$dependOn(c("additionalFitMeasures", "models"))
  fitContainer$position <- 0.5
  modelContainer[["addfit"]] <- fitContainer

  if (!ready || modelContainer$getError()) return()

  blavaanResults <- .bayesiansemComputeResults(modelContainer, dataset, options)
  if (modelContainer$getError()) return()

  # Fit baseline (null) model for incremental indices (BCFI, BTLI, BNFI)
  baselineResults <- .bayesiansemFitBaseline(modelContainer, blavaanResults, dataset, options)

  ciLevel <- if (!is.null(options[["ciLevel"]])) options[["ciLevel"]] else 0.95

  for (i in seq_along(blavaanResults)) {
    modelName <- options[["models"]][[i]][["name"]]

    fitTable <- createJaspTable(title = if (length(blavaanResults) > 1) modelName else gettext("Fit indices"))
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

  if (!is.null(modelContainer[["baselineResults"]])) {
    return(modelContainer[["baselineResults"]][["object"]])
  }

  baselineResults <- vector("list", length(blavaanResults))
  blavaanOptions <- .bayesiansemOptionsToBlavOptions(options, dataset)

  for (i in seq_along(blavaanResults)) {
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
  modelContainer[["baselineResults"]] <- createJaspState(baselineResults)
  modelContainer[["baselineResults"]]$dependOn(optionsFromObject = modelContainer)

  return(baselineResults)
}

.bayesiansemWarningsHtml <- function(modelContainer, dataset, options, ready) {

  if (!options[["warnings"]] || !is.null(modelContainer[["warningsHtml"]])) return()

  if (!ready || modelContainer$getError()) return()

  warnings <- modelContainer[["warnings"]][["object"]]
  warns <- unlist(warnings)

  if (length(warns) > 0) {
    if (length(unique(warns)) == 1) warns <- warns[1] # all warnings across models are the same
    warns <- sub("^[^:]*: ?", "", warns)
    warns <- gsub("\n", "", warns)
    warns <- paste(warns, collapse = ".")

    warnings <- createJaspHtml(text = gettextf("<b>Warnings:</b> %s", warns))

    warnings$dependOn("warnings")
    warnings$position <- 0.1

    modelContainer[["warningsHtml"]] <- warnings
  }

  return()
}

.bayesiansemParameters <- function(modelContainer, dataset, options, ready) {

  if (!is.null(modelContainer[["params"]])) return()
  if (modelContainer$getError()) return()

  params <- createJaspContainer(gettext("Parameter Estimates"))
  params$position <- 1
  params$dependOn(c("ciLevel", "models"))

  modelContainer[["params"]] <- params

  if (length(options[["models"]]) < 2) {
    .bayesiansemParameterTables(modelContainer[["results"]][["object"]][[1]], NULL, params, options, ready, modelContainer, dataset)
  } else {

    for (i in seq_along(options[["models"]])) {
      fit <- modelContainer[["results"]][["object"]][[i]]
      model <- options[["models"]][[i]]
      .bayesiansemParameterTables(fit, model, params, options, ready, modelContainer, dataset)
    }
  }
}

.bayesiansemParameterTables <- function(fit, model, parentContainer, options, ready, modelContainer, dataset) {
  if (!ready) return()
  if (is.null(model)) {
    pecont <- parentContainer
  } else {
    pecont <- createJaspContainer(model[["name"]], initCollapsed = TRUE)
  }

  # Measurement model
  indtab <- createJaspTable(title = gettext("Factor Loadings"))

  if (isTRUE(options[["group"]] != ""))
    indtab$addColumnInfo(name = "group",  title = gettext("Group"),      type = "string", combine = TRUE)

  indtab$addColumnInfo(name = "lhs",      title = gettext("Latent"),     type = "string", combine = TRUE)
  indtab$addColumnInfo(name = "rhs",      title = gettext("Indicator"),  type = "string")
  indtab$addColumnInfo(name = "label",    title = "",                    type = "string")
  indtab$addColumnInfo(name = "est",      title = gettext("Post. Mean"), type = "number")
  indtab$addColumnInfo(name = "se",       title = gettext("Post. SD"),   type = "number")
  indtab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number",
                       overtitle = gettextf("%s%% Credible interval", options[["ciLevel"]] * 100))
  indtab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                       overtitle = gettextf("%s%% Credible interval", options[["ciLevel"]] * 100))

  pecont[["ind"]] <- indtab

  # Structural Model
  regtab <- createJaspTable(title = gettext("Regression coefficients"))

  if (isTRUE(options[["group"]] != ""))
    regtab$addColumnInfo(name = "group",  title = gettext("Group"),      type = "string", combine = TRUE)

  regtab$addColumnInfo(name = "lhs",      title = gettext("Outcome"),    type = "string", combine = TRUE)
  regtab$addColumnInfo(name = "rhs",      title = gettext("Predictor"),  type = "string")
  regtab$addColumnInfo(name = "label",    title = "",                    type = "string")
  regtab$addColumnInfo(name = "est",      title = gettext("Post. Mean"), type = "number")
  regtab$addColumnInfo(name = "se",       title = gettext("Post. SD"),   type = "number")
  regtab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number",
                       overtitle = gettextf("%s%% Credible interval", options[["ciLevel"]] * 100))
  regtab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                       overtitle = gettextf("%s%% Credible interval", options[["ciLevel"]] * 100))

  pecont[["reg"]] <- regtab

  # Latent variances
  lvartab <- createJaspTable(title = gettext("Factor variances"))

  if (isTRUE(options[["group"]] != ""))
    lvartab$addColumnInfo(name = "group",  title = gettext("Group"),      type = "string", combine = TRUE)

  lvartab$addColumnInfo(name = "lhs",      title = gettext("Variable"),   type = "string")
  lvartab$addColumnInfo(name = "label",    title = "",                    type = "string")
  lvartab$addColumnInfo(name = "est",      title = gettext("Post. Mean"), type = "number")
  lvartab$addColumnInfo(name = "se",       title = gettext("Post. SD"),   type = "number")
  lvartab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number",
                        overtitle = gettextf("%s%% Credible interval", options[["ciLevel"]] * 100))
  lvartab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                        overtitle = gettextf("%s%% Credible interval", options[["ciLevel"]] * 100))

  pecont[["lvar"]] <- lvartab

  # Latent covariances
  lcovtab <- createJaspTable(title = gettext("Factor covariances"))

  if (isTRUE(options[["group"]] != ""))
    lcovtab$addColumnInfo(name = "group",  title = gettext("Group"),      type = "string", combine = TRUE)

  lcovtab$addColumnInfo(name = "lhs",      title = gettext("Variables"),   type = "string")
  lcovtab$addColumnInfo(name = "label",    title = "",                    type = "string")
  lcovtab$addColumnInfo(name = "est",      title = gettext("Post. Mean"), type = "number")
  lcovtab$addColumnInfo(name = "se",       title = gettext("Post. SD"),   type = "number")
  lcovtab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number",
                        overtitle = gettextf("%s%% Credible interval", options[["ciLevel"]] * 100))
  lcovtab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                        overtitle = gettextf("%s%% Credible interval", options[["ciLevel"]] * 100))

  pecont[["lcov"]] <- lcovtab

  # Residual variances
  vartab <- createJaspTable(title = gettext("Residual variances"))

  if (isTRUE(options[["group"]] != ""))
    vartab$addColumnInfo(name = "group",  title = gettext("Group"),      type = "string", combine = TRUE)

  vartab$addColumnInfo(name = "lhs",      title = gettext("Variable"),   type = "string")
  vartab$addColumnInfo(name = "label",    title = "",                    type = "string")
  vartab$addColumnInfo(name = "est",      title = gettext("Post. Mean"), type = "number")
  vartab$addColumnInfo(name = "se",       title = gettext("Post. SD"),   type = "number")
  vartab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number",
                       overtitle = gettextf("%s%% Credible interval", options[["ciLevel"]] * 100))
  vartab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                       overtitle = gettextf("%s%% Credible interval", options[["ciLevel"]] * 100))

  pecont[["var"]] <- vartab

  # Residual covariances
  covtab <- createJaspTable(title = gettext("Residual covariances"))

  if (isTRUE(options[["group"]] != ""))
    covtab$addColumnInfo(name = "group",  title = gettext("Group"),      type = "string", combine = TRUE)

  covtab$addColumnInfo(name = "lhs",      title = gettext("Variables"),   type = "string")
  covtab$addColumnInfo(name = "label",    title = "",                    type = "string")
  covtab$addColumnInfo(name = "est",      title = gettext("Post. Mean"), type = "number")
  covtab$addColumnInfo(name = "se",       title = gettext("Post. SD"),   type = "number")
  covtab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number",
                       overtitle = gettextf("%s%% Credible interval", options[["ciLevel"]] * 100))
  covtab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                       overtitle = gettextf("%s%% Credible interval", options[["ciLevel"]] * 100))

  pecont[["cov"]] <- covtab

  allTables <- list(indtab, regtab, lvartab, lcovtab, vartab, covtab)

  # Means/Intercepts
  if (options[["meanStructure"]]) {

    mutab <- createJaspTable(title = gettext("Intercepts"))
    allTables[[length(allTables) + 1]] <- mutab

    if (isTRUE(options[["group"]] != ""))
      mutab$addColumnInfo(name = "group",  title = gettext("Group"),      type = "string", combine = TRUE)

    mutab$addColumnInfo(name = "lhs",      title = gettext("Variable"),   type = "string")
    mutab$addColumnInfo(name = "label",    title = "",                    type = "string")
    mutab$addColumnInfo(name = "est",      title = gettext("Post. Mean"), type = "number")
    mutab$addColumnInfo(name = "se",       title = gettext("Post. SD"),   type = "number")
    mutab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number",
                        overtitle = gettextf("%s%% Credible interval", options[["ciLevel"]] * 100))
    mutab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                        overtitle = gettextf("%s%% Credible interval", options[["ciLevel"]] * 100))

    pecont[["mu"]] <- mutab
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
    mcmcNames <- colnames(allDraws)
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

  # Split by parameter type and fill tables
  ind_params <- paramTable[paramTable$op == "=~", ]
  if (nrow(ind_params) > 0) {
    indtab$setData(ind_params[, c("lhs", "rhs", "label", "est", "se", "ci.lower", "ci.upper")])
  }

  reg_params <- paramTable[paramTable$op == "~", ]
  if (nrow(reg_params) > 0) {
    regtab$setData(reg_params[, c("lhs", "rhs", "label", "est", "se", "ci.lower", "ci.upper")])
  }

  lvar_params <- paramTable[paramTable$op == "~~" & paramTable$lhs == paramTable$rhs & paramTable$lhs %in% unique(ind_params$lhs), ]
  if (nrow(lvar_params) > 0) {
    lvartab$setData(lvar_params[, c("lhs", "label", "est", "se", "ci.lower", "ci.upper")])
  }

  lcov_params <- paramTable[paramTable$op == "~~" & paramTable$lhs != paramTable$rhs & 
                             (paramTable$lhs %in% unique(ind_params$lhs) | paramTable$rhs %in% unique(ind_params$lhs)), ]
  if (nrow(lcov_params) > 0) {
    lcov_params$lhs <- paste(lcov_params$lhs, "-", lcov_params$rhs)
    lcovtab$setData(lcov_params[, c("lhs", "label", "est", "se", "ci.lower", "ci.upper")])
  }

  var_params <- paramTable[paramTable$op == "~~" & paramTable$lhs == paramTable$rhs & !paramTable$lhs %in% unique(ind_params$lhs), ]
  if (nrow(var_params) > 0) {
    vartab$setData(var_params[, c("lhs", "label", "est", "se", "ci.lower", "ci.upper")])
  }

  cov_params <- paramTable[paramTable$op == "~~" & paramTable$lhs != paramTable$rhs & 
                           !paramTable$lhs %in% unique(ind_params$lhs) & !paramTable$rhs %in% unique(ind_params$lhs), ]
  if (nrow(cov_params) > 0) {
    cov_params$lhs <- paste(cov_params$lhs, "-", cov_params$rhs)
    covtab$setData(cov_params[, c("lhs", "label", "est", "se", "ci.lower", "ci.upper")])
  }

  if (options[["meanStructure"]]) {
    mu_params <- paramTable[paramTable$op == "~1", ]
    if (nrow(mu_params) > 0) {
      mutab$setData(mu_params[, c("lhs", "label", "est", "se", "ci.lower", "ci.upper")])
    }
  }

  if (!is.null(model)) {
    parentContainer[[model[["name"]]]] <- pecont
  }
}
