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

SEM <- function(jaspResults, dataset, options, ...) {
  jaspResults$addCitation("Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2), 1-36. URL http://www.jstatsoft.org/v48/i02/")

  # Read dataset
  options <- .semPrepOpts(options)

  # TODO: don't read data if we aren't ready anyway...
  dataset <- .semReadData(dataset, options)
  ready   <- .semIsReady(dataset, options)

  modelContainer <- .semModelContainer(jaspResults)

  # check for errors
  .semCheckErrors(dataset, options, ready, modelContainer)

  # Output functions
  .semFitTab(jaspResults, modelContainer, dataset, options, ready)
  .semParameters(modelContainer, dataset, options, ready)
  .semAdditionalFits(modelContainer, dataset, options, ready)
  .semRsquared(modelContainer, dataset, options, ready)
  .semMardiasCoefficient(modelContainer, dataset, options, ready)
  .semCov(modelContainer, dataset, options, ready)
  .semMI(modelContainer, datset, options, ready)
  .semPathPlot(modelContainer, dataset, options, ready)
}

# helper functions
.semPrepOpts <- function(options) {

  # backwards compatibility after changes to bouncontrollavaantextarea.cpp
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

.semReadData <- function(dataset, options) {
  if (!is.null(dataset)) return(dataset)

  variablesToRead <- if (options[["group"]] == "") character() else options[["group"]]
  for (model in options[["models"]])
    variablesToRead <- unique(c(variablesToRead, model[["columns"]]))

  return(.readDataSetToEnd(columns = variablesToRead))
}

.semIsReady <- function(dataset, options) {

  if (length(options[["models"]]) < 1) return(FALSE)

  for (m in options[["models"]])
    if (length(m[["columns"]]) > 0)
      return(TRUE)

  return(FALSE)
}

.semCheckErrors <- function(dataset, options, ready, modelContainer) {
  if (!ready) return()

  if (options$dataType == "varianceCovariance") {
    # Check if dataset is variance covariance matrix:
    .hasErrors(dataset, type = c("varCovMatrix", "infinity"),
               message='default', exitAnalysisIfErrors = TRUE)
  } else if (ncol(dataset) > 0) {
    if (length(options[["models"]]) < 1) return(FALSE)
    usedvars <- unique(unlist(lapply(options[["models"]], function(x) {
      .semGetUsedVars(x[["syntax"]], colnames(dataset))
    })))
    .hasErrors(dataset[,usedvars],
               type = c("infinity"), message='default', exitAnalysisIfErrors = TRUE)
  }

  # check FIML
  if (!options[["estimator"]] %in% c("default", "ml") && options[["naAction"]] == "fiml") {
    modelContainer$setError(gettext("FIML missing data handling only available with ML-type estimators"))
  }

  # Check whether grouping variable is a grouping variable
  if (options[["group"]] != "") {
    groupfac <- factor(dataset[[.v(options[["group"]])]])
    factab <- table(groupfac)
    if (any(factab < 3)) {
      violations <- names(table(groupfac))[table(groupfac) < 3]
      .quitAnalysis(gettextf("Grouping variable has fewer than 3 observations in group %s",
                    paste(violations, collapse = ", ")))

    }
  }


  # Check mean structure:
  if (options[["dataType"]] == "varianceCovariance") {
    if (options[["meanStructure"]]) {
      modelContainer$setError(gettext("Mean structure can not be included when data is variance-covariance matrix"))
      return()
    }

    options$meanStructure <- FALSE

    if (options[["sampleSize"]] == 0) {
      modelContainer$setError(gettext("Please set the sample size!"))
      return()
    }

    # Check for multiple groups:
    if (options[["group"]] != "") {
      modelContainer$setError(gettext("Multiple group analysis not supported when data is variance-covariance matrix"))
      return()
    }

  } else {
    if (ncol(dataset) > 0 && !nrow(dataset) > ncol(dataset)) {
      modelContainer$setError(gettext("Not more cases than number of variables. Is your data a variance-covariance matrix?"))
      return()
    }
  }
}

checkLavaanModel <- function(model, availableVars) {

  # function returns informative printable string if there is an error, else ""
  if (model == "") return("Enter a model")

  # translate to base64 - function from semsimple.R
  vvars    <- availableVars
  usedvars <- vvars #.semGetUsedVars(model, vvars)
  vmodel   <- model # .semTranslateModel(model, usedvars)

  unvvars <- availableVars
  names(unvvars) <- vvars

  # Check model syntax
  parsed <- try(lavaan::lavParseModelString(vmodel, TRUE), silent = TRUE)
  if (inherits(parsed, "try-error")) {
    msg <- attr(parsed, "condition")$message
    if (msg == "NA/NaN argument") {
      return("Enter a model")
    }
    return(stringr::str_replace_all(msg, unvvars))
  }

  # Check variable names
  if (!missing(availableVars)) {
    latents <- unique(parsed[parsed$op == "=~",]$lhs)
    modelVars <- setdiff(unique(c(parsed$lhs, parsed$rhs)), latents)
    modelVars <- modelVars[modelVars != ""] # e.g., x1 ~ 1 yields an empty rhs entry

    modelVarsInAvailableVars <- (modelVars %in% vvars)
    if (!all(modelVarsInAvailableVars)) {
      notRecognized <- modelVars[!modelVarsInAvailableVars]
      return(paste("Variable(s) in model syntax not recognized:",
                   paste(stringr::str_replace_all(notRecognized, unvvars),
                         collapse = ", ")))
    }
  }

  # if checks pass, return empty string
  return("")
}


.semGetUsedVars <- function(syntax, availablevars) {
  vv <- availablevars
  findpattern <- paste0("(?<=[\\s\\+\\^\\=\\~\\<\\*\\>\\:\\%\\|\\+]|^)\\Q",
                        vv,
                        "\\E(?=[\\s\\+\\^\\=\\~\\<\\*\\>\\:\\%\\|\\+]|$)")
  return(vv[vapply(findpattern,
                   function(p) stringr::str_detect(syntax, p),
                   FUN.VALUE = TRUE,
                   USE.NAMES = FALSE)])
}

.semModelContainer <- function(jaspResults) {
  if (!is.null(jaspResults[["modelContainer"]])) {
    modelContainer <- jaspResults[["modelContainer"]]
  } else {
    modelContainer <- createJaspContainer()
    modelContainer$dependOn(c("samplingWeights", "meanStructure", "manifestInterceptFixedToZero", "latentInterceptFixedToZero", "exogenousCovariateFixed", "orthogonal",
                              "factorScaling", "residualSingleIndicatorOmitted", "residualVariance", "exogenousLatentCorrelation",
                              "dependentCorrelation", "threshold", "scalingParameter", "efaConstrained", "standardizedVariable", "naAction", "estimator", "test",
                              "errorCalculationMethod", "informationMatrix", "emulation", "group", "equalLoading", "equalIntercept",
                              "equalResidual", "equalResidualCovariance", "equalMean", "equalThreshold", "equalRegression",
                              "equalVariance", "equalLatentCovariance", "dataType", "sampleSize", "freeParameters"))
    jaspResults[["modelContainer"]] <- modelContainer
  }

  return(modelContainer)
}

.semComputeResults <- function(modelContainer, dataset, options) {
  #' create result list from options
  # find reusable results
  if (!options[["estimator"]] %in% c("default", "ml") && options[["naAction"]] == "fiml") return()

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

  # generate lavaan options list
  lavopts <- .semOptionsToLavOptions(options, dataset)

  for (i in seq_along(results)) {
    if (!is.null(results[[i]])) next # existing model is reused

    # create options
    lav_args <- lavopts
    syntax   <- .semTranslateModel(options[["models"]][[i]][["syntax"]], dataset)
    lav_args[["model"]] <- syntax
    if (options[["dataType"]] == "raw") {
      lav_args[["data"]]  <- dataset
    } else {
      lav_args[["sample.cov"]] <- .semDataCovariance(dataset, options[["models"]][[i]][["syntax"]])
      lav_args[["sample.nobs"]] <- options[["sampleSize"]]
    }

    # fit the model
    fit <- try(do.call(lavaan::lavaan, lav_args))

    if (isTryError(fit)) {
      err <- .extractErrorMessage(fit)
      if(err == "..constant.."){
        err <- gettext("Invalid model specification. Did you pass a variable name as a string?")
      }
      if(grepl(c("no variance"), err))
        err <- gettext("One or more variables are constants or contain only missing values ")

      if(grepl(c("categorical"), err)){
        if(grepl("ml", err))
          errMissingMethod <- "FIML"
        if(grepl("two.stage", err))
          errMissingMethod <- "Two-stage"
        if(grepl("robust.two.stage", err))
          errMissingMethod <- "Robust two-stage"
        err <- gettextf("Missing data handling '%s' is not supported for categorical data,
                        please select another method under 'Missing data handling'
                        within the 'Estimation options' tab", errMissingMethod)
      }

      errmsg <- gettextf("Estimation failed Message: %s", err)

      modelContainer$setError(paste0("Error in model \"", options[["models"]][[i]][["name"]], "\" - ",
                                    .decodeVarsInMessage(names(dataset), errmsg)))
      modelContainer$dependOn("models") # add dependency so everything gets updated upon model change
      break
    }

    if(isFALSE(slot(fit, "optim")$converged)) {
      errormsg <- gettextf("Estimation failed! Message: Model %s did not converge!", options[["models"]][[i]][["name"]])
      modelContainer$setError(errormsg)
      modelContainer$dependOn("models")
      break
    }

    if(lavaan::fitMeasures(fit, "df") < 0 ) {
      errormsg <- gettextf("Estimation failed! Message: Model %s has negative degrees of freedom.", options[["models"]][[i]][["name"]])
      modelContainer$setError(errormsg)
      modelContainer$dependOn("models")
      break
    }

    if (options[["errorCalculationMethod"]] == "bootstrap") {
      fit <- lavBootstrap(fit, options[["bootstrapSamples"]])
    }
    results[[i]] <- fit

  }

  # store in model container
  if (!modelContainer$getError()) {
    modelContainer[["results"]] <- createJaspState(results)
    modelContainer[["results"]]$dependOn(optionsFromObject = modelContainer)
    modelContainer[["models"]]  <- createJaspState(options[["models"]])
    modelContainer[["models"]]$dependOn(optionsFromObject = modelContainer)
  }

  return(results)
}

.semDataCovariance <- function(dataset, syntax) {
  usedvars <- .semGetUsedVars(syntax, colnames(dataset))
  var_idx  <- match(usedvars, .unv(colnames(dataset)))
  mat <- try(as.matrix(dataset[var_idx, var_idx]))
  if (inherits(mat, "try-error") || any(is.na(mat)))
    .quitAnalysis("Input data does not seem to be a covariance matrix! Please check the format of the input data.
                   All cells must be numeric, and the number of rows must equal the number of columns.")
  colnames(mat) <- rownames(mat) <- colnames(dataset)[var_idx]
  print(mat)
  return(mat)
}

.semOptionsToLavOptions <- function(options, dataset) {
  #' mapping the QML options from JASP to lavaan options
  #' see ?lavOptions for documentation
  lavopts <- lavaan::lavOptions()

  lavopts[["mimic"]] <- options[["emulation"]]


  # model features
  lavopts[["meanstructure"]]   <- options[["meanStructure"]]
  lavopts[["int.ov.free"]]     <- !options[["manifestInterceptFixedToZero"]]
  lavopts[["int.lv.free"]]     <- !options[["latentInterceptFixedToZero"]]
  lavopts[["fixed.x"]]         <- options[["exogenousCovariateFixed"]]
  lavopts[["orthogonal"]]      <- options[["orthogonal"]]
  lavopts[["std.lv"]]          <- options[["factorScaling"]] == "factorVariance"
  lavopts[["effect.coding"]]   <- options[["factorScaling"]] == "effectCoding"
  lavopts[["auto.fix.first"]]  <- options[["factorScaling"]] == "factorLoading"
  lavopts[["auto.fix.single"]] <- options[["residualSingleIndicatorOmitted"]]
  lavopts[["auto.var"]]        <- options[["residualVariance"]]
  lavopts[["auto.cov.lv.x"]]   <- options[["exogenousLatentCorrelation"]]
  lavopts[["auto.cov.y"]]      <- options[["dependentCorrelation"]]
  lavopts[["auto.th"]]         <- options[["threshold"]]
  lavopts[["auto.delta"]]      <- options[["scalingParameter"]]
  lavopts[["auto.efa"]]        <- options[["efaConstrained"]]

  # data options
  lavopts[["std.ov"]]  <- options[["standardizedVariable"]]
  lavopts[["missing"]] <- ifelse(options[["naAction"]] == "fiml", "ml",
                                 ifelse(options[["naAction"]] == "twoStage", "two.stage",
                                        ifelse(options[["naAction"]] == "twoStageRobust", "robust.two.stage",
                                               ifelse(options[["naAction"]] == "doublyRobust", "doubly.robust",
                                                      options[["naAction"]]))))

  # estimation options
  lavopts[["estimator"]]   <- options[["estimator"]]
  lavopts[["se"]]          <- ifelse(options[["errorCalculationMethod"]] == "bootstrap", "standard", options[["errorCalculationMethod"]])
  lavopts[["information"]] <- options[["informationMatrix"]]
  lavopts[["test"]]        <- ifelse(options[["modelTest"]] == "satorraBentler", "Satorra.Bentler",
                                     ifelse(options[["modelTest"]] == "yuanBentler", "Yuan.Bentler",
                                            ifelse(options[["modelTest"]] == "meanAndVarianceAdjusted", "mean.var.adjusted",
                                                   ifelse(options[["modelTest"]] == "scaledAndShifted", "scaled.shifted",
                                                          ifelse(options[["modelTest"]] == "bollenStine", "Bollen.Stine",
                                                                 options[["modelTest"]])))))

  # group.equal options
  equality_constraints <- c(
    options[["equalLoading"]],
    options[["equalIntercept"]],
    options[["equalMean"]],
    options[["equalThreshold"]],
    options[["equalRegression"]],
    options[["equalResidual"]],
    options[["equalResidualCovariance"]],
    options[["equalVariance"]],
    options[["equalLatentCovariance"]]
  )

  if (any(equality_constraints)) {
    lavopts[["group.equal"]] <- c("loadings", "intercepts", "means", "thresholds", "regressions", "residuals",
                                  "residual.covariances", "lv.variances", "lv.covariances")[equality_constraints]
  }

  if (options[["freeParameters"]][1] != ""){
    splitted <- strsplit(options[["freeParameters"]][["model"]], "[\\n,;]+", perl = TRUE)[[1]]
    lavopts[["group.partial"]] <-  splitted
  }

  # group variable
  if (options[["group"]] != "") {
    lavopts[["group"]] <- .v(options[["group"]])
  }

  # sampling weights
  if (options[["samplingWeights"]] != "") {
    lavopts[["sampling.weights"]] <- .v(options[["samplingWeights"]])
  }


  return(lavopts)
}

.semTranslateModel <- function(syntax, dataset) {
  #' translate model syntax to jasp column names syntax
  usedvars <- .semGetUsedVars(syntax, colnames(dataset))

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

.semFitTab <- function(jaspResults, modelContainer, dataset, options, ready) {
  if (!is.null(modelContainer[["fittab"]])) return()

  fittab <- createJaspTable(title = gettext("Model fit"))
  fittab$dependOn("models")
  fittab$position <- 0

  fittab$addColumnInfo(name = "Model",    title = "",                            type = "string" )
  fittab$addColumnInfo(name = "AIC",      title = gettext("AIC"),                type = "number" )
  fittab$addColumnInfo(name = "BIC",      title = gettext("BIC"),                type = "number" )
  fittab$addColumnInfo(name = "N",        title = gettext("n"),                  type = "integer")
  fittab$addColumnInfo(name = "Chisq",    title = gettext("&#967;&sup2;"),       type = "number" ,
                       overtitle = gettext("Baseline test"))
  fittab$addColumnInfo(name = "Df",       title = gettext("df"),                 type = "integer",
                       overtitle = gettext("Baseline test"))
  fittab$addColumnInfo(name = "PrChisq",  title = gettext("p"),                  type = "pvalue",
                       overtitle = gettext("Baseline test"))
  fittab$addColumnInfo(name = "dchisq",   title = gettext("&#916;&#967;&sup2;"), type = "number" ,
                       overtitle = gettext("Difference test"))
  fittab$addColumnInfo(name = "ddf",      title = gettext("&#916;df"),           type = "integer",
                       overtitle = gettext("Difference test"))
  fittab$addColumnInfo(name = "dPrChisq", title = gettext("p"),                  type = "pvalue" ,
                       overtitle = gettext("Difference test"))

  modelContainer[["fittab"]] <- fittab

  if (!ready) return()

  # add data to the table!
  semResults <- .semComputeResults(modelContainer, dataset, options)

  if (modelContainer$getError()) return()

  if (length(semResults) == 1) {
    lrt <- .withWarnings(lavaan::lavTestLRT(semResults[[1]])[-1, ])
    rownames(lrt$value) <- options[["models"]][[1]][["name"]]
    Ns <- lavaan::lavInspect(semResults[[1]], "ntotal")
  } else {
    Ns <- vapply(semResults, lavaan::lavInspect, 0, what = "ntotal")
    lrt_args <- semResults
    names(lrt_args) <- "object" # (the first result is object, the others ...)
    lrt_args[["model.names"]] <- vapply(options[["models"]], getElement, name = "name", "")
    lrt <- .withWarnings(do.call(lavaan::lavTestLRT, lrt_args))
    lrt$value[1,5:7] <- NA
  }

  fittab[["Model"]]    <- rownames(lrt$value)
  fittab[["AIC"]]      <- lrt$value[["AIC"]]
  fittab[["BIC"]]      <- lrt$value[["BIC"]]
  fittab[["N"]]        <- Ns
  fittab[["Chisq"]]    <- lrt$value[["Chisq"]]
  fittab[["Df"]]       <- lrt$value[["Df"]]
  fittab[["PrChisq"]]  <- pchisq(q = lrt$value[["Chisq"]], df = lrt$value[["Df"]], lower.tail = FALSE)
  fittab[["dchisq"]]   <- lrt$value[["Chisq diff"]]
  fittab[["ddf"]]      <- lrt$value[["Df diff"]]
  fittab[["dPrChisq"]] <- lrt$value[["Pr(>Chisq)"]]

  # add warning footnote
  if (!is.null(lrt$warnings)) {
    fittab$addFootnote(gsub("lavaan WARNING: ", "", lrt$warnings[[1]]$message))
  }

  if(options$naAction == "listwise"){
    nrm <- nrow(dataset) - lavaan::lavInspect(semResults[[1]], "ntotal")
    if (nrm != 0) {
      missingFootnote <- gettextf("A total of %g cases were removed due to missing values. You can avoid this by choosing 'FIML' under 'Missing Data Handling' in the Estimation options.",
                                  nrm)
      fittab$addFootnote(message = missingFootnote)
    }
  }

  # add test statistic correction footnote
  test <- lavaan::lavInspect(semResults[[1]], "options")[["test"]]

  if (test != "standard") {
    LUT <- tibble::tribble(
      ~option,              ~name,
      "Satorra.Bentler",    gettext("Satorra-Bentler scaled test-statistic"),
      "Yuan.Bentler",       gettext("Yuan-Bentler scaled test-statistic"),
      "Yuan.Bentler.Mplus", gettext("Yuan-Bentler (Mplus) scaled test-statistic"),
      "mean.var.adjusted",  gettext("mean and variance adjusted test-statistic"),
      "Satterthwaite",      gettext("mean and variance adjusted test-statistic"),
      "scaled.shifted",     gettext("scaled and shifted test-statistic"),
      "Bollen.Stine",       gettext("bootstrap (Bollen-Stine) probability value"),
      "bootstrap",          gettext("bootstrap (Bollen-Stine) probability value"),
      "boot",               gettext("bootstrap (Bollen-Stine) probability value")
    )
    testname <- LUT[test == tolower(LUT$option), "name"][[1]]
    ftext <- gettextf("Model tests based on %s.", testname)
    fittab$addFootnote(message = ftext)
  }

  if (options$estimator %in% c("dwls", "gls", "wls", "uls")) {
    fittab$addFootnote(message = gettext("The AIC, BIC and additional information criteria are only available with ML-type estimators"))
  }
}

.semParameters <- function(modelContainer, dataset, options, ready) {
  if (!is.null(modelContainer[["params"]])) return()


  params <- createJaspContainer(gettext("Parameter estimates"))
  params$position <- 1
  params$dependOn(c("ciLevel", "bootstrapCiType", "standardizedEstimate", "models"))

  modelContainer[["params"]] <- params

  if (length(options[["models"]]) < 2) {
    .semParameterTables(modelContainer[["results"]][["object"]][[1]], NULL, params, options, ready)
  } else {

    for (i in seq_along(options[["models"]])) {
      fit <- modelContainer[["results"]][["object"]][[i]]
      modelname <- options[["models"]][[i]][["name"]]
      .semParameterTables(fit, modelname, params, options, ready)
    }
  }
}

.semParameterTables <- function(fit, modelname, parentContainer, options, ready) {
  if (is.null(modelname)) {
    pecont <- parentContainer
  } else {
    pecont <- createJaspContainer(modelname, initCollapsed = TRUE)
  }


  # Measurement model
  indtab <- createJaspTable(title = gettext("Factor Loadings"))

  if (options[["group"]] != "")
    indtab$addColumnInfo(name = "group",  title = gettext("Group"),      type = "string", combine = TRUE)

  indtab$addColumnInfo(name = "lhs",      title = gettext("Latent"),     type = "string", combine = TRUE)
  indtab$addColumnInfo(name = "rhs",      title = gettext("Indicator"),  type = "string")
  indtab$addColumnInfo(name = "label",    title = "",                    type = "string")
  indtab$addColumnInfo(name = "est",      title = gettext("Estimate"),   type = "number")
  indtab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number")
  indtab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number")
  indtab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "pvalue")
  indtab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
  indtab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))

  if (options[["standardizedEstimate"]]) {
    indtab$addColumnInfo(name = "std.all", title = gettext("All"),  type = "number",
                         overtitle = gettext("Standardized"))
    indtab$addColumnInfo(name = "std.lv",  title = gettext("LV"),   type = "number",
                         overtitle = gettext("Standardized"))
    indtab$addColumnInfo(name = "std.nox", title = gettext("Endo"), type = "number",
                         overtitle = gettext("Standardized"))
  }

  pecont[["ind"]] <- indtab

  # Structural Model
  regtab <- createJaspTable(title = gettext("Regression coefficients"))

  if (options[["group"]] != "")
    regtab$addColumnInfo(name = "group",  title = gettext("Group"),      type = "string", combine = TRUE)

  regtab$addColumnInfo(name = "rhs",      title = gettext("Predictor"),  type = "string", combine = TRUE)
  regtab$addColumnInfo(name = "lhs",      title = gettext("Outcome"),    type = "string")
  regtab$addColumnInfo(name = "label",    title = "",                    type = "string")
  regtab$addColumnInfo(name = "est",      title = gettext("Estimate"),   type = "number")
  regtab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number")
  regtab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number")
  regtab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "pvalue")
  regtab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
  regtab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))

  if (options[["standardizedEstimate"]]) {
    regtab$addColumnInfo(name = "std.all", title = gettext("All"),  type = "number",
                         overtitle = gettext("Standardized"))
    regtab$addColumnInfo(name = "std.lv",  title = gettext("LV"),   type = "number",
                         overtitle = gettext("Standardized"))
    regtab$addColumnInfo(name = "std.nox", title = gettext("Endo"), type = "number",
                         overtitle = gettext("Standardized"))
  }

  pecont[["reg"]] <- regtab

  # Latent variances
  lvartab <- createJaspTable(title = gettext("Factor variances"))

  if (options[["group"]] != "")
    lvartab$addColumnInfo(name = "group",  title = gettext("Group"),      type = "string", combine = TRUE)

  lvartab$addColumnInfo(name = "lhs",      title = gettext("Variable"),   type = "string")
  lvartab$addColumnInfo(name = "label",    title = "",                    type = "string")
  lvartab$addColumnInfo(name = "est",      title = gettext("Estimate"),   type = "number")
  lvartab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number")
  lvartab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number")
  lvartab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "pvalue")
  lvartab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number",
                        overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
  lvartab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                        overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))

  if (options[["standardizedEstimate"]]) {
    lvartab$addColumnInfo(name = "std.all", title = gettext("All"),  type = "number",
                          overtitle = gettext("Standardized"))
    lvartab$addColumnInfo(name = "std.lv",  title = gettext("LV"),   type = "number",
                          overtitle = gettext("Standardized"))
    lvartab$addColumnInfo(name = "std.nox", title = gettext("Endo"), type = "number",
                          overtitle = gettext("Standardized"))
  }

  pecont[["lvar"]] <- lvartab

  # Latent covariances
  lcovtab <- createJaspTable(title = gettext("Factor covariances"))

  if (options[["group"]] != "")
    lcovtab$addColumnInfo(name = "group",  title = gettext("Group"),      type = "string", combine = TRUE)

  lcovtab$addColumnInfo(name = "lhs",      title = gettext("Variables"),   type = "string")
  lcovtab$addColumnInfo(name = "label",    title = "",                    type = "string")
  lcovtab$addColumnInfo(name = "est",      title = gettext("Estimate"),   type = "number")
  lcovtab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number")
  lcovtab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number")
  lcovtab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "pvalue")
  lcovtab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number",
                        overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
  lcovtab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                        overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))

  if (options[["standardizedEstimate"]]) {
    lcovtab$addColumnInfo(name = "std.all", title = gettext("All"),  type = "number",
                          overtitle = gettext("Standardized"))
    lcovtab$addColumnInfo(name = "std.lv",  title = gettext("LV"),   type = "number",
                          overtitle = gettext("Standardized"))
    lcovtab$addColumnInfo(name = "std.nox", title = gettext("Endo"), type = "number",
                          overtitle = gettext("Standardized"))
  }

  pecont[["lcov"]] <- lcovtab

  # Residual variances
  vartab <- createJaspTable(title = gettext("Residual variances"))

  if (options[["group"]] != "")
    vartab$addColumnInfo(name = "group",  title = gettext("Group"),      type = "string", combine = TRUE)

  vartab$addColumnInfo(name = "lhs",      title = gettext("Variable"),   type = "string")
  vartab$addColumnInfo(name = "label",    title = "",                    type = "string")
  vartab$addColumnInfo(name = "est",      title = gettext("Estimate"),   type = "number")
  vartab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number")
  vartab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number")
  vartab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "pvalue")
  vartab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
  vartab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))

  if (options[["standardizedEstimate"]]) {
    vartab$addColumnInfo(name = "std.all", title = gettext("All"),  type = "number",
                         overtitle = gettext("Standardized"))
    vartab$addColumnInfo(name = "std.lv",  title = gettext("LV"),   type = "number",
                         overtitle = gettext("Standardized"))
    vartab$addColumnInfo(name = "std.nox", title = gettext("Endo"), type = "number",
                         overtitle = gettext("Standardized"))
  }

  pecont[["var"]] <- vartab

  # Residual covariances
  covtab <- createJaspTable(title = gettext("Residual covariances"))

  if (options[["group"]] != "")
    covtab$addColumnInfo(name = "group",  title = gettext("Group"),      type = "string", combine = TRUE)

  covtab$addColumnInfo(name = "lhs",      title = gettext("Variables"),   type = "string")
  covtab$addColumnInfo(name = "label",    title = "",                    type = "string")
  covtab$addColumnInfo(name = "est",      title = gettext("Estimate"),   type = "number")
  covtab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number")
  covtab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number")
  covtab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "pvalue")
  covtab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
  covtab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))

  if (options[["standardizedEstimate"]]) {
    covtab$addColumnInfo(name = "std.all", title = gettext("All"),  type = "number",
                         overtitle = gettext("Standardized"))
    covtab$addColumnInfo(name = "std.lv",  title = gettext("LV"),   type = "number",
                         overtitle = gettext("Standardized"))
    covtab$addColumnInfo(name = "std.nox", title = gettext("Endo"), type = "number",
                         overtitle = gettext("Standardized"))
  }

  pecont[["cov"]] <- covtab

  # Means
  if (options[["meanStructure"]]) {
    mutab <- createJaspTable(title = gettext("Means"))

    if (options[["group"]] != "")
      mutab$addColumnInfo(name = "group",  title = gettext("Group"),      type = "string", combine = TRUE)

    mutab$addColumnInfo(name = "lhs",      title = gettext("Variable"),   type = "string")
    mutab$addColumnInfo(name = "label",    title = "",                    type = "string")
    mutab$addColumnInfo(name = "est",      title = gettext("Estimate"),   type = "number")
    mutab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number")
    mutab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number")
    mutab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "pvalue")
    mutab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number",
                        overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
    mutab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                        overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))

    if (options[["standardizedEstimate"]]) {
      mutab$addColumnInfo(name = "std.all", title = gettext("All"),  type = "number",
                          overtitle = gettext("Standardized"))
      mutab$addColumnInfo(name = "std.lv",  title = gettext("LV"),   type = "number",
                          overtitle = gettext("Standardized"))
      mutab$addColumnInfo(name = "std.nox", title = gettext("Endo"), type = "number",
                          overtitle = gettext("Standardized"))
    }

    pecont[["mu"]] <- mutab
  }

  deftab <- createJaspTable(title = gettext("Defined parameters"))

  deftab$addColumnInfo(name = "lhs",      title = gettext("Name"),       type = "string")
  deftab$addColumnInfo(name = "est",      title = gettext("Estimate"),   type = "number")
  deftab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number")
  deftab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number")
  deftab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "pvalue")
  deftab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number",
                      overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
  deftab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                      overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))

  if (options[["standardizedEstimate"]]) {
    deftab$addColumnInfo(name = "std.all", title = gettext("All"),  type = "number",
                         overtitle = gettext("Standardized"))
    deftab$addColumnInfo(name = "std.lv",  title = gettext("LV"),   type = "number",
                         overtitle = gettext("Standardized"))
    deftab$addColumnInfo(name = "std.nox", title = gettext("Endo"), type = "number",
                         overtitle = gettext("Standardized"))
  }

  pecont[["def"]] <- deftab

  if (!is.null(modelname)) parentContainer[[modelname]] <- pecont

  if (!ready || !inherits(fit, "lavaan")) return()

  # fill tables with values
  lvnames <- lavaan::lavNames(fit, "lv")
  ovnames <- lavaan::lavNames(fit, "ov")

  bootstrapCiType <- ifelse(options[["bootstrapCiType"]] == "percentileBiasCorrected", "bca.simple",
                            ifelse(options[["bootstrapCiType"]] == "percentile", "perc",
                                   "norm"))

  pe <- lavaan::parameterestimates(fit, standardized = TRUE, level = options[["ciLevel"]],
                                   boot.ci.type = bootstrapCiType)
  pe <- lavaan::lavMatrixRepresentation(lavaan::lav_partable_complete(pe))

  if (options[["group"]] != "")  {
    pe[pe[["op"]] != ":=", "groupname"] <- lavaan::lavInspect(fit, "group.label")[pe[["group"]]]
  } else {
    pe[["group"]] <- 0
  }

  # Measurement model
  pe_ind <- pe[pe$op == "=~",]
  pe_ind <- pe_ind[order(pe_ind[["group"]], pe_ind[["lhs"]]),]
  if (nrow(pe_ind) == 0) pecont[["ind"]] <- NULL # remove if no estimates

  if (options[["group"]] != "")
    indtab[["group"]] <- pe_ind[["groupname"]]

  indtab[["rhs"]]      <- .unv(pe_ind[["rhs"]])
  indtab[["lhs"]]      <- .unv(pe_ind[["lhs"]])
  indtab[["label"]]    <- pe_ind[["label"]]
  indtab[["est"]]      <- pe_ind[["est"]]
  indtab[["se"]]       <- pe_ind[["se"]]
  indtab[["z"]]        <- pe_ind[["z"]]
  indtab[["pvalue"]]   <- pe_ind[["pvalue"]]
  indtab[["ci.lower"]] <- pe_ind[["ci.lower"]]
  indtab[["ci.upper"]] <- pe_ind[["ci.upper"]]

  if (options[["standardizedEstimate"]]) {
    indtab[["std.all"]] <- pe_ind[["std.all"]]
    indtab[["std.lv"]]  <- pe_ind[["std.lv"]]
    indtab[["std.nox"]] <- pe_ind[["std.nox"]]
  }

  # Structural model
  pe_reg <- pe[pe$op == "~",]
  pe_reg <- pe_reg[order(pe_reg[["group"]], pe_reg[["lhs"]]),]
  if (nrow(pe_reg) == 0) pecont[["reg"]] <- NULL # remove if no estimates

  if (options[["group"]] != "")
    regtab[["group"]] <- pe_reg[["groupname"]]

  regtab[["rhs"]]      <- .unv(pe_reg[["rhs"]])
  regtab[["lhs"]]      <- .unv(pe_reg[["lhs"]])
  regtab[["label"]]    <- pe_reg[["label"]]
  regtab[["est"]]      <- pe_reg[["est"]]
  regtab[["se"]]       <- pe_reg[["se"]]
  regtab[["z"]]        <- pe_reg[["z"]]
  regtab[["pvalue"]]   <- pe_reg[["pvalue"]]
  regtab[["ci.lower"]] <- pe_reg[["ci.lower"]]
  regtab[["ci.upper"]] <- pe_reg[["ci.upper"]]

  if (options[["standardizedEstimate"]]) {
    regtab[["std.all"]] <- pe_reg[["std.all"]]
    regtab[["std.lv"]]  <- pe_reg[["std.lv"]]
    regtab[["std.nox"]] <- pe_reg[["std.nox"]]
  }

  # Latent variances
  pe_lvar <- pe[pe$op == "~~" & pe$lhs %in% lvnames & pe$lhs == pe$rhs,]
  if (nrow(pe_lvar) == 0) pecont[["lvar"]] <- NULL # remove if no estimates

  if (options[["group"]] != "")
    lvartab[["group"]] <- pe_lvar[["groupname"]]

  lvartab[["rhs"]]      <- .unv(pe_lvar[["rhs"]])
  lvartab[["lhs"]]      <- .unv(pe_lvar[["lhs"]])
  lvartab[["label"]]    <- pe_lvar[["label"]]
  lvartab[["est"]]      <- pe_lvar[["est"]]
  lvartab[["se"]]       <- pe_lvar[["se"]]
  lvartab[["z"]]        <- pe_lvar[["z"]]
  lvartab[["pvalue"]]   <- pe_lvar[["pvalue"]]
  lvartab[["ci.lower"]] <- pe_lvar[["ci.lower"]]
  lvartab[["ci.upper"]] <- pe_lvar[["ci.upper"]]

  if (options[["standardizedEstimate"]]) {
    lvartab[["std.all"]] <- pe_lvar[["std.all"]]
    lvartab[["std.lv"]]  <- pe_lvar[["std.lv"]]
    lvartab[["std.nox"]] <- pe_lvar[["std.nox"]]
  }

  # Latent covariances
  pe_lcov <- pe[pe$op == "~~" & pe$lhs %in% lvnames & pe$rhs %in% lvnames & pe$lhs != pe$rhs,]
  if (nrow(pe_lcov) == 0) pecont[["lcov"]] <- NULL # remove if no estimates

  if (options[["group"]] != "")
    lcovtab[["group"]] <- pe_lcov[["groupname"]]

  lcovtab[["lhs"]]      <- paste(.unv(pe_lcov[["lhs"]]), "-", .unv(pe_lcov[["rhs"]]))
  lcovtab[["label"]]    <- pe_lcov[["label"]]
  lcovtab[["est"]]      <- pe_lcov[["est"]]
  lcovtab[["se"]]       <- pe_lcov[["se"]]
  lcovtab[["z"]]        <- pe_lcov[["z"]]
  lcovtab[["pvalue"]]   <- pe_lcov[["pvalue"]]
  lcovtab[["ci.lower"]] <- pe_lcov[["ci.lower"]]
  lcovtab[["ci.upper"]] <- pe_lcov[["ci.upper"]]

  if (options[["standardizedEstimate"]]) {
    lcovtab[["std.all"]] <- pe_lcov[["std.all"]]
    lcovtab[["std.lv"]]  <- pe_lcov[["std.lv"]]
    lcovtab[["std.nox"]] <- pe_lcov[["std.nox"]]
  }

  # Residual variances
  pe_var <- pe[pe$op == "~~" & pe$lhs %in% ovnames & pe$lhs == pe$rhs,]
  if (nrow(pe_var) == 0) pecont[["var"]] <- NULL # remove if no estimates

  if (options[["group"]] != "")
    vartab[["group"]] <- pe_var[["groupname"]]

  vartab[["rhs"]]      <- .unv(pe_var[["rhs"]])
  vartab[["lhs"]]      <- .unv(pe_var[["lhs"]])
  vartab[["label"]]    <- pe_var[["label"]]
  vartab[["est"]]      <- pe_var[["est"]]
  vartab[["se"]]       <- pe_var[["se"]]
  vartab[["z"]]        <- pe_var[["z"]]
  vartab[["pvalue"]]   <- pe_var[["pvalue"]]
  vartab[["ci.lower"]] <- pe_var[["ci.lower"]]
  vartab[["ci.upper"]] <- pe_var[["ci.upper"]]

  if (options[["standardizedEstimate"]]) {
    vartab[["std.all"]] <- pe_var[["std.all"]]
    vartab[["std.lv"]]  <- pe_var[["std.lv"]]
    vartab[["std.nox"]] <- pe_var[["std.nox"]]
  }

  # Residual covariances
  pe_cov <- pe[pe$op == "~~" & pe$lhs %in% ovnames & pe$rhs %in% ovnames & pe$lhs != pe$rhs,]
  if (nrow(pe_cov) == 0) pecont[["cov"]] <- NULL # remove if no estimates

  if (options[["group"]] != "")
    covtab[["group"]] <- pe_cov[["groupname"]]

  covtab[["lhs"]]      <- paste(.unv(pe_cov[["lhs"]]), "-", .unv(pe_cov[["rhs"]]))
  covtab[["label"]]    <- pe_cov[["label"]]
  covtab[["est"]]      <- pe_cov[["est"]]
  covtab[["se"]]       <- pe_cov[["se"]]
  covtab[["z"]]        <- pe_cov[["z"]]
  covtab[["pvalue"]]   <- pe_cov[["pvalue"]]
  covtab[["ci.lower"]] <- pe_cov[["ci.lower"]]
  covtab[["ci.upper"]] <- pe_cov[["ci.upper"]]

  if (options[["standardizedEstimate"]]) {
    covtab[["std.all"]] <- pe_cov[["std.all"]]
    covtab[["std.lv"]]  <- pe_cov[["std.lv"]]
    covtab[["std.nox"]] <- pe_cov[["std.nox"]]
  }


  # Means
  if (options[["meanStructure"]]) {
    pe_mu <- pe[pe$op == "~1",]

    if (options[["group"]] != "")
      mutab[["group"]] <- pe_mu[["groupname"]]

    mutab[["lhs"]] <- .unv(pe_mu[["lhs"]])
    mutab[["label"]]    <- pe_mu[["label"]]
    mutab[["est"]]      <- pe_mu[["est"]]
    mutab[["se"]]       <- pe_mu[["se"]]
    mutab[["z"]]        <- pe_mu[["z"]]
    mutab[["pvalue"]]   <- pe_mu[["pvalue"]]
    mutab[["ci.lower"]] <- pe_mu[["ci.lower"]]
    mutab[["ci.upper"]] <- pe_mu[["ci.upper"]]

    if (options[["standardizedEstimate"]]) {
      mutab[["std.all"]] <- pe_mu[["std.all"]]
      mutab[["std.lv"]]  <- pe_mu[["std.lv"]]
      mutab[["std.nox"]] <- pe_mu[["std.nox"]]
    }
  }

  # defined parameters
  pe_def <- pe[pe$op == ":=",]
  if (nrow(pe_def) == 0) pecont[["def"]] <- NULL # remove if no estimates

  deftab[["lhs"]]      <- pe_def[["lhs"]]
  deftab[["est"]]      <- pe_def[["est"]]
  deftab[["se"]]       <- pe_def[["se"]]
  deftab[["z"]]        <- pe_def[["z"]]
  deftab[["pvalue"]]   <- pe_def[["pvalue"]]
  deftab[["ci.lower"]] <- pe_def[["ci.lower"]]
  deftab[["ci.upper"]] <- pe_def[["ci.upper"]]

  if (options[["standardizedEstimate"]]) {
    deftab[["std.all"]] <- pe_def[["std.all"]]
    deftab[["std.lv"]]  <- pe_def[["std.lv"]]
    deftab[["std.nox"]] <- pe_def[["std.nox"]]
  }

}

.semAdditionalFits <- function(modelContainer, dataset, options, ready) {
  if (!options[["additionalFitMeasures"]] || !is.null(modelContainer[["addfit"]])) return()

  fitms <- createJaspContainer(gettext("Additional fit measures"))
  fitms$dependOn(c("additionalFitMeasures", "models"))
  fitms$position <- 0.5

  # Fit indices
  fitms[["indices"]] <- fitin <- createJaspTable(gettext("Fit indices"))
  fitin$addColumnInfo(name = "index", title = gettext("Index"), type = "string")
  if (length(options[["models"]]) < 2) {
    fitin$addColumnInfo(name = "value", title = gettext("Value"), type = "number")
  } else {
    for (i in seq_along(options[["models"]])) {
      fitin$addColumnInfo(name = paste0("value_", i), title = options[["models"]][[i]][["name"]], type = "number")
    }
  }
  fitin$setExpectedSize(rows = 1, cols = 2)

  # information criteria
  if (!options$estimator %in% c("dwls", "gls", "wls", "uls")) {
    fitms[["incrits"]] <- fitic <- createJaspTable(gettext("Information criteria"))
    fitic$addColumnInfo(name = "index", title = "", type = "string")
    if (length(options[["models"]]) < 2) {
      fitic$addColumnInfo(name = "value", title = gettext("Value"), type = "number")
    } else {
      for (i in seq_along(options[["models"]])) {
        fitic$addColumnInfo(name = paste0("value_", i), title = options[["models"]][[i]][["name"]], type = "number")
      }
    }
    fitic$setExpectedSize(rows = 1, cols = 2)
  }



  # other fit measures
  fitms[["others"]] <- fitot <- createJaspTable(gettext("Other fit measures"))
  fitot$addColumnInfo(name = "index", title = gettext("Metric"), type = "string")
  if (length(options[["models"]]) < 2) {
    fitot$addColumnInfo(name = "value", title = gettext("Value"), type = "number")
  } else {
    for (i in seq_along(options[["models"]])) {
      fitot$addColumnInfo(name = paste0("value_", i), title = options[["models"]][[i]][["name"]], type = "number")
    }
  }
  fitot$setExpectedSize(rows = 1, cols = 2)

  modelContainer[["addfit"]] <- fitms

  if (!ready || modelContainer$getError()) return()

  # actually compute the fit measures
  fmli <- lapply(modelContainer[["results"]][["object"]], lavaan::fitmeasures)

  # Fit indices
  fitin[["index"]] <- c(
    gettext("Comparative Fit Index (CFI)"),
    gettext("Tucker-Lewis Index (TLI)"),
    gettext("Bentler-Bonett Non-normed Fit Index (NNFI)"),
    gettext("Bentler-Bonett Normed Fit Index (NFI)"),
    gettext("Parsimony Normed Fit Index (PNFI)"),
    gettext("Bollen's Relative Fit Index (RFI)"),
    gettext("Bollen's Incremental Fit Index (IFI)"),
    gettext("Relative Noncentrality Index (RNI)")
  )
  if (length(options[["models"]]) == 1) {
    fitin[["value"]] <- fmli[[1]][c("cfi", "tli", "nnfi", "nfi", "pnfi", "rfi", "ifi", "rni")]
  } else {
    for (i in seq_along(options[["models"]])) {
      fitin[[paste0("value_", i)]] <- fmli[[i]][c("cfi", "tli", "nnfi", "nfi", "pnfi", "rfi", "ifi", "rni")]
    }
  }

  # information criteria
  if (!options$estimator %in% c("dwls", "gls", "wls", "uls")) {
    fitic[["index"]] <- c(
      gettext("Log-likelihood"),
      gettext("Number of free parameters"),
      gettext("Akaike (AIC)"),
      gettext("Bayesian (BIC)"),
      gettext("Sample-size adjusted Bayesian (SSABIC)")
    )

    if (length(options[["models"]]) == 1) {
      fitic[["value"]] <- fmli[[1]][c("logl", "npar", "aic", "bic", "bic2")]
    } else {
      for (i in seq_along(options[["models"]])) {
        fitic[[paste0("value_", i)]] <- fmli[[i]][c("logl", "npar", "aic", "bic", "bic2")]
      }
    }
  }


  # other fitmeasures
  fitot[["index"]] <- c(
    gettext("Root mean square error of approximation (RMSEA)"),
    gettextf("RMSEA 90%% CI lower bound"),
    gettextf("RMSEA 90%% CI upper bound"),
    gettext("RMSEA p-value"),
    gettext("Standardized root mean square residual (SRMR)"),
    gettextf("Hoelter's critical N (%s = .05)","\u03B1"),
    gettextf("Hoelter's critical N (%s = .01)","\u03B1"),
    gettext("Goodness of fit index (GFI)"),
    gettext("McDonald fit index (MFI)"),
    gettext("Expected cross validation index (ECVI)")
  )
  if (length(options[["models"]]) == 1) {
    fitot[["value"]] <- fmli[[1]][c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue",
                                    "srmr", "cn_05", "cn_01", "gfi", "mfi", "ecvi")]
  } else {
    for (i in seq_along(options[["models"]])) {
      fitot[[paste0("value_", i)]] <- fmli[[i]][c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue",
                                                  "srmr", "cn_05", "cn_01", "gfi", "mfi", "ecvi")]
    }
  }
}

.semRsquared <- function(modelContainer, dataset, options, ready) {
  if (!options[["rSquared"]] || !is.null(modelContainer[["rsquared"]])) return()

  # init table
  tabr2 <- createJaspTable(gettext("R-Squared"))
  if (options[["group"]] != "")
    tabr2$addColumnInfo(name = "__grp__", title = "", type = "string", combine = TRUE)
  tabr2$addColumnInfo(name = "__var__", title = "", type = "string")
  if (length(options[["models"]]) < 2) {
    tabr2$addColumnInfo(name = "rsq", title = "R\u00B2", type = "number")
  } else {
    for (i in seq_along(options[["models"]])) {
      tabr2$addColumnInfo(name = paste0("rsq_", i), title = options[["models"]][[i]][["name"]],
                          overtitle = "R\u00B2", type = "number")
    }
  }

  tabr2$dependOn(c("rSquared", "models"))
  tabr2$position <- .75

  modelContainer[["rsquared"]] <- tabr2

  if (!ready || modelContainer$getError()) return()

  # compute data and fill table
  if (options[["group"]] == "") {

    if (length(options[["models"]]) < 2) {

      r2res              <- lavaan::inspect(modelContainer[["results"]][["object"]][[1]], "r2")
      tabr2[["__var__"]] <- .unv(names(r2res))
      tabr2[["rsq"]]     <- r2res

    } else {

      # determine variable names
      r2li <- lapply(modelContainer[["results"]][["object"]], lavaan::inspect, what = "r2")

      # generate df with these names
      r2df <- data.frame("varname__" = unique(unlist(lapply(r2li, names))))
      tabr2[["__var__"]] <- .unv(unique(unlist(lapply(r2li, names))))

      for (i in 1:length(r2li)) {
        # fill matching vars from model with df
        r2df[match(names(r2li[[i]]), r2df[["varname__"]]), i + 1] <- r2li[[i]]
        # add column to table
        tabr2[[paste0("rsq_", i)]] <- r2df[[i + 1]]
      }

    }

  } else {

    if (length(options[["models"]]) < 2) {

      r2res              <- lavaan::inspect(modelContainer[["results"]][["object"]][[1]], "r2")
      tabr2[["__grp__"]] <- rep(names(r2res), vapply(r2res, length, 0))
      tabr2[["__var__"]] <- .unv(unlist(lapply(r2res, names)))
      tabr2[["rsq"]]     <- unlist(r2res)

    } else {

      # here is the most difficult case with multiple groups and multiple models
      # create a list with r2 results per model. each element is a list with ngroup elements
      r2li <- lapply(modelContainer[["results"]][["object"]], lavaan::inspect, what = "r2")

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


      for (mod_idx in seq_along(r2li)) {
        for (grpname in names(r2li[[1]])) {
          # find correct rows in r2df for each model and group in r2li
          grp_idx <- which(r2df[["grpname__"]] == grpname)
          # complex code because varnames in r2res can be in different order
          row_idx <- grp_idx[match(names(r2li[[mod_idx]][[grpname]]), r2df[grp_idx, "varname__"])]
          # fill r2df with r2 results
          r2df[row_idx, mod_idx + 2] <- r2li[[mod_idx]][[grpname]]
        }
      }

      # fill jasp table with data
      tabr2[["__grp__"]] <- r2df[["grpname__"]]
      tabr2[["__var__"]] <- .unv(r2df[["varname__"]])
      for (i in seq_along(r2li)) tabr2[[paste0("rsq_", i)]] <- r2df[[i + 2]]

    }

  }
}

.semMardiasCoefficient <- function(modelContainer, dataset, options, ready) {
  if (!options[["mardiasCoefficient"]] || !is.null(modelContainer[["semMardiasTable"]])) return()

  mardiatab <- createJaspTable(title = gettext("Mardia's coefficients"))
  mardiatab$position <- .2

  mardiatab$addColumnInfo(name = "Type",        title = "",                      type = "string")
  mardiatab$addColumnInfo(name = "Coefficient", title = gettext("Coefficient"),  type = "number")
  mardiatab$addColumnInfo(name = "z",           title = gettext("z"),            type = "number")
  mardiatab$addColumnInfo(name = "Chisq",       title = gettext("&#967;&sup2;"), type = "number")
  mardiatab$addColumnInfo(name = "DF",          title = gettext("df"),           type = "integer")
  mardiatab$addColumnInfo(name = "pvalue",      title = gettext("p"),            type = "pvalue")

  mardiatab$dependOn(c("mardiasCoefficient", "models"))
  modelContainer[["mardiasTable"]] <- mardiatab

  if (!ready || modelContainer$getError()) return()

  varNames <- unique(unlist(lapply(modelContainer[["results"]][["object"]], lavaan::lavaanNames, type = "ov")))
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

.semCov <- function(modelContainer, dataset, options, ready) {
  if (!(options[["observedCovariance"]] || options[["impliedCovariance"]] ||
        options[["residualCovariance"]] || options[["standardizedResidual"]]) || !is.null(modelContainer[["covars"]])) return()

  covars <- createJaspContainer(gettext("Covariance tables"))
  covars$position <- 3
  covars$dependOn(c("observedCovariance", "impliedCovariance", "residualCovariance",
                    "standardizedResidual", "models"))

  modelContainer[["covars"]] <- covars

  if (length(options[["models"]]) < 2) {
    .semCovTables(modelContainer[["results"]][["object"]][[1]], NULL, covars, options, ready)
  } else {

    for (i in seq_along(options[["models"]])) {
      fit <- modelContainer[["results"]][["object"]][[i]]
      modelname <- options[["models"]][[i]][["name"]]
      .semCovTables(fit, modelname, covars, options, ready)
    }
  }
}

.semCovTables <- function(fit, modelname, parentContainer, options, ready) {
  if (is.null(modelname)) {
    cocont <- parentContainer
  } else {
    cocont <- createJaspContainer(modelname, initCollapsed = TRUE)
  }

  if (options[["group"]] == "") {

    # without groups, these are tables

    if (options[["observedCovariance"]]) {
      octab <- createJaspTable("Observed covariance matrix")
      octab$dependOn("observedCovariance")
      octab$position <- 1
      cocont[["observed"]] <- octab
    }

    if (options[["impliedCovariance"]]) {
      ictab <- createJaspTable("Implied covariance matrix")
      ictab$dependOn("impliedCovariance")
      ictab$position <- 2
      cocont[["implied"]] <- ictab
    }

    if (options[["residualCovariance"]]) {
      rctab <- createJaspTable("Residual covariance matrix")
      rctab$dependOn("residualCovariance")
      rctab$position <- 3
      cocont[["residual"]] <- rctab
    }

    if (options[["standardizedResidual"]]) {
      srtab <- createJaspTable("Standardized residuals matrix")
      srtab$dependOn("standardizedResidual")
      srtab$position <- 4
      cocont[["stdres"]] <- srtab
    }

  } else {

    # with multiple groups these become containers

    if (options[["observedCovariance"]]) {
      occont <- createJaspContainer("Observed covariance matrix", initCollapsed = TRUE)
      occont$dependOn("observedCovariance")
      occont$position <- 1
      cocont[["observed"]] <- occont
    }

    if (options[["impliedCovariance"]]) {
      iccont <- createJaspContainer("Implied covariance matrix", initCollapsed = TRUE)
      iccont$dependOn("impliedCovariance")
      iccont$position <- 2
      cocont[["implied"]] <- iccont
    }

    if (options[["residualCovariance"]]) {
      rccont <- createJaspContainer("Residual covariance matrix", initCollapsed = TRUE)
      rccont$dependOn("residualCovariance")
      rccont$position <- 3
      cocont[["residual"]] <- rccont
    }

    if (options[["standardizedResidual"]]) {
      srcont <- createJaspContainer("Standardized residuals matrix", initCollapsed = TRUE)
      srcont$dependOn("standardizedResidual")
      srcont$position <- 4
      cocont[["stdres"]] <- srcont
    }
  }


  if (!ready || !inherits(fit, "lavaan")) return()


  if (options[["group"]] == "") {

    # without groups, just fill the tables

    if (options[["observedCovariance"]]) {
      # actually compute the observed covariance
      ov <- lavaan::inspect(fit, "sampstat")
      oc <- ov$cov
      oc[upper.tri(oc)] <- NA

      for (i in 1:ncol(oc)) {
        nm <- colnames(oc)[i]
        octab$addColumnInfo(nm, title = .unv(nm), type ="pvalue")
      }
      octab$addRows(oc, rowNames = colnames(oc))
    }

    if (options[["impliedCovariance"]]) {
      # actually compute the implied covariance
      fv <- lavaan::fitted.values(fit)
      ic <- fv$cov
      ic[upper.tri(ic)] <- NA

      for (i in 1:ncol(ic)) {
        nm <- colnames(ic)[i]
        ictab$addColumnInfo(nm, title = .unv(nm), type = "pvalue")
      }
      ictab$addRows(ic, rowNames = colnames(ic))
    }

    if (options[["residualCovariance"]]) {
      # actually compute the implied covariance
      rv <- lavaan::residuals(fit)
      rc <- rv$cov
      rc[upper.tri(rc)] <- NA

      for (i in 1:ncol(rc)) {
        nm <- colnames(rc)[i]
        rctab$addColumnInfo(nm, title = .unv(nm), type = "pvalue")
      }
      rctab$addRows(rc, rowNames = colnames(rc))
    }

    if (options[["standardizedResidual"]]) {
      # actually compute the implied covariance
      if (options[["errorCalculationMethod"]] == "bootstrap") {
        srtab$setError(gettext("The standardized residual covariance table is currently unavailable when the error calculation method is 'Bootstrap'"))
      } else {
        sv <- try(lavaan::residuals(fit, type = "standardized"))
        if (isTryError(sv)) {
          srtab$setError(gettext("The standardized residual covariance matrix could not be computed"))
        } else {
          sr <- sv$cov
          sr[upper.tri(sr)] <- NA

          for (i in 1:ncol(sr)) {
            nm <- colnames(sr)[i]
            srtab$addColumnInfo(nm, title = .unv(nm), type = "pvalue")
          }
          srtab$addRows(sr, rowNames = colnames(sr))
        }
      }
    }

  } else {

    # with groups, create tables and fill them

    if (options[["observedCovariance"]]) {
      # actually compute the observed covariance
      ov <- lavaan::inspect(fit, "sampstat")
      level_names <- names(ov)

      for (i in 1:length(ov)) {
        oc <- ov[[i]]$cov
        oc[upper.tri(oc)] <- NA

        occont[[level_names[i]]] <- createJaspTable(level_names[i])

        for (j in 1:ncol(oc)) {
          nm <- colnames(oc)[j]
          occont[[level_names[i]]]$addColumnInfo(nm, title = .unv(nm), type = "pvalue")
        }
        occont[[level_names[i]]]$addRows(oc, rowNames = colnames(oc))
      }
    }

    if (options[["impliedCovariance"]]) {
      # actually compute the observed covariance
      fv <- lavaan::fitted.values(fit)
      level_names <- names(fv)

      for (i in 1:length(fv)) {
        ic <- fv[[i]]$cov
        ic[upper.tri(ic)] <- NA

        iccont[[level_names[i]]] <- createJaspTable(level_names[i])

        for (j in 1:ncol(ic)) {
          nm <- colnames(ic)[j]
          iccont[[level_names[i]]]$addColumnInfo(nm, title = .unv(nm), type = "pvalue")
        }
        iccont[[level_names[i]]]$addRows(ic, rowNames = colnames(ic))
      }
    }

    if (options[["residualCovariance"]]) {
      # actually compute the observed covariance
      rv <- lavaan::residuals(fit)
      level_names <- names(rv)

      for (i in 1:length(rv)) {
        rc <- rv[[i]]$cov
        rc[upper.tri(rc)] <- NA

        rccont[[level_names[i]]] <- createJaspTable(level_names[i])

        for (j in 1:ncol(rc)) {
          nm <- colnames(rc)[j]
          rccont[[level_names[i]]]$addColumnInfo(nm, title = .unv(nm), type = "pvalue")
        }
        rccont[[level_names[i]]]$addRows(rc, rowNames = colnames(rc))
      }
    }

    if (options[["standardizedResidual"]]) {
      # actually compute the observed covariance
      if (options[["errorCalculationMethod"]] == "bootstrap") {
        stdResError <- createJaspTable()
        stdResError$setError(gettext("The standardized residual covariance tables are currently unavailable when the error calculation method is 'Bootstrap'"))
        srcont[["stdResError"]] <- stdResError
      } else {
        sv <- try(lavaan::residuals(fit, type = "standardized"))
        if  (isTryError(sv)) {
          stdResError <- createJaspTable()
          stdResError$setError(gettext("The standardized residual covariance matrices could not be computed"))
          srcont[["stdResError"]] <- stdResError
        } else {
          level_names <- names(sv)

          for (i in 1:length(sv)) {
            sr <- sv[[i]]$cov
            sr[upper.tri(sr)] <- NA

            srcont[[level_names[i]]] <- createJaspTable(level_names[i])

            for (j in 1:ncol(sr)) {
              nm <- colnames(sr)[j]
              srcont[[level_names[i]]]$addColumnInfo(nm, title = .unv(nm), type = "pvalue")
            }
            srcont[[level_names[i]]]$addRows(sr, rowNames = colnames(sr))
          }
        }
      }
    }
  }

  if (!is.null(modelname)) {
    parentContainer[[modelname]] <- cocont
  }

  return()
}

.semMI <- function(modelContainer, dataset, options, ready) {
  if (!options[["modificationIndex"]] || !is.null(modelContainer[["modindices"]])) return()

  modindices <- createJaspContainer(gettext("Modification indices"))
  modindices$position <- 4
  modindices$dependOn(c("modificationIndex", "modificationIndexHiddenLow", "modificationIndexThreshold", "models"))

  modelContainer[["modindices"]] <- modindices

  if (length(options[["models"]]) < 2) {
    .semMITable(modelContainer[["results"]][["object"]][[1]], NULL, modindices, options, ready)
  } else {

    for (i in seq_along(options[["models"]])) {
      fit <- modelContainer[["results"]][["object"]][[i]]
      modelname <- options[["models"]][[i]][["name"]]
      .semMITable(fit, modelname, modindices, options, ready)
    }
  }
}

.semMITable <- function(fit, modelname, parentContainer, options, ready) {
  if (is.null(modelname)) {
    micont <- parentContainer
  } else {
    micont <- createJaspContainer(modelname, initCollapsed = TRUE)
  }

  semModIndicesTable <- createJaspTable(title = gettext("Modification Indices"))

  semModIndicesTable$addColumnInfo(name = "lhs",       title = "",                    type = "string")
  semModIndicesTable$addColumnInfo(name = "op",        title = "",                    type = "string")
  semModIndicesTable$addColumnInfo(name = "rhs",       title = "",                    type = "string")
  if (options[["group"]] != "")
    semModIndicesTable$addColumnInfo(name = "group",   title = gettext("group"),      type = "string")
  semModIndicesTable$addColumnInfo(name = "mi",        title = gettext("mi"),         type = "number")
  semModIndicesTable$addColumnInfo(name = "epc",       title = gettext("epc"),        type = "number")
  semModIndicesTable$addColumnInfo(name = "sepc.lv",   title = gettext("sepc (lv)"),  type = "number")
  semModIndicesTable$addColumnInfo(name = "sepc.all",  title = gettext("sepc (all)"), type = "number")
  semModIndicesTable$addColumnInfo(name = "sepc.nox",  title = gettext("sepc (nox)"), type = "number")

  semModIndicesTable$showSpecifiedColumnsOnly <- TRUE

  micont[["table"]] <- semModIndicesTable

  if (!ready || !inherits(fit, "lavaan")) return()

  # Extract modidffication indices:
  semModIndResult <- lavaan:::modificationIndices(fit)

  ### Remove NA:
  semModIndResult <- semModIndResult[!is.na(semModIndResult$mi), , drop=FALSE]

  ## Sort:
  semModIndResult <- semModIndResult[order(semModIndResult$mi, decreasing = TRUE), , drop=FALSE]

  ### Remove low indices:
  if (isTRUE(options$modificationIndexHiddenLow)) {
    semModIndResult <- semModIndResult[semModIndResult$mi > options$modificationIndexThreshold, , drop=FALSE]
  }

  if (options[["group"]] != "")
    semModIndResult[["group"]] <- lavaan::lavInspect(fit, "group.label")[semModIndResult[["group"]]]

  semModIndicesTable$setData(lapply(semModIndResult, .unv))


  if (!is.null(modelname)) {
    parentContainer[[modelname]] <- micont
  }

  return()
}

.semPathPlot <- function(modelContainer, dataset, options, ready) {
  if (!options[["pathPlot"]] || !ready || !is.null(modelContainer[["plot"]])) return()

  pcont <- createJaspContainer(gettext("Path diagram"))
  pcont$position <- 7
  pcont$dependOn(c("pathPlot", "pathPlotParameter", "pathPlotLegend", "models"))

  modelContainer[["plot"]] <- pcont

  if (length(options[["models"]]) < 2) {
    .semCreatePathPlot(modelContainer[["results"]][["object"]][[1]], NULL, pcont, options, ready)
  } else {

    for (i in seq_along(options[["models"]])) {
      fit <- modelContainer[["results"]][["object"]][[i]]
      modelname <- options[["models"]][[i]][["name"]]
      .semCreatePathPlot(fit, modelname, pcont, options, ready)
    }
  }
}

.semCreatePathPlot <- function(fit, modelname, parentContainer, options, ready) {
  if (is.null(modelname)) {
    modelname <- gettext("Path diagram")
  }

  if (options[["group"]] == "") {
    plt <- createJaspPlot(title = modelname, width = 600, height = 400)
  } else {
    plt <- createJaspContainer(title = modelname, initCollapsed = TRUE)
  }

  plt$dependOn(c("pathPlot", "pathPlotParameter", "pathPlotLegend"))
  parentContainer[[modelname]] <- plt

  if (!ready || !inherits(fit, "lavaan")) return()

  if (length(lavaan::lavInspect(fit, "ordered")) > 0) {
    plt$setError(gettext("Model plot not available with ordinal variables"))
    return()
  }

  # create a qgraph object using semplot
  po <- .lavToPlotObj(fit)
  pp <- .suppressGrDevice(semPlot::semPaths(
    object         = po,
    layout         = "tree2",
    intercepts     = FALSE,
    reorder        = FALSE,
    whatLabels     = ifelse(options[["pathPlotParameter"]], "par", "name"),
    edge.color     = "black",
    color          = list(lat = "#EAEAEA", man = "#EAEAEA", int = "#FFFFFF"),
    title          = FALSE,
    legend         = options[["pathPlotLegend"]],
    legend.mode    = "names",
    legend.cex     = 0.6,
    label.cex      = 1.3,
    edge.label.cex = 0.9,
    nodeNames      = decodeColNames(po@Vars$name),
    nCharNodes     = 3,
    rotation       = 2,
    ask            = FALSE
  ))

  if (options[["group"]] == "") {
    plt$plotObject <- pp
  } else {
    level_names <- lavaan::lavInspect(fit, "group.label")
    for (i in seq_along(level_names)) {
      plt[[level_names[i]]] <- createJaspPlot(title = level_names[i], width = 600, height = 400)
      plt[[level_names[i]]]$plotObject <- pp[[i]]
    }
  }
}
