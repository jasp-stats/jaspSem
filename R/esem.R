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

ESEM <- function(jaspResults, dataset, options, ...) {
  jaspResults$addCitation("Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2), 1-36. URL http://www.jstatsoft.org/v48/i02/")

  # Read dataset
  # TODO: don't read data if we aren't ready anyway...
  dataset <- .esemReadData(dataset, options)
  ready   <- .esemIsReady(dataset, options)
  if (!ready) return()
  options <- .esemEditOptions(dataset, options)

  modelContainer <- .esemModelContainer(jaspResults)

  # check for errors
  .esemCheckErrors(dataset, options, ready, modelContainer)

  # Output functions
  .esemFitTab(jaspResults, modelContainer, dataset, options, ready)
  .esemParameters(modelContainer, dataset, options, ready)
  .semAdditionalFits(modelContainer, dataset, options, ready)
  .semRsquared(modelContainer, dataset, options, ready)
  .semMardiasCoefficient(modelContainer, dataset, options, ready)
  .semCov(modelContainer, dataset, options, ready)
  .semMI(modelContainer, datset, options, ready)
  .semPathPlot(modelContainer, dataset, options, ready)
}

.esemReadData <- function(dataset, options) {
  if (!is.null(dataset)) return(dataset)
  if(options[["dataType"]] == "raw") {
    variablesToRead <- if (options[["group"]] == "") character() else options[["group"]]
    for (i in 1:length(options[["models"]])) {
      variablesToRead <- unique(c(variablesToRead, options[["models"]][[i]][["syntax"]][["columns"]]))
      efaBlocks <- options[["models"]][[i]][["efaBlock"]]
      for (j in 1:length(efaBlocks)) {
        efaVars <- efaBlocks[[j]][["variables"]]
        variablesToRead <- unique(c(variablesToRead, efaVars))
      }
    }
    dataset <- .readDataSetToEnd(columns = variablesToRead)
  } else {
    dataset <- .readDataSetToEnd(all.columns = TRUE)
  }

  return(dataset)
}

.esemIsReady <- function(dataset, options) {

  if (length(options[["models"]]) < 1) return(FALSE)

  for (m in options[["models"]])
    if (length(m[["syntax"]][["columns"]]) > 0)
      return(TRUE)

  return(FALSE)
}

.esemEditOptions <- function(dataset, options) {
  indicators <- unlist(unique(lapply(options[["models"]], function(x) {
    parsed <- lavaan::lavParseModelString(x[["syntax"]][["model"]], TRUE)
    return(unique(parsed[parsed$op == "=~",]$rhs))
  })))
  ordered_vars <- sapply(dataset[,indicators], is.ordered)
  if(length(ordered_vars) == 0 || sum(ordered_vars) == 0) {
    options[["order"]] <- FALSE
    if (options[["naAction"]] == "default") {
      if(options[["estimator"]] %in% c("gls", "wls", "uls", "dwls", "pml")) {
        options[["naAction"]] <- "listwise"
      } else {
        options[["naAction"]] <- "fiml"
      }
    }
  } else {
    options[["order"]] <- TRUE
    if (options[["estimator"]] == "default") {
      if(options[["modelTest"]] == "default" && options[["errorCalculationMethod"]] %in% c("standard", "robust")) {
        options[["estimator"]] <- "wlsmv"
      } else {
        options[["estimator"]] <- "dwls"
      }
      if (options[["naAction"]] == "default")
        options[["naAction"]] <- "listwise"
    } else {
      if (options[["naAction"]] == "default") {
        if(options[["estimator"]] %in% c("gls", "wls", "uls", "dwls", "pml")) {
          options[["naAction"]] <- "listwise"
        } else {
          options[["naAction"]] <- "fiml"
        }
      }
    }
  }

  # edit options[["models"]]
  options[["models"]] <- lapply(options[["models"]], .esemSyntax)
  return(options)
}

.esemSyntax <- function(model) {
  #' translate model syntax to jasp column names syntax
  cfaAndRegressionSyntax <- model[["syntax"]][["model"]]
  if(cfaAndRegressionSyntax == "") return()

  efaBlocks <- model[["efaBlock"]]
  efaSyntax <- ""
  for (i in 1:length(efaBlocks)) {
    if (length(efaBlocks[[i]][["variables"]]) < 1) return()
    efaSyntax <- paste0(efaSyntax, sprintf("\n# EFA Block: %1$s\n", efaBlocks[[i]][["name"]]))
    efaFactors <- efaBlocks[[i]][["efaFactors"]]
    for (j in 1:length(efaFactors)) {
      if (length(efaFactors[j]) < 1) return()
      efaRow <- sprintf('\nefa("%1$s")*%2$s =~ %3$s', efaBlocks[[i]][["name"]], efaFactors[j], paste(efaBlocks[[i]][["variables"]], collapse  = " + "))
      efaSyntax <- paste0(efaSyntax, efaRow)
    }
  }

  header <- paste0(
    "# ------------------------------\n",
    "# Lavaan model generated by JASP\n",
    "# ------------------------------\n"
  )

  syntax <- paste0(header, efaSyntax, "\n\n# CFA and regression\n\n", cfaAndRegressionSyntax)


  model[["syntax"]][["model"]] <- syntax
  return(model)
}

.esemCheckErrors <- function(dataset, options, ready, modelContainer) {
  if (!ready) return()

  if (ncol(dataset) > 0) {
    if (length(options[["models"]]) < 1) return(FALSE)
    usedvars <- unique(unlist(lapply(options[["models"]], function(x) {
      .semGetUsedVars(x[["syntax"]][["model"]], colnames(dataset))
    })))
    .hasErrors(dataset[,usedvars],
               type = c("infinity"), message='default', exitAnalysisIfErrors = TRUE)
  }

  # check FIML
  if(options[["order"]] && options[["estimator"]] %in% c("default", "ml", "mlr", "mlf", "pml")) {
    .quitAnalysis(gettext("ML estimation only available when all endogenous variables are of scale type."))
  }
  if (options[["estimator"]] %in% c("gls", "wls", "uls", "dwls", "wlsmv") && options[["naAction"]] == "fiml") {
    .quitAnalysis(gettext("FIML missing data handling only available with ML-type estimators"))
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

.esemModelContainer <- function(jaspResults) {
  if (!is.null(jaspResults[["modelContainer"]])) {
    modelContainer <- jaspResults[["modelContainer"]]
  } else {
    modelContainer <- createJaspContainer()
    modelContainer$dependOn(c("samplingWeights", "meanStructure", "manifestInterceptFixedToZero", "latentInterceptFixedToZero", "exogenousCovariateFixed", "orthogonal",
                              "factorScaling", "residualSingleIndicatorOmitted", "residualVariance", "exogenousLatentCorrelation",
                              "dependentCorrelation", "threshold", "scalingParameter", "efaConstrained", "standardizedVariable", "naAction", "estimator", "modelTest",
                              "errorCalculationMethod", "informationMatrix", "emulation", "group", "equalLoading", "equalIntercept",
                              "equalResidual", "equalResidualCovariance", "equalMean", "equalThreshold", "equalRegression",
                              "equalVariance", "equalLatentCovariance", "dataType", "sampleSize", "freeParameters", "rotation"))
    jaspResults[["modelContainer"]] <- modelContainer
  }

  return(modelContainer)
}

.esemComputeResults <- function(modelContainer, dataset, options) {
  #' create result list from options
  # find reusable results
  if (!options[["estimator"]] %in% c("default", "ml", "mlr", "mlf") && options[["naAction"]] == "fiml") return()

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
  lavopts <- .esemOptionsToLavOptions(options, dataset)

  for (i in seq_along(results)) {
    if (!is.null(results[[i]])) next # existing model is reused

    # create options
    lav_args <- lavopts
    syntax   <- .semTranslateModel(options[["models"]][[i]][["syntax"]][["model"]], dataset)
    lav_args[["model"]] <- syntax
    if (options[["dataType"]] == "raw") {
      lav_args[["data"]]  <- dataset
    } else {
      lav_args[["sample.cov"]] <- .semDataCovariance(dataset, options[["models"]][[i]][["syntax"]][["model"]])
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

    if (options[["errorCalculationMethod"]] == "bootstrap" && (options[["estimator"]] %in% c("default", "ml", "gls", "wls", "uls", "dwls", "pml"))) {
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

.esemOptionsToLavOptions <- function(options, dataset) {
  #' mapping the QML options from JASP to lavaan options
  #' see ?lavOptions for documentation
  lavopts <- lavaan::lavOptions()


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
  lavopts[["missing"]] <- switch(options[["naAction"]],
                                 "fiml" = "ml",
                                 "twoStage" = "two.stage",
                                 "twoStageRobust" = "robust.two.stage",
                                 "doublyRobust" = "doubly.robust",
                                 options[["naAction"]])

  # rotation options
  lavopts[["rotation"]] <- options[["rotation"]]


  # estimation options
  lavopts[["information"]] <- options[["informationMatrix"]]
  lavopts[["estimator"]]   <- options[["estimator"]]
  if (options[["estimator"]] %in% c("default", "ml", "gls", "wls", "uls", "dwls")) {
    lavopts[["se"]]        <- switch(options[["errorCalculationMethod"]],
                                     "bootstrap" = "standard",
                                     "robust" = "robust.sem",
                                     options[["errorCalculationMethod"]])

    lavopts[["test"]]      <- switch(options[["modelTest"]],
                                     "satorraBentler" = "Satorra.Bentler",
                                     "yuanBentler" = "Yuan.Bentler",
                                     "meanAndVarianceAdjusted" = "mean.var.adjusted",
                                     "scaledAndShifted" = "scaled.shifted",
                                     "bollenStine" = "Bollen.Stine",
                                     options[["modelTest"]])
  }

  lavopts[["mimic"]]       <- options[["emulation"]]



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

# output functions

.esemFitTab <- function(jaspResults, modelContainer, dataset, options, ready) {
  if (!is.null(modelContainer[["fittab"]])) return()

  fittab <- createJaspTable(title = gettext("Model fit"))
  fittab$dependOn(c("models", "estimator"))
  fittab$position <- 0

  fittab$addColumnInfo(name = "Model",    title = "",                            type = "string" )
  if (options[["estimator"]] %in% c("default", "ml", "pml", "mlr", "mlf")) {
    fittab$addColumnInfo(name = "AIC",      title = gettext("AIC"),                type = "number" )
    fittab$addColumnInfo(name = "BIC",      title = gettext("BIC"),                type = "number" )
  }
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

  # add data to the table!
  semResults <- .esemComputeResults(modelContainer, dataset, options)

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
  if (options[["estimator"]] %in% c("default", "ml", "pml", "mlr", "mlf")) {
    fittab[["AIC"]]      <- lrt$value[["AIC"]]
    fittab[["BIC"]]      <- lrt$value[["BIC"]]
  }
  fittab[["N"]]        <- Ns
  fittab[["Chisq"]]    <- lrt$value[["Chisq"]]
  fittab[["Df"]]       <- round(lrt$value[["Df"]], 3)
  fittab[["PrChisq"]]  <- pchisq(q = lrt$value[["Chisq"]], df = lrt$value[["Df"]], lower.tail = FALSE)
  fittab[["dchisq"]]   <- lrt$value[["Chisq diff"]]
  fittab[["ddf"]]      <- round(lrt$value[["Df diff"]], 3)
  fittab[["dPrChisq"]] <- lrt$value[["Pr(>Chisq)"]]

  # add warning footnote
  if (!is.null(lrt$warnings)) {
    fittab$addFootnote(gsub("lavaan WARNING: ", "", lrt$warnings[[1]]$message))
  }

  # add missing data handling footnote
  nrm <- nrow(dataset) - lavaan::lavInspect(semResults[[1]], "ntotal")
  method <- switch(options[["naAction"]],
                   "twoStage" = "two-stage",
                   "twoStageRobust" = "robust two-stage",
                   "doublyRobust" = "doubly robust",
                   "fiml" = "full information maximum likelihood",
                   options[["naAction"]])
  if(nrm > 0)
    fittab$addFootnote(gettextf("Missing data handling: <i>%1$s</i>. Removed cases: %2$s", method, nrm))

  #add ordinal endogenous estimation footnote
  if (options[["estimator"]] == "wlsmv") {
    fittab$addFootnote(message = gettext("Ordinal endogenous variable(s) detected! Automatically switched to <i>DWLS</i> estimation with <i>robust</i> standard errors, <i>robust</i> confidence intervals and a <i>scaled and shifted</i> test-statistic. <br><i>If you wish to override these settings, please select another (LS-)estimator and/or model test and \u2014optionally\u2014 change the error calculation method in the 'Estimation' tab.</i>"))
  }

  # add test statistic correction footnote
  if (options[["estimator"]] != "wlsmv") {
    test <- lavaan::lavInspect(semResults[[1]], "options")[["test"]]
    if(length(test) > 1)
      test <- test[[2]]

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
  }


  if (options$estimator %in% c("dwls", "gls", "wls", "uls", "wlsm", "wlsmvs", "wlsmv", "ulsm", "ulsmv", "ulsmvs")) {
    fittab$addFootnote(message = gettext("The AIC, BIC and additional information criteria are only available with ML-type estimators"))
  }
}

.esemParameters <- function(modelContainer, dataset, options, ready) {
  if (!is.null(modelContainer[["params"]])) return()


  params <- createJaspContainer(gettext("Parameter estimates"))
  params$position <- 1
  params$dependOn(c("ciLevel", "bootstrapCiType", "standardizedEstimate", "models", "standardizedEstimateType"))

  modelContainer[["params"]] <- params

  if (length(options[["models"]]) < 2) {
    .esemParameterTables(modelContainer[["results"]][["object"]][[1]], NULL, params, options, ready)
  } else {

    for (i in seq_along(options[["models"]])) {
      fit <- modelContainer[["results"]][["object"]][[i]]
      modelname <- options[["models"]][[i]][["name"]]
      .esemParameterTables(fit, modelname, params, options, ready)
    }
  }
}

.esemParameterTables <- function(fit, modelname, parentContainer, options, ready) {
  if (is.null(fit)) return()
  if (is.null(modelname)) {
    pecont <- parentContainer
  } else {
    pecont <- createJaspContainer(modelname, initCollapsed = TRUE)
  }

  #Estimator, SE, CI footnote
  modelOptions <- lavaan::lavInspect(fit, what = "options")
  if(options[["estimator"]] == "mlf") {
    se_type <- gettext("first-order derivatives based")
    ci_type <- gettext("robust")
  } else {
    se_type <- modelOptions$se
    se_type <- gsub('.esem', '', se_type)
    se_type <- gettext(stringr::str_to_title(gsub('\\.', ' ', se_type)))
    se_type <- paste0(tolower(substr(se_type, 1, 1)), substr(se_type, 2, nchar(se_type)))
    if(se_type == "standard") {
      se_type <- gettext("delta method")
      ci_type <- gettext("normal theory")
    } else if (se_type == "bootstrap") {
      se_type <- gettext("bootstrap")
      ci_type <- switch(options$bootstrapCiType,
                        "percentile"              = gettext("percentile bootstrap"),
                        "normalTheory"            = gettext("normal theory bootstrap"),
                        "percentileBiasCorrected" = gettext("bias-corrected percentile bootstrap")
      )
    } else {
      ci_type <- gettext("robust")
    }

  }
  est_type <- gettext(modelOptions$estimator)

  est_se_ci_footnote <- gettextf("<i>%1$s</i> estimation with <i>%2$s</i> standard errors and <i>%3$s</i> confidence intervals", est_type, se_type, ci_type)

  est_title <- ifelse(options[["standardizedEstimate"]], gettext("Standardized Estimate"), gettext("Estimate"))


  # efa tab
  efatab <- createJaspTable(title = gettext("EFA Factor Loadings"))

  if (options[["group"]] != "")
    efatab$addColumnInfo(name = "group",  title = gettext("Group"),      type = "string", combine = TRUE)

  efatab$addColumnInfo(name = "lhs",      title = gettext("Latent"),     type = "string", combine = TRUE)
  efatab$addColumnInfo(name = "rhs",      title = gettext("Indicator"),  type = "string")
  efatab$addColumnInfo(name = "label",    title = "",                    type = "string")
  efatab$addColumnInfo(name = "est",      title = est_title,             type = "number")
  efatab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number")
  efatab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number")
  efatab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "pvalue")
  efatab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
  efatab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))


  efatab$addFootnote(message = gettextf("Applied rotation method is <i>%s</i>. Different EFA blocks are rotated seperately.", options[["rotation"]]))

  pecont[["efa"]] <- efatab

  # Measurement model
  indtab <- createJaspTable(title = gettext("CFA Factor Loadings"))

  if (options[["group"]] != "")
    indtab$addColumnInfo(name = "group",  title = gettext("Group"),      type = "string", combine = TRUE)

  indtab$addColumnInfo(name = "lhs",      title = gettext("Latent"),     type = "string", combine = TRUE)
  indtab$addColumnInfo(name = "rhs",      title = gettext("Indicator"),  type = "string")
  indtab$addColumnInfo(name = "label",    title = "",                    type = "string")
  indtab$addColumnInfo(name = "est",      title = est_title,             type = "number")
  indtab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number")
  indtab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number")
  indtab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "pvalue")
  indtab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
  indtab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))


  indtab$addFootnote(est_se_ci_footnote)

  pecont[["ind"]] <- indtab

  # Structural Model
  regtab <- createJaspTable(title = gettext("Regression coefficients"))

  if (options[["group"]] != "")
    regtab$addColumnInfo(name = "group",  title = gettext("Group"),      type = "string", combine = TRUE)

  regtab$addColumnInfo(name = "rhs",      title = gettext("Predictor"),  type = "string", combine = ifelse(options[["group"]] != "", FALSE, TRUE))
  regtab$addColumnInfo(name = "lhs",      title = gettext("Outcome"),    type = "string")
  regtab$addColumnInfo(name = "label",    title = "",                    type = "string")
  regtab$addColumnInfo(name = "est",      title = est_title,             type = "number")
  regtab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number")
  regtab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number")
  regtab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "pvalue")
  regtab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
  regtab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))


  regtab$addFootnote(est_se_ci_footnote)

  pecont[["reg"]] <- regtab

  #thresholds
  thrtab <- createJaspTable(title = gettext("Thresholds"))

  if (options[["group"]] != "")
    thrtab$addColumnInfo(name = "group",  title = gettext("Group"),      type = "string", combine = TRUE)

  thrtab$addColumnInfo(name = "lhs",      title = gettext("Variable"),   type = "string", combine = TRUE)
  thrtab$addColumnInfo(name = "rhs",      title = gettext("Threshold"),  type = "string")
  thrtab$addColumnInfo(name = "label",    title = "",                    type = "string")
  thrtab$addColumnInfo(name = "est",      title = est_title,             type = "number")
  thrtab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number")
  thrtab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number")
  thrtab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "pvalue")
  thrtab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
  thrtab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))


  thrtab$addFootnote(est_se_ci_footnote)

  pecont[["thr"]] <- thrtab

  # Latent variances
  lvartab <- createJaspTable(title = gettext("Factor variances"))

  if (options[["group"]] != "")
    lvartab$addColumnInfo(name = "group",  title = gettext("Group"),      type = "string", combine = TRUE)

  lvartab$addColumnInfo(name = "lhs",      title = gettext("Variable"),   type = "string")
  lvartab$addColumnInfo(name = "label",    title = "",                    type = "string")
  lvartab$addColumnInfo(name = "est",      title = est_title,             type = "number")
  lvartab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number")
  lvartab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number")
  lvartab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "pvalue")
  lvartab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number",
                        overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
  lvartab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                        overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))


  lvartab$addFootnote(est_se_ci_footnote)

  pecont[["lvar"]] <- lvartab

  # Latent covariances
  lcovtab <- createJaspTable(title = gettext("Factor covariances"))

  if (options[["group"]] != "")
    lcovtab$addColumnInfo(name = "group",  title = gettext("Group"),      type = "string", combine = TRUE)

  lcovtab$addColumnInfo(name = "lhs",      title = gettext("Variables"),   type = "string")
  lcovtab$addColumnInfo(name = "label",    title = "",                    type = "string")
  lcovtab$addColumnInfo(name = "est",      title = est_title,             type = "number")
  lcovtab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number")
  lcovtab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number")
  lcovtab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "pvalue")
  lcovtab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number",
                        overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
  lcovtab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                        overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))


  lcovtab$addFootnote(est_se_ci_footnote)

  pecont[["lcov"]] <- lcovtab

  # Residual variances
  vartab <- createJaspTable(title = gettext("Residual variances"))

  if (options[["group"]] != "")
    vartab$addColumnInfo(name = "group",  title = gettext("Group"),      type = "string", combine = TRUE)

  vartab$addColumnInfo(name = "lhs",      title = gettext("Variable"),   type = "string")
  vartab$addColumnInfo(name = "label",    title = "",                    type = "string")
  vartab$addColumnInfo(name = "est",      title = est_title,             type = "number")
  vartab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number")
  vartab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number")
  vartab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "pvalue")
  vartab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
  vartab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))

  vartab$addFootnote(est_se_ci_footnote)

  pecont[["var"]] <- vartab

  # Residual covariances
  covtab <- createJaspTable(title = gettext("Residual covariances"))

  if (options[["group"]] != "")
    covtab$addColumnInfo(name = "group",  title = gettext("Group"),      type = "string", combine = TRUE)

  covtab$addColumnInfo(name = "lhs",      title = gettext("Variables"),   type = "string")
  covtab$addColumnInfo(name = "label",    title = "",                    type = "string")
  covtab$addColumnInfo(name = "est",      title = est_title,             type = "number")
  covtab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number")
  covtab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number")
  covtab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "pvalue")
  covtab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
  covtab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))

  covtab$addFootnote(est_se_ci_footnote)

  pecont[["cov"]] <- covtab

  # Means
  if (options[["meanStructure"]]) {
    mutab <- createJaspTable(title = gettext("Means"))

    if (options[["group"]] != "")
      mutab$addColumnInfo(name = "group",  title = gettext("Group"),      type = "string", combine = TRUE)

    mutab$addColumnInfo(name = "lhs",      title = gettext("Variable"),   type = "string")
    mutab$addColumnInfo(name = "label",    title = "",                    type = "string")
    mutab$addColumnInfo(name = "est",      title = est_title,             type = "number")
    mutab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number")
    mutab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number")
    mutab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "pvalue")
    mutab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number",
                        overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
    mutab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                        overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))


    mutab$addFootnote(est_se_ci_footnote)

    pecont[["mu"]] <- mutab
  }

  deftab <- createJaspTable(title = gettext("Defined parameters"))

  deftab$addColumnInfo(name = "lhs",      title = gettext("Name"),       type = "string")
  deftab$addColumnInfo(name = "est",      title = est_title,             type = "number")
  deftab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number")
  deftab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number")
  deftab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "pvalue")
  deftab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
  deftab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))


  deftab$addFootnote(est_se_ci_footnote)

  pecont[["def"]] <- deftab

  if (!is.null(modelname)) parentContainer[[modelname]] <- pecont

  if (!ready || !inherits(fit, "lavaan")) return()

  # fill tables with values
  lvnames <- lavaan::lavNames(fit, "lv")
  ovnames <- lavaan::lavNames(fit, "ov")

  bootstrapCiType <- ifelse(options[["bootstrapCiType"]] == "percentileBiasCorrected", "bca.simple",
                            ifelse(options[["bootstrapCiType"]] == "percentile", "perc",
                                   "norm"))
  if (options[["standardizedEstimate"]]) {
    type <- switch(options[["standardizedEstimateType"]],
                   "all" = "std.all",
                   "latents" = "std.lv",
                   "noX" = "std.nox")
    pe <- lavaan::standardizedsolution(fit, type = type, level = options[["ciLevel"]])
  } else {
    pe <- lavaan::parameterestimates(fit, standardized = TRUE, level = options[["ciLevel"]],
                                     boot.ci.type = bootstrapCiType)
  }
  pe <- lavaan::lavMatrixRepresentation(lavaan::lav_partable_complete(pe))

  if (options[["group"]] != "")  {
    pe[pe[["op"]] != ":=", "groupname"] <- lavaan::lavInspect(fit, "group.label")[pe[["group"]]]
  } else {
    pe[["group"]] <- 0
  }

  #efa tab
  pe_efa <- pe[pe$op == "=~", ]
  pe_efa <- pe_efa[pe_efa$efa != "", ]
  pe_efa <- pe_efa[order(pe_efa[["group"]], pe_efa[["lhs"]]),]
  if (nrow(pe_efa) == 0) pecont[["efa"]] <- NULL # remove if no estimates

  if (options[["group"]] != "")
    efatab[["group"]] <- pe_efa[["groupname"]]

  efatab[["rhs"]]      <- pe_efa[["rhs"]]
  efatab[["lhs"]]      <- paste0("(", pe_efa[["efa"]], ") ", pe_efa[["lhs"]])
  efatab[["label"]]    <- pe_efa[["label"]]
  efatab[["est"]]      <- if (options[["standardizedEstimate"]]) pe_efa[["est.std"]] else pe_efa[["est"]]
  efatab[["se"]]       <- pe_efa[["se"]]
  efatab[["z"]]        <- pe_efa[["z"]]
  efatab[["pvalue"]]   <- pe_efa[["pvalue"]]
  efatab[["ci.lower"]] <- pe_efa[["ci.lower"]]
  efatab[["ci.upper"]] <- pe_efa[["ci.upper"]]

  # Measurement model
  pe_ind <- pe[pe$op == "=~", ]
  pe_ind <- pe_ind[pe_ind$efa == "", ]
  pe_ind <- pe_ind[order(pe_ind[["group"]], pe_ind[["lhs"]]),]
  if (nrow(pe_ind) == 0) pecont[["ind"]] <- NULL # remove if no estimates

  if (options[["group"]] != "")
    indtab[["group"]] <- pe_ind[["groupname"]]

  indtab[["rhs"]]      <- .unv(pe_ind[["rhs"]])
  indtab[["lhs"]]      <- .unv(pe_ind[["lhs"]])
  indtab[["label"]]    <- pe_ind[["label"]]
  indtab[["est"]]      <- if (options[["standardizedEstimate"]]) pe_ind[["est.std"]] else pe_ind[["est"]]
  indtab[["se"]]       <- pe_ind[["se"]]
  indtab[["z"]]        <- pe_ind[["z"]]
  indtab[["pvalue"]]   <- pe_ind[["pvalue"]]
  indtab[["ci.lower"]] <- pe_ind[["ci.lower"]]
  indtab[["ci.upper"]] <- pe_ind[["ci.upper"]]

  # Structural model
  pe_reg <- pe[pe$op == "~",]
  pe_reg <- pe_reg[order(pe_reg[["group"]], pe_reg[["lhs"]]),]
  if (nrow(pe_reg) == 0) pecont[["reg"]] <- NULL # remove if no estimates

  if (options[["group"]] != "")
    regtab[["group"]] <- pe_reg[["groupname"]]

  regtab[["rhs"]]      <- .unv(pe_reg[["rhs"]])
  regtab[["lhs"]]      <- .unv(pe_reg[["lhs"]])
  regtab[["label"]]    <- pe_reg[["label"]]
  regtab[["est"]]      <- if (options[["standardizedEstimate"]]) pe_reg[["est.std"]] else pe_reg[["est"]]
  regtab[["se"]]       <- pe_reg[["se"]]
  regtab[["z"]]        <- pe_reg[["z"]]
  regtab[["pvalue"]]   <- pe_reg[["pvalue"]]
  regtab[["ci.lower"]] <- pe_reg[["ci.lower"]]
  regtab[["ci.upper"]] <- pe_reg[["ci.upper"]]


  #Thresholds
  pe_thr <- pe[pe$op == "|",]
  if (nrow(pe_thr) == 0) pecont[["thr"]] <- NULL # remove if no estimates
  if (options[["group"]] != "")
    thrtab[["group"]] <- pe_thr[["groupname"]]

  thrtab[["lhs"]]      <- pe_thr[["lhs"]]
  thrtab[["rhs"]]      <- pe_thr[["rhs"]]
  thrtab[["label"]]    <- pe_thr[["label"]]
  thrtab[["est"]]      <- if (options[["standardizedEstimate"]]) pe_thr[["est.std"]] else pe_thr[["est"]]
  thrtab[["se"]]       <- pe_thr[["se"]]
  thrtab[["z"]]        <- pe_thr[["z"]]
  thrtab[["pvalue"]]   <- pe_thr[["pvalue"]]
  thrtab[["ci.lower"]] <- pe_thr[["ci.lower"]]
  thrtab[["ci.upper"]] <- pe_thr[["ci.upper"]]


  # Latent variances
  pe_lvar <- pe[pe$op == "~~" & pe$lhs %in% lvnames & pe$lhs == pe$rhs,]
  if (nrow(pe_lvar) == 0) pecont[["lvar"]] <- NULL # remove if no estimates

  if (options[["group"]] != "")
    lvartab[["group"]] <- pe_lvar[["groupname"]]

  lvartab[["rhs"]]      <- .unv(pe_lvar[["rhs"]])
  lvartab[["lhs"]]      <- .unv(pe_lvar[["lhs"]])
  lvartab[["label"]]    <- pe_lvar[["label"]]
  lvartab[["est"]]      <- if (options[["standardizedEstimate"]]) pe_lvar[["est.std"]] else pe_lvar[["est"]]
  lvartab[["se"]]       <- pe_lvar[["se"]]
  lvartab[["z"]]        <- pe_lvar[["z"]]
  lvartab[["pvalue"]]   <- pe_lvar[["pvalue"]]
  lvartab[["ci.lower"]] <- pe_lvar[["ci.lower"]]
  lvartab[["ci.upper"]] <- pe_lvar[["ci.upper"]]


  # Latent covariances
  pe_lcov <- pe[pe$op == "~~" & pe$lhs %in% lvnames & pe$rhs %in% lvnames & pe$lhs != pe$rhs,]
  if (nrow(pe_lcov) == 0) pecont[["lcov"]] <- NULL # remove if no estimates

  if (options[["group"]] != "")
    lcovtab[["group"]] <- pe_lcov[["groupname"]]

  lcovtab[["lhs"]]      <- paste(.unv(pe_lcov[["lhs"]]), "-", .unv(pe_lcov[["rhs"]]))
  lcovtab[["label"]]    <- pe_lcov[["label"]]
  lcovtab[["est"]]      <- if (options[["standardizedEstimate"]]) pe_lcov[["est.std"]] else pe_lcov[["est"]]
  lcovtab[["se"]]       <- pe_lcov[["se"]]
  lcovtab[["z"]]        <- pe_lcov[["z"]]
  lcovtab[["pvalue"]]   <- pe_lcov[["pvalue"]]
  lcovtab[["ci.lower"]] <- pe_lcov[["ci.lower"]]
  lcovtab[["ci.upper"]] <- pe_lcov[["ci.upper"]]


  # Residual variances
  pe_var <- pe[pe$op == "~~" & pe$lhs %in% ovnames & pe$lhs == pe$rhs,]
  if (nrow(pe_var) == 0) pecont[["var"]] <- NULL # remove if no estimates

  if (options[["group"]] != "")
    vartab[["group"]] <- pe_var[["groupname"]]

  vartab[["rhs"]]      <- .unv(pe_var[["rhs"]])
  vartab[["lhs"]]      <- .unv(pe_var[["lhs"]])
  vartab[["label"]]    <- pe_var[["label"]]
  vartab[["est"]]      <- if (options[["standardizedEstimate"]]) pe_var[["est.std"]] else pe_var[["est"]]
  vartab[["se"]]       <- pe_var[["se"]]
  vartab[["z"]]        <- pe_var[["z"]]
  vartab[["pvalue"]]   <- pe_var[["pvalue"]]
  vartab[["ci.lower"]] <- pe_var[["ci.lower"]]
  vartab[["ci.upper"]] <- pe_var[["ci.upper"]]


  # Residual covariances
  pe_cov <- pe[pe$op == "~~" & pe$lhs %in% ovnames & pe$rhs %in% ovnames & pe$lhs != pe$rhs,]
  if (nrow(pe_cov) == 0) pecont[["cov"]] <- NULL # remove if no estimates

  if (options[["group"]] != "")
    covtab[["group"]] <- pe_cov[["groupname"]]

  covtab[["lhs"]]      <- paste(.unv(pe_cov[["lhs"]]), "-", .unv(pe_cov[["rhs"]]))
  covtab[["label"]]    <- pe_cov[["label"]]
  covtab[["est"]]      <- if (options[["standardizedEstimate"]]) pe_cov[["est.std"]] else pe_cov[["est"]]
  covtab[["se"]]       <- pe_cov[["se"]]
  covtab[["z"]]        <- pe_cov[["z"]]
  covtab[["pvalue"]]   <- pe_cov[["pvalue"]]
  covtab[["ci.lower"]] <- pe_cov[["ci.lower"]]
  covtab[["ci.upper"]] <- pe_cov[["ci.upper"]]



  # Means
  if (options[["meanStructure"]]) {
    pe_mu <- pe[pe$op == "~1",]

    if (options[["group"]] != "")
      mutab[["group"]] <- pe_mu[["groupname"]]

    mutab[["lhs"]] <- .unv(pe_mu[["lhs"]])
    mutab[["label"]]    <- pe_mu[["label"]]
    mutab[["est"]]      <- if (options[["standardizedEstimate"]]) pe_mu[["est.std"]] else pe_mu[["est"]]
    mutab[["se"]]       <- pe_mu[["se"]]
    mutab[["z"]]        <- pe_mu[["z"]]
    mutab[["pvalue"]]   <- pe_mu[["pvalue"]]
    mutab[["ci.lower"]] <- pe_mu[["ci.lower"]]
    mutab[["ci.upper"]] <- pe_mu[["ci.upper"]]

  }

  # defined parameters
  pe_def <- pe[pe$op == ":=",]
  if (nrow(pe_def) == 0) pecont[["def"]] <- NULL # remove if no estimates

  deftab[["lhs"]]      <- pe_def[["lhs"]]
  deftab[["est"]]      <- if (options[["standardizedEstimate"]]) pe_def[["est.std"]] else pe_def[["est"]]
  deftab[["se"]]       <- pe_def[["se"]]
  deftab[["z"]]        <- pe_def[["z"]]
  deftab[["pvalue"]]   <- pe_def[["pvalue"]]
  deftab[["ci.lower"]] <- pe_def[["ci.lower"]]
  deftab[["ci.upper"]] <- pe_def[["ci.upper"]]


}
