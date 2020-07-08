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

# options <- list(
#   models = list(
#     list(
#       modelName = "Model 1",
#       syntax = "visual  =~ x1 + x2 + x3\ntextual =~ x4 + x5 + x6\nspeed   =~ x7 + x8 + x9"
#     ),
#     list(
#       modelName = "Model 2",
#       syntax = "visual  =~ x1 + x2 + x3\ntextual =~ x4 + x5 + x6\nspeed   =~ x7 + x8 + x9\nx4~~x7"
#     ),
#     list(
#       modelName = "Model 3",
#       syntax = "visual  =~ x1 + a*x2 + x3\ntextual =~ x4 + a*x5 + x6\nspeed   =~ x7 + x8 + x9\nx4~~x7"
#     )
#   ),
#   errorCalculation = "standard",
#   errorCalculationBootstrapSamples = 1000
# )
# 
# # here are all the options we should support
# 
# opts <-  list(
#   .meta = list(), 
#   Data = "raw", 
#   SampleSize = 0, 
#   addPathDiagram = FALSE, 
#   ddScalingParameters = TRUE, 
#   addThresholds = TRUE, 
#   assumeFactorsUncorrelated = FALSE, 
#   correlateDependentVariables = TRUE, 
#   correlateExogenousLatents = TRUE, 
#   emulation = "none", 
#   eq_intercepts = FALSE, 
#   eq_loadings = FALSE, 
#   eq_lvcovariances = FALSE, 
#   eq_means = FALSE, 
#   eq_regressions = FALSE, 
#   eq_residualcovariances = FALSE, 
#   eq_residuals = FALSE, 
#   eq_thresholds = FALSE, 
#   eq_variances = FALSE, 
#   errorCalculation = "standard", 
#   errorCalculationBootstrapSamples = 1000, 
#   estimator = "automatic", 
#   factorStandardisation = "factorLoadings", 
#   fixExogenousCovariates = TRUE, 
#   fixLatentInterceptsToZero = TRUE, 
#   fixManifestInterceptsToZero = FALSE,
#   groupingVariable = "", 
#   includeMeanStructure = FALSE, 
#   model = "contGamma ~ contNormal", 
#   omitResidualSingleIndicator = TRUE, 
#   utputAdditionalFitMeasures = FALSE, 
#   outputFittedCovarianceCorrelations = FALSE, 
#   outputMardiasCoefficients = FALSE, 
#   outputModificationIndices = FALSE, 
#   outputModificationIndicesHideLowIndices = FALSE, 
#   outputModificationIndicesHideLowIndicesThreshold = 10, 
#   outputObservedCovarianceCorrelations = FALSE, 
#   outputRSquared = FALSE, 
#   outputResidualCovarianceCorrelations = FALSE, 
#   outputpathdiagramstandardizedparameter = FALSE, 
#   plotHeight = 320, 
#   plotWidth = 480, 
#   residualVariances = TRUE
# )
# 
# dataset <- lavaan::HolzingerSwineford1939
# 

SEM <- function(jaspResults, dataset, options, ...) {
  jaspResults$addCitation("Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2), 1-36. URL http://www.jstatsoft.org/v48/i02/")
  
  # Read dataset
  dataset <- .semReadData(dataset, options)
  ready   <- .semIsReady(dataset, options)
  
  modelContainer <- .semModelContainer(jaspResults)
  
  # Output functions
  .semFitTab(jaspResults, modelContainer, dataset, options, ready)
  .semParameters(modelContainer, dataset, options, ready)
  .semAdditionalFits(modelContainer, dataset, options, ready)
  .semRsquared(modelContainer, dataset, options, ready)
  .semCov(modelContainer, dataset, options, ready)
}

# helper functions

.semReadData <- function(dataset, options) {
  if (!is.null(dataset)) return(dataset)
  return(JASP::.readDataSetToEnd(all.columns = TRUE))
}

.semIsReady <- function(dataset, options) {
  usedvars <- unique(unlist(lapply(options[["models"]], function(x) {
    .semGetUsedVars(x[["syntax"]], colnames(dataset))
  })))
  
  ready <- length(usedvars) > 1 && options[["models"]][[1]][["syntax"]] != ""
}

.semGetUsedVars <- function(syntax, availablevars) {
  vv <- .unv(availablevars)
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
    modelContainer$dependOn(c("meanstructure", "int.ov.free", "int.lv.free", "fixed.x", "orthogonal", "std.lv", 
                              "effect.coding", "auto.fix.first", "auto.fix.single", "auto.var", "auto.cov.lv.x", 
                              "auto.cov.y", "auto.th", "auto.delta", "auto.efa", "std.ov", "missing", "estimator",
                              "se", "information", "emulation"))
    jaspResults[["modelContainer"]] <- modelContainer
  }
  
  return(modelContainer)
}

.semComputeResults <- function(modelContainer, dataset, options) {
  #' create result list from options
  
  # find reusable results
  oldmodels  <- modelContainer[["models"]][["object"]]
  oldresults <- modelContainer[["results"]][["object"]]
  reuse <- match(options[["models"]], oldmodels)
  if (identical(reuse, seq_along(reuse))) {
    # store in model container
    print("IDENTICAL!")
    return(oldresults) # reuse everything
  }
  
  # create results list
  results <- vector("list", length(options[["models"]]))
  if (any(!is.na(reuse))) {
    # where possible, prefill results with old results
    results[seq_along(reuse)] <- options[["oldresults"]][reuse]
  }
  
  # generate lavaan options list
  lavopts <- .semOptionsToLavOptions(options)
  
  for (i in seq_along(results)) {
    if (!is.null(results[[i]])) {print(i, "reused"); next} # existing model is reused
    
    # create options
    lav_args <- lavopts
    syntax   <- .semTranslateModel(options[["models"]][[i]][["syntax"]], dataset)
    lav_args[["model"]] <- syntax
    lav_args[["data"]]  <- dataset
    
    # fit the model
    fit <- try(do.call(lavaan::lavaan, lav_args))
    
    if (inherits(fit, "try-error")) {
      errmsg <- gettextf("Estimation failed\nMessage:\n%s", attr(fit, "condition")$message)
      modelContainer$setError(paste0("Error in model \"", options[["models"]][[i]][["modelName"]], "\" - ", 
                                    .decodeVarsInMessage(names(dataset), errmsg)))
    }
    
    if (options[["se"]] == "bootstrap") {
      fit <- lavBootstrap(fit, options[["errorCalculationBootstrapSamples"]])
    }
    results[[i]] <- fit
  }
  
  # store in model container
  deps <- c("meanstructure", "int.ov.fixed", "int.lv.fixed", "fixed.x", "orthogonal", "factorStandardisation", 
            "auto.fix.single", "auto.var", "auto.cov.lv.x", "auto.cov.y", "auto.th", "auto.delta", 
            "auto.efa", "std.ov", "missing", "estimator", "se", "information", "emulation")
  modelContainer[["results"]] <- createJaspState(results, dependencies = deps)
  modelContainer[["models"]]  <- createJaspState(options[["models"]], dependencies = deps)
  
  return(results)
}

.semOptionsToLavOptions <- function(options) {
  #' mapping the QML options from JASP to lavaan options
  #' see ?lavOptions for documentation
  lavopts <- lavaan::lavOptions()
  
  lavopts[["mimic"]] <- options[["emulation"]]
  
  
  # model features
  lavopts[["meanstructure"]]   <- options[["meanstructure"]]
  lavopts[["int.ov.free"]]     <- !options[["int.ov.fixed"]]
  lavopts[["int.lv.free"]]     <- !options[["int.lv.fixed"]]
  lavopts[["fixed.x"]]         <- options[["fixed.x"]]
  lavopts[["orthogonal"]]      <- options[["orthogonal"]]
  lavopts[["std.lv"]]          <- options[["factorStandardisation"]] == "std.lv"
  lavopts[["effect.coding"]]   <- options[["factorStandardisation"]] == "effect.coding"
  lavopts[["auto.fix.first"]]  <- options[["factorStandardisation"]] == "auto.fix.first"
  lavopts[["auto.fix.single"]] <- options[["auto.fix.single"]]
  lavopts[["auto.var"]]        <- options[["auto.var"]]
  lavopts[["auto.cov.lv.x"]]   <- options[["auto.cov.lv.x"]]
  lavopts[["auto.cov.y"]]      <- options[["auto.cov.y"]]
  lavopts[["auto.th"]]         <- options[["auto.th"]]
  lavopts[["auto.delta"]]      <- options[["auto.delta"]]
  lavopts[["auto.efa"]]        <- options[["auto.efa"]]
  
  # data options
  lavopts[["std.ov"]]  <- options[["std.ov"]]
  lavopts[["missing"]] <- options[["missing"]]
  
  # estimation options
  lavopts[["estimator"]]   <- ifelse(options[["estimator"]] == "automatic",  "default",  options[["estimator"]])
  lavopts[["se"]]          <- ifelse(options[["se"]] == "bootstrap", "standard", options[["se"]])
  lavopts[["information"]] <- options[["information"]]
  
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

# output functions

.semFitTab <- function(jaspResults, modelContainer, dataset, options, ready) {
  if (!is.null(modelContainer[["fittab"]])) return()
  
  fittab <- createJaspTable(title = gettext("Model fit"))
  fittab$dependOn("models")
  fittab$position <- 0
  
  fittab$addColumnInfo(name = "Model",    title = "",                            type = "string" )
  fittab$addColumnInfo(name = "AIC",      title = gettext("AIC"),                type = "number" )
  fittab$addColumnInfo(name = "BIC",      title = gettext("BIC"),                type = "number" )
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
  
  lrt_args <- semResults
  if (length(semResults) == 1) {
    lrt <- .withWarnings(lavaan::lavTestLRT(semResults[[1]])[-1, ])
    rownames(lrt$value) <- options[["models"]][[1]][["modelName"]]
  } else {
    names(lrt_args) <- "object" # (the first result is object, the others ...)
    lrt_args[["model.names"]] <- vapply(options[["models"]], getElement, name = "modelName", "")
    lrt <- .withWarnings(do.call(lavaan::lavTestLRT, lrt_args))
    lrt$value[1,5:7] <- NA
  }
  
  fittab[["Model"]]    <- rownames(lrt$value)
  fittab[["AIC"]]      <- lrt$value[["AIC"]]
  fittab[["BIC"]]      <- lrt$value[["BIC"]]
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
}

.semParameters <- function(modelContainer, dataset, options, ready) {
  if (!is.null(modelContainer[["params"]])) return()
  
  
  params <- createJaspContainer("Parameter estimates")
  params$position <- 1
  params$dependOn(c("ciWidth", "bootCItype", "std"))
  
  modelContainer[["params"]] <- params
  
  if (length(options[["models"]]) < 2) {
    .semParameterTables(modelContainer[["results"]][["object"]][[1]], NULL, params, options, ready)
  } else {
    
    for (i in seq_along(options[["models"]])) {
      fit <- modelContainer[["results"]][["object"]][[i]]
      modelname <- options[["models"]][[i]][["modelName"]]
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
  
  indtab$addColumnInfo(name = "lhs",      title = gettext("Latent"),     type = "string", combine = TRUE)
  indtab$addColumnInfo(name = "rhs",      title = gettext("Indicator"),  type = "string")
  indtab$addColumnInfo(name = "label",    title = "",                    type = "string")
  indtab$addColumnInfo(name = "est",      title = gettext("Estimate"),   type = "number", format = "sf:4;dp:3")
  indtab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number", format = "sf:4;dp:3")
  indtab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number", format = "sf:4;dp:3")
  indtab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "number", format = "dp:3;p:.001")
  indtab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number", format = "sf:4;dp:3",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciWidth * 100))
  indtab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number", format = "sf:4;dp:3",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciWidth * 100))
  
  if (options[["std"]]) {
    indtab$addColumnInfo(name = "std.all", title = gettext("All"),  type = "number", format = "sf:4;dp:3", 
                         overtitle = gettext("Standardized"))
    indtab$addColumnInfo(name = "std.lv",  title = gettext("LV"),   type = "number", format = "sf:4;dp:3", 
                         overtitle = gettext("Standardized"))
    indtab$addColumnInfo(name = "std.nox", title = gettext("Endo"), type = "number", format = "sf:4;dp:3", 
                         overtitle = gettext("Standardized"))
  }
  
  pecont[["ind"]] <- indtab
  
  # Structural Model
  regtab <- createJaspTable(title = gettext("Regression coefficients"))
  
  regtab$addColumnInfo(name = "rhs",      title = gettext("Predictor"),  type = "string", combine = TRUE)
  regtab$addColumnInfo(name = "lhs",      title = gettext("Outcome"),    type = "string")
  regtab$addColumnInfo(name = "label",    title = "",                    type = "string")
  regtab$addColumnInfo(name = "est",      title = gettext("Estimate"),   type = "number", format = "sf:4;dp:3")
  regtab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number", format = "sf:4;dp:3")
  regtab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number", format = "sf:4;dp:3")
  regtab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "number", format = "dp:3;p:.001")
  regtab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number", format = "sf:4;dp:3",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciWidth * 100))
  regtab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number", format = "sf:4;dp:3",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciWidth * 100))
  
  if (options[["std"]]) {
    regtab$addColumnInfo(name = "std.all", title = gettext("All"),  type = "number", format = "sf:4;dp:3", 
                         overtitle = gettext("Standardized"))
    regtab$addColumnInfo(name = "std.lv",  title = gettext("LV"),   type = "number", format = "sf:4;dp:3", 
                         overtitle = gettext("Standardized"))
    regtab$addColumnInfo(name = "std.nox", title = gettext("Endo"), type = "number", format = "sf:4;dp:3", 
                         overtitle = gettext("Standardized"))
  }
  
  pecont[["reg"]] <- regtab
  
  # Latent variances
  lvartab <- createJaspTable(title = gettext("Factor variances"))
  
  lvartab$addColumnInfo(name = "lhs",      title = gettext("Variable"),   type = "string")
  lvartab$addColumnInfo(name = "label",    title = "",                    type = "string")
  lvartab$addColumnInfo(name = "est",      title = gettext("Estimate"),   type = "number", format = "sf:4;dp:3")
  lvartab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number", format = "sf:4;dp:3")
  lvartab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number", format = "sf:4;dp:3")
  lvartab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "number", format = "dp:3;p:.001")
  lvartab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number", format = "sf:4;dp:3",
                        overtitle = gettextf("%s%% Confidence Interval", options$ciWidth * 100))
  lvartab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number", format = "sf:4;dp:3",
                        overtitle = gettextf("%s%% Confidence Interval", options$ciWidth * 100))
  
  if (options[["std"]]) {
    lvartab$addColumnInfo(name = "std.all", title = gettext("All"),  type = "number", format = "sf:4;dp:3", 
                          overtitle = gettext("Standardized"))
    lvartab$addColumnInfo(name = "std.lv",  title = gettext("LV"),   type = "number", format = "sf:4;dp:3", 
                          overtitle = gettext("Standardized"))
    lvartab$addColumnInfo(name = "std.nox", title = gettext("Endo"), type = "number", format = "sf:4;dp:3", 
                          overtitle = gettext("Standardized"))
  }
  
  pecont[["lvar"]] <- lvartab
  
  # Latent covariances
  lcovtab <- createJaspTable(title = gettext("Factor covariances"))
  
  lcovtab$addColumnInfo(name = "lhs",      title = gettext("Variables"),   type = "string")
  lcovtab$addColumnInfo(name = "label",    title = "",                    type = "string")
  lcovtab$addColumnInfo(name = "est",      title = gettext("Estimate"),   type = "number", format = "sf:4;dp:3")
  lcovtab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number", format = "sf:4;dp:3")
  lcovtab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number", format = "sf:4;dp:3")
  lcovtab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "number", format = "dp:3;p:.001")
  lcovtab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number", format = "sf:4;dp:3",
                        overtitle = gettextf("%s%% Confidence Interval", options$ciWidth * 100))
  lcovtab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number", format = "sf:4;dp:3",
                        overtitle = gettextf("%s%% Confidence Interval", options$ciWidth * 100))
  
  if (options[["std"]]) {
    lcovtab$addColumnInfo(name = "std.all", title = gettext("All"),  type = "number", format = "sf:4;dp:3", 
                          overtitle = gettext("Standardized"))
    lcovtab$addColumnInfo(name = "std.lv",  title = gettext("LV"),   type = "number", format = "sf:4;dp:3", 
                          overtitle = gettext("Standardized"))
    lcovtab$addColumnInfo(name = "std.nox", title = gettext("Endo"), type = "number", format = "sf:4;dp:3", 
                          overtitle = gettext("Standardized"))
  }
  
  pecont[["lcov"]] <- lcovtab
  
  # Residual variances
  vartab <- createJaspTable(title = gettext("Residual variances"))
  
  vartab$addColumnInfo(name = "lhs",      title = gettext("Variable"),   type = "string")
  vartab$addColumnInfo(name = "label",    title = "",                    type = "string")
  vartab$addColumnInfo(name = "est",      title = gettext("Estimate"),   type = "number", format = "sf:4;dp:3")
  vartab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number", format = "sf:4;dp:3")
  vartab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number", format = "sf:4;dp:3")
  vartab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "number", format = "dp:3;p:.001")
  vartab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number", format = "sf:4;dp:3",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciWidth * 100))
  vartab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number", format = "sf:4;dp:3",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciWidth * 100))
  
  if (options[["std"]]) {
    vartab$addColumnInfo(name = "std.all", title = gettext("All"),  type = "number", format = "sf:4;dp:3", 
                         overtitle = gettext("Standardized"))
    vartab$addColumnInfo(name = "std.lv",  title = gettext("LV"),   type = "number", format = "sf:4;dp:3", 
                         overtitle = gettext("Standardized"))
    vartab$addColumnInfo(name = "std.nox", title = gettext("Endo"), type = "number", format = "sf:4;dp:3", 
                         overtitle = gettext("Standardized"))
  }
  
  pecont[["var"]] <- vartab
  
  # Residual covariances
  covtab <- createJaspTable(title = gettext("Residual covariances"))
  
  covtab$addColumnInfo(name = "lhs",      title = gettext("Variables"),   type = "string")
  covtab$addColumnInfo(name = "label",    title = "",                    type = "string")
  covtab$addColumnInfo(name = "est",      title = gettext("Estimate"),   type = "number", format = "sf:4;dp:3")
  covtab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number", format = "sf:4;dp:3")
  covtab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number", format = "sf:4;dp:3")
  covtab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "number", format = "dp:3;p:.001")
  covtab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number", format = "sf:4;dp:3",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciWidth * 100))
  covtab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number", format = "sf:4;dp:3",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciWidth * 100))
  
  if (options[["std"]]) {
    covtab$addColumnInfo(name = "std.all", title = gettext("All"),  type = "number", format = "sf:4;dp:3", 
                         overtitle = gettext("Standardized"))
    covtab$addColumnInfo(name = "std.lv",  title = gettext("LV"),   type = "number", format = "sf:4;dp:3", 
                         overtitle = gettext("Standardized"))
    covtab$addColumnInfo(name = "std.nox", title = gettext("Endo"), type = "number", format = "sf:4;dp:3", 
                         overtitle = gettext("Standardized"))
  }
  
  pecont[["cov"]] <- covtab
  
  # Means
  if (options[["meanstructure"]]) {
    mutab <- createJaspTable(title = gettext("Means"))
    
    mutab$addColumnInfo(name = "lhs",      title = gettext("Variable"),   type = "string")
    mutab$addColumnInfo(name = "label",    title = "",                    type = "string")
    mutab$addColumnInfo(name = "est",      title = gettext("Estimate"),   type = "number", format = "sf:4;dp:3")
    mutab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number", format = "sf:4;dp:3")
    mutab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number", format = "sf:4;dp:3")
    mutab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "number", format = "dp:3;p:.001")
    mutab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number", format = "sf:4;dp:3",
                        overtitle = gettextf("%s%% Confidence Interval", options$ciWidth * 100))
    mutab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number", format = "sf:4;dp:3",
                        overtitle = gettextf("%s%% Confidence Interval", options$ciWidth * 100))
    
    if (options[["std"]]) {
      mutab$addColumnInfo(name = "std.all", title = gettext("All"),  type = "number", format = "sf:4;dp:3", 
                          overtitle = gettext("Standardized"))
      mutab$addColumnInfo(name = "std.lv",  title = gettext("LV"),   type = "number", format = "sf:4;dp:3", 
                          overtitle = gettext("Standardized"))
      mutab$addColumnInfo(name = "std.nox", title = gettext("Endo"), type = "number", format = "sf:4;dp:3", 
                          overtitle = gettext("Standardized"))
    }
    
    pecont[["mu"]] <- mutab
  }
  
  if (!is.null(modelname)) parentContainer[[modelname]] <- pecont
  
  if (!ready || !inherits(fit, "lavaan")) return()
  
  # fill tables with values
  lvnames <- lavaan::lavNames(fit, "lv")
  ovnames <- lavaan::lavNames(fit, "ov")
  pe <- lavaan::parameterestimates(fit, standardized = TRUE, level = options[["ciWidth"]], 
                                   boot.ci.type = options[["bootCItype"]])
  pe <- lavaan::lavMatrixRepresentation(lavaan::lav_partable_complete(pe))
  
  # Measurement model
  pe_ind <- pe[pe$op == "=~",]
  pe_ind <- pe_ind[order(pe_ind$lhs),]
  if (nrow(pe_ind) == 0) pecont[["int"]] <- NULL # remove if no estimates
  
  indtab[["rhs"]]      <- .unv(pe_ind[["rhs"]])
  indtab[["lhs"]]      <- .unv(pe_ind[["lhs"]])
  indtab[["label"]]    <- pe_ind[["label"]]
  indtab[["est"]]      <- pe_ind[["est"]]
  indtab[["se"]]       <- pe_ind[["se"]]
  indtab[["z"]]        <- pe_ind[["z"]]
  indtab[["pvalue"]]   <- pe_ind[["pvalue"]]
  indtab[["ci.lower"]] <- pe_ind[["ci.lower"]]
  indtab[["ci.upper"]] <- pe_ind[["ci.upper"]]
  
  if (options[["std"]]) {
    indtab[["std.all"]] <- pe_ind[["std.all"]]
    indtab[["std.lv"]]  <- pe_ind[["std.lv"]]
    indtab[["std.nox"]] <- pe_ind[["std.nox"]]
  }
  
  # Structural model
  pe_reg <- pe[pe$op == "~",]
  pe_reg <- pe_reg[order(pe_reg$lhs),]
  if (nrow(pe_reg) == 0) pecont[["reg"]] <- NULL # remove if no estimates
  
  regtab[["rhs"]]      <- .unv(pe_reg[["rhs"]])
  regtab[["lhs"]]      <- .unv(pe_reg[["lhs"]])
  regtab[["label"]]    <- pe_reg[["label"]]
  regtab[["est"]]      <- pe_reg[["est"]]
  regtab[["se"]]       <- pe_reg[["se"]]
  regtab[["z"]]        <- pe_reg[["z"]]
  regtab[["pvalue"]]   <- pe_reg[["pvalue"]]
  regtab[["ci.lower"]] <- pe_reg[["ci.lower"]]
  regtab[["ci.upper"]] <- pe_reg[["ci.upper"]]
  
  if (options[["std"]]) {
    regtab[["std.all"]] <- pe_reg[["std.all"]]
    regtab[["std.lv"]]  <- pe_reg[["std.lv"]]
    regtab[["std.nox"]] <- pe_reg[["std.nox"]]
  }
  
  # Latent variances
  pe_lvar <- pe[pe$op == "~~" & pe$lhs %in% lvnames & pe$lhs == pe$rhs,]
  if (nrow(pe_lvar) == 0) pecont[["lvar"]] <- NULL # remove if no estimates
  
  lvartab[["rhs"]]      <- .unv(pe_lvar[["rhs"]])
  lvartab[["lhs"]]      <- .unv(pe_lvar[["lhs"]])
  lvartab[["label"]]    <- pe_lvar[["label"]]
  lvartab[["est"]]      <- pe_lvar[["est"]]
  lvartab[["se"]]       <- pe_lvar[["se"]]
  lvartab[["z"]]        <- pe_lvar[["z"]]
  lvartab[["pvalue"]]   <- pe_lvar[["pvalue"]]
  lvartab[["ci.lower"]] <- pe_lvar[["ci.lower"]]
  lvartab[["ci.upper"]] <- pe_lvar[["ci.upper"]]
  
  if (options[["std"]]) {
    lvartab[["std.all"]] <- pe_lvar[["std.all"]]
    lvartab[["std.lv"]]  <- pe_lvar[["std.lv"]]
    lvartab[["std.nox"]] <- pe_lvar[["std.nox"]]
  }
  
  # Latent covariances
  pe_lcov <- pe[pe$op == "~~" & pe$lhs %in% lvnames & pe$rhs %in% lvnames & pe$lhs != pe$rhs,]
  if (nrow(pe_lcov) == 0) pecont[["lcov"]] <- NULL # remove if no estimates
  
  lcovtab[["lhs"]]      <- paste(.unv(pe_lcov[["lhs"]]), "-", .unv(pe_lcov[["rhs"]]))
  lcovtab[["label"]]    <- pe_lcov[["label"]]
  lcovtab[["est"]]      <- pe_lcov[["est"]]
  lcovtab[["se"]]       <- pe_lcov[["se"]]
  lcovtab[["z"]]        <- pe_lcov[["z"]]
  lcovtab[["pvalue"]]   <- pe_lcov[["pvalue"]]
  lcovtab[["ci.lower"]] <- pe_lcov[["ci.lower"]]
  lcovtab[["ci.upper"]] <- pe_lcov[["ci.upper"]]
  
  if (options[["std"]]) {
    lcovtab[["std.all"]] <- pe_lcov[["std.all"]]
    lcovtab[["std.lv"]]  <- pe_lcov[["std.lv"]]
    lcovtab[["std.nox"]] <- pe_lcov[["std.nox"]]
  }
  
  # Residual variances
  pe_var <- pe[pe$op == "~~" & pe$lhs %in% ovnames & pe$lhs == pe$rhs,]
  if (nrow(pe_var) == 0) pecont[["var"]] <- NULL # remove if no estimates
  
  vartab[["rhs"]]      <- .unv(pe_var[["rhs"]])
  vartab[["lhs"]]      <- .unv(pe_var[["lhs"]])
  vartab[["label"]]    <- pe_var[["label"]]
  vartab[["est"]]      <- pe_var[["est"]]
  vartab[["se"]]       <- pe_var[["se"]]
  vartab[["z"]]        <- pe_var[["z"]]
  vartab[["pvalue"]]   <- pe_var[["pvalue"]]
  vartab[["ci.lower"]] <- pe_var[["ci.lower"]]
  vartab[["ci.upper"]] <- pe_var[["ci.upper"]]
  
  if (options[["std"]]) {
    vartab[["std.all"]] <- pe_var[["std.all"]]
    vartab[["std.lv"]]  <- pe_var[["std.lv"]]
    vartab[["std.nox"]] <- pe_var[["std.nox"]]
  }
  
  # Residual covariances
  pe_cov <- pe[pe$op == "~~" & pe$lhs %in% ovnames & pe$rhs %in% ovnames & pe$lhs != pe$rhs,]
  if (nrow(pe_cov) == 0) pecont[["cov"]] <- NULL # remove if no estimates
  
  covtab[["lhs"]]      <- paste(.unv(pe_cov[["lhs"]]), "-", .unv(pe_cov[["rhs"]]))
  covtab[["label"]]    <- pe_cov[["label"]]
  covtab[["est"]]      <- pe_cov[["est"]]
  covtab[["se"]]       <- pe_cov[["se"]]
  covtab[["z"]]        <- pe_cov[["z"]]
  covtab[["pvalue"]]   <- pe_cov[["pvalue"]]
  covtab[["ci.lower"]] <- pe_cov[["ci.lower"]]
  covtab[["ci.upper"]] <- pe_cov[["ci.upper"]]
  
  if (options[["std"]]) {
    covtab[["std.all"]] <- pe_cov[["std.all"]]
    covtab[["std.lv"]]  <- pe_cov[["std.lv"]]
    covtab[["std.nox"]] <- pe_cov[["std.nox"]]
  }
  
  
  # Means
  if (options[["meanstructure"]]) {
    pe_mu <- pe[pe$op == "~1",]
    mutab[["lhs"]] <- .unv(pe_mu[["lhs"]])
    mutab[["label"]]    <- pe_mu[["label"]]
    mutab[["est"]]      <- pe_mu[["est"]]
    mutab[["se"]]       <- pe_mu[["se"]]
    mutab[["z"]]        <- pe_mu[["z"]]
    mutab[["pvalue"]]   <- pe_mu[["pvalue"]]
    mutab[["ci.lower"]] <- pe_mu[["ci.lower"]]
    mutab[["ci.upper"]] <- pe_mu[["ci.upper"]]
    if (options[["std"]]) {
      mutab[["std.all"]] <- pe_mu[["std.all"]]
      mutab[["std.lv"]]  <- pe_mu[["std.lv"]]
      mutab[["std.nox"]] <- pe_mu[["std.nox"]]
    }
  }
}

.semAdditionalFits <- function(modelContainer, dataset, options, ready) {
  if (!options[["outputAdditionalFitMeasures"]] || !is.null(modelContainer[["addfit"]])) return()
  
  fitms <- createJaspContainer(gettext("Additional fit measures"))
  fitms$dependOn("outputAdditionalFitMeasures")
  fitms$position <- 0.5
  
  # Fit indices
  fitms[["indices"]] <- fitin <- createJaspTable(gettext("Fit indices"))
  fitin$addColumnInfo(name = "index", title = gettext("Index"), type = "string")
  if (length(options[["models"]]) < 2) {
    fitin$addColumnInfo(name = "value", title = gettext("Value"), type = "number", format = "sf:4;dp:3")
  } else {
    for (i in seq_along(options[["models"]])) {
      fitin$addColumnInfo(name = paste0("value_", i), title = options[["models"]][[i]][["modelName"]], type = "number", 
                          format = "sf:4;dp:3")
    }
  }
  fitin$setExpectedSize(rows = 1, cols = 2)
  
  # information criteria
  fitms[["incrits"]] <- fitic <- createJaspTable(gettext("Information criteria"))
  fitic$addColumnInfo(name = "index", title = "",               type = "string")
  if (length(options[["models"]]) < 2) {
    fitic$addColumnInfo(name = "value", title = gettext("Value"), type = "number", format = "sf:4;dp:3")
  } else {
    for (i in seq_along(options[["models"]])) {
      fitic$addColumnInfo(name = paste0("value_", i), title = options[["models"]][[i]][["modelName"]], type = "number", 
                          format = "sf:4;dp:3")
    }
  }
  fitic$setExpectedSize(rows = 1, cols = 2)
  
  # other fit measures
  fitms[["others"]] <- fitot <- createJaspTable(gettext("Other fit measures"))
  fitot$addColumnInfo(name = "index", title = gettext("Metric"), type = "string")
  if (length(options[["models"]]) < 2) {
    fitot$addColumnInfo(name = "value", title = gettext("Value"), type = "number", format = "sf:4;dp:3")
  } else {
    for (i in seq_along(options[["models"]])) {
      fitot$addColumnInfo(name = paste0("value_", i), title = options[["models"]][[i]][["modelName"]], type = "number", 
                          format = "sf:4;dp:3")
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
  if (!options[["outputRSquared"]] || !is.null(modelContainer[["rsquared"]])) return()
  
  tabr2 <- createJaspTable(gettext("R-Squared"))
  tabr2$addColumnInfo(name = "__var__", title = "", type = "string")
  if (length(options[["models"]]) < 2) {
    tabr2$addColumnInfo(name = "rsq", title = "R\u00B2", type = "number", format = "sf:4;dp:3")
  } else {
    for (i in seq_along(options[["models"]])) {
      tabr2$addColumnInfo(name = paste0("rsq_", i), title = options[["models"]][[i]][["modelName"]], 
                          overtitle = "R\u00B2", type = "number", format = "sf:4;dp:3")
    }
  }
  
  tabr2$dependOn(options = "outputRSquared")
  tabr2$position <- .75
  
  modelContainer[["rsquared"]] <- tabr2 
  
  if (!ready || modelContainer$getError()) return()
  
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
}

.semCov <- function(modelContainer, dataset, options, ready) {
  if (!(options[["outputObservedCovariances"]] || options[["outputImpliedCovariances"]] || 
        options[["outputResidualCovariances"]]) || !is.null(modelContainer[["covars"]])) return()
  
  covars <- createJaspContainer("Covariance tables")
  covars$position <- 3
  covars$dependOn(c("outputObservedCovariances", "outputImpliedCovariances", "outputResidualCovariances"))
  
  modelContainer[["covars"]] <- covars
  
  if (length(options[["models"]]) < 2) {
    .semCovTables(modelContainer[["results"]][["object"]][[1]], NULL, covars, options, ready)
  } else {
    
    for (i in seq_along(options[["models"]])) {
      fit <- modelContainer[["results"]][["object"]][[i]]
      modelname <- options[["models"]][[i]][["modelName"]]
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
  
  if (options[["outputObservedCovariances"]]) {
    octab <- createJaspTable("Observed covariance matrix")
    octab$dependOn("outputObservedCovariances")
    octab$position <- 1
    cocont[["observed"]] <- octab
  }
  
  if (options[["outputImpliedCovariances"]]) {
    ictab <- createJaspTable("Implied covariance matrix")
    ictab$dependOn("outputImpliedCovariances")
    ictab$position <- 2
    cocont[["implied"]] <- ictab
  }
  
  if (options[["outputResidualCovariances"]]) {
    rctab <- createJaspTable("Residual covariance matrix")
    rctab$dependOn("outputResidualCovariances")
    rctab$position <- 3
    cocont[["residual"]] <- rctab
  }
    
  if (!ready || !inherits(fit, "lavaan")) return()
  
  
  if (options[["outputObservedCovariances"]]) {
    # actually compute the observed covariance
    ov <- lavaan::inspect(fit, "sampstat")
    oc <- ov$cov
    oc[upper.tri(oc)] <- NA
    
    for (i in 1:ncol(oc)) {
      nm <- colnames(oc)[i]
      octab$addColumnInfo(nm, title = .unv(nm), type = "number", format = "sf:4;dp:3")
    }
    octab$addRows(oc, rowNames = colnames(oc))
  }
  
  if (options[["outputImpliedCovariances"]]) {
    # actually compute the implied covariance
    fv <- lavaan::fitted.values(fit)
    ic <- fv$cov
    ic[upper.tri(ic)] <- NA
    
    for (i in 1:ncol(ic)) {
      nm <- colnames(ic)[i]
      ictab$addColumnInfo(nm, title = .unv(nm), type = "number", format = "sf:4;dp:3")
    }
    ictab$addRows(ic, rowNames = colnames(ic))
  }
  
  if (options[["outputResidualCovariances"]]) {
    # actually compute the implied covariance
    rv <- lavaan::residuals(fit)
    rc <- rv$cov
    rc[upper.tri(rc)] <- NA
    
    for (i in 1:ncol(rc)) {
      nm <- colnames(rc)[i]
      rctab$addColumnInfo(nm, title = .unv(nm), type = "number", format = "sf:4;dp:3")
    }
    rctab$addRows(rc, rowNames = colnames(rc))
    
  }
  
  if (!is.null(modelname)) {
    parentContainer[[modelname]] <- cocont
  }
  
  return()
}


.semResidualCovTable <- function(modelContainer, dataset, options, ready) {
  if (!options[["residCov"]]) return()
  tab <- createJaspTable("Residual covariance matrix")
  tab$dependOn("residCov")
  tab$position <- 5
  modelContainer[["rescov"]] <- tab
  
  if (!ready || modelContainer$getError()) return()
  
  # actually compute the implied covariance
  rv <- lavaan::residuals(modelContainer[["model"]][["object"]])
  rc <- rv$cov
  rc[upper.tri(rc)] <- NA
  
  for (i in 1:ncol(rc)) {
    nm <- colnames(rc)[i]
    tab$addColumnInfo(nm, title = .unv(nm), type = "number", format = "sf:4;dp:3")
  }
  tab$addRows(rc, rowNames = colnames(rc))
  
  return()
}




