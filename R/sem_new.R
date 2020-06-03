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

.semModelContainer <- function(jaspResults, options) {
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

.semComputeResults <- function(jaspResults, modelContainer, dataset, options) {
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
      modelContainer$setError(paste0("Error in ", options[["models"]][[i]][["modelName"]], ": ", 
                                    .decodeVarsInMessage(names(dataset), errmsg)))
    }
    
    if (options[["se"]] == "bootstrap") {
      fit <- lavBootstrap(fit, options[["errorCalculationBootstrapSamples"]])
    }
    results[[i]] <- fit
  }
  
  # store in model container
  deps <- c("meanstructure", "int.ov.free", "int.lv.free", "fixed.x", "orthogonal", "std.lv", "effect.coding", 
            "auto.fix.first", "auto.fix.single", "auto.var", "auto.cov.lv.x", "auto.cov.y", "auto.th", "auto.delta", 
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
  
  fittab$addColumnInfo(name = "Model",   title = "",                            type = "string" )
  fittab$addColumnInfo(name = "Df",      title = gettext("df"),                 type = "integer")
  fittab$addColumnInfo(name = "AIC",     title = gettext("AIC"),                type = "number" )
  fittab$addColumnInfo(name = "BIC",     title = gettext("BIC"),                type = "number" )
  fittab$addColumnInfo(name = "Chisq",   title = gettext("&#967;&sup2;"),       type = "number" )
  fittab$addColumnInfo(name = "dchisq",  title = gettext("&#916;&#967;&sup2;"), type = "number" )
  fittab$addColumnInfo(name = "ddf",     title = gettext("&#916;df"),           type = "integer")
  fittab$addColumnInfo(name = "PrChisq", title = gettext("p"),                  type = "pvalue" )
  
  modelContainer[["fittab"]] <- fittab
  
  if (!ready) return()
  
  # add data to the table!
  semResults <- .semComputeResults(jaspResults, modelContainer, dataset, options)
  
  if (modelContainer$getError()) return()
  
  # create args for likelihood-ratio test (anova table)
  lrt_args <- semResults
  names(lrt_args) <- "object" # (the first result is object, the others ...)
  lrt_args[["model.names"]] <- vapply(options[["models"]], getElement, name = "modelName", "")
  lrt <- do.call(lavaan::lavTestLRT, lrt_args)
  
  fittab[["Model"]]   <- rownames(lrt)
  fittab[["Df"]]      <- lrt[["Df"]]
  fittab[["AIC"]]     <- lrt[["AIC"]]
  fittab[["BIC"]]     <- lrt[["BIC"]]
  fittab[["Chisq"]]   <- lrt[["Chisq"]]
  fittab[["dchisq"]]  <- lrt[["Chisq diff"]]
  fittab[["ddf"]]     <- lrt[["Df diff"]]
  fittab[["PrChisq"]] <- lrt[["Pr(>Chisq)"]]
}

.semParameters <- function(modelContainer, dataset, options, ready) {
  if (!is.null(modelContainer[["params"]])) return()
  
  params <- createJaspContainer("Parameter estimates")
  params$position <- 1
  params$dependOn("")
  
  
  
}

.semFitToParamContainer <- function(parentContainer, fit, options) {
  
}