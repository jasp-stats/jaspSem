

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

SEMInternal <- function(jaspResults, dataset, options, ...) {
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
  .semAve(modelContainer, dataset, options, ready)
  .semReliability(modelContainer, dataset, options, ready)
  .semHtmt(modelContainer, dataset, options, ready)
  .semMardiasCoefficient(modelContainer, dataset, options, ready)
  .semCov(modelContainer, dataset, options, ready)
  .semMI(modelContainer, datset, options, ready)
  .semSensitivity(modelContainer, dataset, options, ready)
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

  if(options[["dataType"]] == "raw") {
    variablesToRead <- if (options[["group"]] == "") character() else options[["group"]]
    for (model in options[["models"]])
      variablesToRead <- unique(c(variablesToRead, model[["columns"]]))

    dataset <- .readDataSetToEnd(columns = variablesToRead)
  } else {
    dataset <- .readDataSetToEnd(all.columns = TRUE)
  }

  return(dataset)
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

  if (ncol(dataset) > 0) {
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
    groupfac <- factor(dataset[[options[["group"]]]])
    factab <- table(groupfac)
    if (any(factab < 3)) {
      violations <- names(table(groupfac))[table(groupfac) < 3]
      .quitAnalysis(gettextf("Grouping variable has fewer than 3 observations in group %s",
                             paste(violations, collapse = ", ")))

    }
  }

  # Check variance covariance matrix input and its implications:
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

  # Check if meanstructure is true but then no checkbox to fix the intercepts to zero is checked
  if (options[["meanStructure"]]) {
    if (!any(c(options[["manifestInterceptFixedToZero"]], options[["latentInterceptFixedToZero"]],
               options[["manifestMeanFixedToZero"]]))) {
      .quitAnalysis(gettext("When mean structure is included, at least one of the checkboxes to fix the intercepts to zero has to be checked"))
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
                              "dependentCorrelation", "threshold", "scalingParameter", "efaConstrained", "standardizedVariable", "naAction", "estimator", "modelTest",
                              "errorCalculationMethod", "informationMatrix", "emulation", "group", "equalLoading", "equalIntercept",
                              "equalResidual", "equalResidualCovariance", "equalMean", "equalThreshold", "equalRegression",
                              "equalLatentVariance", "equalLatentCovariance", "dataType", "sampleSize", "freeParameters", "manifestMeanFixedToZero"))
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
    originalSyntax   <- .semTranslateModel(options[["models"]][[i]][["syntax"]], dataset)
    if(options[["group"]] == "")
      syntaxTable <- lavaan::lavaanify(originalSyntax)
    if (options[["group"]] != "") {
      fit <- lavaan::sem(model = originalSyntax, data = dataset, group = options[["group"]])
      syntaxTable <- lavaan::parTable(fit)
    }

    if(nrow(syntaxTable[syntaxTable$op == ":=",]) == 0) {
      regressions <- syntaxTable[syntaxTable$op == "~",]
      if (nrow(regressions) > 0) {
        syntax <- .semEffectsSyntax(originalSyntax, syntaxTable, regressions, dataset, options)
      }
    }

    if (exists("syntax")) {
      lav_args[["model"]] <- syntax
    } else {
      lav_args[["model"]] <- originalSyntax
    }
    if (options[["dataType"]] == "raw") {
      lav_args[["data"]]  <- dataset
    } else {
      lav_args[["sample.cov"]] <- .semDataCovariance(dataset, options[["models"]][[i]][["syntax"]])
      lav_args[["sample.nobs"]] <- options[["sampleSize"]]
    }

    # fit the enriched model
    fit <- try(do.call(lavaan::lavaan, lav_args))
    if (isTryError(fit)) { # if try-error, fit original model
      lav_args[["model"]] <- originalSyntax
      fit <- try(do.call(lavaan::lavaan, lav_args))
    }

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
    modelContainer[["originalSyntax"]] <- createJaspState(list(syntaxTable))
    modelContainer[["originalSyntax"]]$dependOn(optionsFromObject = modelContainer)
  }

  return(results)
}

.semDataCovariance <- function(dataset, syntax) {
  usedvars <- .semGetUsedVars(syntax, colnames(dataset))
  var_idx  <- match(usedvars, colnames(dataset))
  mat <- try(as.matrix(dataset[var_idx, var_idx]))
  if (inherits(mat, "try-error") || any(is.na(mat)))
    .quitAnalysis("Input data does not seem to be a covariance matrix! Please check the format of the input data.
                   All cells must be numeric, and the number of rows must equal the number of columns.")
  .hasErrors(mat, type = "varCovMatrix", message='default', exitAnalysisIfErrors = TRUE)

  colnames(mat) <- rownames(mat) <- colnames(dataset)[var_idx]
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
  lavopts[["effect.coding"]]   <- ifelse(options[["factorScaling"]] == "effectCoding", TRUE,
                                         ifelse(options[["manifestMeanFixedToZero"]], "intercepts", FALSE))
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
  lavopts[["test"]]        <- ifelse(options[["modelTest"]] == "satorraBentler", "satorra.bentler",
                                     ifelse(options[["modelTest"]] == "yuanBentler", "yuan.bentler",
                                            ifelse(options[["modelTest"]] == "meanAndVarianceAdjusted", "mean.var.adjusted",
                                                   ifelse(options[["modelTest"]] == "scaledAndShifted", "scaled.shifted",
                                                          ifelse(options[["modelTest"]] == "bollenStine", "bollen.stine",
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
    options[["equalLatentVariance"]],
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
    lavopts[["group"]] <- options[["group"]]
  }

  # sampling weights
  if (options[["samplingWeights"]] != "") {
    lavopts[["sampling.weights"]] <- options[["samplingWeights"]]
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

.semEffectsSyntax <- function(originalSyntax, syntaxTable, regressions, dataset, options) {

    if(options[["group"]] == "") {
    regressionLabels <- letters[1:nrow(regressions)]
    if (!any(syntaxTable[, "label"] %in% regressionLabels)) {
      regressions[, "label"] <- letters[1:nrow(regressions)]
    } else {
      while (any(syntaxTable[, "label"] %in% regressionLabels)) {
        regressionLabels <- paste0(sample(letters, nrow(regressions)), sample(letters, nrow(regressions)))
      }
      regressions[, "label"] <- regressionLabels
    }
  } else {
    groups <- unique(dataset[, options[["group"]]])
    groupLevels <- length(groups)
    regressions <- regressions[1:(nrow(regressions) / groupLevels),]
    regressionsPerGroup <- list()
    labelsPerGroup <- list()
    for (group in seq_along(groups)) {
      regressionsPerGroup[[group]] <- regressions
      labelsPerGroup[[group]] <- paste(letters[1:nrow(regressions)], group, sep = "")
    }
    if (!any(syntaxTable[, "label"] %in% unlist(labelsPerGroup))) {
      for (group in seq_along(groups)) {
        regressionsPerGroup[[group]][, "label"] <- labelsPerGroup[[group]]
        labelsLetters <- NULL
      }
    } else {
      while (any(syntaxTable[, "label"] %in% unlist(labelsPerGroup))) {
        labelsLetters <- paste0(sample(letters, nrow(regressions)), sample(letters, nrow(regressions)))
        for (group in seq_along(groups)) {
          labelsPerGroup[[group]] <- paste(labelsLetters, group, sep = "")
        }
        if(!any(syntaxTable[, "label"] %in% unlist(labelsPerGroup))) {
          for (group in seq_along(groups)) {
            regressionsPerGroup[[group]][, "label"] <- labelsPerGroup[[group]]
          }
        }
      }
    }

    create_group_vector <- function(letter, n) {
      groups <- paste(letter, 1:n, sep="")
      groups <- paste0(groups, collapse = ", ")
      groups <- paste0("c(", groups, ")")
      return(groups)
    }

    if(is.null(labelsLetters)) {
      for (label in 1:nrow(regressions)) {
        regressions[label, "label"] <- create_group_vector(letters[label], groupLevels)
      }
    } else {
      for (label in seq_along(labelsLetters)) {
        regressions[label, "label"] <- create_group_vector(labelsLetters[label], groupLevels)
      }
    }
  }

  # add labels to syntax
  syntax_splitted <- stringr::str_split_1(originalSyntax, "\n")
  syntax_splitted <- unlist(lapply(syntax_splitted, trimws))

  for (j in 1:nrow(regressions)) {
    idx <- unlist(lapply(syntax_splitted, function(x) {
      all(c(startsWith(x, regressions[j, "lhs"]), grepl(regressions[j, "rhs"], x)))
    }))
    syntax_splitted[idx] <- gsub(regressions[j, "rhs"], paste0(regressions[j, "label"], "*", regressions[j, "rhs"]), syntax_splitted[idx])
  }
  syntax <- paste0(syntax_splitted, collapse = "\n")

  if(options[["group"]] != "") {
    regressions <- regressionsPerGroup
  } else {
    groups <- 1
    regressions <- list(regressions)
  }

  get_indirect_effects <- function(df, current_indirect = "", current_predictor = "", idx){
    outcome <- df$lhs
    predictor <- df$rhs
    label <- df$label
    indirect <- c()

    if (current_predictor == "") {
      current_predictor <- outcome[idx]
      current_indirect <- label[idx]
    }

    for (row in 1:nrow(df)){
      if (current_predictor == predictor[row]){
        indirect_effect <- paste0(current_indirect, "*", label[row])
        indirect <- c(indirect, indirect_effect)
        indirect <- c(indirect, get_indirect_effects(df, indirect_effect, outcome[row]))
      }
    }

    return(indirect)
  }

  for (group in seq_along(groups)) {
    # enrich model
    indirect_effects <- list()
    for (idx in 1:nrow(regressions[[group]])) {
      indirect_effects[[idx]] <- get_indirect_effects(regressions[[group]], idx = idx)
    }
    indirect_effects <- unlist(indirect_effects)
    indirect_effects_with_parentheses <- unlist(lapply(indirect_effects, function(x) {
      paste0("(", x, ")")
    }))
    pred <- c()
    out <- c()
    if (length(indirect_effects) > 0) {
      indirect_effects_splitted <- lapply(strsplit(indirect_effects, split = "\\*"), as.list)
      effect_names <- c()
      for (effect in 1:length(indirect_effects_splitted)) {
        for (label in 1:length(indirect_effects_splitted[[effect]])) {
          if (label == 1) {
            pred <- c(pred, regressions[[group]][regressions[[group]]$label == indirect_effects_splitted[[effect]][[label]], "rhs"])
            effect_name <- paste0(regressions[[group]][regressions[[group]]$label == indirect_effects_splitted[[effect]][[label]], "rhs"], "_", regressions[[group]][regressions[[group]]$label == indirect_effects_splitted[[effect]][[label]], "lhs"], "_", groups[group])
          } else if (label == length(indirect_effects_splitted[[effect]])) {
            out <- c(out, regressions[[group]][regressions[[group]]$label == indirect_effects_splitted[[effect]][[label]], "lhs"])
            effect_name <- paste0(effect_name, "_", regressions[[group]][regressions[[group]]$label == indirect_effects_splitted[[effect]][[label]], "lhs"])
          } else {
            effect_name <- paste0(effect_name, "_", regressions[[group]][regressions[[group]]$label == indirect_effects_splitted[[effect]][[label]], "lhs"])
          }
        }
        effect_names <- c(effect_names, effect_name)
      }
      names(indirect_effects) <- effect_names
    }

    total_effects <- c()
    for (pred in unique(c(pred,regressions[[group]][["rhs"]]))) {
      for (out in unique(c(out, regressions[[group]][["lhs"]]))) {
        total_effect <- NULL
        total_effect_name <- paste0("total_", groups[group], "_", pred, "_", out)
        indirect_effect_idx <- unlist(lapply(names(indirect_effects), function(x) {
          startsWith(x, pred) && endsWith(x, out)
        }))
        if(sum(indirect_effect_idx > 0)) {
          total_effect <- paste(indirect_effects_with_parentheses[indirect_effect_idx], collapse = "+")
        }
        direct_effect <- regressions[[group]][regressions[[group]][["rhs"]] == pred,]
        direct_effect <- direct_effect[direct_effect$lhs == out,]
        if (sum(indirect_effect_idx) > 0 && nrow(direct_effect) > 0)
          total_effect <- paste0(total_effect, "+", direct_effect[["label"]])
        if (sum(indirect_effect_idx) == 0 && nrow(direct_effect) > 0)
          total_effect <- direct_effect[["label"]]

        if(length(total_effect) > 0) {
          names(total_effect) <- total_effect_name
          total_effects <- c(total_effects, total_effect)
        }
      }
    }
    effects_all <- c(indirect_effects, total_effects)
    for (effect in seq_along(effects_all))
      syntax <- paste0(syntax, "\n", names(effects_all)[effect], " := ", effects_all[effect], "\n")
  }
  return(syntax)
}


# output functions

.semFitTab <- function(jaspResults, modelContainer, dataset, options, ready) {
  if (!is.null(modelContainer[["fittab"]])) return()

  fittab <- createJaspTable(title = gettext("Model fit"))
  fittab$dependOn("models")
  fittab$position <- 0

  fittab$addColumnInfo(name = "Model",    title = "",                            type = "string" , combine = TRUE)

  if (options[["group"]] != "") {
    fittab$addColumnInfo(name = "group",    title = gettext("Group"),              type = "string" )
  }
  fittab$addColumnInfo(name = "AIC",      title = gettext("AIC"),                type = "number" )
  fittab$addColumnInfo(name = "BIC",      title = gettext("BIC"),                type = "number" )
  fittab$addColumnInfo(name = "N",        title = gettext("n(Observations)"),    type = "integer")
  fittab$addColumnInfo(name = "npar",     title = gettext("Total"),              overtitle = gettext("n(Parameters)"), type = "integer")
  fittab$addColumnInfo(name = "nfree",    title = gettext("Free"),               overtitle = gettext("n(Parameters)"), type = "integer")
  fittab$addColumnInfo(name = "Chisq",    title = gettext("&#967;&sup2;"),       type = "number" ,
                       overtitle = gettext("Baseline test"))
  fittab$addColumnInfo(name = "Df",       title = gettext("df"),                 type = "integer",
                       overtitle = gettext("Baseline test"))
  fittab$addColumnInfo(name = "PrChisq",  title = gettext("p"),                  type = "pvalue",
                       overtitle = gettext("Baseline test"))

  if (length(options[["models"]]) > 1) {
    fittab$addColumnInfo(name = "dchisq",   title = "\u0394\u03C7\u00B2", type = "number" ,
                         overtitle = gettext("Difference test"))
    fittab$addColumnInfo(name = "ddf",      title = gettextf("%1$sdf", "\u0394"),           type = "integer",
                         overtitle = gettext("Difference test"))
    fittab$addColumnInfo(name = "dPrChisq", title = gettext("p"),                  type = "pvalue" ,
                         overtitle = gettext("Difference test"))
  }

  modelContainer[["fittab"]] <- fittab

  if (!ready) return()

  # add data to the table!
  semResults <- .semComputeResults(modelContainer, dataset, options)

  if (modelContainer$getError()) return()

  testName <- switch(options[["modelTest"]],
                     "satorraBentler" = "satorra.bentler",
                     "yuanBentler" = "yuan.bentler",
                     "scaledAndShifted" = "scaled.shifted",
                     "meanAndVarianceAdjusted" = "mean.var.adjusted",
                     "bollenStine" = "bollen.stine",
                     options[["modelTest"]])
  if (testName == "default")
    testName <- "standard"

  if (length(semResults) == 1) {
    lrt <- .withWarnings(lavaan::lavTestLRT(semResults[[1]], type = "Chisq")[-1, ])
    chiSq <- lavaan::lavInspect(semResults[[1]], what = "test")[[testName]]$stat
    dfs <- lavaan::lavInspect(semResults[[1]], what = "test")[[testName]]$df
    rownames(lrt$value) <- options[["models"]][[1]][["name"]]
    Ns <- lavaan::lavInspect(semResults[[1]], "ntotal")
    npar <- lavaan::lavInspect(semResults[[1]], "npar")
    nfree <- if (length(semResults[[1]]@Model@eq.constraints.K) == 0) lavaan::lavInspect(semResults[[1]], "npar") else length(semResults[[1]]@Model@eq.constraints.K[1,])
  } else {
    Ns <- vapply(semResults, lavaan::lavInspect, 0, what = "ntotal")
    npar <- vapply(semResults, lavaan::lavInspect, 0, what = "npar") # without eq constraints
    nfree <- sapply(semResults, function(x) x@loglik$npar) # with eq constraints

    lrt_args <- semResults
    names(lrt_args) <- "object" # (the first result is object, the others ...)
    lrt_args[["model.names"]] <- vapply(options[["models"]], getElement, name = "name", "")
    lrt_args[["type"]] <- "Chisq"
    lrt <- .withWarnings(do.call(lavaan::lavTestLRT, lrt_args))

    chiSq <- unlist(lapply(semResults, function(x) {lavaan::lavInspect(x, what = "test")[[testName]]$stat}))
    dfs <- unlist(lapply(semResults, function(x) {round(lavaan::lavInspect(x, what = "test")[[testName]]$df, 3)}))
    # because the LRT orders the models according to the df, we need to reorder this as well
    chiSq <- chiSq[order(dfs)]
    dfs <- sort(dfs)

  }

  dtFill <- data.frame(matrix(ncol = 0, nrow = length(rownames(lrt$value))))

  dtFill[["Model"]]    <- rownames(lrt$value)
  dtFill[["AIC"]]      <- lrt$value[["AIC"]]
  dtFill[["BIC"]]      <- lrt$value[["BIC"]]
  dtFill[["N"]]        <- Ns
  dtFill[["npar"]]     <- npar
  dtFill[["nfree"]]    <- nfree
  dtFill[["Chisq"]]    <- chiSq
  dtFill[["Df"]]       <- dfs
  dtFill[["PrChisq"]]  <- pchisq(q = chiSq, df = dfs, lower.tail = FALSE)

  if (length(options[["models"]]) > 1) {
    dtFill[["dchisq"]]   <- lrt$value[["Chisq diff"]]
    dtFill[["ddf"]]      <- lrt$value[["Df diff"]]
    dtFill[["dPrChisq"]] <- lrt$value[["Pr(>Chisq)"]]
  }

  # add warning footnote
  fnote <- ""
  if (!is.null(lrt$warnings)) {
    fnote <- paste0(fnote, gsub("lavaan WARNING: ", "", lrt$warnings[[1]]$message))
  }

  if(options$naAction == "listwise"){
    nrm <- nrow(dataset) - lavaan::lavInspect(semResults[[1]], "ntotal")
    if (nrm != 0) {
      missingFootnote <- gettextf("A total of %g cases were removed due to missing values. You can avoid this by choosing 'FIML' under 'Missing Data Handling' in the Estimation options.",
                                  nrm)
      fnote <- paste0(fnote, missingFootnote)
    }
  }

  # add test statistic correction footnote
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
    if (length(semResults) == 1)
      ftext <- gettextf("Baseline tests based on %s.", testname)
    if (length(semResults) > 1)
      ftext <- gettextf("Baseline tests based on %s. Difference tests based on a function of two standard test-statistics.", testname)

    fnote <- paste0(fnote, ftext)

  }

  if (options$estimator %in% c("dwls", "gls", "wls", "uls")) {
    fnote <- paste0(fnote, gettext("The AIC, BIC and additional information criteria are only available with ML-type estimators"))
  }

  if (options[["group"]] != "") {

    groupNames <- semResults[[1]]@Data@group.label
    models <- rep(rownames(lrt$value), each = length(groupNames))
    modelDfs <- unlist(lapply(semResults, function(x) {lavaan::lavInspect(x, what = "test")[[testName]]$df}))
    ord <- match(modelDfs, sort(modelDfs))
    modelDfs <- modelDfs[ord]

    chiSq <- sapply(semResults, function(x) {lavaan::lavInspect(x, what = "test")[[testName]]$stat.group})
    logLGroup <- sapply(semResults, function(x) x@loglik$loglik.group)

    nfree <- sapply(semResults, function(x) x@loglik$npar)
    npar <- sapply(semResults, lavaan::lavInspect, 0, what = "npar") # without eq constraints

    aics <- -2 * logLGroup + 2 * matrix(npar, nrow(logLGroup), ncol(logLGroup), byrow = TRUE)
    Ns  <- sapply(semResults, function(x) x@Data@nobs)
    bics <- -2 * logLGroup + matrix(npar, nrow(logLGroup), ncol(logLGroup), byrow = TRUE) * matrix(sapply(Ns, log), nrow(Ns), ncol(Ns))

    aics <- aics[, ord]
    bics <- bics[, ord]
    chiSq <- chiSq[, ord]
    modelDfsRep <- rep(modelDfs, each = length(groupNames))

    dtFillGroup <- data.frame(matrix(ncol = 0, nrow = length(models)))

    dtFillGroup[["Model"]]    <- models
    dtFillGroup[["group"]]    <- rep(groupNames, length(rownames(lrt$value)))
    dtFillGroup[["AIC"]]      <- c(aics)
    dtFillGroup[["BIC"]]      <- c(bics)
    dtFillGroup[["N"]]        <- c(Ns)
    dtFillGroup[["npar"]]     <- c(npar)
    dtFillGroup[["nfree"]]    <- c(nfree)
    dtFillGroup[["Chisq"]]    <- c(chiSq)
    dtFillGroup[["Df"]]       <- c(modelDfsRep)

    dtFillGroup[["PrChisq"]]  <- apply(data.frame(c(chiSq), modelDfsRep), 1, function(x) {pchisq(x[1], x[2], lower.tail = FALSE)})

    # we want the LRT for multiple models
    if (length(semResults) > 1) {

      nGroups <- length(groupNames)
      dchisq <- diff(c(chiSq), lag = nGroups)
      ddf <- diff(c(modelDfsRep), lag = nGroups)
      dprchisq <- apply(data.frame(dchisq, ddf), 1, function(x) {pchisq(x[1], x[2], lower.tail = FALSE)})

      dtFillGroup[["dchisq"]] <- c(rep(NA, nGroups), dchisq)
      dtFillGroup[["ddf"]] <- c(rep(NA, nGroups), ddf)
      dtFillGroup[["dPrChisq"]] <- c(rep(NA, nGroups), dprchisq)

    }
    dtFill[["group"]] <- gettext("all")
    dtFill <- dtFill[, c(1, ncol(dtFill), 2:(ncol(dtFill)-1))]
    dtFill <- rbind(dtFill, dtFillGroup)

  }

  fittab$setData(dtFill)
  fittab$addFootnote(message = fnote)

}

.semParameters <- function(modelContainer, dataset, options, ready) {
  if (!is.null(modelContainer[["params"]])) return()


  params <- createJaspContainer(gettext("Parameter estimates"))
  params$position <- 1
  params$dependOn(c("ciLevel", "bootstrapCiType", "standardizedEstimate", "models"))

  modelContainer[["params"]] <- params

  if (length(options[["models"]]) < 2) {
    .semParameterTables(modelContainer[["results"]][["object"]][[1]], NULL, params, options, ready, modelContainer, dataset)
  } else {

    for (i in seq_along(options[["models"]])) {
      fit <- modelContainer[["results"]][["object"]][[i]]
      model <- options[["models"]][[i]]
      .semParameterTables(fit, model, params, options, ready, modelContainer, dataset)
    }
  }
}

.semParameterTables <- function(fit, model, parentContainer, options, ready, modelContainer, dataset) {
  if (!ready) return()
  if (is.null(model)) {
    pecont <- parentContainer
  } else {
    pecont <- createJaspContainer(model[["name"]], initCollapsed = TRUE)
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
  }

  pecont[["ind"]] <- indtab

  # Structural Model
  regtab <- createJaspTable(title = gettext("Regression coefficients"))

  if (options[["group"]] != "")
    regtab$addColumnInfo(name = "group",  title = gettext("Group"),      type = "string", combine = TRUE)

  regtab$addColumnInfo(name = "lhs",      title = gettext("Outcome"),    type = "string", combine = TRUE)
  regtab$addColumnInfo(name = "rhs",      title = gettext("Predictor"),  type = "string")
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
  }

  pecont[["cov"]] <- covtab

  allTables <- list(indtab, regtab, lvartab, lcovtab, vartab, covtab)

  # Means
  if (options[["meanStructure"]]) {
    mutab <- createJaspTable(title = gettext("Intercepts"))
    allTables[[length(allTables) + 1]] <- mutab

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
    }

    pecont[["mu"]] <- mutab
  }

  originalSyntaxTable <- modelContainer[["originalSyntax"]][["object"]][[1]]
  if(nrow(originalSyntaxTable[originalSyntaxTable$op == ":=",]) > 0) {
    deftab <- createJaspTable(title = gettext("Defined parameters"))
    allTables[[length(allTables) + 1]] <- deftab

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
    }

    pecont[["def"]] <- deftab
  } else {
    indefftab <- createJaspTable(title = gettext("Indirect effects"))
    allTables[[length(allTables) + 1]] <- indefftab

    if (options[["group"]] != "")
      indefftab$addColumnInfo(name = "group",  title = gettext("Group"),      type = "string", combine = TRUE)

    indefftab$addColumnInfo(name = "path",     title = "",                    type = "string")
    indefftab$addColumnInfo(name = "est",      title = gettext("Estimate"),   type = "number")
    indefftab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number")
    indefftab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number")
    indefftab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "pvalue")
    indefftab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number",
                         overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
    indefftab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                         overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))

    if (options[["standardizedEstimate"]]) {
      indefftab$addColumnInfo(name = "std.all", title = gettext("All"),  type = "number",
                           overtitle = gettext("Standardized"))
      indefftab$addColumnInfo(name = "std.lv",  title = gettext("LV"),   type = "number",
                           overtitle = gettext("Standardized"))
    }

    pecont[["indeff"]] <- indefftab

    totefftab <- createJaspTable(title = gettext("Total effects"))
    allTables[[length(allTables) + 1]] <- totefftab

    if (options[["group"]] != "")
      totefftab$addColumnInfo(name = "group",  title = gettext("Group"),      type = "string", combine = TRUE)

    totefftab$addColumnInfo(name = "path",     title = "",                    type = "string")
    totefftab$addColumnInfo(name = "est",      title = gettext("Estimate"),   type = "number")
    totefftab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number")
    totefftab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number")
    totefftab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "pvalue")
    totefftab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number",
                            overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
    totefftab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number",
                            overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))

    if (options[["standardizedEstimate"]]) {
      totefftab$addColumnInfo(name = "std.all", title = gettext("All"),  type = "number",
                              overtitle = gettext("Standardized"))
      totefftab$addColumnInfo(name = "std.lv",  title = gettext("LV"),   type = "number",
                              overtitle = gettext("Standardized"))
    }

    pecont[["toteff"]] <- totefftab
  }



  if (!is.null(model)) parentContainer[[model[["name"]]]] <- pecont

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

  # the standardized value "std.nox" is conditionally present since lavaan 0.6-18 (see https://lavaan.ugent.be/history/dot6.html)
  # if it exists, add the column info and the column values.
  hasStdNox <- "std.nox" %in% colnames(pe)
  if (hasStdNox) {
    lapply(allTables, function(table) {
      table$addColumnInfo(name = "std.nox", title = gettext("Endo"), type = "number", overtitle = gettext("Standardized"))
    })
  }

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

  indtab[["rhs"]]      <- pe_ind[["rhs"]]
  indtab[["lhs"]]      <- pe_ind[["lhs"]]
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
    if (hasStdNox)
      indtab[["std.nox"]] <- pe_ind[["std.nox"]]
  }

  # Structural model
  # coefficients
  pe_reg <- pe[pe$op == "~",]
  pe_reg <- pe_reg[order(pe_reg[["group"]], pe_reg[["lhs"]]),]
  if (nrow(pe_reg) == 0) pecont[["reg"]] <- NULL # remove if no estimates

  if (options[["group"]] != "")
    regtab[["group"]] <- pe_reg[["groupname"]]

  regtab[["lhs"]]      <- pe_reg[["lhs"]]
  regtab[["rhs"]]      <- pe_reg[["rhs"]]
  if (nrow(originalSyntaxTable[originalSyntaxTable$op == ":=",]) > 0)
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
    if (hasStdNox)
      regtab[["std.nox"]] <- pe_reg[["std.nox"]]
  }

  # Latent variances
  pe_lvar <- pe[pe$op == "~~" & pe$lhs %in% lvnames & pe$lhs == pe$rhs,]
  if (nrow(pe_lvar) == 0) pecont[["lvar"]] <- NULL # remove if no estimates

  if (options[["group"]] != "")
    lvartab[["group"]] <- pe_lvar[["groupname"]]

  lvartab[["rhs"]]      <- pe_lvar[["rhs"]]
  lvartab[["lhs"]]      <- pe_lvar[["lhs"]]
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
    if (hasStdNox)
      lvartab[["std.nox"]] <- pe_lvar[["std.nox"]]
  }

  # Latent covariances
  pe_lcov <- pe[pe$op == "~~" & pe$lhs %in% lvnames & pe$rhs %in% lvnames & pe$lhs != pe$rhs,]
  if (nrow(pe_lcov) == 0) pecont[["lcov"]] <- NULL # remove if no estimates

  if (options[["group"]] != "")
    lcovtab[["group"]] <- pe_lcov[["groupname"]]

  lcovtab[["lhs"]]      <- paste(pe_lcov[["lhs"]], "-", pe_lcov[["rhs"]])
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
    if (hasStdNox)
      lcovtab[["std.nox"]] <- pe_lcov[["std.nox"]]
  }

  # Residual variances
  pe_var <- pe[pe$op == "~~" & pe$lhs %in% ovnames & pe$lhs == pe$rhs,]
  if (nrow(pe_var) == 0) pecont[["var"]] <- NULL # remove if no estimates

  if (options[["group"]] != "")
    vartab[["group"]] <- pe_var[["groupname"]]

  vartab[["rhs"]]      <- pe_var[["rhs"]]
  vartab[["lhs"]]      <- pe_var[["lhs"]]
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
    if (hasStdNox)
      vartab[["std.nox"]] <- pe_var[["std.nox"]]
  }

  # Residual covariances
  pe_cov <- pe[pe$op == "~~" & pe$lhs %in% ovnames & pe$rhs %in% ovnames & pe$lhs != pe$rhs,]
  if (nrow(pe_cov) == 0) pecont[["cov"]] <- NULL # remove if no estimates

  if (options[["group"]] != "")
    covtab[["group"]] <- pe_cov[["groupname"]]

  covtab[["lhs"]]      <- paste(pe_cov[["lhs"]], "-", pe_cov[["rhs"]])
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
    if (hasStdNox)
      covtab[["std.nox"]] <- pe_cov[["std.nox"]]
  }


  # Means
  if (options[["meanStructure"]]) {
    pe_mu <- pe[pe$op == "~1",]

    if (options[["group"]] != "")
      mutab[["group"]] <- pe_mu[["groupname"]]

    mutab[["lhs"]] <- pe_mu[["lhs"]]
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
      if (hasStdNox)
        mutab[["std.nox"]] <- pe_mu[["std.nox"]]
    }
  }

  # defined parameters
  if (nrow(originalSyntaxTable[originalSyntaxTable$op == ":=",]) > 0) {
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
  } else {
    pe_eff <- pe[pe$op == ":=",]
    pe_toteff <- subset(pe_eff, substring(lhs, 1, nchar("total_")) == "total_")
    if (nrow(pe_toteff) == 0) {
      pecont[["toteff"]] <- NULL
      pecont[["indeff"]] <- NULL
      return()
    }

    if (options[["group"]] != "") {
      groups <- unique(dataset[, options[["group"]]])
      groupvec <- c()
      for (group in groups)
        groupvec <- c(groupvec, rep(group, (nrow(pe_toteff) / length(groups))))
    } else {
      groups <- 1
    }

    path <- list()
    for (idx in 1:nrow(pe_toteff)) {
      path[[idx]] <- gsub("_", " \u2192 ", pe_toteff[idx, "lhs"])
      path[[idx]] <- gsub("total \u2192", "", path[[idx]])
      for (group in groups)
        path[[idx]] <- gsub(paste0(" ", group, " \u2192"), "", path[[idx]])

    }

    if (options[["group"]] != "")
      totefftab[["group"]]  <- groupvec
    totefftab[["path"]]     <- path
    totefftab[["est"]]      <- pe_toteff$est
    totefftab[["se"]]       <- pe_toteff$se
    totefftab[["z"]]        <- pe_toteff$z
    totefftab[["pvalue"]]   <- pe_toteff$pvalue
    totefftab[["ci.lower"]] <- pe_toteff$ci.lower
    totefftab[["ci.upper"]] <- pe_toteff$ci.upper

    if (options[["standardizedEstimate"]]) {
      totefftab[["std.all"]] <- pe_toteff[["std.all"]]
      totefftab[["std.lv"]]  <- pe_toteff[["std.lv"]]
      if (hasStdNox)
        totefftab[["std.nox"]] <- pe_toteff[["std.nox"]]
    }

    pe_indeff <- subset(pe_eff, substring(lhs, 1, nchar("total_")) != "total_")
    if (nrow(pe_indeff) == 0) {
      pecont[["indeff"]] <- NULL
      return()
    }

    if (nrow(pe_indeff) > 0) {
      groupvec <- c()
      for (group in groups)
        groupvec <- c(groupvec, rep(group, (nrow(pe_indeff) / length(groups))))

      path <- list()
      for (idx in 1:nrow(pe_indeff)) {
        path[[idx]] <- gsub("_", " \u2192 ", pe_indeff[idx, "lhs"])
        for (group in groups)
          path[[idx]] <- gsub(paste0(" ", group, " \u2192"), "", path[[idx]])
      }

      if (options[["group"]] != "")
        indefftab[["group"]]  <- groupvec
      indefftab[["path"]]     <- path
      indefftab[["est"]]      <- pe_indeff$est
      indefftab[["se"]]       <- pe_indeff$se
      indefftab[["z"]]        <- pe_indeff$z
      indefftab[["pvalue"]]   <- pe_indeff$pvalue
      indefftab[["ci.lower"]] <- pe_indeff$ci.lower
      indefftab[["ci.upper"]] <- pe_indeff$ci.upper

      if (options[["standardizedEstimate"]]) {
        indefftab[["std.all"]] <- pe_indeff[["std.all"]]
        indefftab[["std.lv"]]  <- pe_indeff[["std.lv"]]
        if (hasStdNox)
          indefftab[["std.nox"]] <- pe_indeff[["std.nox"]]
      }
    }
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
  fitin$addCitation("Katerina M. Marcoulides & Ke-Hai Yuan (2017) New Ways to Evaluate Goodness of Fit: A Note on Using Equivalence Testing to Assess Structural Equation Models, Structural Equation Modeling: A Multidisciplinary Journal, 24:1, 148-153, DOI: 10.1080/10705511.2016.1225260")

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
  fitot$addCitation("Katerina M. Marcoulides & Ke-Hai Yuan (2017) New Ways to Evaluate Goodness of Fit: A Note on Using Equivalence Testing to Assess Structural Equation Models, Structural Equation Modeling: A Multidisciplinary Journal, 24:1, 148-153, DOI: 10.1080/10705511.2016.1225260")

  modelContainer[["addfit"]] <- fitms

  if (!ready || modelContainer$getError()) return()

  # actually compute the fit measures

  .fitMeasures <- function(fit, alpha = 0.05) {
    fm <- lavaan::fitMeasures(fit, fit.measures = "all")

    ncp_chi2 <- function(alpha, chisqModel, df){
      z <- qnorm(1-alpha)
      z2 <- z*z
      z3 <- z2*z
      z4 <- z3*z
      z5 <- z4*z
      sig2 <- 2*(2*chisqModel-df+2)
      sig <- sqrt(sig2)
      sig3 <- sig*sig2
      sig4 <- sig2*sig2
      sig5 <- sig4*sig
      sig6 <- sig2*sig4

      delta <- chisqModel-df+2+sig*(z+(z2-1)/sig-z/sig2 + 2*(df-1)*(z2-1)/(3*sig3)
                                    +( -(df-1)*(4*z3-z)/6+(df-2)*z/2 )/sig4
                                    +4*(df-1)*(3*z4+2*z2-11)/(15*sig5)
                                    +(-(df-1)*(96*z5+164*z3-767*z)/90-4*(df-1)*(df-2)*(2*z3-5*z)/9+(df-2)*z/2)/sig6
      )
      delta <- max(delta,0)
      return(delta)
    }

    chisqModel <- c(fm["chisq"], use.names = FALSE)
    chisqBaseline <- c(fm["baseline.chisq"], use.names = FALSE)
    df <- c(fm["df"], use.names = FALSE)
    dfBaseline <- c(fm["baseline.df"], use.names = FALSE)
    n <- lavaan::lavInspect(fit, what = "ntotal")

    delta_t <- ncp_chi2(alpha, chisqModel, df)
    rmsea_t <- sqrt(delta_t / (df*(n-1)))

    delta_t <- ncp_chi2(alpha/2, chisqModel, df)
    delta_bt <- ncp_chi2(1-alpha/2, chisqBaseline, dfBaseline)
    cfi_t <- 1 - max(delta_t, 0) / max(delta_t, delta_bt, 0)

    rmsea_e05 <- exp(2.06034 - 0.62974*log(df) + 0.02512*log(df)*log(df) - 0.98388*log(n-1) + 0.05442*log(n-1)*log(n-1) - 0.00005188*(n-1) + 0.05260*log(df)*log(n-1))
    rmsea_e08 <- exp(2.84129 - 0.54809*log(df) + 0.02296*log(df)*log(df) - 0.76005*log(n-1) + 0.10229*log(n-1)*log(n-1) - 1.11167*((n-1)^.2) + 0.04845*log(df)*log(n-1))

    cfi_e90 <- 1 - exp(5.96633 - .40425*log(df) + .01384*((log(df))^2) - .00411*((log(dfBaseline))^2) - 1.20242*log(n-1) + .18763*((log(n-1))^2) - 2.06704*((n-1)^(1/5)) + .05245*log(df)*log(n-1) - .01533*log(dfBaseline)*log(n-1))
    cfi_e95 <- 1 - exp(4.12132 - .46285*log(df) + .52478*(df^(1/5)) - .31832*((dfBaseline)^(1/5)) - 1.74422*log(n-1) + .13042*((log(n-1))^2) - .02360*((n-1)^(1/2)) + .04215*log(df)*log(n-1))


    fm <- c(fm, rmsea.t = rmsea_t, cfi.t = cfi_t, rmsea.t.e05 = rmsea_e05, rmsea.t.e08 = rmsea_e08, cfi.t.e90 = cfi_e90, cfi.t.e95 = cfi_e95)

    return(fm)
  }

  fmli <- lapply(modelContainer[["results"]][["object"]], .fitMeasures)

  # Fit indices
  cfi_t_footnote <- ""
  fitin[["index"]] <- c(
    gettext("Comparative Fit Index (CFI)"),
    gettext("T-size CFI"),
    gettext("Tucker-Lewis Index (TLI)"),
    gettext("Bentler-Bonett Non-normed Fit Index (NNFI)"),
    gettext("Bentler-Bonett Normed Fit Index (NFI)"),
    gettext("Parsimony Normed Fit Index (PNFI)"),
    gettext("Bollen's Relative Fit Index (RFI)"),
    gettext("Bollen's Incremental Fit Index (IFI)"),
    gettext("Relative Noncentrality Index (RNI)")
  )
  if (length(options[["models"]]) == 1) {
    fitin[["value"]] <- fmli[[1]][c("cfi", "cfi.t", "tli", "nnfi", "nfi", "pnfi", "rfi", "ifi", "rni")]
    cfi_t_footnote <- paste0(cfi_t_footnote,
                             gettextf("T-size CFI is computed for <i>%1$s = 0.05</i>. The T-size equivalents of the
                                      conventional CFI cut-off values (poor < 0.90 < fair < 0.95 < close) are <b>poor
                                      < %2$s < fair < %3$s < close</b> for model: %4$s.", "\u03B1",
                                      round(fmli[[1]]["cfi.t.e90"], 3),
                                      round(fmli[[1]]["cfi.t.e95"], 3),
                                      options[["models"]][[1]][["name"]]))
    fitin$addFootnote(cfi_t_footnote)
  } else {
    for (i in seq_along(options[["models"]])) {
      fitin[[paste0("value_", i)]] <- fmli[[i]][c("cfi", "cfi.t", "tli", "nnfi", "nfi", "pnfi", "rfi", "ifi", "rni")]

      cfi_t_footnote <- paste0(cfi_t_footnote,
                               gettextf(" The T-size equivalents of the conventional CFI cut-off values
                                        (poor < 0.90 < fair < 0.95 < close) are <b>poor < %1$s < fair < %2$s <
                                        close</b> for model: %3$s.",
                                        round(fmli[[i]]["cfi.t.e90"], 3),
                                        round(fmli[[i]]["cfi.t.e95"], 3),
                                        options[["models"]][[i]][["name"]]))

    }
    fitin$addFootnote(cfi_t_footnote)
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
  rmsea_t_footnote <- ""
  fitot[["index"]] <- c(
    gettext("Root mean square error of approximation (RMSEA)"),
    gettextf("RMSEA 90%% CI lower bound"),
    gettextf("RMSEA 90%% CI upper bound"),
    gettext("RMSEA p-value"),
    gettext("T-size RMSEA"),
    gettext("Standardized root mean square residual (SRMR)"),
    gettextf("Hoelter's critical N (%s = .05)","\u03B1"),
    gettextf("Hoelter's critical N (%s = .01)","\u03B1"),
    gettext("Goodness of fit index (GFI)"),
    gettext("McDonald fit index (MFI)"),
    gettext("Expected cross validation index (ECVI)")
  )
  if (length(options[["models"]]) == 1) {
    fitot[["value"]] <- fmli[[1]][c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue", "rmsea.t",
                                    "srmr", "cn_05", "cn_01", "gfi", "mfi", "ecvi")]
    rmsea_t_footnote <- paste0(rmsea_t_footnote,
                               gettextf("T-size RMSEA is computed for <i>%1$s = 0.05</i>. The T-size equivalents of the
                                        conventional RMSEA cut-off values (close < 0.05 < fair < 0.08 < poor) are
                                        <b>close < %2$s < fair < %3$s < poor</b> for model: %4$s.", "\u03B1",
                                        round(fmli[[1]]["rmsea.t.e05"], 3),
                                        round(fmli[[1]]["rmsea.t.e08"], 3),
                                        options[["models"]][[1]][["name"]]))
    fitot$addFootnote(rmsea_t_footnote)
  } else {
    for (i in seq_along(options[["models"]])) {
      fitot[[paste0("value_", i)]] <- fmli[[i]][c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue", "rmsea.t",
                                                  "srmr", "cn_05", "cn_01", "gfi", "mfi", "ecvi")]

      rmsea_t_footnote <- paste0(rmsea_t_footnote,
                                 gettextf(" The T-size equivalents of the conventional RMSEA cut-off values
                                          (close < 0.05 < fair < 0.08 < poor) are <b>close < %1$s < fair < %2$s
                                          < poor</b> for model: %3$s.",
                                          round(fmli[[i]]["rmsea.t.e05"], 3),
                                          round(fmli[[i]]["rmsea.t.e08"], 3),
                                          options[["models"]][[i]][["name"]]))

    }
    fitot$addFootnote(rmsea_t_footnote)
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
      tabr2[["__var__"]] <- names(r2res)
      tabr2[["rsq"]]     <- r2res

    } else {

      # determine variable names
      r2li <- lapply(modelContainer[["results"]][["object"]], lavaan::inspect, what = "r2")

      # generate df with these names
      r2df <- data.frame("varname__" = unique(unlist(lapply(r2li, names))))
      tabr2[["__var__"]] <- unique(unlist(lapply(r2li, names)))

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
      tabr2[["__var__"]] <- unlist(lapply(r2res, names))
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
      tabr2[["__var__"]] <- r2df[["varname__"]]
      for (i in seq_along(r2li)) tabr2[[paste0("rsq_", i)]] <- r2df[[i + 2]]

    }

  }
}

.semAve <- function(modelContainer, dataset, options, ready) {
  if (!options[["ave"]] || !is.null(modelContainer[["AVE"]])) return()

  # init table
  avetab <- createJaspTable(gettext("Average variance extracted"))
  if (options[["group"]] != "")
    avetab$addColumnInfo(name = "group", title = gettext("Group"), type = "string", combine = TRUE)
  avetab$addColumnInfo(name = "factor", title = gettext("Latent"), type = "string")
  if (length(options[["models"]]) < 2) {
    avetab$addColumnInfo(name = "ave", title = gettext("AVE"), type = "number")
  } else {
    for (i in seq_along(options[["models"]])) {
      avetab$addColumnInfo(name = paste0("ave_", i), title = options[["models"]][[i]][["name"]],
                          overtitle = gettext("AVE"), type = "number")
    }
  }

  avetab$dependOn(c("ave", "models"))
  avetab$position <- .9

  modelContainer[["AVE"]] <- avetab

  if (!ready || modelContainer$getError()) return()

  # compute data and fill table
  if (options[["group"]] == "") {

    if (length(options[["models"]]) < 2) {

      ave_result          <- semTools::AVE(modelContainer[["results"]][["object"]][[1]])
      avetab[["factor"]]  <- names(ave_result)
      avetab[["ave"]]     <- ave_result

    } else {

      # determine variable names
      avelist <- lapply(modelContainer[["results"]][["object"]], semTools::AVE)

      # generate df with these names
      avedf <- data.frame("factor" = unique(unlist(lapply(avelist, names))))
      avetab[["factor"]] <- unique(unlist(lapply(avelist, names)))

      for (i in 1:length(avelist)) {
        # fill matching vars from model with df
        avedf[match(names(avelist[[i]]), avedf[["factor"]]), i + 1] <- avelist[[i]]
        # add column to table
        avetab[[paste0("ave_", i)]] <- avedf[[i + 1]]
      }
    }
  } else {
    if (length(options[["models"]]) < 2) {

      ave_result          <- semTools::AVE(modelContainer[["results"]][["object"]][[1]])
      groups <- ave_result[, "group"]
      ave_result <- ave_result[, -1, drop = FALSE]

      avetab[["group"]]   <- rep(groups, rep(ncol(ave_result), length(groups)))
      avetab[["factor"]]  <- rep(names(ave_result), length(groups))
      avetab[["ave"]]     <- c(t(ave_result))

    } else {
      avelist <- lapply(modelContainer[["results"]][["object"]], semTools::AVE)
      # for each group, find all variable names in each model
      groups <- unique(unlist(lapply(avelist, function(ave_result) { ave_result[, "group"] })))
      avelist <- lapply(avelist, function(ave_result) { ave_result[, -1] })
      all_names <- unique(unlist(lapply(avelist, names)))

      # generate df with these names
      avedf <- data.frame(
        "group" = rep(groups, rep(length(all_names), length(groups))),
        "factor" = rep(all_names, length(groups)),
        stringsAsFactors = FALSE
      )

      for (mod_idx in seq_along(avelist)) {
        avedf[mod_idx + 2] <- NA
        for (grp in seq_along(groups)) {
          grp_idx <- which(avedf[["group"]] == groups[grp])
          row_idx <- grp_idx[match(names(avelist[[mod_idx]]), avedf[grp_idx, "factor"])]
          for (row in row_idx)
            avedf[row, mod_idx + 2] <- c(avelist[[mod_idx]][grp, which(names(avelist[[mod_idx]]) == avedf[row, "factor"])])
        }
      }

      # fill jasp table with data
      avetab[["group"]] <- avedf[["group"]]
      avetab[["factor"]] <- avedf[["factor"]]
      for (i in seq_along(avelist)) avetab[[paste0("ave_", i)]] <- avedf[[i + 2]]

    }
  }
}

.semReliability <- function(modelContainer, dataset, options, ready) {
  if (!options[["reliability"]] || !is.null(modelContainer[["reliability"]])) return()

  # init table
  reliabilitytab <- createJaspTable(gettext("Reliability"))
  if (options[["group"]] != "")
    reliabilitytab$addColumnInfo(name = "group", title = gettext("Group"), type = "string", combine = TRUE)
  reliabilitytab$addColumnInfo(name = "factor", title = "", type = "string")
  if (length(options[["models"]]) < 2) {
    reliabilitytab$addColumnInfo(name = "reliabilityAlpha", title = gettext("Coefficient \u03B1"), type = "number")
    reliabilitytab$addColumnInfo(name = "reliabilityOmega", title = gettext("Coefficient \u03C9"), type = "number")
  } else {
    for (i in seq_along(options[["models"]])) {
      reliabilitytab$addColumnInfo(name = paste0("reliabilityAlpha_", i), title = gettext("Coefficient \u03B1"),
                                  overtitle = options[["models"]][[i]][["name"]], type = "number")
      reliabilitytab$addColumnInfo(name = paste0("reliabilityOmega_", i), title = gettext("Coefficient \u03C9"),
                                  overtitle = options[["models"]][[i]][["name"]], type = "number")
    }
  }

  reliabilitytab$dependOn(c("reliability", "models"))
  reliabilitytab$position <- .95

  modelContainer[["reliability"]] <- reliabilitytab

  if (!ready || modelContainer$getError()) return()

  # compute data and fill table
  if (options[["group"]] == "") {

    if (length(options[["models"]]) < 2) {

      parTable <- lavaan::parameterTable(modelContainer[["results"]][["object"]][[1]])
      parTable <- parTable[parTable$op == "=~",]
      higherOrder <- unique(parTable[!parTable$rhs %in% names(dataset),]$lhs)

      reliability_alpha          <- semTools::compRelSEM(modelContainer[["results"]][["object"]][[1]], tau.eq = TRUE, return.total = TRUE)
      reliability_omega          <- semTools::compRelSEM(modelContainer[["results"]][["object"]][[1]], tau.eq = FALSE, higher = higherOrder, return.total = TRUE)
      reliabilitytab[["factor"]] <- names(reliability_omega)
      reliabilitytab[["reliabilityAlpha"]]     <- reliability_alpha
      reliabilitytab[["reliabilityOmega"]]     <- reliability_omega

    } else {
      alphalist <- list()
      omegalist <- list()
      for (i in seq_along(options[["models"]])) {
        parTable <- lavaan::parameterTable(modelContainer[["results"]][["object"]][[i]])
        parTable <- parTable[parTable$op == "=~",]
        higherOrder <- unique(parTable[!parTable$rhs %in% names(dataset),]$lhs)

        reliability_alpha          <- semTools::compRelSEM(modelContainer[["results"]][["object"]][[i]], tau.eq = TRUE, higher = higherOrder, return.total = TRUE)
        reliability_omega          <- semTools::compRelSEM(modelContainer[["results"]][["object"]][[i]], tau.eq = FALSE, higher = higherOrder, return.total = TRUE)
        alphalist[[i]] <- reliability_alpha
        omegalist[[i]] <- reliability_omega
      }
      alphadf <- data.frame("factor" = unique(unlist(lapply(omegalist, names))))
      omegadf <- data.frame("factor" = unique(unlist(lapply(omegalist, names))))
      reliabilitytab[["factor"]] <- unique(unlist(lapply(omegalist, names)))

      for (i in 1:length(alphalist)) {
          # fill matching vars from model with df
          alphadf[match(names(alphalist[[i]]), alphadf[["factor"]]), i + 1] <- alphalist[[i]]
          omegadf[match(names(omegalist[[i]]), omegadf[["factor"]]), i + 1] <- omegalist[[i]]
          # add column to table
          reliabilitytab[[paste0("reliabilityAlpha_", i)]] <- alphadf[[i + 1]]
          reliabilitytab[[paste0("reliabilityOmega_", i)]] <- omegadf[[i + 1]]
        }
      }
  } else {
    if (length(options[["models"]]) < 2) {

      parTable <- lavaan::parameterTable(modelContainer[["results"]][["object"]][[1]])
      parTable <- parTable[parTable$op == "=~",]
      higherOrder <- unique(parTable[!parTable$rhs %in% names(dataset),]$lhs)

      reliability_alpha          <- semTools::compRelSEM(modelContainer[["results"]][["object"]][[1]], tau.eq = TRUE, higher = higherOrder, return.total = TRUE)
      reliability_omega          <- semTools::compRelSEM(modelContainer[["results"]][["object"]][[1]], tau.eq = FALSE, higher = higherOrder, return.total = TRUE)
      groups <- reliability_alpha[, "group"]
      reliability_alpha <- reliability_alpha[, -1]
      if(length(higherOrder > 0))
        for (i in 1:length(higherOrder))
          reliability_alpha <- cbind(reliability_alpha, rep(NA, length(groups)))
      reliability_omega <- reliability_omega[, -1]

      reliabilitytab[["group"]]   <- rep(groups, rep(ncol(reliability_omega), length(groups)))
      reliabilitytab[["factor"]]  <- rep(names(reliability_omega), length(groups))
      reliabilitytab[["reliabilityAlpha"]]     <- c(t(reliability_alpha))
      reliabilitytab[["reliabilityOmega"]]     <- c(t(reliability_omega))

    } else {
      alphalist <- list()
      omegalist <- list()
      for (i in seq_along(options[["models"]])) {
        parTable <- lavaan::parameterTable(modelContainer[["results"]][["object"]][[i]])
        parTable <- parTable[parTable$op == "=~",]
        higherOrder <- unique(parTable[!parTable$rhs %in% names(dataset),]$lhs)

        reliability_alpha          <- semTools::compRelSEM(modelContainer[["results"]][["object"]][[i]], tau.eq = TRUE, higher = higherOrder, return.total = TRUE)
        reliability_omega          <- semTools::compRelSEM(modelContainer[["results"]][["object"]][[i]], tau.eq = FALSE, higher = higherOrder, return.total = TRUE)
        alphalist[[i]] <- reliability_alpha
        omegalist[[i]] <- reliability_omega
      }
      # for each group, find all variable names in each model
      groups <- unique(unlist(lapply(alphalist, function(reliability_alpha) { reliability_alpha[, "group"] })))
      alphalist <- lapply(alphalist, function(reliability_alpha) { reliability_alpha[, -1] })
      omegalist <- lapply(omegalist, function(reliability_omega) { reliability_omega[, -1] })
      all_names <- unique(unlist(lapply(omegalist, names)))

      # generate df with these names
      alphadf <- data.frame(
        "group" = rep(groups, rep(length(all_names), length(groups))),
        "factor" = rep(all_names, length(groups)),
        stringsAsFactors = FALSE
      )
      omegadf <- data.frame(
        "group" = rep(groups, rep(length(all_names), length(groups))),
        "factor" = rep(all_names, length(groups)),
        stringsAsFactors = FALSE
      )

      for (mod_idx in seq_along(alphalist)) {
        alphadf[mod_idx + 2] <- NA
        omegadf[mod_idx + 2] <- NA
        for (grp in seq_along(groups)) {
          grp_idx_alpha <- which(alphadf[["group"]] == groups[grp])
          grp_idx_omega <- which(omegadf[["group"]] == groups[grp])
          row_idx_alpha <- grp_idx_alpha[match(names(alphalist[[mod_idx]]), alphadf[grp_idx_alpha, "factor"])]
          for (row in row_idx_alpha)
            alphadf[row, mod_idx + 2] <- c(alphalist[[mod_idx]][grp, which(names(alphalist[[mod_idx]]) == alphadf[row, "factor"])])
          row_idx_omega <- grp_idx_omega[match(names(omegalist[[mod_idx]]), omegadf[grp_idx_omega, "factor"])]
          for (row in row_idx_omega)
            omegadf[row, mod_idx + 2] <- c(omegalist[[mod_idx]][grp, which(names(omegalist[[mod_idx]]) == omegadf[row, "factor"])])
        }
      }

      # fill jasp table with data
      reliabilitytab[["group"]] <- alphadf[["group"]]
      reliabilitytab[["factor"]] <- alphadf[["factor"]]
      for (i in seq_along(alphalist)) reliabilitytab[[paste0("reliabilityAlpha_", i)]] <- alphadf[[i + 2]]
      for (i in seq_along(omegalist)) reliabilitytab[[paste0("reliabilityOmega_", i)]] <- omegadf[[i + 2]]
    }
  }
}

.semHtmt <- function(modelContainer, dataset, options, ready) {
  if (!options[["htmt"]] || !is.null(modelContainer[["htmt"]])) return()


  htmt <- createJaspContainer()
  htmt$position <- 0.95
  htmt$dependOn(c("htmt", "naAction", "models"))

  modelContainer[["htmt"]] <- htmt

  if (length(options[["models"]]) < 2) {
    .semHtmtTables(modelContainer[["results"]][["object"]][[1]], NULL, htmt, options, ready, dataset)
  } else {
    htmt$title <- gettext("Heterotrait-monotrait ratio")

    for (i in seq_along(options[["models"]])) {
      fit <- modelContainer[["results"]][["object"]][[i]]
      model <- options[["models"]][[i]]
      .semHtmtTables(fit, model, htmt, options, ready, dataset)
    }
  }
}

.semHtmtTables <- function(fit, model, parentContainer, options, ready, dataset) {
  if (is.null(model)) {
    htmtcont <- parentContainer
    title <- gettext("Heterotrait-monotrait ratio")
  } else {
    htmtcont <- createJaspContainer(model[["name"]], initCollapsed = TRUE)
    title <- ""
  }

  htmttab <- createJaspTable(title = title)
  htmtcont[["htmttab"]] <- htmttab

  if (options[["group"]] == "") {
    lavopts <- .semOptionsToLavOptions(options, dataset)
    lavmodel <- ifelse(is.null(model), .semTranslateModel(options[["models"]][[1]][["syntax"]], dataset), .semTranslateModel(model[["syntax"]], dataset))

    parTable <- lavaan::lavaanify(lavmodel)
    latents  <- parTable[parTable$op == "=~",]
    higherOrder <- unique(latents[!latents$rhs %in% names(dataset),]$lhs)
    lavmodel <- parTable[!parTable$lhs %in% higherOrder, ]

    if (options[["dataType"]] == "raw") {
      htmt_result <- semTools::htmt(model = lavmodel, data = dataset, missing = lavopts[["missing"]])
    } else {
      htmt_result <- semTools::htmt(model = lavmodel, sample.cov = .semDataCovariance(dataset, model), missing = lavopts[["missing"]])
    }
    htmt_result[upper.tri(htmt_result)] <- NA

    for (i in 1:ncol(htmt_result)) {
      name <- colnames(htmt_result)[i]
      htmttab$addColumnInfo(name, title = name, type ="number")
    }
    htmttab$addRows(htmt_result, rowNames = colnames(htmt_result))

  } else {

    lavopts <- .semOptionsToLavOptions(options, dataset)
    lavmodel <- ifelse(is.null(model), .semTranslateModel(options[["models"]][[1]][["syntax"]], dataset), .semTranslateModel(model[["syntax"]], dataset))

    parTable <- lavaan::lavaanify(lavmodel)
    latents  <- parTable[parTable$op == "=~",]
    higherOrder <- unique(latents[!latents$rhs %in% names(dataset),]$lhs)
    lavmodel <- parTable[!parTable$lhs %in% higherOrder, ]

    # prepare the columns
    lvNames <- unique(lavmodel[lavmodel$op == "=~", "lhs"])
    htmttab$addColumnInfo(name = "group", title = gettext("Group"), type = "string")
    for (name in lvNames) {
      htmttab$addColumnInfo(name, title = name, type ="number")
    }

    fillMat <- NULL
    for (group in unique(dataset[, options[["group"]]])) {

      dataset_per_group <- dataset[dataset[, options[["group"]]] == group, ]

      htmt_result <- semTools::htmt(model = lavmodel, data = dataset_per_group, missing = lavopts[["missing"]])
      htmt_result[upper.tri(htmt_result)] <- NA
      groupCol <- data.frame(group = c(group, rep(NA, nrow(htmt_result) - 1)))
      htmtFill <- cbind(groupCol, as.data.frame(htmt_result))
      fillMat <- rbind(fillMat, htmtFill)

    }
    htmttab$setData(fillMat)

  }
  if (!is.null(model)) parentContainer[[model[["name"]]]] <- htmtcont
}

.semMardiasCoefficient <- function(modelContainer, dataset, options, ready) {
  if (!options[["mardiasCoefficient"]] || !is.null(modelContainer[["semMardiasTable"]])) return()

  mardiatab <- createJaspTable(title = gettext("Mardia's coefficients"))
  mardiatab$position <- .2

  mardiatab$addColumnInfo(name = "Type",        title = "",                      type = "string")
  mardiatab$addColumnInfo(name = "Coefficient", title = gettext("Coefficient"),  type = "number")
  mardiatab$addColumnInfo(name = "z",           title = gettext("z"),            type = "number")
  mardiatab$addColumnInfo(name = "Chisq",       title = "\u03C7\u00B2",          type = "number")
  mardiatab$addColumnInfo(name = "DF",          title = gettext("df"),           type = "integer")
  mardiatab$addColumnInfo(name = "pvalue",      title = gettext("p"),            type = "pvalue")

  mardiatab$dependOn(c("mardiasCoefficient", "models"))
  modelContainer[["mardiasTable"]] <- mardiatab

  if (!ready || modelContainer$getError()) return()

  varNames <- unique(unlist(lapply(options[["models"]], function(x) {
    .semGetUsedVars(x[["syntax"]], colnames(dataset))
  })))
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
        octab$addColumnInfo(nm, title = nm, type ="number")
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
        ictab$addColumnInfo(nm, title = nm, type = "number")
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
        rctab$addColumnInfo(nm, title = nm, type = "number")
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
            srtab$addColumnInfo(nm, title = nm, type = "number")
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
          occont[[level_names[i]]]$addColumnInfo(nm, title = nm, type = "number")
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
          iccont[[level_names[i]]]$addColumnInfo(nm, title = nm, type = "number")
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
          rccont[[level_names[i]]]$addColumnInfo(nm, title = nm, type = "number")
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
              srcont[[level_names[i]]]$addColumnInfo(nm, title = nm, type = "number")
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

  semModIndicesTable$setData(semModIndResult)


  if (!is.null(modelname)) {
    parentContainer[[modelname]] <- micont
  }

  return()
}

.semSensitivity <- function(modelContainer, dataset, options, ready) {
  if (!options[["sensitivityAnalysis"]] || !is.null(modelContainer[["sensitivity"]])) return()

  sensitivity <- createJaspContainer(gettext("Sensitivity analysis"))
  sensitivity$position <- 4.1
  sensitivity$dependOn(c("sensitivityAnalysis", "searchAlgorithm", "optimizerFunction", "sizeOfSolutionArchive", "numberOfAnts", "alpha", "maxIterations", "setSeed", "seed", "models"))

  modelContainer[["sensitivity"]] <- sensitivity

  if (length(options[["models"]]) < 2) {
    .semSensitivityTables(modelContainer[["results"]][["object"]][[1]], NULL, sensitivity, dataset, options, ready)
  } else {

    for (i in seq_along(options[["models"]])) {
      fit <- modelContainer[["results"]][["object"]][[i]]
      model <- options[["models"]][[i]]
      .semSensitivityTables(fit, model, sensitivity, dataset, options, ready)
    }
  }
}

.semSensitivityTables <- function(fit, model, parentContainer, dataset, options, ready) {
  if (is.null(model)) {
    sencont <- parentContainer
  } else {
    sencont <- createJaspContainer(model[["name"]], initCollapsed = TRUE)
  }


  # Summary of sensitivity analysis
  sensumtab <- createJaspTable(title = gettext("Summary of sensitivity analysis"))

  if (options[["group"]] != "")
    sensumtab$addColumnInfo(name = "group",  title = gettext("Group"),      type = "string", combine = TRUE)

  sensumtab$addColumnInfo(name = "path",       title = gettext("Path"),          type = "string")
  sensumtab$addColumnInfo(name = "est",        title = gettext("Standardized estimate"), overtitle = gettext("Original model"),    type = "number")
  sensumtab$addColumnInfo(name = "pvalue",     title = gettext("p"),                     overtitle = gettext("Original model"),    type = "pvalue")
  sensumtab$addColumnInfo(name = "pvaluesens", title = gettext("p\u002A"),               overtitle = gettext("Sensitivity model"), type = "pvalue")
  sensumtab$addColumnInfo(name = "mean",       title = gettext("Mean"),                  overtitle = gettext("Sensitivity model"), type = "number")
  sensumtab$addColumnInfo(name = "min",        title = gettext("Min"),                   overtitle = gettext("Sensitivity model"), type = "number")
  sensumtab$addColumnInfo(name = "max",        title = gettext("Max"),                   overtitle = gettext("Sensitivity model"), type = "number")

  sencont[["sensum"]] <- sensumtab

  if (!ready || !inherits(fit, "lavaan")) return()

  # create SEMsens model
  if (is.null(model)) {
    analyticModel <- .semTranslateModel(options[["models"]][[1]][["syntax"]], dataset)
  } else {
    analyticModel <- .semTranslateModel(model[["syntax"]], dataset)
  }
  #enrich model
  modelTable <- lavaan::lavInspect(fit, what = "list")
  pathVars <- unique(c(modelTable[modelTable$op == "~", ]$rhs, modelTable[modelTable$op == "~", ]$lhs))

  if (length(pathVars) < 2) {
    .quitAnalysis(gettext("Please include at least one regression path in the model to perform a sensitivity analysis."))
  }

  sensModel <- analyticModel
  for (i in seq_along(pathVars)) {
    sensParameter <- paste0("\n", pathVars[i], " ~", " phantom", i, "*phantom\n")
    sensModel <- paste0(sensModel, sensParameter)
  }
  sensModel <- paste0(sensModel, "\nphantom =~ 0\nphantom ~~ 1*phantom\n")
  optimizerFunction <- switch(options[["optimizerFunction"]],
                              "percentChangeMeanEstimate" = 1,
                              "sdOfDeviance"              = 2,
                              "changeOfPvalue"            = 3,
                              "distanceOfPvalue"          = 4,
                              "changeOfRmsea"             = 5,
                              "distanceOfRmsea"           = 6
  )

  if (options[["group"]] != "") {
    saTables <- lapply(1:5, function(x) data.frame())
    for (group in unique(dataset[, options[["group"]]])) {
      data <- dataset[dataset[[options[["group"]]]] == group,]
      if(options[["searchAlgorithm"]] == "antColonyOptimization") {
        iter <- ifelse((2 * options[["sizeOfSolutionArchive"]]) >= options[["maxIterations"]], (options[["sizeOfSolutionArchive"]] + options[["numberOfAnts"]]), (options[["maxIterations"]] + options[["numberOfAnts"]]))
        startProgressbar(iter,
                         gettextf("Performing sensitivity analysis (model: %1$s, group: %2$s)",
                                 ifelse(is.null(model),
                                        options[["models"]][[1]][["name"]],
                                        model[["name"]]),
                                 group))
        sa <- .sa.aco(data = data, model = analyticModel, sens.model = sensModel, n.of.ants = options[["numberOfAnts"]], k = options[["sizeOfSolutionArchive"]], rate.of.conv = options[["convergenceRateThreshold"]], opt.fun = optimizerFunction, max.iter = options[["maxIterations"]], sig.level = options[["alpha"]], seed = if (options[["setSeed"]]) options[["seed"]] else NULL)
      }
      if(options[["searchAlgorithm"]] == "tabuSearch") {
        startProgressbar(options[["maxIterations"]],
                         gettextf("Performing sensitivity analysis (model: %1$s, group: %2$s)",
                                 ifelse(is.null(model),
                                        options[["models"]][[1]][["name"]],
                                        model[["name"]]),
                                 group))
        sa <- .sa.tabu(data = data, model = analyticModel, sens.model = sensModel, opt.fun = optimizerFunction, max.iter = options[["maxIterations"]], sig.level = options[["alpha"]], seed = if (options[["setSeed"]]) options[["seed"]] else NULL)
      }
      saTablesRows <- SEMsens::sens.tables(sa)
      saTablesRows <- sapply(saTablesRows, function(x) {
        cbind(x, "group" = rep(group, length(x[, 1])), "rowname" = row.names(x))
      })
      for (table in seq_along(saTablesRows)) {
        saTables[[table]] <- rbind(saTables[[table]], saTablesRows[[table]])
      }
    }
    saTables <- sapply(saTables, as.data.frame)
    saTables <- sapply(saTables, function(x) {x[order(x[["group"]], x[["rowname"]]),]})
  } else {
    if (options[["dataType"]] == "raw") {
      if(options[["searchAlgorithm"]] == "antColonyOptimization") {
        iter <- ifelse((2 * options[["sizeOfSolutionArchive"]]) >= options[["maxIterations"]], (options[["sizeOfSolutionArchive"]] + options[["numberOfAnts"]]), (options[["maxIterations"]] + options[["numberOfAnts"]]))
        startProgressbar(iter,
                         gettextf("Performing sensitivity analysis (model: %1$s)",
                                 ifelse(is.null(model),
                                        options[["models"]][[1]][["name"]],
                                        model[["name"]])
                         ))
        sa <- .sa.aco(data = dataset, model = analyticModel, sens.model = sensModel, n.of.ants = options[["numberOfAnts"]], k = options[["sizeOfSolutionArchive"]], rate.of.conv = options[["convergenceRateThreshold"]], opt.fun = optimizerFunction, max.iter = options[["maxIterations"]], sig.level = options[["alpha"]], seed = if (options[["setSeed"]]) options[["seed"]] else NULL)
      }
      if(options[["searchAlgorithm"]] == "tabuSearch") {
        startProgressbar(options[["maxIterations"]],
                         gettextf("Performing sensitivity analysis (model: %1$s)",
                                 ifelse(is.null(model),
                                        options[["models"]][[1]][["name"]],
                                        model[["name"]])
                         ))
        sa <- .sa.tabu(data = dataset, model = analyticModel, sens.model = sensModel, opt.fun = optimizerFunction, max.iter = options[["maxIterations"]], sig.level = options[["alpha"]], seed = if (options[["setSeed"]]) options[["seed"]] else NULL)
      }
    } else {
      if (is.null(model)) {
        syntax <- options[["models"]][[1]][["syntax"]]
      } else {
        syntax <- model[["syntax"]]
      }
      dataset <- .semDataCovariance(dataset, syntax)
      if(options[["searchAlgorithm"]] == "antColonyOptimization") {
        iter <- ifelse((2 * options[["sizeOfSolutionArchive"]]) >= options[["maxIterations"]], (options[["sizeOfSolutionArchive"]] + options[["numberOfAnts"]]), (options[["maxIterations"]] + options[["numberOfAnts"]]))
        startProgressbar(iter,
                         gettextf("Performing sensitivity analysis (model: %1$s)",
                                 ifelse(is.null(model),
                                        options[["models"]][[1]][["name"]],
                                        model[["name"]])
                         ))
        sa <- .sa.aco(sample.cov = dataset, sample.nobs = options[["sampleSize"]], model = analyticModel, sens.model = sensModel, n.of.ants = options[["numberOfAnts"]], k = options[["sizeOfSolutionArchive"]], rate.of.conv = options[["convergenceRateThreshold"]], opt.fun = optimizerFunction, max.iter = options[["maxIterations"]], sig.level = options[["alpha"]], seed = if (options[["setSeed"]]) options[["seed"]] else NULL)
      }
      if(options[["searchAlgorithm"]] == "tabuSearch") {
        startProgressbar(options[["maxIterations"]],
                         gettextf("Performing sensitivity analysis (model: %1$s)",
                                 ifelse(is.null(model),
                                        options[["models"]][[1]][["name"]],
                                        model[["name"]])
                         ))
        sa <- .sa.tabu(sample.cov = dataset, sample.nobs = options[["sampleSize"]], model = analyticModel, sens.model = sensModel, opt.fun = optimizerFunction, max.iter = options[["maxIterations"]], sig.level = options[["alpha"]], seed = if (options[["setSeed"]]) options[["seed"]] else NULL)
      }
    }
    saTables <- SEMsens::sens.tables(sa)
    saTables <- sapply(saTables, function(x) {
      cbind(x, "rowname" = row.names(x))
    })
    saTables <- sapply(saTables, as.data.frame)
    saTables <- sapply(saTables, function(x) {
      x[order(x[["rowname"]]),]
    })
  }


  # Fill table

  if (options[["group"]] != "")
    sensumtab[["group"]] <- saTables[[1]][["group"]]

  sensumtab[["path"]]       <- saTables[[1]][["rowname"]]
  sensumtab[["est"]]        <- saTables[[1]][["model.est"]]
  sensumtab[["pvalue"]]     <- saTables[[1]][["model.pvalue"]]
  sensumtab[["pvaluesens"]] <- saTables[[5]][["p.changed"]]
  sensumtab[["mean"]]       <- saTables[[1]][["mean.est.sens"]]
  sensumtab[["min"]]        <- saTables[[1]][["min.est.sens"]]
  sensumtab[["max"]]        <- saTables[[1]][["max.est.sens"]]


  # Sensitivity parameters that led to a change in significance
  senpartab <- createJaspTable(title = gettext("Sensitivity parameters that led to a change in significance"))

  if (options[["group"]] != "")
    senpartab$addColumnInfo(name = "group", title = gettext("Group"), type = "string", combine = TRUE)

  senpartab$addColumnInfo(name = "path", title = gettext("Path"), type = "string")

  sensitivityParameters <- grep("~", colnames(saTables[[5]]), value = TRUE)
  for (par in sensitivityParameters) {
    # unfortunately the title is set by the r package and is (usually) "phantom", not sure how to translate such a thing
    senpartab$addColumnInfo(name = par, title = par, overtitle = gettext("Sensitivity parameters"), type = "number")
  }

  sencont[["senpar"]] <- senpartab

  # Fill table
  saTable_clean <- saTables[[5]][!is.na(saTables[[5]][["p.changed"]]), ]
  if (nrow(saTable_clean) == 0) sencont[["senpar"]] <- NULL


  if (options[["group"]] != "")
    senpartab[["group"]] <- saTable_clean[["group"]]

  senpartab[["path"]]    <- saTable_clean[["rowname"]]
  for (par in sensitivityParameters) {
    senpartab[[par]]     <- saTable_clean[[par]]
  }

  # # Not sure why this is here??? Lorenzo would know, lets just leave it in
  # # Sensitivity parameters that led to min est
  # senparmintab <- createJaspTable(title = gettext("Sensitivity parameters that led to the minimum estimates in the sensitivity model"))
  #
  # if (options[["group"]] != "")
  #   senparmintab$addColumnInfo(name = "group", title = gettext("Group"), type = "string", combine = TRUE)
  #
  # senparmintab$addColumnInfo(name = "path", title = gettext("Path"), type = "string")
  #
  # sensitivityParameters <- grep("~", colnames(saTables[[3]]), value = TRUE)
  # for (par in sensitivityParameters) {
  #   senparmintab$addColumnInfo(name = par, title = gettext(par), overtitle = gettext("Sensitivity parameters"), type = "number")
  # }
  #
  # sencont[["senparmin"]] <- senparmintab
  #
  # # Fill table
  # if (options[["group"]] != "")
  #   senparmintab[["group"]] <- saTables[[3]][["group"]]
  #
  # senparmintab[["path"]]    <- saTables[[3]][["rowname"]]
  # for (par in sensitivityParameters) {
  #   senparmintab[[par]]     <- saTables[[3]][[par]]
  # }

  # # Sensitivity parameters that led to max est
  # senparmaxtab <- createJaspTable(title = gettext("Sensitivity parameters that led to the maximum estimates in the sensitivity model"))
  #
  # if (options[["group"]] != "")
  #   senparmaxtab$addColumnInfo(name = "group", title = gettext("Group"), type = "string", combine = TRUE)
  #
  # senparmaxtab$addColumnInfo(name = "path", title = gettext("Path"), type = "string")
  #
  # sensitivityParameters <- grep("~", colnames(saTables[[4]]), value = TRUE)
  # for (par in sensitivityParameters) {
  #   senparmaxtab$addColumnInfo(name = par, title = gettext(par), overtitle = gettext("Sensitivity parameters"), type = "number")
  # }
  #
  # sencont[["senparmax"]] <- senparmaxtab
  #
  # # Fill table
  # if (options[["group"]] != "")
  #   senparmaxtab[["group"]] <- saTables[[4]][["group"]]
  #
  # senparmaxtab[["path"]]    <- saTables[[4]][["rowname"]]
  # for (par in sensitivityParameters) {
  #   senparmaxtab[[par]]     <- saTables[[4]][[par]]
  # }

  # Summary of sensitivity parameters
  sensumpartab <- createJaspTable(title = gettext("Summary of sensitivity parameters"))

  if (options[["group"]] != "")
    sensumpartab$addColumnInfo(name = "group", title = gettext("Group"), type = "string", combine = TRUE)

  sensumpartab$addColumnInfo(name = "par",      title = gettext("Sensitivity parameter"), type = "string")
  sensumpartab$addColumnInfo(name = "mean",     title = gettext("Mean"),                  type = "number")
  sensumpartab$addColumnInfo(name = "min",      title = gettext("Min"),                   type = "number")
  sensumpartab$addColumnInfo(name = "max",      title = gettext("Max"),                   type = "number")

  sencont[["sensumpar"]] <- sensumpartab

  #Fill table

  if (options[["group"]] != "")
    sensumpartab[["group"]] <- saTables[[2]][["group"]]

  sensumpartab[["par"]]        <- saTables[[2]][["rowname"]]
  sensumpartab[["mean"]]       <- saTables[[2]][["mean.phan"]]
  sensumpartab[["min"]]        <- saTables[[2]][["min.phan"]]
  sensumpartab[["max"]]        <- saTables[[2]][["max.phan"]]

  if (!is.null(model)) parentContainer[[model[["name"]]]] <- sencont
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
