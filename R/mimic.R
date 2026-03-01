#
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
#

MIMICInternal <- function(jaspResults, dataset, options, ...) {
  jaspResults$addCitation("Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2), 1-36. URL http://www.jstatsoft.org/v48/i02/")

  # Read dataset
  dataset <- .mimicReadData(dataset, options)
  ready   <- .mimicCheckErrors(dataset, options)

  modelContainer <- .mimicModelContainer(jaspResults)

  # Output functions
  .mimicFitTable(     modelContainer, dataset, options, ready)
  .mimicParTable(     modelContainer, options, ready)
  .mimicAdditionalFit(modelContainer, options, ready)
  .mimicRsquared(     modelContainer, options, ready)
  .mimicPathPlot(     modelContainer, dataset, options, ready)
  .mimicSyntax(       modelContainer, options, ready)

}

# Preprocessing functions ----
.mimicReadData <- function(dataset, options) {
  if (!is.null(dataset)) return(dataset)

  vars <- c(options$predictors, options$indicators)
  return(.readDataSetToEnd(columns = vars))
}

.mimicCheckErrors <- function(dataset, options) {
  if (length(options$indicators) < 3 || length(options$predictors) == 0) return(FALSE)

  # Check for missing value handling
  if (options$estimator %in% c("gls", "wls", "uls", "dwls") && anyNA(dataset) && options$naAction == "fiml")
    jaspBase:::.quitAnalysis(gettext("FIML only available with ML-type estimators."))

  # Exogenous variables can be binary or continuous
  exo <- options$predictors
  # Endogenous variables need to be scale or ordinal
  endo <- options$indicators

  customChecks <- list(
    checkExogenous = function() {
      admissible <- vapply(exo, function(exo_var) {
        var <- na.omit(dataset[[exo_var]])
        if (is.ordered(var)) return(FALSE)
        if ((is.character(var) || is.factor(var)) && length(unique(var)) != 2) return(FALSE)
        return(TRUE)
      }, TRUE)
      if (!all(admissible))
        gettextf("Not all exogenous variables are admissible. Inadmissible exogenous variables: %s. Only binary or continuous exogenous variables allowed.", paste(exo[!admissible], collapse = ", "))
    },

    checkEndogenous = function() {
      if (length(options$confounds) > 0) endo <- c(endo, options$predictor)
      admissible <- vapply(endo, function(endo_var) {
        var <- na.omit(dataset[[endo_var]])
        if (!(is.ordered(var) || is.numeric(var))) {
          return(FALSE)
        }
        return(TRUE)
      }, TRUE)
      if (!all(admissible))
        gettextf("Not all endogenous variables are admissible. Inadmissible endogenous variables: %s. Only scale or ordinal endogenous variables allowed.", paste(endo[!admissible], collapse = ", "))
    },

    checkCategoricalEndo = function() {
      if (length(options$confounds) > 0) endo <- c(endo, options$predictor)

      admissible <- vapply(endo, function(endo_var) {
        var <- na.omit(dataset[[endo_var]])
        if (is.ordered(var) && anyNA(dataset) && options$naAction == "fiml") {
          return(FALSE)
        }
        return(TRUE)
      }, TRUE)

      if (!all(admissible))
        gettextf("FIML missing value handling only available when all endogenous variables are of scale type. Ordinal endogenous variables in the model: %s", paste(endo[!admissible], collapse = ", "))
    }

  )

  .hasErrors(dataset, type = c('observations', 'variance', 'infinity'), custom = customChecks,
             all.target = c(endo, exo), observations.amount = paste('<', length(c(endo, exo))),
             exitAnalysisIfErrors = TRUE)

  return(TRUE)
}

# Results functions ----

.mimicComputeResults <- function(modelContainer, dataset, options, ready) {

  miss <- if (anyNA(dataset)) options[["naAction"]] else "listwise"

  mimicResult <- try(lavaan::sem(
    model           = .mimicToLavMod(options),
    data            = dataset,
    se              = ifelse(options$errorCalculationMethod == "bootstrap", "standard", options$errorCalculationMethod),
    mimic           = options$emulation,
    estimator       = options$estimator,
    missing         = miss,
    std.lv          = TRUE
  ))

  if (inherits(mimicResult, "try-error")) {
    errmsg <- gettextf("Estimation failed\nMessage:\n%s", attr(mimicResult, "condition")$message)
    modelContainer$setError(.decodeVarsInMessage(names(dataset), errmsg))
    return()
  }

  if(isFALSE(slot(mimicResult, "optim")$converged)) {
    errormsg <- gettext("Estimation failed! Message: The model did not converge. Try using a different estimator or specify an other MIMIC model.")
    modelContainer$setError(errormsg)
    return()
  }

  if (options$errorCalculationMethod == "bootstrap") {
    type <- switch(options[["standardizedEstimateType"]],
                   "all" = "std.all",
                   "latents" = "std.lv",
                   "nox" = "std.nox")
    mimicResult <- lavBootstrap(mimicResult, samples = options[["bootstrapSamples"]],
                              standard = options[["standardizedEstimate"]],
                              typeStd = type)
  }

  modelContainer[["model"]] <- createJaspState(mimicResult)
  return(mimicResult)
}

.mimicToLavMod <- function(options, base64 = TRUE) {

  if (!base64) .v <- I

  header <- "
  # -----------------------------
  # MIMIC model generated by JASP
  # -----------------------------
  "
  measurement <- paste(
    "  Y =~",
    paste(
      paste("lambda", seq_along(options[["indicators"]]), "*", sep = ""),
      options[["indicators"]],
      collapse = " + ", sep = ""
    )
  )
  structural <- paste(
    "  Y ~ ",
    paste(
      paste("beta", seq_along(options[["predictors"]]), "*", sep = ""),
      options[["predictors"]],
      collapse = " + ", sep = ""
    )
  )

  return(paste(header, measurement, structural, sep = "\n"))
}

# Output functions ----

.mimicModelContainer <- function(jaspResults, options) {
  if (!is.null(jaspResults[["modelContainer"]])) {
    modelContainer <- jaspResults[["modelContainer"]]
  } else {
    modelContainer <- createJaspContainer()
    modelContainer$dependOn(c(
      "predictors", "indicators", "includemeanstructure",
      "bootstrapSamples", "emulation", "errorCalculationMethod", "estimator",
      "naAction", "standardizedEstimate", "standardizedEstimateType")
    )
    jaspResults[["modelContainer"]] <- modelContainer
  }

  return(modelContainer)
}

.mimicFitTable <- function(modelContainer, dataset, options, ready) {
  if (!is.null(modelContainer[["fittab"]])) return()

  fittab <- createJaspTable(title = gettext("Chi-square test"))
  fittab$position <- 0

  fittab$addColumnInfo(name="Model", title = "",                      type = "string")
  fittab$addColumnInfo(name="df",    title = gettext("df"),           type = "integer")
  fittab$addColumnInfo(name="chisq", title = gettext("&#967;&sup2;"), type = "number")
  fittab$addColumnInfo(name="pval",  title = gettext("p"),            type = "pvalue")

  fittab$addCitation("Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2), 1-36. URL http://www.jstatsoft.org/v48/i02/")

  modelContainer[["fittab"]] <- fittab

  fittab[["Model"]] <- c(gettext("Baseline model"), gettext("Factor model"))

  if (!ready) return()

  # add data to the table!
  .mimicComputeResults(modelContainer, dataset, options, ready)

  if (modelContainer$getError()) return()

  fittab$addFootnote(.mimicFootMessage(modelContainer[["model"]][["object"]]))

  fm <- lavaan::fitmeasures(modelContainer[["model"]][["object"]])
  fittab[["df"]]    <- fm[c("baseline.df", "df")]
  fittab[["chisq"]] <- fm[c("baseline.chisq", "chisq")]
  fittab[["pval"]]  <- fm[c("baseline.pvalue", "pvalue")]
}

.mimicAdditionalFit <- function(modelContainer, options, ready) {
  if (!options[["additionalFitMeasures"]] || !is.null(modelContainer[["fitIndices"]])) return()

  fitinds <- .additionalFitTables(modelContainer, dataset, options, ready)

  fitinds$position <- .75
  modelContainer[["fitIndices"]] <- fitinds
}

.mimicParTable <- function(modelContainer, options, ready) {
  if (!is.null(modelContainer[["parest"]])) return()
  modelContainer[["parest"]] <- pecont <- createJaspContainer(gettext("Parameter estimates"))
  pecont$dependOn(options = c("ciLevel", "bootstrapCiType"))
  pecont$position <- 0.5

  estTitle <- ifelse(options[["standardizedEstimate"]], gettext("Std. estimate"), gettext("Estimate"))

  ## betas
  bettab <- createJaspTable(title = gettext("Predictor coefficients"))

  bettab$addColumnInfo(name = "rhs",      title = gettext("Predictor"),  type = "string")
  bettab$addColumnInfo(name = "est",      title = estTitle,   type = "number", format = "sf:4;dp:3")
  bettab$addColumnInfo(name = "se",       title = gettext("Std. error"), type = "number", format = "sf:4;dp:3")
  bettab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number", format = "sf:4;dp:3")
  bettab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "number", format = "dp:3;p:.001")
  bettab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number", format = "sf:4;dp:3",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
  bettab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number", format = "sf:4;dp:3",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))


  pecont[["bet"]] <- bettab

  ## lambdas
  lamtab <- createJaspTable(title = gettext("Indicator coefficients"))

  lamtab$addColumnInfo(name = "rhs",      title = gettext("Indicator"),  type = "string")
  lamtab$addColumnInfo(name = "est",      title = estTitle,   type = "number", format = "sf:4;dp:3")
  lamtab$addColumnInfo(name = "se",       title = gettext("Std. error"), type = "number", format = "sf:4;dp:3")
  lamtab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number", format = "sf:4;dp:3")
  lamtab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "number", format = "dp:3;p:.001")
  lamtab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number", format = "sf:4;dp:3",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
  lamtab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number", format = "sf:4;dp:3",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))

  pecont[["lam"]] <- lamtab

  if (!ready || modelContainer$getError()) return()

  bootstrapCiType <- ifelse(options[["bootstrapCiType"]] == "percentileBiasCorrected", "bca.simple",
                            ifelse(options[["bootstrapCiType"]] == "percentile", "perc",
                                   "norm"))

  #' we need the second option in the if statement because when we require standardized estimates and bootstrapped CIs
  #' the standardization happens in each bootstrap run and the standardized estimates replace the regular raw estimates
  #' in the fit object, and we only need to call parameterEstimates

  fit <- modelContainer[["model"]][["object"]]

  if (!options[["standardizedEstimate"]] ||
      (options[["standardizedEstimate"]] && options[["errorCalculationMethod"]] == "bootstrap")) {
    pe <- lavaan::parameterestimates(fit, level = options[["ciLevel"]],
                                     boot.ci.type = bootstrapCiType)

  } else {
    type <- switch(options[["standardizedEstimateType"]],
                   "all" = "std.all",
                   "latents" = "std.lv",
                   "nox" = "std.nox")
    pe <- lavaan::standardizedSolution(fit, level = options[["ciLevel"]], type = type)
    colnames(pe)[colnames(pe) == "est.std"] <- "est"

  }

  pe_bet <- pe[substr(pe$label, 1, 1) == "b", ]
  bettab[["rhs"]]      <- pe_bet$rhs
  bettab[["est"]]      <- pe_bet$est
  bettab[["se"]]       <- pe_bet$se
  bettab[["z"]]        <- pe_bet$z
  bettab[["pvalue"]]   <- pe_bet$pvalue
  bettab[["ci.lower"]] <- pe_bet$ci.lower
  bettab[["ci.upper"]] <- pe_bet$ci.upper

  pe_lam <- pe[substr(pe$label, 1, 1) == "l", ]
  lamtab[["rhs"]]      <- pe_lam$rhs
  lamtab[["est"]]      <- pe_lam$est
  lamtab[["se"]]       <- pe_lam$se
  lamtab[["z"]]        <- pe_lam$z
  lamtab[["pvalue"]]   <- pe_lam$pvalue
  lamtab[["ci.lower"]] <- pe_lam$ci.lower
  lamtab[["ci.upper"]] <- pe_lam$ci.upper

}

.mimicRsquared <- function(modelContainer, options, ready) {
  if (!options$rSquared || !is.null(modelContainer[["rsquared"]])) return()

  tabr2 <- createJaspTable(gettext("R-Squared"))
  tabr2$addColumnInfo(name = "__var__", title = "", type = "string")
  tabr2$addColumnInfo(name = "rsq", title = "R\u00B2", type = "number", format = "sf:4;dp:3")
  tabr2$dependOn(options = "rSquared")
  tabr2$position <- 1

  modelContainer[["rsquared"]] <- tabr2

  if (!ready || modelContainer$getError()) return()

  r2res              <- lavaan::inspect(modelContainer[["model"]][["object"]], "r2")
  tabr2[["__var__"]] <- names(r2res)
  tabr2[["rsq"]]     <- r2res
}

.mimicPathPlot <- function(modelContainer, dataset, options, ready) {
  if (!options$pathPlot || !ready || !is.null(modelContainer[["plot"]])) return()

  plt <- createJaspPlot(title = gettext("Path plot"), width = 600, height = 400)
  plt$dependOn(options = c("pathPlot", "pathPlotParameter", "pathPlotLegend"))
  plt$position <- 2

  modelContainer[["plot"]] <- plt

  if (any(sapply(dataset, is.ordered))) {
    plt$setError(gettext("Model plot not available with ordinal variables"))
    return()
  }

  if (modelContainer$getError()) return()

  # create a qgraph object using semplot
  po <- .lavToPlotObj(modelContainer[["model"]][["object"]])
  pp <- jaspBase:::.suppressGrDevice(semPlot::semPaths(
    object         = po,
    layout         = "tree2",
    intercepts     = FALSE,
    reorder        = FALSE,
    whatLabels     = ifelse(options$pathPlotParameter, "par", "name"),
    edge.color     = "black",
    color          = list(lat = "#EAEAEA", man = "#EAEAEA", int = "#FFFFFF"),
    title          = FALSE,
    legend         = options$pathPlotLegend,
    legend.mode    = "names",
    legend.cex     = 0.6,
    label.cex      = 1.3,
    edge.label.cex = 0.9,
    nodeNames      = decodeColNames(po@Vars$name),
    nCharNodes     = 3,
    rotation       = 2
  ))

  plt$plotObject <- pp
}

.mimicSyntax <- function(modelContainer, options, ready) {
  if (!options$syntax || !ready) return()
  modelContainer[["syntax"]] <- createJaspHtml(.mimicToLavMod(options, FALSE), class = "jasp-code", title = gettext("Model syntax"))
  modelContainer[["syntax"]]$dependOn("syntax")
  modelContainer[["syntax"]]$position <- 3
}

.mimicFootMessage <- function(fit) {
  check <- .withWarnings(lavaan:::lav_object_post_check(fit))
  if (check$value) return("")
  wrn <- lapply(check$warnings, function(w) w$message)
  return(paste(wrn, collapse = "\n- "))
}
