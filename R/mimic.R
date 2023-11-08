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
  options <- .mimicEditOptions(dataset, options)
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

.mimicEditOptions <- function(dataset, options) {
  if (length(options$indicators) == 0) return(options)

  indicators <- options$indicators
  ordered_vars <- sapply(dataset[,indicators], is.ordered)
  if (sum(ordered_vars > 0) && options[["estimator"]] == "default") {
    options[["estimator"]] <- "wlsmv"
    if (options[["naAction"]] == "default")
      options[["naAction"]] <- "listwise"
  } else {
    if (options[["naAction"]] == "default") {
      if(options[["estimator"]] %in% c("gls", "wls", "uls", "dwls")) {
        options[["naAction"]] <- "listwise"
      } else {
        options[["naAction"]] <- "fiml"
      }
    }
  }
  return(options)
}

.mimicCheckErrors <- function(dataset, options) {
  if (length(options$indicators) < 3 || length(options$predictors) == 0) return(FALSE)

  # Check for missing value handling
  if (options$estimator %in% c("gls", "wls", "uls", "dwls") && options$naAction == "fiml")
    jaspBase:::.quitAnalysis(gettext("FIML missing data handling only available with ML-type estimators."))

  # Exogenous variables can be binary or continuous
  exo <- options$predictors
  # Endogenous variables need to be scale or ordinal
  endo <- options$indicators

  customChecks <- list(
    checkExogenous = function() {
      admissible <- vapply(exo, function(exo_var) {
        var <- na.omit(dataset[[.v(exo_var)]])
        if (is.ordered(var)) return(FALSE)
        if ((is.character(var) || is.factor(var)) && length(unique(var)) != 2) return(FALSE)
        return(TRUE)
      }, TRUE)
      if (!all(admissible))
        gettextf("Not all exogenous variables are admissible. Inadmissible exogenous variables: %s. Only binary or scale exogenous variables allowed.", paste(exo[!admissible], collapse = ", "))
    },

    checkEndogenous = function() {
      if (length(options$confounds) > 0) endo <- c(endo, options$predictor)
      admissible <- vapply(endo, function(endo_var) {
        var <- na.omit(dataset[[.v(endo_var)]])
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
        var <- na.omit(dataset[[.v(endo_var)]])
        if (is.ordered(var) && options$naAction == "fiml") {
          return(FALSE)
        }
        return(TRUE)
      }, TRUE)

      if (!all(admissible)) {
        if (options[["estimator"]] %in% c("ml", "mlf", "mlr")) {
          gettextf("ML estimation only available when all endogenous variables are of scale type. Ordinal endogenous variables in the model: %s", paste(endo[!admissible], collapse = ", "))
        } else {
          gettextf("FIML missing value handling only available when all endogenous variables are of scale type. Ordinal endogenous variables in the model: %s", paste(endo[!admissible], collapse = ", "))
        }
      }
    }

  )

  .hasErrors(dataset, type = c('observations', 'variance', 'infinity'), custom = customChecks,
             all.target = c(endo, exo), observations.amount = paste('<', length(c(endo, exo))),
             exitAnalysisIfErrors = TRUE)

  return(TRUE)
}

# Results functions ----

.mimicComputeResults <- function(modelContainer, dataset, options, ready) {
  if (options[["estimator"]] %in% c("default", "ml", "gls", "wls", "uls", "dwls")) {
    mimicResult <- try(lavaan::sem(
      model           = .mimicToLavMod(options),
      data            = dataset,
      se              = switch(options[["errorCalculationMethod"]],
                               "bootstrap" = "standard",
                               "robust" = "robust.sem",
                               "standard" = "standard"),
      information     = "expected",
      mimic           = options$emulation,
      estimator       = options$estimator,
      test            = switch(options[["modelTest"]],
                               "satorraBentler" = "Satorra.Bentler",
                               "yuanBentler" = "Yuan.Bentler",
                               "meanAndVarianceAdjusted" = "mean.var.adjusted",
                               "scaledAndShifted" = "scaled.shifted",
                               "bollenStine" = "Bollen.Stine",
                               "default" = "default",
                               "standard" = "standard"
      ),
      missing         = options$naAction,
      std.lv          = TRUE,
      std.ov          = options[["standardizedVariable"]]
    ))
  } else {
    mimicResult <- try(lavaan::sem(
      model           = .mimicToLavMod(options),
      data            = dataset,
      information     = "expected",
      mimic           = options$emulation,
      estimator       = options$estimator,
      missing         = options$naAction,
      std.lv          = TRUE,
      std.ov          = options[["standardizedVariable"]]
    ))
  }

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
    mimicResult <- lavBootstrap(mimicResult, options$bootstrapSamples)
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
      .v(options[["indicators"]]),
      collapse = " + ", sep = ""
    )
  )
  structural <- paste(
    "  Y ~ ",
    paste(
      paste("beta", seq_along(options[["predictors"]]), "*", sep = ""),
      .v(options[["predictors"]]),
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
      "bootstrapSamples", "emulation", "errorCalculationMethod", "estimator", "standardizedVariable",
      "naAction", "modelTest")
    )
    jaspResults[["modelContainer"]] <- modelContainer
  }

  return(modelContainer)
}

.mimicFitTable <- function(modelContainer, dataset, options, ready) {
  if (!is.null(modelContainer[["fittab"]])) return()

  fittab <- createJaspTable(title = gettext("Chi Square test"))
  fittab$position <- 0

  fittab$addColumnInfo(name="Model", title = "",                                        type = "string")
  fittab$addColumnInfo(name="chisq", title = gettextf("%1$s%2$s", "\u03C7",  "\u00B2"), type = "number")
  fittab$addColumnInfo(name="df",    title = gettext("df"),                             type = "integer")
  fittab$addColumnInfo(name="pval",  title = gettext("p"),                              type = "pvalue")

  fittab$addCitation("Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2), 1-36. URL http://www.jstatsoft.org/v48/i02/")

  modelContainer[["fittab"]] <- fittab

  fittab[["Model"]] <- c(gettext("Baseline model"), gettext("Factor model"))

  if (!ready) return()

  # add data to the table!
  .mimicComputeResults(modelContainer, dataset, options, ready)

  if (modelContainer$getError()) return()

  #add ordinal endogenous estimation footnote
  if (options[["estimator"]] == "wlsmv") {
    fittab$addFootnote(message = gettext("Ordinal endogenous variable(s) detected! Automatically switched to <i>DWLS</i> estimation with <i>robust</i> standard errors, <i>robust</i> confidence intervals and a <i>scaled and shifted</i> test-statistic. <br><i>If you wish to override these settings, please select another (LS-)estimator and \u2014optionally\u2014 select the preferred error calculation method and model test in the 'Estimation' tab.</i>"))
  }

  # add test statistic correction footnote
  if (options[["estimator"]] != "wlsmv") {
    test <- lavaan::lavInspect(modelContainer[["model"]][["object"]], what = "options")[["test"]]

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

  # add missing data handling footnote
  nrm <- nrow(dataset) - lavaan::lavInspect(modelContainer[["model"]][["object"]], "ntotal")
  if(nrm == 0) {
    fittab$addFootnote(gettextf("Missing data handling: <i>%1$s</i>.", ifelse(options[["naAction"]] == "fiml", "Full information maximum likelihood", options[["naAction"]])))
  } else {
    fittab$addFootnote(gettextf("Missing data handling: <i>%1$s</i>. Removed cases: %2$s", ifelse(options[["naAction"]] == "fiml", "Full information maximum likelihood", options[["naAction"]]), nrm))
  }

  # warnings footnote
  fittab$addFootnote(.mimicFootMessage(modelContainer[["model"]][["object"]]))

  fm <- lavaan::fitmeasures(modelContainer[["model"]][["object"]])

  if (lavaan::lavInspect(modelContainer[["model"]][["object"]], what = "options")[["test"]] != "standard")
    fm[c("chisq", "df", "baseline.chisq", "baseline.df", "pvalue", "baseline.pvalue")] <- fm[c("chisq.scaled", "df.scaled", "baseline.chisq.scaled", "baseline.df.scaled", "pvalue.scaled", "baseline.pvalue.scaled")]

  fittab[["df"]]    <- round(fm[c("baseline.df", "df")], 3)
  fittab[["chisq"]] <- fm[c("baseline.chisq", "chisq")]
  fittab[["pval"]]  <- fm[c("baseline.pvalue", "pvalue")]
}

.mimicAdditionalFit <- function(modelContainer, options, ready) {
  if (!options$additionalFitMeasures || !is.null(modelContainer[["addfit"]])) return()
  fitms <- createJaspContainer(gettext("Additional fit measures"))
  fitms$dependOn("additionalFitMeasures")
  fitms$position <- 0.75

  # Fit indices
  fitms[["indices"]] <- fitin <- createJaspTable(gettext("Fit indices"))
  fitin$addColumnInfo(name = "index", title = gettext("Index"), type = "string")
  fitin$addColumnInfo(name = "value", title = gettext("Value"), type = "number", format = "sf:4;dp:3")
  fitin$setExpectedSize(rows = 1, cols = 2)

  # information criteria
  fitms[["incrits"]] <- fitic <- createJaspTable(gettext("Information criteria"))
  fitic$addColumnInfo(name = "index", title = "",               type = "string")
  fitic$addColumnInfo(name = "value", title = gettext("Value"), type = "number", format = "sf:4;dp:3")
  fitic$setExpectedSize(rows = 1, cols = 2)
  if (options$estimator %in% c("dwls", "gls", "wls", "uls", "wlsmv"))
    fitic$setError(gettext("The information criteria are only available with ML-type estimators, please select the 'ML', 'MLF' or 'MLR' estimator in the 'Estimation' tab."))

  # other fit measures
  fitms[["others"]] <- fitot <- createJaspTable(gettext("Other fit measures"))
  fitot$addColumnInfo(name = "index", title = gettext("Metric"), type = "string")
  fitot$addColumnInfo(name = "value", title = gettext("Value"),  type = "number", format = "sf:4;dp:3")
  fitot$setExpectedSize(rows = 1, cols = 2)

  modelContainer[["addfit"]] <- fitms

  if (!ready || modelContainer$getError()) return()

  # actually compute the fit measures
  fm <- lavaan::fitmeasures(modelContainer[["model"]][["object"]])

  if (lavaan::lavInspect(modelContainer[["model"]][["object"]], what = "options")[["test"]] != "standard") {
    fm[c("chisq", "df", "baseline.chisq", "baseline.df", "cfi", "tli", "nnfi", "nfi", "rfi", "ifi", "rni", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue")] <- fm[c("chisq.scaled", "df.scaled", "baseline.chisq.scaled", "baseline.df.scaled", "cfi.scaled", "tli.scaled", "nnfi.scaled", "nfi.scaled",  "rfi.scaled", "ifi.scaled", "rni.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "rmsea.pvalue.scaled")]
    fm["pnfi"] <- NA
    }

  # Fit indices
  fm_fitin <- fm[c("cfi", "tli", "nnfi", "nfi", "pnfi", "rfi", "ifi", "rni")]
  isna <- is.na(fm_fitin)
  indices <- c(
    gettext("Comparative Fit Index (CFI)"),
    gettext("Tucker-Lewis Index (TLI)"),
    gettext("Bentler-Bonett Non-normed Fit Index (NNFI)"),
    gettext("Bentler-Bonett Normed Fit Index (NFI)"),
    gettext("Parsimony Normed Fit Index (PNFI)"),
    gettext("Bollen's Relative Fit Index (RFI)"),
    gettext("Bollen's Incremental Fit Index (IFI)"),
    gettext("Relative Noncentrality Index (RNI)")
  )
  fitin[["index"]] <- indices[!isna]
  fitin[["value"]] <- fm_fitin[!isna]

  if (sum(isna) > 0)
    fitin$addFootnote(gettextf("The following fit indices could not be computed: %s.", paste(indices[isna], collapse = ", ")))

  # information criteria
  fm_fitic <- fm[c("logl", "npar", "aic", "bic", "bic2")]
  isna <- is.na(fm_fitic)

  indices <- c(
    gettext("Log-likelihood"),
    gettext("Number of free parameters"),
    gettext("Akaike (AIC)"),
    gettext("Bayesian (BIC)"),
    gettext("Sample-size adjusted Bayesian (SSABIC)")
  )
  fitic[["index"]] <- indices[!isna]
  fitic[["value"]] <- fm_fitic[!isna]

  if (sum(isna) > 0)
    fitic$addFootnote(gettextf("The following information criteria could not be computed: %s.", paste(indices[isna], collapse = ", ")))

  # other fitmeasures
  fm_fitot <- fm[c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue",
                   "srmr", "cn_05", "cn_01", "gfi", "mfi", "ecvi")]
  isna <- is.na(fm_fitot)
  indices <- c(
    gettext("Root mean square error of approximation (RMSEA)"),
    gettext("RMSEA 90%% CI lower bound"),
    gettext("RMSEA 90%% CI upper bound"),
    gettext("RMSEA p-value"),
    gettext("Standardized root mean square residual (SRMR)"),
    gettextf("Hoelter's critical N (%s = .05)","\u03B1"),
    gettextf("Hoelter's critical N (%s = .01)","\u03B1"),
    gettext("Goodness of fit index (GFI)"),
    gettext("McDonald fit index (MFI)"),
    gettext("Expected cross validation index (ECVI)")
  )
  fitot[["index"]] <- indices[!isna]
  fitot[["value"]] <- fm_fitot[!isna]

  if (sum(isna) > 0)
    fitot$addFootnote(gettextf("The following other fit measure(s) could not be computed: %s.", paste(indices[isna], collapse = ", ")))

  return()
}

.mimicParTable <- function(modelContainer, options, ready) {
  if (!is.null(modelContainer[["parest"]])) return()
  modelContainer[["parest"]] <- pecont <- createJaspContainer(gettext("Parameter estimates"))
  pecont$dependOn(c("ciLevel", "bootstrapCiType", "standardizedEstimate", "standardizedEstimateType"))
  pecont$position <- 0.5

  ## betas
  bettab <- createJaspTable(title = gettext("Predictor coefficients"))

  est_title <- ifelse(options[["standardizedEstimate"]], gettext("Standardized Estimate"), gettext("Estimate"))

  bettab$addColumnInfo(name = "rhs",      title = gettext("Predictor"),  type = "string")
  bettab$addColumnInfo(name = "est",      title = est_title,             type = "number", format = "sf:4;dp:3")
  bettab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number", format = "sf:4;dp:3")
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
  lamtab$addColumnInfo(name = "est",      title = est_title,             type = "number", format = "sf:4;dp:3")
  lamtab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number", format = "sf:4;dp:3")
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

  if (options[["standardizedEstimate"]]) {
    type <- switch(options[["standardizedEstimateType"]],
                   "all" = "std.all",
                   "latents" = "std.lv",
                   "noX" = "std.nox")
    pe <- lavaan::standardizedsolution(modelContainer[["model"]][["object"]], type = type, level = options[["ciLevel"]])
  } else {
    pe <- lavaan::parameterestimates(modelContainer[["model"]][["object"]], standardized = TRUE, level = options[["ciLevel"]],
                                     boot.ci.type = bootstrapCiType)
  }

  foot_message <- .mimicEstFootMessage(modelContainer, options)

  pe_bet <- pe[substr(pe$label, 1, 1) == "b", ]
  bettab[["rhs"]]      <- pe_bet$rhs
  bettab[["est"]]      <- if (options[["standardizedEstimate"]]) pe_bet[["est.std"]] else pe_bet[["est"]]
  bettab[["se"]]       <- pe_bet$se
  bettab[["z"]]        <- pe_bet$z
  bettab[["pvalue"]]   <- pe_bet$pvalue
  bettab[["ci.lower"]] <- pe_bet$ci.lower
  bettab[["ci.upper"]] <- pe_bet$ci.upper
  bettab$addFootnote(foot_message)

  pe_lam <- pe[substr(pe$label, 1, 1) == "l", ]
  lamtab[["rhs"]]      <- pe_lam$rhs
  lamtab[["est"]]      <- if (options[["standardizedEstimate"]]) pe_lam[["est.std"]] else pe_lam[["est"]]
  lamtab[["se"]]       <- pe_lam$se
  lamtab[["z"]]        <- pe_lam$z
  lamtab[["pvalue"]]   <- pe_lam$pvalue
  lamtab[["ci.lower"]] <- pe_lam$ci.lower
  lamtab[["ci.upper"]] <- pe_lam$ci.upper
  lamtab$addFootnote(foot_message)
}

.mimicRsquared <- function(modelContainer, options, ready) {
  if (!options$rSquared || !is.null(modelContainer[["rsquared"]])) return()

  tabr2 <- createJaspTable(gettext("R-Squared"))
  tabr2$addColumnInfo(name = "var", title = "", type = "string")
  tabr2$addColumnInfo(name = "rsq", title = "R\u00B2", type = "number", format = "sf:4;dp:3")
  tabr2$dependOn(options = "rSquared")
  tabr2$position <- 1

  modelContainer[["rsquared"]] <- tabr2

  if (!ready || modelContainer$getError()) return()

  r2res              <- lavaan::inspect(modelContainer[["model"]][["object"]], "r2")
  tabr2[["var"]] <- .unv(names(r2res))
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

.mimicEstFootMessage <- function(modelContainer, options) {
  if (is.null(modelContainer[["model"]][["object"]])) return()

  fit <- modelContainer[["model"]][["object"]]
  # Create the footnote message
  if (options[["estimator"]] %in% c("default", "ml", "gls", "wls", "uls", "dwls", "pml")) {
    se_type <- switch(options$errorCalculationMethod,
                      "bootstrap" = gettext("delta method"),
                      "standard"  = gettext("delta method"),
                      "default"   = gettext("delta method"),
                      "robust"    = gettext("robust")
    )
    ci_type <- switch(options$errorCalculationMethod,
                      "bootstrap" = switch(options$bootstrapCiType,
                                           "percentile"              = gettext("percentile bootstrap"),
                                           "normalTheory"            = gettext("normal theory bootstrap"),
                                           "percentileBiasCorrected" = gettext("bias-corrected percentile bootstrap")
                      ),
                      "standard"  = gettext("normal theory"),
                      "default"   = gettext("normal theory"),
                      "robust"    = gettext("robust")
    )
  } else {
    modelOptions <- lavaan::lavInspect(fit, what = "options")
    if(options[["estimator"]] == "mlf") {
      se_type <- gettext("first-order derivatives based")
    } else {
      se_type <- modelOptions$se
      se_type <- gsub('.sem', '', se_type)
      se_type <- gettext(stringr::str_to_title(gsub('\\.', ' ', se_type)))
      se_type <- paste0(tolower(substr(se_type, 1, 1)), substr(se_type, 2, nchar(se_type)))
    }
    ci_type <- gettext("robust")
  }

  if (options$errorCalculationMethod == "bootstrap" && nrow(fit@boot[["coef"]]) < options$bootstrapSamples) {
    return(gettextf(
      "<i>%1$s</i> estimation with <i>%2$s</i> standard errors and <i>%3$s</i> confidence intervals. NB: Not all bootstrap samples were successful: CI based on %4$.0f samples.",
      fit@Options$estimator, se_type, ci_type, nrow(fit@boot[["coef"]])
    ))
  } else {
    return(gettextf(
      "<i>%1$s</i> estimation with <i>%2$s</i> standard errors and <i>%3$s</i> confidence intervals.",
      fit@Options$estimator, se_type, ci_type
    ))
  }
}
