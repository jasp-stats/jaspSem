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

MediationAnalysisInternal <- function(jaspResults, dataset, options, ...) {
  jaspResults$addCitation("Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2), 1-36. URL http://www.jstatsoft.org/v48/i02/")

  # Read dataset
  dataset <- .medReadData(dataset, options)
  options <- .medEditOptions(dataset, options)
  ready   <- .medCheckErrors(dataset, options)

  modelContainer <- .getModelContainer(jaspResults)

  # Output functions
  .medParTable(   modelContainer, dataset, options, ready)
  .medTotIndTable(modelContainer, options, ready)
  .medResTable(   modelContainer, options, ready)
  .medPathTable(  modelContainer, options, ready)
  .medRsquared(   modelContainer, options, ready)
  .medPathPlot(   modelContainer, options, ready)
  .medSyntax(     modelContainer, options, ready)

}

# Preprocessing functions ----
.medReadData <- function(dataset, options) {
  if (!is.null(dataset)) return(dataset)

  vars <- c(options$predictors, options$mediators, options$outcomes, options$confounds)
  return(.readDataSetToEnd(columns = vars))
}

.medEditOptions <- function(dataset, options) {
  if (length(options$outcomes) == 0 || length(options$mediators) == 0) return(options)

  indicators <- c(options$mediators, options$outcomes)
  ordered_vars <- sapply(dataset[,indicators], is.ordered)
  if (sum(ordered_vars > 0) && options[["estimator"]] == "default") {
    options[["estimator"]] <- "wlsmv"
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
  return(options)
}

.medCheckErrors <- function(dataset, options) {
  if (length(options$outcomes) == 0 || length(options$mediators) == 0 || length(options$predictors) == 0) return(FALSE)

  # Check for missing value handling
  if (options$estimator %in% c("gls", "wls", "uls", "dwls", "pml") && options$naAction == "fiml")
    jaspBase:::.quitAnalysis(gettext("FIML missing data handling only available with ML estimators, please select the 'ML', 'MLF' or 'MLR' estimator in the 'Estimation' tab."))

  # Exogenous variables can be binary or continuous
  exo <- ifelse(length(options$confounds) > 0, options$confounds, options$predictors)
  # Endogenous variables need to be scale or ordinal
  endo <- c(options$mediators, options$outcomes)

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
      if (length(options$confounds) > 0) endo <- c(endo, options$predictors)
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
      if (length(options$confounds) > 0) endo <- c(endo, options$predictors)

      admissible <- vapply(endo, function(endo_var) {
        var <- na.omit(dataset[[endo_var]])
        if (is.ordered(var) && (options$naAction == "fiml" || options$estimator %in% c("ml", "mlf", "mlr", "pml"))) {
          return(FALSE)
        }
        return(TRUE)
      }, TRUE)

      if (!all(admissible)) {
        if (options[["estimator"]] %in% c("ml", "mlf", "mlr", "pml")) {
          gettextf("ML estimation only available when all endogenous variables are of scale type. Ordinal endogenous variables in the model: %s", paste(endo[!admissible], collapse = ", "))
        } else {
          gettextf("FIML missing value handling only available when all endogenous variables are of scale type. Ordinal endogenous variables in the model: %s", paste(endo[!admissible], collapse = ", "))
        }
      }
    },

    checkNaAction = function() {
      if (options$naAction %in% c("twoStage", "twoStageRobust"))
        gettext("Missing data handling methods 'two-stage' and 'robust two-stage' are currently only available for the Structural Equation Modeling analysis.")
    }

  )

  .hasErrors(dataset, type = c('observations', 'variance', 'infinity'), custom = customChecks,
             all.target = c(endo, exo), observations.amount = paste('<', length(c(endo, exo))),
             exitAnalysisIfErrors = TRUE)

  return(TRUE)
}

# Results functions ----

.medComputeResults <- function(modelContainer, dataset, options, ready) {
  if (options[["estimator"]] %in% c("default", "ml", "gls", "wls", "uls", "dwls")) {
    medResult <- try(lavaan::sem(
      model           = .medToLavMod(options),
      data            = dataset,
      information     = options$informationMatrix,
      se              = switch(options[["errorCalculationMethod"]], "bootstrap" = "standard", "robust" = "robust.sem", "standard" = "standard"),
      mimic           = options$emulation,
      estimator       = options$estimator,
      std.ov          = options$standardizedVariable,
      missing         = switch(options[["naAction"]],
                               "twoStage" = "two.stage",
                               "twoStageRobust" = "robust.two.stage",
                               "doublyRobust" = "doubly.robust",
                               options[["naAction"]]
      )
    ))
  } else {
    medResult <- try(lavaan::sem(
      model           = .medToLavMod(options),
      data            = dataset,
      information     = options$informationMatrix,
      mimic           = options$emulation,
      estimator       = options$estimator,
      std.ov          = options$standardizedVariable,
      missing         = switch(options[["naAction"]],
                               "twoStage" = "two.stage",
                               "twoStageRobust" = "robust.two.stage",
                               "doublyRobust" = "doubly.robust",
                               options[["naAction"]]
      )
    ))
  }


  if (inherits(medResult, "try-error")) {
    errmsg <- gettextf("Estimation failed\nMessage:\n%s", attr(medResult, "condition")$message)
    modelContainer$setError(.decodeVarsInMessage(names(dataset), errmsg))
  }

  if (options$errorCalculationMethod == "bootstrap") {
    medResult <- lavBootstrap(medResult, options$bootstrapSamples)
  }

  modelContainer[["model"]] <- createJaspState(medResult)
  return(medResult)
}

.medToLavMod <- function(options, base64 = TRUE) {

  if (!base64) .v <- I

  n_pred <- length(options$predictors)
  n_medi <- length(options$mediators)
  n_deps <- length(options$outcomes)
  n_conf <- length(options$confounds)

  title    <- "
  # ---------------------------------
  # Mediation model generated by JASP
  # ---------------------------------\n
  "
  dep_part <- "# dependent regression"
  for (d in 1:n_deps) {
    dep_part <- paste0(dep_part, "\n", options$outcomes[d], " ~")
    for (m in 1:n_medi) {
      par_name <- paste0(" b", d, m)
      dep_part <- paste0(dep_part, par_name, "*", options$mediators[m], " +")
    }
    for (p in 1:n_pred) {
      par_name <- paste0(" c", d, p)
      dep_part <- paste0(dep_part, par_name, "*", options$predictors[p])
      if (p != n_pred)
        dep_part <- paste0(dep_part, " +")
    }
  }
  dep_part <- paste0(dep_part, "\n\n")

  med_part <- "# mediator regression"
  for (m in 1:n_medi) {
    med_part <- paste0(med_part, "\n", options$mediators[m], " ~")
    for (p in 1:n_pred) {
      par_name <- paste0(" a", m, p)
      med_part <- paste0(med_part, par_name, "*", options$predictors[p])
      if (p != n_pred)
        med_part <- paste0(med_part, " +")
    }
  }
  med_part <- paste0(med_part, "\n\n")

  conf_part     <- NULL
  pred_res_part <- NULL
  if (n_conf > 0) {
    conf_part <- "# confounder adjustment"
    for (var in c(options$predictors, options$mediators, options$outcomes)) {
      conf_part <- paste0(conf_part, "\n", var, " ~ ", paste(options$confounds, collapse = " + "))
    }
    conf_part <- paste0(conf_part, "\n\n")
    if (n_pred > 1) {
      pred_res_part <- "# predictor residual covariance"
      idx_mat  <- which(upper.tri(diag(n_pred)), arr.ind = TRUE)
      for (i in 1:nrow(idx_mat)) {
        v1 <- options$predictors[idx_mat[i,1]]
        v2 <- options$predictors[idx_mat[i,2]]
        pred_res_part <- paste0(pred_res_part, "\n", v1, " ~~ ", v2)
      }
      pred_res_part <- paste0(pred_res_part, "\n\n")
    }
  }

  med_res_part <- NULL
  if (n_medi > 1) {
    med_res_part <- "# mediator residual covariance"
    idx_mat  <- which(upper.tri(diag(n_medi)), arr.ind = TRUE)
    for (i in 1:nrow(idx_mat)) {
      v1 <- options$mediators[idx_mat[i,1]]
      v2 <- options$mediators[idx_mat[i,2]]
      med_res_part <- paste0(med_res_part, "\n", v1, " ~~ ", v2)
    }
    med_res_part <- paste0(med_res_part, "\n\n")
  }

  res_part <- NULL
  if (n_deps > 1) {
    res_part <- "# dependent residual covariance"
    idx_mat  <- which(upper.tri(diag(n_deps)), arr.ind = TRUE)
    for (i in 1:nrow(idx_mat)) {
      v1 <- options$outcomes[idx_mat[i,1]]
      v2 <- options$outcomes[idx_mat[i,2]]
      res_part <- paste0(res_part, "\n", v1, " ~~ ", v2)
    }
    res_part <- paste0(res_part, "\n\n")
  }


  dec_part <- "# effect decomposition"
  for (d in 1:n_deps) {
    for (p in 1:n_pred) {
      dec_part <- paste0(dec_part, "\n# y", d, " ~ x", p)
      parnames <- c()
      for (m in 1:n_medi) {
        par_name <- paste0("ind_x", p, "_m", m, "_y", d)
        parnames <- c(parnames, par_name)
        a_name   <- paste0("a", m, p)
        b_name   <- paste0("b", d, m)
        dec_part <- paste0(dec_part, "\n", par_name, " := ", a_name, "*", b_name)
      }
      dec_part <- paste0(dec_part, "\nind_x", p, "_y", d, "    := ", paste(parnames, collapse = " + "))
      dec_part <- paste0(dec_part, "\ntot_x", p, "_y", d, "    := ", "ind_x", p, "_y", d, " + c", d, p, "\n")
    }
  }

  return(paste0(title, dep_part, med_part, conf_part,
                pred_res_part, med_res_part, res_part,
                dec_part))
}

# Output functions ----

.getModelContainer <- function(jaspResults, options) {
  if (!is.null(jaspResults[["modelContainer"]])) {
    modelContainer <- jaspResults[["modelContainer"]]
  } else {
    modelContainer <- createJaspContainer()
    modelContainer$dependOn(c(
      "predictors", "mediators", "outcomes", "confounds", "includemeanstructure",
      "bootstrapSamples", "fixManifestInterceptsToZero", "emulation", "informationMatrix", "errorCalculationMethod", "estimator",
      "standardizedVariable", "naAction")
    )
    jaspResults[["modelContainer"]] <- modelContainer
  }

  return(modelContainer)
}


.medParTable <- function(modelContainer, dataset, options, ready) {
  if (!is.null(modelContainer[["parest"]])) return()
  modelContainer[["parest"]] <- pecont <- createJaspContainer(gettext("Parameter estimates"))
  pecont$dependOn(options = c("ciLevel", "bootstrapCiType", "standardizedEstimate", "standardizedEstimateType"))
  pecont$position <- 0

  est_title <- ifelse(options[["standardizedEstimate"]], gettext("Standardized Estimate"), gettext("Estimate"))
  ## direct effects
  dirtab <- createJaspTable(title = gettext("Direct effects"))

  dirtab$addColumnInfo(name = "lhs",      title = "",                    type = "string")
  dirtab$addColumnInfo(name = "op",       title = "",                    type = "string")
  dirtab$addColumnInfo(name = "rhs",      title = "",                    type = "string")
  dirtab$addColumnInfo(name = "est",      title = est_title,             type = "number", format = "sf:4;dp:3")
  dirtab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number", format = "sf:4;dp:3")
  dirtab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number", format = "sf:4;dp:3")
  dirtab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "number", format = "dp:3;p:.001")
  dirtab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number", format = "sf:4;dp:3",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
  dirtab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number", format = "sf:4;dp:3",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))


  pecont[["dir"]] <- dirtab

  ## indirect effects
  indtab <- createJaspTable(title = gettext("Indirect effects"))

  indtab$addColumnInfo(name = "x",        title = "",                    type = "string")
  indtab$addColumnInfo(name = "op1",      title = "",                    type = "string")
  indtab$addColumnInfo(name = "m",        title = "",                    type = "string")
  indtab$addColumnInfo(name = "op2",      title = "",                    type = "string")
  indtab$addColumnInfo(name = "y",        title = "",                    type = "string")
  indtab$addColumnInfo(name = "est",      title = est_title,             type = "number", format = "sf:4;dp:3")
  indtab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number", format = "sf:4;dp:3")
  indtab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number", format = "sf:4;dp:3")
  indtab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "number", format = "dp:3;p:.001")
  indtab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number", format = "sf:4;dp:3",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
  indtab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number", format = "sf:4;dp:3",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))

  pecont[["ind"]] <- indtab

  ## total effects
  tottab <- createJaspTable(title = "Total effects")

  tottab$addColumnInfo(name = "lhs",      title = "",                    type = "string")
  tottab$addColumnInfo(name = "op",       title = "",                    type = "string")
  tottab$addColumnInfo(name = "rhs",      title = "",                    type = "string")
  tottab$addColumnInfo(name = "est",      title = est_title,             type = "number", format = "sf:4;dp:3")
  tottab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number", format = "sf:4;dp:3")
  tottab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number", format = "sf:4;dp:3")
  tottab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "number", format = "dp:3;p:.001")
  tottab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number", format = "sf:4;dp:3",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
  tottab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number", format = "sf:4;dp:3",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))

  pecont[["tot"]] <- tottab

  if (!ready) return()


  # add data to the tables!
  .medComputeResults(modelContainer, dataset, options, ready)

  if (modelContainer$getError()) return()

  foot_message <- .medFootMessage(modelContainer, options)

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

  # Fill direct effects
  pe_dir <- pe[substr(pe$label, 1, 1) == "c", ]
  dirtab[["lhs"]]      <- pe_dir$rhs
  dirtab[["op"]]       <- rep("\u2192", nrow(pe_dir))
  dirtab[["rhs"]]      <- pe_dir$lhs
  dirtab[["est"]]      <- if (options[["standardizedEstimate"]]) pe_dir[["est.std"]] else pe_dir[["est"]]
  dirtab[["se"]]       <- pe_dir$se
  dirtab[["z"]]        <- pe_dir$z
  dirtab[["pvalue"]]   <- pe_dir$pvalue
  dirtab[["ci.lower"]] <- pe_dir$ci.lower
  dirtab[["ci.upper"]] <- pe_dir$ci.upper

  #Footnotes
  if (options[["estimator"]] == "wlsmv") {
    dirtab$addFootnote(message = gettext("Ordinal endogenous variable(s) detected! Automatically switched to <i>DWLS</i> estimation with <i>robust</i> standard errors, <i>robust</i> confidence intervals and a <i>scaled and shifted</i> test-statistic. <br><i>If you wish to override these settings, please select another (LS-)estimator and/or model test and \u2014optionally\u2014 change the error calculation method in the 'Estimation' tab.</i>"))
  } else {
    dirtab$addFootnote(foot_message)
  }
  nrm <- nrow(dataset) - lavaan::lavInspect(modelContainer[["model"]][["object"]], "ntotal")
  method <- switch(options[["naAction"]],
                   "twoStage" = "two-stage",
                   "twoStageRobust" = "robust two-stage",
                   "doublyRobust" = "doubly robust",
                   "fiml" = "full information maximum likelihood",
                   options[["naAction"]])
  if(nrm > 0)
    dirtab$addFootnote(gettextf("Missing data handling: <i>%1$s</i>. Removed cases: %2$s", method, nrm))

  # Fill indirect effects
  pe_ind <- pe[pe$op == ":=" & vapply(gregexpr("_", pe$lhs), length, 1) == 3, ]
  # get predictors, mediators, outcome combinations
  terms <- strsplit(pe_ind[["label"]], "_")
  termsCombinations <- list(
    x = .medGetTermsFromLavaanTable(terms, 2, "x"),
    m = .medGetTermsFromLavaanTable(terms, 3, "m"),
    y = .medGetTermsFromLavaanTable(terms, 4, "y")
  )

  indtab[["x"]]        <- options[["predictors"]][termsCombinations[["x"]]]
  indtab[["op1"]]      <- rep("\u2192", nrow(pe_ind))
  indtab[["m"]]        <- options[["mediators"]][termsCombinations[["m"]]]
  indtab[["op2"]]      <- rep("\u2192", nrow(pe_ind))
  indtab[["y"]]        <- options[["outcomes"]][termsCombinations[["y"]]]
  indtab[["est"]]      <- if (options[["standardizedEstimate"]]) pe_ind[["est.std"]] else pe_ind[["est"]]
  indtab[["se"]]       <- pe_ind$se
  indtab[["z"]]        <- pe_ind$z
  indtab[["pvalue"]]   <- pe_ind$pvalue
  indtab[["ci.lower"]] <- pe_ind$ci.lower
  indtab[["ci.upper"]] <- pe_ind$ci.upper
  indtab$addFootnote(foot_message)


  # Fill total effects
  pe_tot <- pe[pe$op == ":=" & substr(pe$lhs, 1, 3) == "tot",]

  # get predictors, outcome combinations
  terms <- strsplit(pe_tot[["label"]], "_")
  termsCombinations <- list(
    x = .medGetTermsFromLavaanTable(terms, 2, "x"),
    y = .medGetTermsFromLavaanTable(terms, 3, "y")
  )

  tottab[["lhs"]]      <- options[["predictors"]][termsCombinations[["x"]]]
  tottab[["op"]]       <- rep("\u2192", nrow(pe_tot))
  tottab[["rhs"]]      <- options[["outcomes"]][termsCombinations[["y"]]]
  tottab[["est"]]      <- if (options[["standardizedEstimate"]]) pe_tot[["est.std"]] else pe_tot[["est"]]
  tottab[["se"]]       <- pe_tot$se
  tottab[["z"]]        <- pe_tot$z
  tottab[["pvalue"]]   <- pe_tot$pvalue
  tottab[["ci.lower"]] <- pe_tot$ci.lower
  tottab[["ci.upper"]] <- pe_tot$ci.upper
  tottab$addFootnote(foot_message)
}

.medTotIndTable <- function(modelContainer, options, ready) {
  if (!options[["totalIndirectEffect"]] || !length(options$mediators) > 1) return()

  est_title <- ifelse(options[["standardizedEstimate"]], gettext("Standardized Estimate"), gettext("Estimate"))

  ttitab <- createJaspTable(title = gettext("Total indirect effects"))
  ttitab$dependOn(c("totalIndirectEffect","ciLevel", "bootstrapCiType", "standardizedEstimate", "standardizedEstimateType"))

  ttitab$addColumnInfo(name = "lhs",      title = "",                    type = "string")
  ttitab$addColumnInfo(name = "op",       title = "",                    type = "string")
  ttitab$addColumnInfo(name = "rhs",      title = "",                    type = "string")
  ttitab$addColumnInfo(name = "est",      title = est_title,             type = "number", format = "sf:4;dp:3")
  ttitab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number", format = "sf:4;dp:3")
  ttitab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number", format = "sf:4;dp:3")
  ttitab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "number", format = "dp:3;p:.001")
  ttitab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number", format = "sf:4;dp:3",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
  ttitab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number", format = "sf:4;dp:3",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))

  modelContainer[["parest"]][["tti"]] <- ttitab

  if (!ready || modelContainer$getError()) return()

  foot_message <- .medFootMessage(modelContainer, options)

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

  pe_tti <- pe[pe$op == ":=" & substr(pe$lhs, 1, 3) == "ind" & vapply(gregexpr("_", pe$lhs), length, 1) == 2,]
  # get predictors, outcome combinations
  terms <- strsplit(pe_tti[["label"]], "_")
  termsCombinations <- list(
    x = .medGetTermsFromLavaanTable(terms, 2, "x"),
    y = .medGetTermsFromLavaanTable(terms, 3, "y")
  )
  ttitab[["lhs"]]      <- options[["predictors"]][termsCombinations[["x"]]]
  ttitab[["op"]]       <- rep("\u2192", nrow(pe_tti))
  ttitab[["rhs"]]      <- options[["outcomes"]][termsCombinations[["y"]]]
  ttitab[["est"]]      <- if (options[["standardizedEstimate"]]) pe_tti[["est.std"]] else pe_tti[["est"]]
  ttitab[["se"]]       <- pe_tti$se
  ttitab[["z"]]        <- pe_tti$z
  ttitab[["pvalue"]]   <- pe_tti$pvalue
  ttitab[["ci.lower"]] <- pe_tti$ci.lower
  ttitab[["ci.upper"]] <- pe_tti$ci.upper
  ttitab$addFootnote(foot_message)
}

.medResTable <- function(modelContainer, options, ready) {
  if (!options[["residualCovariance"]] || !length(c(options$mediators, options$outcomes)) > 2) return()

  est_title <- ifelse(options[["standardizedEstimate"]], gettext("Standardized Estimate"), gettext("Estimate"))

  restab <- createJaspTable(title = gettext("Residual covariances"))
  restab$dependOn(c("residualCovariance","ciLevel", "bootstrapCiType", "standardizedEstimate", "standardizedEstimateType"))

  restab$addColumnInfo(name = "lhs",      title = "",                    type = "string")
  restab$addColumnInfo(name = "op",       title = "",                    type = "string")
  restab$addColumnInfo(name = "rhs",      title = "",                    type = "string")
  restab$addColumnInfo(name = "est",      title = est_title,             type = "number", format = "sf:4;dp:3")
  restab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number", format = "sf:4;dp:3")
  restab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number", format = "sf:4;dp:3")
  restab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "number", format = "dp:3;p:.001")
  restab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number", format = "sf:4;dp:3",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
  restab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number", format = "sf:4;dp:3",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))

  modelContainer[["parest"]][["res"]] <- restab

  if (!ready || modelContainer$getError()) return()

  foot_message <- .medFootMessage(modelContainer, options)

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

  pe_res <- pe[pe$op == "~~" &
                 pe$lhs != pe$rhs &
                 !pe$lhs %in% options$predictors &
                 !pe$lhs %in% options$confounds,]

  restab[["lhs"]]      <- pe_res$lhs
  restab[["op"]]       <- rep("\u2194", nrow(pe_res))
  restab[["rhs"]]      <- pe_res$rhs
  restab[["est"]]      <- if (options[["standardizedEstimate"]]) pe_res[["est.std"]] else pe_res[["est"]]
  restab[["se"]]       <- pe_res$se
  restab[["z"]]        <- pe_res$z
  restab[["pvalue"]]   <- pe_res$pvalue
  restab[["ci.lower"]] <- pe_res$ci.lower
  restab[["ci.upper"]] <- pe_res$ci.upper
  restab$addFootnote(foot_message)
}

.medPathTable <- function(modelContainer, options, ready) {
  if (!options[["pathCoefficient"]]) return()

  est_title <- ifelse(options[["standardizedEstimate"]], gettext("Standardized Estimate"), gettext("Estimate"))

  pathtab <- createJaspTable(title = gettext("Path coefficients"))
  pathtab$dependOn(c("pathCoefficient", "ciLevel", "bootstrapCiType", "standardizedEstimate", "standardizedEstimateType"))

  pathtab$addColumnInfo(name = "lhs",      title = "",                    type = "string")
  pathtab$addColumnInfo(name = "op",       title = "",                    type = "string")
  pathtab$addColumnInfo(name = "rhs",      title = "",                    type = "string")
  pathtab$addColumnInfo(name = "est",      title = est_title,             type = "number", format = "sf:4;dp:3")
  pathtab$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number", format = "sf:4;dp:3")
  pathtab$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number", format = "sf:4;dp:3")
  pathtab$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "number", format = "dp:3;p:.001")
  pathtab$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number", format = "sf:4;dp:3",
                        overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
  pathtab$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number", format = "sf:4;dp:3",
                        overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))

  modelContainer[["parest"]][["path"]] <- pathtab

  if (!ready || modelContainer$getError()) return()

  foot_message <- .medFootMessage(modelContainer, options)

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

  pe_path <- pe[pe$op == "~",]

  pathtab[["lhs"]]      <- pe_path$rhs
  pathtab[["op"]]       <- rep("\u2192", nrow(pe_path))
  pathtab[["rhs"]]      <- pe_path$lhs
  pathtab[["est"]]      <- if (options[["standardizedEstimate"]]) pe_path[["est.std"]] else pe_path[["est"]]
  pathtab[["se"]]       <- pe_path$se
  pathtab[["z"]]        <- pe_path$z
  pathtab[["pvalue"]]   <- pe_path$pvalue
  pathtab[["ci.lower"]] <- pe_path$ci.lower
  pathtab[["ci.upper"]] <- pe_path$ci.upper
  pathtab$addFootnote(foot_message)
}

.medFootMessage <- function(modelContainer, options) {
  if (is.null(modelContainer[["model"]][["object"]])) return()

  fit <- modelContainer[["model"]][["object"]]
  # Create the footnote message
  if (options[["estimator"]] %in% c("default", "ml", "gls", "wls", "uls", "dwls")) {
    se_type <- switch(options$errorCalculationMethod,
                      "bootstrap" = gettext("bootstrap"),
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

.medRsquared <- function(modelContainer, options, ready) {
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

.medPathPlot <- function(modelContainer, options, ready) {
  if (!options$pathPlot || !ready || !is.null(modelContainer[["plot"]])) return()

  plt <- createJaspPlot(title = gettext("Path plot"), width = 600, height = 400)
  plt$dependOn(options = c("pathPlot", "pathPlotParameter", "pathPlotLegend"))
  plt$position <- 2

  modelContainer[["plot"]] <- plt

  if (modelContainer$getError()) return()

  # compute the layout from the options
  n_pred <- length(options$predictors)
  n_medi <- length(options$mediators)
  n_deps <- length(options$outcomes)
  n_conf <- length(options$confounds)

  n_totl <- n_pred + n_medi + n_deps + n_conf
  pred_l <- cbind(rep(-1, n_pred), .medPathLayout(n_pred))
  deps_l <- cbind(rep(1,  n_deps), .medPathLayout(n_deps))
  if (n_medi == 1 && n_pred == 1 && n_deps == 1) {
    medi_l <- cbind(0, .25)
  } else {
    medi_l <- cbind(rep(0,  n_medi), .medPathLayout(n_medi))
  }
  if (n_conf > 0) {
    conf_l <- cbind(rep(-2, n_conf), .medPathLayout(n_conf) + 0.25)
  } else {
    conf_l <- NULL
  }

  # create a qgraph object using semplot
  po <- try(.medLavToPlotObj(modelContainer[["model"]][["object"]]), silent = TRUE)

  if(jaspBase::isTryError(po)) {
    message <- jaspBase::.extractErrorMessage(po)
    if(message == "length of 'dimnames' [2] not equal to array extent") {
      # Related to https://github.com/SachaEpskamp/semPlot/issues/40
      plt$setError(gettext("Currently it is not possible to plot mediation models with ordinal variables. The plot could not be generated."))
    } else {
      plt$setError(gettextf("The plot could not be generated. The underlying code resulted in the following error message: %s", message))
    }
    return()
  }

  pp <- jaspBase:::.suppressGrDevice(semPlot::semPaths(
    object         = po,
    layout         = rbind(deps_l, medi_l, pred_l, conf_l),
    intercepts     = FALSE,
    reorder        = FALSE,
    whatLabels     = ifelse(options$pathPlotParameter, "par", "name"),
    edge.color     = "black",
    color          = list(lat = "#EAEAEA", man = "#EAEAEA", int = "#FFFFFF"),
    border.width   = 1.5,
    edge.label.cex = 0.9,
    lty            = 2,
    title          = FALSE,
    sizeMan        = round(8*exp(-n_totl/80)+1),
    legend         = options$pathPlotLegend,
    legend.mode    = "names",
    legend.cex     = 0.6,
    nodeNames      = po@Vars$name,
    nCharNodes     = 3
  ))

  # post-process plot
  pp <- .medPlotPostProcess(pp, options)

  plt$plotObject <- pp
}

.medPathLayout <- function(n, min = -1, max = 1) {
  ss <- seq(min, max, length.out = n + 2)
  return(rev(ss[-c(1, n + 2)]))
}

.medLavToPlotObj <- function(lavResult, options) {
  # Create semplot model and unv the names of the manifest variables
  # Sorry, this code is really ugly but all it does is replace names for plot.
  semPlotMod <- semPlot::semPlotModel(list(lavResult), list(mplusStd = "std"))[[1]]

  manifests <- semPlotMod@Vars$name[semPlotMod@Vars$manifest]
  semPlotMod@Vars$name[semPlotMod@Vars$manifest] <- decodeColNames(manifests)
  semPlotMod@Pars$lhs <- ifelse(nchar(semPlotMod@Pars$lhs) > 0 , decodeColNames(semPlotMod@Pars$lhs), "")
  semPlotMod@Pars$rhs <- ifelse(nchar(semPlotMod@Pars$rhs) > 0 , decodeColNames(semPlotMod@Pars$rhs), "")
  if(.hasSlot(semPlotMod, "Thresholds"))
    semPlotMod@Thresholds$lhs <- ifelse(nchar(semPlotMod@Thresholds$lhs) > 0, decodeColNames(semPlotMod@Thresholds$lhs), "")

  return(semPlotMod)
}

.medPlotPostProcess <- function(plt, options) {
  node_names    <- plt$graphAttributes$Nodes$names
  confounds_idx <- which(node_names %in% options$confounds)
  predictor_idx <- which(node_names %in% options$predictors)
  dependent_idx <- which(node_names %in% options$outcomes)

  if (options$pathPlotParameter) {
    # change big numbers to scientific notation
    labs <- vapply(plt$graphAttributes$Edges$labels, function(lab) format(as.numeric(lab), digits = 2), "")
    plt$graphAttributes$Edges$labels <- labs
  }

  # remove focus from confounder edges
  confound_edges <- plt$Edgelist$from %in% confounds_idx
  plt$graphAttributes$Edges$labels[confound_edges] <- ""
  plt$graphAttributes$Edges$lty[confound_edges] <- 3
  plt$graphAttributes$Edges$color[confound_edges] <- "#888888FF"

  # place unidirectional edge labels at 1/3
  uni_edges <- !plt$Edgelist$bidirectional
  plt$graphAttributes$Edges$edge.label.position[uni_edges] <- 1/3


  return(plt)
}

.medSyntax <- function(modelContainer, options, ready) {
  if (!options$syntax || !ready) return()
  modelContainer[["syntax"]] <- createJaspHtml(.medToLavMod(options, FALSE), class = "jasp-code", title = gettext("Model syntax"))
  modelContainer[["syntax"]]$dependOn("syntax")
  modelContainer[["syntax"]]$position <- 3
}

# Helper functions ----
.medGetTermsFromLavaanTable <- function(terms, which, strip) {
  as.integer(gsub(strip, "", vapply(terms, FUN = "[[", FUN.VALUE = character(1), which)))
}
