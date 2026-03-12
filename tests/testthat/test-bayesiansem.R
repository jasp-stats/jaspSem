context("Bayesian Structural Equation Modeling")

# Note: MCMC sampling is stochastic; snapshot tests are not used.
# Tests verify structural output only. Reduced MCMC settings for speed.

model <- "
# latent variable definitions
  ind60 =~ x1 + x2 + x3
  dem60 =~ y1 + y2 + y3 + y4
# regressions
  dem60 ~ ind60
"

options <- jaspTools::analysisOptions("BayesianSEM")
options$models <- list(list(
  name   = "Model 1",
  syntax = list(model = model, columns = c("x1", "x2", "x3", "y1", "y2", "y3", "y4"))
))
options$mcmcBurnin   <- 100L
options$mcmcSamples  <- 200L
options$mcmcChains   <- 2L
options$mcmcThin     <- 1L
options$group        <- ""
options$dataType     <- "raw"
options$equalLoading            <- FALSE
options$equalIntercept          <- FALSE
options$equalResidual           <- FALSE
options$equalResidualCovariance <- FALSE
options$equalMean               <- FALSE
options$equalThreshold          <- FALSE
options$equalRegression         <- FALSE
options$equalLatentVariance     <- FALSE
options$equalLatentCovariance   <- FALSE
options$factorScaling               <- "factorLoading"
options$ciLevel                     <- 0.95
options$meanStructure               <- FALSE
options$manifestInterceptFixedToZero <- FALSE
options$latentInterceptFixedToZero  <- TRUE
options$orthogonal                  <- FALSE
options$warnings                    <- FALSE
options$additionalFitMeasures       <- TRUE
options$posteriorPredictivePvalue   <- TRUE
options$priorType                          <- "default"
options$priorLoadingParam1                 <- 0
options$priorLoadingParam2                 <- 10
options$priorRegressionParam1              <- 0
options$priorRegressionParam2              <- 10
options$priorObservedInterceptParam1       <- 0
options$priorObservedInterceptParam2       <- 32
options$priorLatentInterceptParam1         <- 0
options$priorLatentInterceptParam2         <- 10
options$priorThresholdParam1               <- 0
options$priorThresholdParam2               <- 1.5
options$priorResidualSdParam1              <- 1
options$priorResidualSdParam2              <- 0.5
options$priorLatentSdParam1                <- 1
options$priorLatentSdParam2                <- 0.5
options$priorCorrelationParam1             <- 1
options$priorCorrelationParam2             <- 1

set.seed(123)
results <- jaspTools::runAnalysis("BayesianSEM", testthat::test_path("poldem_grouped.csv"), options)


test_that("Bayesian SEM runs without critical errors", {
  expect_true(results[["status"]] == "complete")
  expect_true(!is.null(results[["results"]][["modelContainer"]]))
})

test_that("Model fit table is created with PPP", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_fittab"]][["data"]]
  expect_true(!is.null(table))
  expect_true(length(table) > 0)
  expect_true("Model" %in% names(table[[1]]))
  hasBayesianFit <- any(c("DIC", "WAIC", "LOO") %in% names(table[[1]]))
  expect_true(hasBayesianFit)
  # PPP column
  expect_true("PPP" %in% names(table[[1]]))
  ppp <- table[[1]][["PPP"]]
  expect_true(is.numeric(ppp))
  expect_true(ppp >= 0 && ppp <= 1)
})

test_that("Single-group fit table has npar and nfree columns", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_fittab"]][["data"]]
  expect_true("npar" %in% names(table[[1]]))
  expect_true("nfree" %in% names(table[[1]]))
  # No equality constraints: npar == nfree
  expect_equal(table[[1]][["npar"]], table[[1]][["nfree"]])
})

test_that("Parameter estimates container is created", {
  parcont <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]]
  expect_true(!is.null(parcont))
  expect_true(!is.null(parcont[["collection"]]))
})

test_that("Parameter tables have expected structure", {
  parcont <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]]

  # Check for factor loadings table
  indtab <- parcont[["modelContainer_params_ind"]][["data"]]
  expect_true(!is.null(indtab))
  expect_true(length(indtab) > 0)
  expect_true("est" %in% names(indtab[[1]]))  # posterior mean
  expect_true("se" %in% names(indtab[[1]]))   # posterior SD

  # Check for regression coefficients table
  regtab <- parcont[["modelContainer_params_reg"]][["data"]]
  expect_true(!is.null(regtab))
  expect_true(length(regtab) > 0)
  expect_true("est" %in% names(regtab[[1]]))
  expect_true("se" %in% names(regtab[[1]]))
})

test_that("Additional Bayesian fit measures table is created", {
  addfit <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_addfit"]]
  expect_true(!is.null(addfit))

  fitTable <- addfit[["collection"]][["modelContainer_addfit_fitTable_1"]][["data"]]
  expect_true(!is.null(fitTable))
  expect_true(length(fitTable) == 7)  # BRMSEA, BGammaHat, adjBGammaHat, BMc, BCFI, BTLI, BNFI

  # Check structure of each row
  firstRow <- fitTable[[1]]
  expect_true(all(c("index", "eap", "median", "sd", "lower", "upper") %in% names(firstRow)))

  # Check all indices are named
  indexNames <- vapply(fitTable, function(x) x$index, "")
  expect_true("Bayesian RMSEA" %in% indexNames)
  expect_true("Bayesian CFI" %in% indexNames)
  expect_true("Bayesian TLI" %in% indexNames)
  expect_true("Bayesian NFI" %in% indexNames)

  # Check values are numeric and in reasonable range
  eapValues <- vapply(fitTable, function(x) x$eap, 0)
  expect_true(all(is.finite(eapValues)))
  expect_true(all(eapValues >= 0 & eapValues <= 1))
})


# Multigroup test with equality constraints
options_mg <- options
options_mg$group                  <- "group"
options_mg$equalLoading           <- TRUE
options_mg$additionalFitMeasures  <- FALSE

# blavaan/Stan corrupts future.globals.method.default to NULL after the first MCMC run;
# reset it to the correct default before each subsequent run.
options("future.globals.method.default" = c("ordered", "dfs"))
set.seed(789)
results_mg <- jaspTools::runAnalysis("BayesianSEM", testthat::test_path("poldem_grouped.csv"), options_mg)

test_that("Multigroup BayesianSEM with equality constraints runs without errors", {
  expect_true(results_mg[["status"]] == "complete")
  expect_true(!is.null(results_mg[["results"]][["modelContainer"]]))
})

test_that("Multigroup parameter tables contain Group column", {
  parcont <- results_mg[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]]
  indtab  <- parcont[["modelContainer_params_ind"]][["data"]]
  expect_true(!is.null(indtab))
  expect_true("group" %in% names(indtab[[1]]))
})

test_that("Multigroup with equal loadings: nfree < npar", {
  table <- results_mg[["results"]][["modelContainer"]][["collection"]][["modelContainer_fittab"]][["data"]]
  npar  <- table[[1]][["npar"]]
  nfree <- table[[1]][["nfree"]]
  expect_true(is.numeric(npar) && is.numeric(nfree))
  expect_true(nfree < npar)
})


# Multi-model test: two models (one with regression, one without)
model_no_reg <- "
  ind60 =~ x1 + x2 + x3
  dem60 =~ y1 + y2 + y3 + y4
"

options_mm <- options
options_mm$additionalFitMeasures <- FALSE
options_mm$models <- list(
  list(name = "Model 1", syntax = list(model = model,        columns = c("x1", "x2", "x3", "y1", "y2", "y3", "y4"))),
  list(name = "Model 2", syntax = list(model = model_no_reg, columns = c("x1", "x2", "x3", "y1", "y2", "y3", "y4")))
)

options("future.globals.method.default" = c("ordered", "dfs"))
set.seed(321)
results_mm <- jaspTools::runAnalysis("BayesianSEM", testthat::test_path("poldem_grouped.csv"), options_mm)

test_that("Multi-model BayesianSEM runs without critical errors", {
  expect_true(results_mm[["status"]] == "complete")
  expect_true(!is.null(results_mm[["results"]][["modelContainer"]]))
})

test_that("Multi-model fit table has one row per model", {
  table <- results_mm[["results"]][["modelContainer"]][["collection"]][["modelContainer_fittab"]][["data"]]
  expect_equal(length(table), 2)
  modelNames <- vapply(table, function(x) x[["Model"]], "")
  expect_true("Model 1" %in% modelNames)
  expect_true("Model 2" %in% modelNames)
  hasBayesianFit <- any(c("DIC", "WAIC", "LOO") %in% names(table[[1]]))
  expect_true(hasBayesianFit)
})

test_that("Multi-model parameter estimates has per-model sub-containers", {
  params <- results_mm[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]]
  expect_true(!is.null(params[["modelContainer_params_Model 1"]]))
  expect_true(!is.null(params[["modelContainer_params_Model 2"]]))
})

test_that("Multi-model parameter sub-containers reflect model differences", {
  params <- results_mm[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]]

  # Model 1 has regression; Model 2 does not
  m1 <- params[["modelContainer_params_Model 1"]][["collection"]]
  m2 <- params[["modelContainer_params_Model 2"]][["collection"]]

  expect_true(!is.null(m1[["modelContainer_params_Model 1_reg"]]))
  expect_null(m2[["modelContainer_params_Model 2_reg"]])
})
