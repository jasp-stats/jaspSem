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

set.seed(123)
results <- jaspTools::runAnalysis("BayesianSEM", testthat::test_path("poldem_grouped.csv"), options)


test_that("Bayesian SEM runs without critical errors", {
  # Check that analysis completed
  expect_true(results[["status"]] == "complete")

  # Check that model container exists
  expect_true(!is.null(results[["results"]][["modelContainer"]]))
})

test_that("Model fit table is created", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_fittab"]][["data"]]
  expect_true(!is.null(table))
  expect_true(length(table) > 0)
  expect_true("Model" %in% names(table[[1]]))
  # Bayesian fit indices should be present
  hasBayesianFit <- any(c("DIC", "WAIC", "LOO") %in% names(table[[1]]))
  expect_true(hasBayesianFit)
})

test_that("Parameter estimates container is created", {
  parcont <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]]
  expect_true(!is.null(parcont))
  # Check if parameter containers collection exists
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


# Multigroup test with equality constraints
options_mg <- options
options_mg$group           <- "group"
options_mg$equalLoading    <- TRUE
options_mg$equalIntercept  <- FALSE

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

# Additional fit measures test (separate run with additionalFitMeasures = TRUE)
options2 <- options
options2$additionalFitMeasures <- TRUE

options("future.globals.method.default" = c("ordered", "dfs"))
set.seed(456)
results2 <- jaspTools::runAnalysis("BayesianSEM", testthat::test_path("poldem_grouped.csv"), options2)

test_that("Additional Bayesian fit measures table is created", {
  expect_true(results2[["status"]] == "complete")

  addfit <- results2[["results"]][["modelContainer"]][["collection"]][["modelContainer_addfit"]]
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

