context("Bayesian Structural Equation Modeling")

# Note: These tests use reduced MCMC settings for faster execution
# In production, use default or higher values for more reliable estimates

# Basic test with simple model
options <- jaspTools::analysisOptions("BayesianSEM")
options$models <- list(list(
  name = "Model1", 
  syntax = list(model = "
# latent variable definitions
  ind60 =~ x1 + x2 + x3
  dem60 =~ y1 + y2 + y3 + y4
# regressions
  dem60 ~ ind60
", 
  columns = c("x1", "x2", "x3", "y1", "y2", "y3", "y4"))
))
options$mcmcBurnin <- 100    # Reduced for faster testing (default: 500)
options$mcmcSamples <- 200   # Reduced for faster testing (default: 1000)
options$mcmcChains <- 2      # Reduced for faster testing (default: 3)
options$mcmcThin <- 1
options$group <- ""
options$dataType <- "raw"
options$factorScaling <- "factorLoading"
options$ciLevel <- 0.95
options$meanStructure <- FALSE
options$manifestInterceptFixedToZero <- FALSE
options$latentInterceptFixedToZero <- TRUE
options$orthogonal <- FALSE
options$warnings <- FALSE

set.seed(123)
results <- jaspTools::runAnalysis("BayesianSEM", testthat::test_path("poldem_grouped.csv"), options, makeTests = FALSE)


test_that("Bayesian SEM runs without critical errors", {
  # Check that analysis completed
  expect_true(is.null(results[["results"]][["error"]]) || results[["results"]][["error"]] == FALSE)
  
  # Check that model container exists
  expect_true(!is.null(results[["results"]][["modelContainer"]]))
})

test_that("Model fit table is created", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_fittab"]][["data"]]
  expect_true(!is.null(table))
  expect_true(nrow(table) > 0)
  expect_true("Model" %in% names(table))
  # Bayesian fit indices should be present (at least one)
  hasBayesianFit <- any(c("DIC", "WAIC", "LOO") %in% names(table))
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
  if (!is.null(parcont[["modelContainer_params_ind"]])) {
    indtab <- parcont[["modelContainer_params_ind"]][["data"]]
    if (!is.null(indtab) && nrow(indtab) > 0) {
      # Should have posterior summaries
      expect_true("est" %in% names(indtab)) # posterior mean
      expect_true("se" %in% names(indtab))  # posterior SD
    }
  }
  
  # Check for regression coefficients table
  if (!is.null(parcont[["modelContainer_params_reg"]])) {
    regtab <- parcont[["modelContainer_params_reg"]][["data"]]
    if (!is.null(regtab) && nrow(regtab) > 0) {
      expect_true("est" %in% names(regtab))
      expect_true("se" %in% names(regtab))
    }
  }
})

