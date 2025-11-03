context("Bayesian Structural Equation Modeling")

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
options$mcmcBurnin <- 100    # Reduced for faster testing
options$mcmcSamples <- 200   # Reduced for faster testing
options$mcmcChains <- 2      # Reduced for faster testing
options$group <- ""
options$dataType <- "raw"
options$factorScaling <- "factorLoading"
options$ciLevel <- 0.95

set.seed(123)
results <- jaspTools::runAnalysis("BayesianSEM", testthat::test_path("poldem_grouped.csv"), options, makeTests = FALSE)


test_that("Model fit table is created", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_fittab"]][["data"]]
  expect_true(!is.null(table))
  expect_true(nrow(table) > 0)
  expect_true("Model" %in% names(table))
  expect_true("DIC" %in% names(table) || "WAIC" %in% names(table))
})

test_that("Parameter estimates table is created", {
  parcont <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]]
  expect_true(!is.null(parcont))
  # Check if parameter containers exist
  expect_true(!is.null(parcont[["collection"]]))
})

test_that("Bayesian SEM runs without errors", {
  expect_true(results[["status"]] == "complete" || is.null(results[["status"]]))
  expect_true(is.null(results[["results"]][["error"]]) || results[["results"]][["error"]] == FALSE)
})
