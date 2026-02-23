context("MIMIC")

# basic MIMIC works
options <- list(
  .meta = list(
    indicators = list(
      hasTypes = TRUE,
      shouldEncode = TRUE
    ),
    predictors = list(
      hasTypes = TRUE,
      shouldEncode = TRUE
    )
  ),
  additionalFitMeasures = FALSE,
  bootstrapCiType = "percentileBiasCorrected",
  bootstrapSamples = 200,
  ciLevel = 0.95,
  emulation = "lavaan",
  errorCalculationMethod = "standard",
  estimator = "default",
  # indicators = list(
  #   types = c("scale", "scale", "scale", "scale", "scale", "scale"),
  #   value = c("y1", "y2", "y3", "y4", "y5", "y6")
  # ),
  indicators = c("y1", "y2", "y3", "y4", "y5", "y6"),
  indicators.types = c("scale", "scale", "scale", "scale", "scale", "scale"),
  naAction = "fiml",
  pathPlot = FALSE,
  pathPlotLegend = FALSE,
  pathPlotParameter = FALSE,
  plotHeight = 320,
  plotWidth = 480,
  # predictors = list(
  #   types = c("scale", "scale", "scale"),
  #   value = c("x1", "x2", "x3")
  # ),
  predictors = c("x1", "x2", "x3"),
  predictors.types = c("scale", "scale", "scale"),
  rSquared = FALSE,
  standardizedEstimate = TRUE,
  standardizedEstimateType = "all",
  syntax = FALSE
)

set.seed(1)
results <- jaspTools::runAnalysis("MIMIC", testthat::test_path("poldem_grouped.csv"), options, makeTests = F)

test_that("Chi Square test table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_fittab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Baseline model", 339.026474043099, 33, 0, "Factor model", 52.7391959078519,
                                      24, 0.000626079544455149))
})

test_that("Predictor coefficients table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_bet"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.0533116784473734, 0.866589259294009, 0.406638790423318, 0.0831330739617449,
                                      "x1", 0.234672918736631, 1.73278958906921, -0.359967008399682,
                                      0.711056159579429, 0.175544575589873, 0.520554827759032, "x2",
                                      0.273225216490508, 0.642490388861936, -0.481277681774235, 0.316909833152795,
                                      -0.0821839243107201, 0.686500820378854, "x3", 0.20362300563251,
                                      -0.40360824679625))
})

test_that("Indicator coefficients table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_lam"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.782378053939416, 0.9306998603974, 0.856538957168408, 0, "y1",
                                      0.0378378908051188, 22.6370693223876, 0.644355124235808, 0.862030535134675,
                                      0.753192829685241, 0, "y2", 0.0555304619411027, 13.5635974086457,
                                      0.573204173402704, 0.825203587273007, 0.699203880337855, 0,
                                      "y3", 0.064286746046877, 10.8763302443089, 0.777827470888026,
                                      0.928427325411374, 0.8531273981497, 0, "y4", 0.0384190361943538,
                                      22.2058511263507, 0.711366803436262, 0.8955477273046, 0.803457265370431,
                                      0, "y5", 0.0469857929332205, 17.1000043888237, 0.652062622619335,
                                      0.865933901606924, 0.75899826211313, 0, "y6", 0.0545600022945775,
                                      13.911257884763))
})


# Additional fit measures, R-squared, path plot, syntax
options2 <- options
options2$additionalFitMeasures <- TRUE
options2$rSquared              <- TRUE
options2$pathPlot              <- TRUE
options2$syntax                <- TRUE

set.seed(1)
results2 <- jaspTools::runAnalysis("MIMIC", testthat::test_path("poldem_grouped.csv"), options2, makeTests = FALSE)

test_that("Additional fit measures table results match", {
  table <- results2[["results"]][["modelContainer"]][["collection"]][["modelContainer_fitIndices"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Comparative Fit Index (CFI)", 0.90608918395797, "Tucker-Lewis Index (TLI)",
                                      0.870872627942209, "Bentler-Bonett Non-normed Fit Index (NNFI)",
                                      0.870872627942209, "Bentler-Bonett Normed Fit Index (NFI)",
                                      0.844439299152939, "Parsimony Normed Fit Index (PNFI)", 0.614137672111228,
                                      "Bollen's Relative Fit Index (RFI)", 0.786104036335291, "Bollen's Incremental Fit Index (IFI)",
                                      0.908772124643973, "Relative Noncentrality Index (RNI)", 0.90608918395797,
                                      "Root mean square error of approximation (RMSEA)", 0.126357508478154,
                                      "RMSEA 90% CI lower bound", 0.079926210051774, "RMSEA 90% CI upper bound",
                                      0.17273936397227, "RMSEA p-value", 0.00617362342636585, "Standardized root mean square residual (SRMR)",
                                      0.0656147157724176, "Hoelter's critical N (\u03B1 = .05)",
                                      52.7855285925758, "Hoelter's critical N (\u03B1 = .01)",
                                      62.1212676826463, "Goodness of fit index (GFI)", 0.815106974640689,
                                      "McDonald fit index (MFI)", 0.825641483206126, "Expected cross validation index (ECVI)",
                                      1.10318927877137, "Log-likelihood", -1010.62207609676, "Number of free parameters",
                                      15, "Akaike (AIC)", 2051.24415219353, "Bayesian (BIC)", 2086.00647389657,
                                      "Sample-size adjusted Bayesian (SSABIC)", 2038.73042606611))
})

test_that("R-squared table results match", {
  table <- results2[["results"]][["modelContainer"]][["collection"]][["modelContainer_rsquared"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("y1", 0.733658985147143, "y2", 0.567299438689261, "y3", 0.488886066279514,
                                      "y4", 0.727826357473676, "y5", 0.645543577276531, "y6", 0.576078361890751,
                                      "Y", 0.252668653756736))
})

test_that("Path plot is created", {
  expect_true(!is.null(results2[["results"]][["modelContainer"]][["collection"]][["modelContainer_plot"]][["data"]]))
})

test_that("Model syntax is shown", {
  syntax <- results2[["results"]][["modelContainer"]][["collection"]][["modelContainer_syntax"]]
  expect_true(!is.null(syntax))
  expect_equal(syntax[["title"]], "Model syntax")
})
