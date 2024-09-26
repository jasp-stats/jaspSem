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
