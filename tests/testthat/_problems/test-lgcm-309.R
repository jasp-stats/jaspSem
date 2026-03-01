# Extracted from test-lgcm.R:309

# prequel ----------------------------------------------------------------------
context("LGCM")
options <- list(
  .meta = list(
    categorical = list(
      hasTypes = TRUE,
      shouldEncode = TRUE
    ),
    curvePlotCategorical = list(
      shouldEncode = TRUE
    ),
    group = list(
      shouldEncode = TRUE
    ),
    regressions = list(
      hasTypes = TRUE,
      shouldEncode = TRUE
    ),
    timings = list(
      hasTypes = TRUE,
      shouldEncode = TRUE
    ),
    variables = list(
      hasTypes = TRUE,
      shouldEncode = TRUE
    )
  ),
  additionalFitMeasures = FALSE,
  bootstrapCiType = "percentileBiasCorrected",
  bootstrapSamples = 1000,
  categorical = "group",
  categorical.types = "nominal",
  ciLevel = 0.95,
  colorPalette = "colorblind",
  covariates = list(),
  covaryingLatentCurve = TRUE,
  cubic = FALSE,
  curvePlot = FALSE,
  curvePlotCategorical = "group",
  curvePlotMaxLines = 30,
  dependentCorrelation = TRUE,
  emulation = "lavaan",
  errorCalculationMethod = "standard",
  estimator = "default",
  exogenousLatentCorrelation = TRUE,
  group = "",
  impliedCovariance = FALSE,
  intercept = TRUE,
  latentInterceptFixedToZero = TRUE,
  linear = TRUE,
  manifestInterceptFixedToZero = FALSE,
  misfitPlot = FALSE,
  naAction = "fiml",
  pathPlot = FALSE,
  pathPlotMean = FALSE,
  pathPlotParameter = FALSE,
  plotHeight = 320,
  plotWidth = 480,
  quadratic = FALSE,
  rSquared = FALSE,
  regressions = "x3",
  regressions.types = "scale",
  residualCovariance = FALSE,
  residualSingleIndicatorOmitted = TRUE,
  residualVariance = TRUE,
  scalingParameter = TRUE,
  standardizedEstimate = FALSE,
  standardizedEstimateType = "all",
  syntax = FALSE,
  threshold = TRUE,
  timings = list(
    list(timing = 0, variable = "y1"),
    list(timing = 1, variable = "y2"),
    list(timing = 2, variable = "y3"),
    list(timing = 3, variable = "y4")
  ),
  timings.types = c("scale", "scale", "scale", "scale"),
  variables = c("y1", "y2", "y3", "y4"),
  variables.types = c("scale", "scale", "scale", "scale")
)
set.seed(1)
results <- runAnalysis("LatentGrowthCurve", data = testthat::test_path("poldem_grouped.csv"), options = options, makeTests = F)
options <- list(
  .meta = list(
    categorical = list(
      hasTypes = TRUE,
      shouldEncode = TRUE
    ),
    curvePlotCategorical = list(
      shouldEncode = TRUE
    ),
    group = list(
      shouldEncode = TRUE
    ),
    regressions = list(
      hasTypes = TRUE,
      shouldEncode = TRUE
    ),
    timings = list(
      hasTypes = TRUE,
      shouldEncode = TRUE
    ),
    variables = list(
      hasTypes = TRUE,
      shouldEncode = TRUE
    )
  ),
  additionalFitMeasures = FALSE,
  bootstrapCiType = "percentileBiasCorrected",
  bootstrapSamples = 1000,
  categorical = "group",
  categorical.types = "nominal",
  ciLevel = 0.95,
  colorPalette = "colorblind",
  covariates = list(),
  covaryingLatentCurve = TRUE,
  cubic = FALSE,
  curvePlot = FALSE,
  curvePlotCategorical = "group",
  curvePlotMaxLines = 30,
  dependentCorrelation = TRUE,
  emulation = "lavaan",
  errorCalculationMethod = "standard",
  estimator = "default",
  exogenousLatentCorrelation = TRUE,
  group = "",
  impliedCovariance = FALSE,
  intercept = TRUE,
  latentInterceptFixedToZero = TRUE,
  linear = TRUE,
  manifestInterceptFixedToZero = FALSE,
  misfitPlot = FALSE,
  naAction = "fiml",
  pathPlot = FALSE,
  pathPlotMean = FALSE,
  pathPlotParameter = FALSE,
  plotHeight = 320,
  plotWidth = 480,
  quadratic = FALSE,
  rSquared = FALSE,
  regressions = "x3",
  regressions.types = "scale",
  residualCovariance = FALSE,
  residualSingleIndicatorOmitted = TRUE,
  residualVariance = TRUE,
  scalingParameter = TRUE,
  standardizedEstimate = TRUE,
  standardizedEstimateType = "all",
  syntax = FALSE,
  threshold = TRUE,
  timings = list(
    list(timing = 0, variable = "y1"),
    list(timing = 1, variable = "y2"),
    list(timing = 2, variable = "y3"),
    list(timing = 3, variable = "y4")
  ),
  timings.types = c("scale", "scale", "scale", "scale"),
  variables = c("y1", "y2", "y3", "y4"),
  variables.types = c("scale", "scale", "scale", "scale")
)
set.seed(1)
results <- runAnalysis("LatentGrowthCurve", data = testthat::test_path("poldem_grouped.csv"), options = options, makeTests = F)
options3 <- options
options3$quadratic             <- TRUE
options3$additionalFitMeasures <- TRUE
options3$rSquared              <- TRUE
options3$impliedCovariance     <- TRUE
options3$residualCovariance    <- TRUE
options3$syntax                <- TRUE
options3$curvePlot             <- TRUE
options3$pathPlot              <- TRUE
set.seed(1)
results3 <- jaspTools::runAnalysis("LatentGrowthCurve", testthat::test_path("poldem_grouped.csv"), options3, makeTests = FALSE)

# test -------------------------------------------------------------------------
table <- results3[["results"]][["modelContainer"]][["collection"]][["modelContainer_partabs"]][["collection"]][["modelContainer_partabs_latcur"]][["data"]]
jaspTools::expect_equal_tables(table,
                                 list(2.371585871767, 7.72700796179435, "Intercept", 5.04929691678068,
                                      "Mean", 0.000219145336890048, 1.36620420892176, 3.69585811828652,
                                      -1.97641133217644, 15.104696260994, "Intercept", 6.56414246440878,
                                      "Variance", 0.131964743587794, 4.35750547660673, 1.50639913125718,
                                      -4.91494619483031, 0.282882554665925, "Linear slope", -2.31603182008219,
                                      "Mean", 0.0807017308606015, 1.32600108739141, -1.74662889955726,
                                      -18.5440083955832, 1.12688470004072, "Linear slope", -8.70856184777123,
                                      "Variance", 0.0826693747067031, 5.01817718355679, -1.73540342025125,
                                      -0.130716299906619, 1.53033615238327, "Quadratic slope", 0.699809926238323,
                                      "Mean", 0.0986394222418541, 0.423745656907998, 1.65148577886254,
                                      -2.77982618527278, -0.97558138836837, "Quadratic slope", -1.87770378682058,
                                      "Variance", 4.51276191153482e-05, 0.460274987483461, -4.07952601788523))
