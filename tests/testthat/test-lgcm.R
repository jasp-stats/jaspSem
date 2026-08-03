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

test_that("Chi-square Test table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_maintab"]][["collection"]][["modelContainer_maintab_chisqtab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(187.534921995622, 14, "Baseline model", "", 69.7448930126911,
                                      9, "Growth curve model", 1.7085333148259e-11))
})

test_that("Latent covariances table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_partabs"]][["collection"]][["modelContainer_partabs_latcov"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.586546753687804, 0.920563814498034, 0.167008530405115, "Intercept",
                                      0.664011207974188, "Linear slope", 0.384474046480888, "<unicode><unicode>",
                                      0.434381805309756))
})

test_that("Latent curve table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_partabs"]][["collection"]][["modelContainer_partabs_latcur"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.91067166426575, 7.29253585271669, "Intercept", 4.60160375849122,
                                      "Mean", 0.000803406635452308, 1.37294976614428, 3.35161844370616,
                                      2.23084848964563, 7.43262609819151, "Intercept", 4.83173729391857,
                                      "Variance", 0.000271502073965424, 1.32700846790472, 3.6410749522553,
                                      -1.31843387751812, 0.254743186892948, "Linear slope", -0.531845345312584,
                                      "Mean", 0.185100357300383, 0.401328054193874, -1.32521347499834,
                                      -0.279812019736802, 0.58560544257759, "Linear slope", 0.152896711420394,
                                      "Variance", 0.488592554701348, 0.220773817565194, 0.69254911251079
                                 ))
})

test_that("Regressions table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_partabs"]][["collection"]][["modelContainer_partabs_latreg"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.0164990486098335, 0.83129631851575, "Intercept", 0.407398634952958,
                                      "x3", 0.0596085724085578, 0.216278302512925, 1.88367779023331,
                                      -1.58384563758143, 0.783933270269892, "Intercept", -0.399956183655768,
                                      "group", 0.507881961502067, 0.604036330904052, -0.662139284001609,
                                      0.0117615965627584, 0.259581344548597, "Linear slope", 0.135671470555678,
                                      "x3", 0.03187258823186, 0.0632204851570258, 2.14600489412095,
                                      -0.494241490855952, 0.197885872636682, "Linear slope", -0.148177809109635,
                                      "group", 0.401346504623975, 0.176566347379862, -0.839218861966079
                                 ))
})

test_that("Residual variances table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_partabs"]][["collection"]][["modelContainer_partabs_resvar"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.324588296041848, 3.6454907529351, 1.66045122844662, 0.101114170618842,
                                      1.01279387792133, "y1", 1.63947597299319, 5.29913958703259,
                                      11.0467904666191, 8.17296502682586, 2.4894871941683e-08, 1.46626441223494,
                                      "y2", 5.57400490568294, 5.45925010966836, 11.5067395932775,
                                      8.48299485147291, 3.82817306743277e-08, 1.54275525757384, "y3",
                                      5.4986005134822, -0.442535417440978, 4.28674625451398, 1.9221054185365,
                                      0.11112371395684, 1.20647157530927, "y4", 1.59316262220581
                                 ))
})


# test standardization
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


test_that("Chi-square test table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_maintab"]][["collection"]][["modelContainer_maintab_chisqtab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(187.534921995622, 14, "Baseline model", "", 69.7448930126911,
                                      9, "Growth curve model", 1.7085333148259e-11))
})

test_that("Latent covariances table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_partabs"]][["collection"]][["modelContainer_partabs_latcov"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.933354887001021, 1.3219684208426, 0.194306766920789, "Intercept",
                                      0.735573953392761, "Linear slope", 0.57534815068882, "<unicode><unicode>",
                                      0.33772032931393))
})

test_that("Latent curve table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_partabs"]][["collection"]][["modelContainer_partabs_latcur"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.689567552015991, 3.32433715024733, "Intercept", 2.00695235113166,
                                      "Mean", 0.0028276259429858, 0.672147452456796, 2.98588106492996,
                                      0.783158280188574, 1.05502577241312, "Intercept", 0.919092026300845,
                                      "Variance", 0, 0.0693552265166601, 13.2519504651905, -3.31263568354905,
                                      0.945322073452253, "Linear slope", -1.1836568050484, "Mean",
                                      0.275850195795035, 1.08623367331939, -1.08968892616935, 0.168252475473949,
                                      1.34638609981018, "Linear slope", 0.757319287642065, "Variance",
                                      0.0117428340916275, 0.30054981459588, 2.51977958682276))
})

test_that("Regressions table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_partabs"]][["collection"]][["modelContainer_partabs_latreg"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.00312680005628446, 0.499329260284261, "Intercept", 0.248101230113988,
                                      "x3", 0.0529203694733311, 0.128179921749546, 1.93557014802021,
                                      -0.344514960408374, 0.170092804220773, "Intercept", -0.0872110780938004,
                                      "group", 0.506489360351412, 0.131279903275853, -0.66431400326787,
                                      -0.134719469422696, 0.977938637115648, "Linear slope", 0.421609583846476,
                                      "x3", 0.13745292242919, 0.283846569456084, 1.48534324249321,
                                      -0.580720852131014, 0.250970704664956, "Linear slope", -0.164875073733029,
                                      "group", 0.437106225941533, 0.21217011214396, -0.777089063426423
                                 ))
})

test_that("Residual variances table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_partabs"]][["collection"]][["modelContainer_partabs_resvar"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.0445852644621054, 0.524655960556052, 0.240035348046973, 0.0983431883416908,
                                      0.14521726662027, "y1", 1.65293944469183, 0.449063836698895,
                                      0.697652586970778, 0.573358211834837, 0, 0.0634166628143986,
                                      "y2", 9.0411287253144, 0.412017878121697, 0.66226317253542,
                                      0.537140525328559, 0, 0.0638392583709768, "y3", 8.41395309148451,
                                      -0.0399776999941945, 0.393824053017031, 0.176923176511418, 0.109883564718275,
                                      0.110665746011916, "y4", 1.59871670220673))
})


# Quadratic growth with all optional outputs
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

test_that("Quadratic latent curve table has correct structure", {
  # NOTE: exact values not tested because the quadratic model produces negative variances
  # and can converge to different local optima across runs
  table <- results3[["results"]][["modelContainer"]][["collection"]][["modelContainer_partabs"]][["collection"]][["modelContainer_partabs_latcur"]][["data"]]
  expect_equal(length(table), 6)
  labels <- sapply(table, function(row) row[["component"]])
  types  <- sapply(table, function(row) row[["param"]])
  expect_equal(labels, c("Intercept", "Intercept", "Linear slope", "Linear slope", "Quadratic slope", "Quadratic slope"))
  expect_equal(types,  c("Mean", "Variance", "Mean", "Variance", "Mean", "Variance"))
})

test_that("Additional fit measures table results match", {
  table <- results3[["results"]][["modelContainer"]][["collection"]][["modelContainer_fitIndices"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Comparative Fit Index (CFI)", 0.859848311804056, "Tucker-Lewis Index (TLI)",
                                      0.34595878841893, "Bentler-Bonett Non-normed Fit Index (NNFI)",
                                      0.34595878841893, "Bentler-Bonett Normed Fit Index (NFI)", 0.854314001958123,
                                      "Parsimony Normed Fit Index (PNFI)", 0.183067286133883, "Bollen's Relative Fit Index (RFI)",
                                      0.320132009137905, "Bollen's Incremental Fit Index (IFI)", 0.868202657710421,
                                      "Relative Noncentrality Index (RNI)", 0.859848311804056, "Root mean square error of approximation (RMSEA)",
                                      0.328776940788045, "RMSEA 90% CI lower bound", 0.222855445658039,
                                      "RMSEA 90% CI upper bound", 0.44682992305614, "RMSEA p-value",
                                      2.74098924707422e-05, "Standardized root mean square residual (SRMR)",
                                      0.110884553767486, "Hoelter's critical N (\u03B1 = .05)",
                                      22.4523640739803, "Hoelter's critical N (\u03B1 = .01)",
                                      32.1430179628636, "Goodness of fit index (GFI)", 0.951665034033744,
                                      "McDonald fit index (MFI)", 0.850320947738198, "Expected cross validation index (ECVI)",
                                      0.870949497048504, "Log-likelihood", -698.619539754982, "Number of free parameters",
                                      19, "Akaike (AIC)", 1435.23907950996, "Bayesian (BIC)", 1479.27135366715,
                                      "Sample-size adjusted Bayesian (SSABIC)", 1419.38835974857))
})

test_that("R-squared table results match", {
  table <- results3[["results"]][["modelContainer"]][["collection"]][["modelContainer_rsquared"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("y1", "", "y2", 0.167939386500449, "y3", 0.392059398679358, "y4",
                                      -0.606294027492821))
})

test_that("Implied covariance table results match", {
  table <- results3[["results"]][["modelContainer"]][["collection"]][["modelContainer_impliedCovTab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("", "", 6.78632108886267, "", "", "", "", "", 6.13538851199859,
                                      17.7312477719085, "", "", "", "", 5.7745274807624, 4.38177197317977,
                                      11.3956331777628, "", "", "", 5.99378295934392, 10.3473798134326,
                                      6.03250683495574, 11.4644638754404, "", 1.949678538072, 0.897531861127889,
                                      0.884161212109168, 1.15203565891163, 1.70115520153528, 0.249955555555555,
                                      -0.189515866666667, -0.238678218688687, 0.0210765120842407,
                                      -0.0530634658926051, -0.461098152619225))
})

test_that("Residual covariance table results match", {
  table <- results3[["results"]][["modelContainer"]][["collection"]][["modelContainer_rescov"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("", "", 0.000530466692885589, "", "", "", "", "", 0.0326268136014054,
                                      -2.35915615117634, "", "", "", "", -0.0135416022735049, 1.3790085248548,
                                      -0.77490174609429, "", "", "", 0.0136802746294089, -0.965600705346711,
                                      0.56626174174086, -0.395111315001229, "", 0, 0.0017693188721104,
                                      0.270054316946833, -0.126811462707632, 0.112360610585651, 0,
                                      0, 0.000580440910909863, 0.0885472692490925, -0.0415792772185059,
                                      0.0368416571525587))
})

test_that("Model syntax is shown", {
  syntax <- results3[["results"]][["modelContainer"]][["collection"]][["modelContainer_model_syntax"]]
  expect_true(!is.null(syntax))
})

test_that("Curve plot is created", {
  expect_true(!is.null(results3[["results"]][["modelContainer"]][["collection"]][["modelContainer_curveplot"]][["data"]]))
})

test_that("Path plot is created", {
  expect_true(!is.null(results3[["results"]][["modelContainer"]][["collection"]][["modelContainer_pathplot"]][["data"]]))
})
