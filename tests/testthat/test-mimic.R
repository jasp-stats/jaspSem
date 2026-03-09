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
  fixedX = TRUE,
  includePredictorCovariances = TRUE,
  indicators = c("y1", "y2", "y3", "y4", "y5", "y6"),
  indicators.types = c("scale", "scale", "scale", "scale", "scale", "scale"),
  naAction = "fiml",
  pathPlot = FALSE,
  pathPlotLegend = FALSE,
  pathPlotParameter = FALSE,
  plotHeight = 320,
  plotWidth = 480,
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
                                 list("Baseline model", 558.191544351837, 36, 0, "Factor model", 52.7391959079395,
                                      24, 0.000626079544438607))
})

test_that("Predictor coefficients table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_bet"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.0559173260109581, 0.869197074402585, 0.406639874195813, 0.0848832660272527,
                                      "x1", 0.236002908142886, 1.72302908212307, -0.360426059977816,
                                      0.711515044647295, 0.17554449233474, 0.520912206291856, "x2",
                                      0.273459388305205, 0.6419398998246, -0.481442768967115, 0.317072324532559,
                                      -0.082185222217278, 0.686617913419291, "x3", 0.203706573130491,
                                      -0.403449044153482))
})

test_that("Indicator coefficients table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_lam"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.781806563190719, 0.931271302948561, 0.85653893306964, 0, "y1",
                                      0.0381294607800962, 22.4639665902844, 0.643560825957947, 0.862825688169588,
                                      0.753193257063767, 0, "y2", 0.0559359416655547, 13.4652825113264,
                                      0.572378229515537, 0.826029449546026, 0.699203839530782, 0,
                                      "y3", 0.064708132912456, 10.8055016898222, 0.77724478618396,
                                      0.929010552768734, 0.853127669476347, 0, "y4", 0.0387164682060189,
                                      22.0352658444119, 0.710649355110998, 0.896264744292479, 0.803457049701738,
                                      0, "y5", 0.0473517346863491, 16.9678482746983, 0.651275316226256,
                                      0.866723001633908, 0.758999158930082, 0, "y6", 0.0549621541791266,
                                      13.8094870964561))
})

test_that("Residual variances table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_var"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.138318689283745, 0.394363422988101, 0.266341056135923, "y1",
                                      4.55107947492372e-05, 0.0653187343553261, 4.07755996445153,
                                      0.2675510996716, 0.597848735355747, 0.432699917513674, "y2",
                                      2.81807430280168e-07, 0.0842611492582244, 5.13522449340957,
                                      0.333760082548626, 0.688467899022199, 0.511113990785413, "y3",
                                      1.61952267152543e-08, 0.0904883506205887, 5.64839548162921,
                                      0.142697598651946, 0.401648760495767, 0.272173179573856, "y4",
                                      3.7874242010183e-05, 0.0660601837294956, 4.12007905833801, 0.20532277684685,
                                      0.503590761722307, 0.354456769284578, "y5", 3.18710673785638e-06,
                                      0.0760901698266286, 4.65837794937516, 0.260395667585526, 0.58744488590133,
                                      0.423920276743428, "y6", 3.75453897083489e-07, 0.0834324561307061,
                                      5.08099960618815, 1, 1, 1, "x1", "", 0, "", 1, 1, 1, "x2", "",
                                      0, "", 1, 1, 1, "x3", "", 0, "", 0.56694894509283, 0.927713773227238,
                                      0.747331359160034, "Y", 4.44089209850063e-16, 0.0920335350496424,
                                      8.12020703928114))
})

test_that("Predictor covariances table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_cov"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.849259684622463, 0.939731267865368, 0.894495476243915, "x1 \u2013 x2",
                                      0, 0.0230799096199045, 38.7564548984407, 0.717497592901233,
                                      0.880992447843138, 0.799245020372185, "x1 \u2013 x3", 0,
                                      0.0417086375646521, 19.1625779943851, 0.787892389101237, 0.913111904300617,
                                      0.850502146700927, "x2 \u2013 x3", 0, 0.0319443408621524,
                                      26.6245013591312))
})

# fixedX = FALSE: freely estimate predictor variances and covariances
options2         <- options
options2$fixedX  <- FALSE
options2$standardizedEstimate <- FALSE

set.seed(1)
results2 <- jaspTools::runAnalysis("MIMIC", testthat::test_path("poldem_grouped.csv"), options2, makeTests = FALSE)

test_that("Predictor covariances have SEs when fixedX is FALSE", {
  table <- results2[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_cov"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.645448754445758, 1.30886149855849, 0.977155126502126, "x1 \u2013 x2",
                                      7.752756214785e-09, 0.169241054770815, 5.77374755685245, 0.517939054332182,
                                      1.10694971368303, 0.812444384007607, "x1 \u2013 x3", 6.41238702137059e-08,
                                      0.150260582336433, 5.4069029373821, 1.15950918130724, 2.40450857582805,
                                      1.78200887856764, "x2 \u2013 x3", 2.01483325579233e-08, 0.317607722473784,
                                      5.61072276419455))
})

test_that("Residual variances have SEs for predictors when fixedX is FALSE", {
  table <- results2[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_var"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.03398100790029, 2.58125512377407, 1.80761806583718, "y1", 4.66102037943728e-06,
                                      0.394720037735002, 4.57949405408888, 4.23595324812564, 9.06705378412694,
                                      6.65150351612629, "y2", 6.77671172510941e-08, 1.23244625261188,
                                      5.39699277110865, 3.52705026375725, 7.32976315224158, 5.42840670799941,
                                      "y3", 2.1969248642506e-08, 0.970097644262764, 5.59573228540802,
                                      1.73590405606311, 4.28966040353454, 3.01278222979882, "y4",
                                      3.7547251556802e-06, 0.65148042709334, 4.62451687649424, 1.47127293155907,
                                      3.30303516566337, 2.38715404861122, "y5", 3.2480657297107e-07,
                                      0.46729487086319, 5.1084533502404, 3.02137927865361, 6.49449301175144,
                                      4.75793614520253, "y6", 7.87182625749949e-08, 0.886014682028167,
                                      5.37004210168525, 0.3603584518716, 0.6996140881333, 0.52998627000245,
                                      "x1", 9.14129882900738e-10, 0.0865463954791275, 6.12372435695796,
                                      1.53100358457986, 2.97235064473054, 2.2516771146552, "x2", 9.14129882900738e-10,
                                      0.367697333093834, 6.12372435695797, 1.32566221741406, 2.57369282888187,
                                      1.94967752314797, "x3", 9.14129882900738e-10, 0.318381006312391,
                                      6.12372435695793, 1, 1, 1, "Y", "", 0, ""))
})
