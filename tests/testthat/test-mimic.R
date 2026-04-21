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

test_that("Residual variances table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_var"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.139297610500386, 0.393384419205328, 0.266341014852857, "y1",
                                      3.97410634915829e-05, 0.064819254514151, 4.10897991421223, 0.268749000355915,
                                      0.596652122265563, 0.432700561310739, "y2", 2.30695557990046e-07,
                                      0.0836502926829541, 5.17273218577648, 0.334914963508945, 0.687312903932027,
                                      0.511113933720486, "y3", 1.30492630034951e-08, 0.0898990856981945,
                                      5.6854185974302, 0.143692781570986, 0.400654503481662, 0.272173642526324,
                                      "y4", 3.2960466163745e-05, 0.065552664216679, 4.15198445064988,
                                      0.206474918180893, 0.502437927266045, 0.354456422723469, "y5",
                                      2.67059590797203e-06, 0.0755021549935792, 4.6946530566394, 0.261593709643463,
                                      0.586249566575035, 0.423921638109249, "y6", 3.08019081129984e-07,
                                      0.0828218935379469, 5.11847314762272, 0.577374616199496, 0.917288076287032,
                                      0.747331346243264, "Y", 0, 0.086714210763241, 8.61832610439978,
                                      1, 1, 1, "x1", "", 0, "", 1, 1, 1, "x2", "", 0, "", 1, 1, 1,
                                      "x3", "", 0, ""))
})

test_that("Predictor covariances table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_cov"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.894495472364479, 0.894495472364479, 0.894495472364479, "x1 \u2013 x2",
                                      "", 0, "", 0.799245053428667, 0.799245053428667, 0.799245053428667,
                                      "x1 \u2013 x3", "", 0, "", 0.850502180006117, 0.850502180006117,
                                      0.850502180006117, "x2 \u2013 x3", "", 0, ""))
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
                                 list(0.645448696452428, 1.30886136527697, 0.977155030864699, "x1 \u2013 x2",
                                      7.75275221798211e-09, 0.16924103556429, 5.77374764699725, 0.517939216362844,
                                      1.10695001656577, 0.812444616464306, "x1 \u2013 x3", 6.41237793974625e-08,
                                      0.150260618268746, 5.4069031914352, 1.15950946597789, 2.40450916420934,
                                      1.78200931509361, "x2 \u2013 x3", 2.01483318917894e-08, 0.31760779995241,
                                      5.61072276990876))
})

test_that("Residual variances have SEs for predictors when fixedX is FALSE", {
  table <- results2[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_var"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.03397986325268, 2.58125337722449, 1.80761662023859, "y1", 4.66106229168872e-06,
                                      0.394719884185759, 4.57949217320885, 4.2359564120482, 9.06706000625442,
                                      6.65150820915131, "y2", 6.77669695914318e-08, 1.23244703278054,
                                      5.3969931625741, 3.52704381994281, 7.3297510185918, 5.4283974192673,
                                      "y3", 2.1969400965105e-08, 0.970096192747483, 5.59573108301057,
                                      1.73590513978263, 4.28966247648356, 3.0127838081331, "y4", 3.75471372038305e-06,
                                      0.651480679452441, 4.62451750781818, 1.4712708629505, 3.30303136977368,
                                      2.38715111636209, "y5", 3.24809078522392e-07, 0.467294430222156,
                                      5.10845189236948, 3.02138970851299, 6.49451199153998, 4.75795085002648,
                                      "y6", 7.87167884208628e-08, 0.886016863172622, 5.3700454785808,
                                      1, 1, 1, "Y", "", 0, "", 0.36035839418987, 0.699613976147726,
                                      0.529986185168798, "x1", 9.14129882900738e-10, 0.0865463816258514,
                                      6.12372435695788, 1.5310034438971, 2.97235037160353, 2.25167690775032,
                                      "x2", 9.14129882900738e-10, 0.367697299306413, 6.12372435695789,
                                      1.32566298405078, 2.57369431726059, 1.94967865065568, "x3",
                                      9.14129882900738e-10, 0.318381190433632, 6.1237243569579))
})
