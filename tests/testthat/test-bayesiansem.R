context("Bayesian Structural Equation Modeling")

# Note: MCMC sampling is stochastic; snapshot tests are not used.
# Tests verify coarse rounding output. Reduced MCMC settings for speed.

oldRounder <- getOption("jaspRoundToPrecision")
on.exit(options("jaspRoundToPrecision" = oldRounder), add = TRUE)

options("jaspRoundToPrecision" = function(x) signif(round(x, digits = 3), digits = 3))


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
options$looComparison               <- FALSE
options$convergenceDiagnostics      <- TRUE
options$tracePlots                  <- TRUE
options$tracePlotsType              <- "loadings"
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
options$setSeed <- TRUE
options$seed <- 1

set.seed(123)
results <- jaspTools::runAnalysis("BayesianSEM", testthat::test_path("poldem_grouped.csv"), options, makeTests = FALSE)

isMacOS <- Sys.info()[["sysname"]] == "Darwin"

expect_bsem_table <- function(table, expected) {
  expect_true(!is.null(table))
  expect_gt(length(table), 0L)

  if (isMacOS)
    jaspTools::expect_equal_tables(table, expected)
}

test_that("Factor Loadings traceplot exists", {
  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_traceplots"]][["collection"]][["modelContainer_traceplots_loadings"]][["data"]]
  expect_true(!is.null(plotName))
  expect_true(plotName %in% names(results[["state"]][["figures"]]))
})

test_that("Fit Indices table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_addfit"]][["collection"]][["modelContainer_addfit_fitTable_1"]][["data"]]
  expect_bsem_table(table,
                                 list(0.0914176789267399, "Bayesian RMSEA", 0.0320047673382468, 0.0941987138446978,
                                      0.03099716732867, 0.158552754287515, 0.959188319219869, "Bayesian Gamma Hat",
                                      0.919178228311554, 0.960576648137676, 0.0217296048419563, 1,
                                      0.911762557586066, "Bayesian Adj. Gamma Hat", 0.825258203317533,
                                      0.914764212764065, 0.0469807839145192, 1, 0.928171394533754,
                                      "Bayesian McDonald's Centrality", 0.857379605922294, 0.930696225558687,
                                      0.038375208085957, 1, 0.970689102540015, "Bayesian CFI", 0.941047589511724,
                                      0.97199690423997, 0.0162408729547958, 1, 0.956873388088708,
                                      "Bayesian TLI", 0.903277630402953, 0.95862086283336, 0.0243612019613584,
                                      0.999418703842821, 0.932998466919882, "Bayesian NFI", 0.898782496394001,
                                      0.934262253334632, 0.0155083628207357, 0.959814529394037))
})

test_that("Model Fit table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_fittab"]][["data"]]
  expect_false("Rank" %in% names(table))
  expect_false("elpdDiff" %in% names(table))
  expect_false("seDiff" %in% names(table))
  expect_bsem_table(table,
                                 list(1919.57818952403, 1918.40775635642, "Model 1", 75, 0.2075, 1918.27705443898,
                                      22, 22))
})

test_that("Factor Loadings table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_ind"]][["data"]]
  expect_bsem_table(table,
                                 list(1, 1, 1, "", "ind60", "", "", "", "x1", 0, 1.93284378928454, 2.47623854435757,
                                      2.20623079712006, "", "ind60", 466.336786426605, "normal(0,10)",
                                      0.995756020213819, "x2", 0.150386671911007, 1.55128696483138,
                                      2.16125715027978, 1.83648319405802, "", "ind60", 553.18454103817,
                                      "normal(0,10)", 0.996293110726149, "x3", 0.163934758322592,
                                      1, 1, 1, "", "dem60", "", "", "", "y1", 0, 1.10312794072486,
                                      2.0679550362986, 1.50234555545864, "", "dem60", 192.145035038708,
                                      "normal(0,10)", 1.00136028961257, "y2", 0.256634548997049, 0.803717109506823,
                                      1.60097424440386, 1.14283391581511, "", "dem60", 330.86110596523,
                                      "normal(0,10)", 1.00107726532242, "y3", 0.195149731448548, 1.17275754632082,
                                      2.26947170217641, 1.53939606710227, "", "dem60", 106.406010816455,
                                      "normal(0,10)", 1.02658883608126, "y4", 0.26079634734102))
})

test_that("Factor Variances table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_lvar"]][["data"]]
  expect_bsem_table(table,
                                 list(0.31874060238519, 0.687140674866777, 0.464343293047851, "", "ind60",
                                      426.95460120826, "gamma(1,0.5)[sd]", 0.999736870198587, 0.094965668871913,
                                      1.66614274941638, 5.41920510121869, 3.37748702101865, "", "dem60",
                                      240.039171094774, "gamma(1,0.5)[sd]", 1.00321162584399, 0.951903679800013
                                 ))
})

test_that("Regression Coefficients table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_reg"]][["data"]]
  expect_bsem_table(table,
                                 list(0.669383204348159, 2.25914268110066, 1.38878879741143, "", "dem60",
                                      250.839794410615, "normal(0,10)", 1.00848315411559, "ind60",
                                      0.405880107843451))
})

test_that("Residual Variances table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_var"]][["data"]]
  expect_bsem_table(table,
                                 list(0.0449943232251762, 0.128320803994404, 0.0871578855944751, "",
                                      "x1", 361.532799473324, "gamma(1,0.5)[sd]", 0.998712004530478,
                                      0.021012241932814, 0.0112462349702997, 0.301624429789977, 0.133168622307669,
                                      "", "x2", 208.598741625897, "gamma(1,0.5)[sd]", 0.998094462604731,
                                      0.0769687743782306, 0.347821862349497, 0.726747839602854, 0.511215223700493,
                                      "", "x3", 419.277549607866, "gamma(1,0.5)[sd]", 1.00201511302709,
                                      0.09752059334974, 1.56103829672179, 4.14781214039233, 2.68036577262972,
                                      "", "y1", 160.780907391314, "gamma(1,0.5)[sd]", 1.01083395597735,
                                      0.665318748137161, 4.55935063796017, 9.86649150345634, 6.87230958520979,
                                      "", "y2", 410.321866131622, "gamma(1,0.5)[sd]", 0.997535554280817,
                                      1.39717311810869, 3.72018873616012, 8.21856675860155, 5.71847633654275,
                                      "", "y3", 387.798485465013, "gamma(1,0.5)[sd]", 1.00262058254484,
                                      1.21304520198098, 0.15911722766676, 4.03859371473005, 2.10371548546532,
                                      "", "y4", 81.5781257334457, "gamma(1,0.5)[sd]", 1.03333225881661,
                                      0.932893744742066))
})




# Multigroup test: constrain loadings equal across groups, then release one loading
options$additionalFitMeasures       <- FALSE
options$posteriorPredictivePvalue   <- FALSE
options$convergenceDiagnostics      <- FALSE
options$tracePlots                  <- FALSE
options_mg_released_loading <- options
options_mg_released_loading$priorPredictivePlots     <- TRUE
options_mg_released_loading$priorPredictiveBurnin    <- 20L
options_mg_released_loading$priorPredictiveSamples   <- 50L
options_mg_released_loading$priorPredictiveReplicates <- 10L
options_mg_released_loading$group                 <- "group"
options_mg_released_loading$equalLoading          <- TRUE
options_mg_released_loading$additionalFitMeasures <- FALSE
options_mg_released_loading$freeParameters        <- list(model = "ind60 =~ x2", columns = list(), modelOriginal = "")

# blavaan/Stan corrupts future.globals.method.default to NULL after the first MCMC run;
# reset it to the correct default before each subsequent run.
options("future.globals.method.default" = c("ordered", "dfs"))
set.seed(789)
results <- jaspTools::runAnalysis("BayesianSEM", testthat::test_path("poldem_grouped.csv"), options_mg_released_loading,
                                  makeTests = FALSE)

test_that("Model Fit table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_fittab"]][["data"]]
  expect_bsem_table(table,
                                 list(1909.86378118953, 1911.94654472705, "Model 1", 75, 1911.51812802157,
                                      40, 44))
})

test_that("Factor Loadings table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_ind"]][["data"]]
  expect_bsem_table(table,
                                 list(1, 1, 1, 1, "", "ind60", "", "x1", 0, 2.0005218466633, 2.5660540404468,
                                      2.27701530328946, 1, "", "ind60", "normal(0,10)", "x2", 0.152275101561803,
                                      1.56834425126883, 2.2374893339703, 1.90425888632184, 1, ".p3.",
                                      "ind60", "normal(0,10)", "x3", 0.174651308519066, 1, 1, 1, 1,
                                      "", "dem60", "", "y1", 0, 1.21605249527393, 2.18484047491123,
                                      1.66692155563828, 1, ".p5.", "dem60", "normal(0,10)", "y2",
                                      0.2479505787661, 0.756867968315127, 1.60399046933345, 1.13459311634964,
                                      1, ".p6.", "dem60", "normal(0,10)", "y3", 0.214155331013545,
                                      1.20492362605759, 1.99021622072774, 1.54415822946378, 1, ".p7.",
                                      "dem60", "normal(0,10)", "y4", 0.20289379150498, 1, 1, 1, 2,
                                      "", "ind60", "", "x1", 0, 1.89391761626021, 2.96048344075863,
                                      2.42919103681659, 2, "", "ind60", "normal(0,10)", "x2", 0.277771663819553,
                                      1.56834425126883, 2.2374893339703, 1.90425888632184, 2, ".p3.",
                                      "ind60", "", "x3", 0.174651308519066, 1, 1, 1, 2, "", "dem60",
                                      "", "y1", 0, 1.21605249527393, 2.18484047491123, 1.66692155563828,
                                      2, ".p5.", "dem60", "", "y2", 0.2479505787661, 0.756867968315127,
                                      1.60399046933345, 1.13459311634964, 2, ".p6.", "dem60", "",
                                      "y3", 0.214155331013545, 1.20492362605759, 1.99021622072774,
                                      1.54415822946378, 2, ".p7.", "dem60", "", "y4", 0.20289379150498
                                 ))
})

test_that("Factor Variances table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_lvar"]][["data"]]
  expect_bsem_table(table,
                                 list(0.270793357756254, 0.796386572717091, 0.465776610258736, 1, "",
                                      "ind60", "gamma(1,0.5)[sd]", 0.137498852664916, 1.38414801704946,
                                      4.84241398400487, 2.81824013826655, 1, "", "dem60", "gamma(1,0.5)[sd]",
                                      0.896054564789262, 0.190441656031967, 0.594488650977301, 0.353904147310565,
                                      2, "", "ind60", "gamma(1,0.5)[sd]", 0.114157665666657, 1.82753834890526,
                                      6.48036356283564, 3.84946333449042, 2, "", "dem60", "gamma(1,0.5)[sd]",
                                      1.19930694413497))
})

test_that("Regression Coefficients table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_reg"]][["data"]]
  expect_bsem_table(table,
                                 list(0.862228957606158, 2.76339975469788, 1.69599325510857, 1, "",
                                      "dem60", "normal(0,10)", "ind60", 0.47989253892453, -0.302225606456611,
                                      2.2295038849341, 0.924827478999557, 2, "", "dem60", "normal(0,10)",
                                      "ind60", 0.626920062369738))
})

test_that("Residual Variances table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_var"]][["data"]]
  expect_bsem_table(table,
                                 list(0.0277522979119088, 0.109906054535616, 0.0624288396693118, 1,
                                      "", "x1", "gamma(1,0.5)[sd]", 0.0202937453193912, 8.13082474372971e-06,
                                      0.176452636176827, 0.0439700965353578, 1, "", "x2", "gamma(1,0.5)[sd]",
                                      0.0512997030375734, 0.343395337286201, 0.953541741486473, 0.574285388612906,
                                      1, "", "x3", "gamma(1,0.5)[sd]", 0.157790973006875, 2.01799485440762,
                                      6.46045109911417, 3.71381337228729, 1, "", "y1", "gamma(1,0.5)[sd]",
                                      1.16158082606578, 3.24412821516093, 8.62644868858408, 5.26315879953397,
                                      1, "", "y2", "gamma(1,0.5)[sd]", 1.37623855559522, 3.86502497027946,
                                      10.826153264339, 6.45094346938068, 1, "", "y3", "gamma(1,0.5)[sd]",
                                      1.75279953662885, 0.000306015521691507, 1.92313339058641, 0.542549883720036,
                                      1, "", "y4", "gamma(1,0.5)[sd]", 0.572653018029761, 0.0509859045030925,
                                      0.194418889457323, 0.107305026469711, 2, "", "x1", "gamma(1,0.5)[sd]",
                                      0.0373070303314962, 3.56195901674587e-05, 0.524974165889329,
                                      0.185438833399035, 2, "", "x2", "gamma(1,0.5)[sd]", 0.143091315575435,
                                      0.260864830207312, 0.85134638422735, 0.520928829625502, 2, "",
                                      "x3", "gamma(1,0.5)[sd]", 0.148340485507057, 1.05445614622381,
                                      3.94199293044316, 2.16541728738221, 2, "", "y1", "gamma(1,0.5)[sd]",
                                      0.780625676999269, 3.11422044808143, 10.8613232456897, 6.11365239395048,
                                      2, "", "y2", "gamma(1,0.5)[sd]", 2.03891254493602, 3.8311993171096,
                                      10.9892949594862, 6.61687809750709, 2, "", "y3", "gamma(1,0.5)[sd]",
                                      1.83177431854201, 0.927800412500054, 6.88193136567574, 3.45120056767015,
                                      2, "", "y4", "gamma(1,0.5)[sd]", 1.47141035823601))
})

test_that("Multigroup prior predictive plots create separate group containers", {
  ppCont <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_priorPredictivePlots"]][["collection"]]

  expect_true("modelContainer_priorPredictivePlots_group1" %in% names(ppCont))
  expect_true("modelContainer_priorPredictivePlots_group2" %in% names(ppCont))

  group1Plot <- ppCont[["modelContainer_priorPredictivePlots_group1"]][["collection"]][["modelContainer_priorPredictivePlots_group1_plot"]][["data"]]
  group2Plot <- ppCont[["modelContainer_priorPredictivePlots_group2"]][["collection"]][["modelContainer_priorPredictivePlots_group2_plot"]][["data"]]

  expect_true(!is.null(group1Plot))
  expect_true(!is.null(group2Plot))
  expect_true(group1Plot %in% names(results[["state"]][["figures"]]))
  expect_true(group2Plot %in% names(results[["state"]][["figures"]]))
})

test_that("Prior predictive plot styling matches expectations", {
  ppCont   <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_priorPredictivePlots"]][["collection"]]
  plotName <- ppCont[["modelContainer_priorPredictivePlots_group1"]][["collection"]][["modelContainer_priorPredictivePlots_group1_plot"]][["data"]]
  plotObj  <- results[["state"]][["figures"]][[plotName]][["obj"]]

  expect_equal(plotObj$labels$y, "Density")
  expect_equal(plotObj$labels$x, "")
})



# Multi-model test: two models (one with regression, one without)
model_no_reg <- "
  ind60 =~ x1 + x2 + x3
  dem60 =~ y1 + y2 + y3 + y4
"

options_mm <- options
options_mm$additionalFitMeasures <- FALSE
options_mm$looComparison         <- TRUE
options_mm$models <- list(
  list(name = "Model 1", syntax = list(model = model,        columns = c("x1", "x2", "x3", "y1", "y2", "y3", "y4"))),
  list(name = "Model 2", syntax = list(model = model_no_reg, columns = c("x1", "x2", "x3", "y1", "y2", "y3", "y4")))
)

options("future.globals.method.default" = c("ordered", "dfs"))
set.seed(321)
results <- jaspTools::runAnalysis("BayesianSEM", testthat::test_path("poldem_grouped.csv"), options_mm,
                                     makeTests = FALSE)

test_that("Model Fit table adds LOO comparison for multiple models", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_fittab"]][["data"]]
  expect_equal(length(table), 2L)
  expect_true(all(vapply(table, function(row) all(c("Rank", "elpdDiff", "seDiff") %in% names(row)), TRUE)))

  ranks     <- vapply(table, function(row) row$Rank, integer(1))
  elpdDiffs <- vapply(table, function(row) row$elpdDiff, numeric(1))
  seDiffs   <- vapply(table, function(row) row$seDiff, numeric(1))

  expect_equal(sort(ranks), c(1L, 2L))

  bestRow  <- which(ranks == 1L)
  worseRow <- which(ranks == 2L)

  expect_length(bestRow, 1L)
  expect_length(worseRow, 1L)
  expect_equal(elpdDiffs[bestRow], 0)
  expect_equal(seDiffs[bestRow], 0)
  expect_lte(elpdDiffs[worseRow], 0)
  expect_true(is.finite(seDiffs[worseRow]))
  expect_gte(seDiffs[worseRow], 0)
})

test_that("Factor Loadings table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_Model 1"]][["collection"]][["modelContainer_params_Model 1_ind"]][["data"]]
  expect_bsem_table(table,
                                 list(1, 1, 1, "", "ind60", "", "x1", 0, 1.93284378928454, 2.47623854435757,
                                      2.20623079712006, "", "ind60", "normal(0,10)", "x2", 0.150386671911007,
                                      1.55128696483138, 2.16125715027978, 1.83648319405802, "", "ind60",
                                      "normal(0,10)", "x3", 0.163934758322592, 1, 1, 1, "", "dem60",
                                      "", "y1", 0, 1.10312794072486, 2.0679550362986, 1.50234555545864,
                                      "", "dem60", "normal(0,10)", "y2", 0.256634548997049, 0.803717109506823,
                                      1.60097424440386, 1.14283391581511, "", "dem60", "normal(0,10)",
                                      "y3", 0.195149731448548, 1.17275754632082, 2.26947170217641,
                                      1.53939606710227, "", "dem60", "normal(0,10)", "y4", 0.26079634734102
                                 ))
})

test_that("Factor Variances table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_Model 1"]][["collection"]][["modelContainer_params_Model 1_lvar"]][["data"]]
  expect_bsem_table(table,
                                 list(0.31874060238519, 0.687140674866777, 0.464343293047851, "", "ind60",
                                      "gamma(1,0.5)[sd]", 0.094965668871913, 1.66614274941638, 5.41920510121869,
                                      3.37748702101865, "", "dem60", "gamma(1,0.5)[sd]", 0.951903679800013
                                 ))
})

test_that("Regression Coefficients table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_Model 1"]][["collection"]][["modelContainer_params_Model 1_reg"]][["data"]]
  expect_bsem_table(table,
                                 list(0.669383204348159, 2.25914268110066, 1.38878879741143, "", "dem60",
                                      "normal(0,10)", "ind60", 0.405880107843451))
})

test_that("Residual Variances table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_Model 1"]][["collection"]][["modelContainer_params_Model 1_var"]][["data"]]
  expect_bsem_table(table,
                                 list(0.0449943232251762, 0.128320803994404, 0.0871578855944751, "",
                                      "x1", "gamma(1,0.5)[sd]", 0.021012241932814, 0.0112462349702997,
                                      0.301624429789977, 0.133168622307669, "", "x2", "gamma(1,0.5)[sd]",
                                      0.0769687743782306, 0.347821862349497, 0.726747839602854, 0.511215223700493,
                                      "", "x3", "gamma(1,0.5)[sd]", 0.09752059334974, 1.56103829672179,
                                      4.14781214039233, 2.68036577262972, "", "y1", "gamma(1,0.5)[sd]",
                                      0.665318748137161, 4.55935063796017, 9.86649150345634, 6.87230958520979,
                                      "", "y2", "gamma(1,0.5)[sd]", 1.39717311810869, 3.72018873616012,
                                      8.21856675860155, 5.71847633654275, "", "y3", "gamma(1,0.5)[sd]",
                                      1.21304520198098, 0.15911722766676, 4.03859371473005, 2.10371548546532,
                                      "", "y4", "gamma(1,0.5)[sd]", 0.932893744742066))
})

test_that("Factor Loadings table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_Model 2"]][["collection"]][["modelContainer_params_Model 2_ind"]][["data"]]
  expect_bsem_table(table,
                                 list(1, 1, 1, "", "ind60", "", "x1", 0, 1.95899323190788, 2.52492888448402,
                                      2.2206282613949, "", "ind60", "normal(0,10)", "x2", 0.144396941307241,
                                      1.54645111504178, 2.13965574355883, 1.83237946108101, "", "ind60",
                                      "normal(0,10)", "x3", 0.150683282577894, 1, 1, 1, "", "dem60",
                                      "", "y1", 0, 1.06804451036949, 2.14876088370561, 1.5059598528438,
                                      "", "dem60", "normal(0,10)", "y2", 0.253740470566943, 0.846193297685248,
                                      1.60754577629635, 1.14611855049912, "", "dem60", "normal(0,10)",
                                      "y3", 0.199352837373486, 1.11987334147141, 2.16846675165793,
                                      1.54517907931288, "", "dem60", "normal(0,10)", "y4", 0.270410368847354
                                 ))
})

test_that("Factor Covariances table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_Model 2"]][["collection"]][["modelContainer_params_Model 2_lcov"]][["data"]]
  expect_bsem_table(table,
                                 list(0.299257202080091, 1.03349167156974, 0.615882730446758, "", "ind60 - dem60",
                                      "lkj_corr(1)", 0.204379982217316))
})

test_that("Factor Variances table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_Model 2"]][["collection"]][["modelContainer_params_Model 2_lvar"]][["data"]]
  expect_bsem_table(table,
                                 list(0.304526947933073, 0.663777744447057, 0.466319134759897, "", "ind60",
                                      "gamma(1,0.5)[sd]", 0.0897936846476894, 2.04390584047335, 6.99700052224652,
                                      4.25523361577678, "", "dem60", "gamma(1,0.5)[sd]", 1.26407912592141
                                 ))
})

test_that("Residual Variances table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_Model 2"]][["collection"]][["modelContainer_params_Model 2_var"]][["data"]]
  expect_bsem_table(table,
                                 list(0.0504318784687333, 0.141074198896967, 0.0908664276636496, "",
                                      "x1", "gamma(1,0.5)[sd]", 0.0221737998855765, 0.00373272327799819,
                                      0.302705378582595, 0.112294755815397, "", "x2", "gamma(1,0.5)[sd]",
                                      0.0790896808632661, 0.355393504660779, 0.716647373812272, 0.514195216928497,
                                      "", "x3", "gamma(1,0.5)[sd]", 0.10096718473366, 1.46116742662118,
                                      4.32644727604757, 2.71849723547813, "", "y1", "gamma(1,0.5)[sd]",
                                      0.703745949093753, 4.73515756871552, 10.2832559377124, 6.9856333665067,
                                      "", "y2", "gamma(1,0.5)[sd]", 1.45639726214503, 3.77397715132625,
                                      8.09083427822476, 5.74812265746696, "", "y3", "gamma(1,0.5)[sd]",
                                      1.1676183395679, 0.178930477650961, 4.17454866163301, 2.10985831645301,
                                      "", "y4", "gamma(1,0.5)[sd]", 0.996016728226475))
})
