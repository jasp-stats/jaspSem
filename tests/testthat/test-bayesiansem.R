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
  jaspTools::expect_equal_tables(table,
                                 list(0.0977812449233765, "Bayesian RMSEA", 0.0441573130088842, 0.0980718607777259,
                                      0.0274811509445776, 0.151686965781034, 0.958556607896851, "Bayesian Gamma Hat",
                                      0.915347972685177, 0.960826387426461, 0.020590958806948, 0.993297051069999,
                                      0.921785473676437, "Bayesian Adj. Gamma Hat", 0.840239471656203,
                                      0.926069141633091, 0.0388605277682152, 0.98734978125762, 0.927065193919297,
                                      "Bayesian McDonald's Centrality", 0.850576234212444, 0.931137037900274,
                                      0.0363857835067295, 0.988260138432836, 0.970780848454299, "Bayesian CFI",
                                      0.94046568042853, 0.972540906924432, 0.0152027900038884, 0.997742950300803,
                                      0.966683624865785, "Bayesian TLI", 0.932101310997279, 0.968682997732147,
                                      0.017354446113696, 0.997425842494, 0.935775153033415, "Bayesian NFI",
                                      0.905336991298884, 0.937548961295833, 0.0145886865675074, 0.960277403014753
                                 ))
})

test_that("Model Fit table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_fittab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1906.36346173569, 1905.83340954543, "Model 1", 75, 0.1575, 1905.62733904596,
                                      15, 15))
})

test_that("Factor Loadings table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_ind"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 1, 1, "", "ind60", "", "", "", "x1", 0, 1.9532168575293, 2.56517267860099,
                                      2.23915232240683, "", "ind60", 234.158080082042, "normal(0,10)",
                                      0.997693090997038, "x2", 0.160873314749524, 1.5402366210022,
                                      2.16807785385677, 1.84369511512563, "", "ind60", 351.43604433154,
                                      "normal(0,10)", 0.99911319228417, "x3", 0.154847216471292, 1,
                                      1, 1, "", "dem60", "", "", "", "y1", 0, 1.06838755727553, 2.00612389474236,
                                      1.49692306578284, "", "dem60", 142.215489654916, "normal(0,10)",
                                      1.01060758321359, "y2", 0.258672775324425, 0.774568619239911,
                                      1.58341442825733, 1.14647510898058, "", "dem60", 156.923984126535,
                                      "normal(0,10)", 1.01074724462383, "y3", 0.194020553612609, 1.10029865408536,
                                      2.0293567295824, 1.52279668471034, "", "dem60", 99.5268411607054,
                                      "normal(0,10)", 1.00731214698584, "y4", 0.244980052550673))
})

test_that("Factor Variances table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_lvar"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.283932707489185, 0.664706603654407, 0.446681847345986, "", "ind60",
                                      332.538342458098, "gamma(1,0.5)[sd]", 1.00191626886132, 0.0950770068678501,
                                      1.71418079014573, 5.56841007837074, 3.41220758029049, "", "dem60",
                                      176.015603843345, "gamma(1,0.5)[sd]", 0.998115219997891, 1.02470540118021
                                 ))
})

test_that("Regression Coefficients table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_reg"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.608355320401092, 2.08918040399425, 1.35613928209665, "", "dem60",
                                      112.582981008041, "normal(0,10)", 1.02054836443106, "ind60",
                                      0.362601909681302))
})

test_that("Residual Variances table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_var"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0537626650941761, 0.139315006891883, 0.0913901143044193, "",
                                      "x1", 173.961694715611, "gamma(1,0.5)[sd]", 0.999309053656452,
                                      0.022396773153692, 8.049983539546e-05, 0.277406757498406, 0.104709707889598,
                                      "", "x2", 91.8859157917485, "gamma(1,0.5)[sd]", 0.996733901172181,
                                      0.0776319924126817, 0.347110879560647, 0.714105472715479, 0.506376270388377,
                                      "", "x3", 281.326323691425, "gamma(1,0.5)[sd]", 0.998178951669858,
                                      0.10049671962408, 1.50320897034426, 3.95192676335891, 2.62279866493932,
                                      "", "y1", 186.254355714611, "gamma(1,0.5)[sd]", 0.995805413875161,
                                      0.60869791140341, 4.44861907764544, 9.83134046264562, 6.88746763454273,
                                      "", "y2", 598.086034133994, "gamma(1,0.5)[sd]", 0.998493854709131,
                                      1.42268927130471, 3.94234538925821, 8.08873898165505, 5.69624995761987,
                                      "", "y3", 448.458736442092, "gamma(1,0.5)[sd]", 0.995292366857604,
                                      1.08915436269113, 0.432834786721287, 3.9046922943355, 2.10977838773889,
                                      "", "y4", 138.510701182285, "gamma(1,0.5)[sd]", 0.997033530261236,
                                      0.860767407353457))
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
  jaspTools::expect_equal_tables(table,
                                 list(1881.32522055122, 1882.20127389458, "Model 1", 75, 1882.03059237359,
                                      26, 30))
})

test_that("Factor Loadings table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_ind"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 1, 1, 1, "", "ind60", "", "x1", 0, 2.01007229197278, 2.66440161565327,
                                      2.28762428711893, 1, "", "ind60", "normal(0,10)", "x2", 0.15858242401967,
                                      1.57369817320054, 2.24376688416746, 1.89090831722004, 1, ".p3.",
                                      "ind60", "normal(0,10)", "x3", 0.173784437585129, 1, 1, 1, 1,
                                      "", "dem60", "", "y1", 0, 1.23682273525706, 2.17535550511813,
                                      1.65555335893949, 1, ".p5.", "dem60", "normal(0,10)", "y2",
                                      0.248284460279494, 0.754262962905413, 1.51569200329892, 1.10585186750284,
                                      1, ".p6.", "dem60", "normal(0,10)", "y3", 0.190801846533562,
                                      1.16688271685381, 1.97572090483757, 1.53696408454058, 1, ".p7.",
                                      "dem60", "normal(0,10)", "y4", 0.209475514998085, 1, 1, 1, 2,
                                      "", "ind60", "", "x1", 0, 1.85450153764912, 2.87652122163773,
                                      2.37748103397316, 2, "", "ind60", "normal(0,10)", "x2", 0.261538468477678,
                                      1.57369817320054, 2.24376688416746, 1.89090831722004, 2, ".p3.",
                                      "ind60", "", "x3", 0.173784437585129, 1, 1, 1, 2, "", "dem60",
                                      "", "y1", 0, 1.23682273525706, 2.17535550511813, 1.65555335893949,
                                      2, ".p5.", "dem60", "", "y2", 0.248284460279494, 0.754262962905413,
                                      1.51569200329892, 1.10585186750284, 2, ".p6.", "dem60", "",
                                      "y3", 0.190801846533562, 1.16688271685381, 1.97572090483757,
                                      1.53696408454058, 2, ".p7.", "dem60", "", "y4", 0.209475514998085
                                 ))
})

test_that("Factor Variances table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_lvar"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.263226572626372, 0.732043056317347, 0.446421435173051, 1, "",
                                      "ind60", "gamma(1,0.5)[sd]", 0.124955136019569, 1.28779551158517,
                                      5.19334305313828, 2.83816884164325, 1, "", "dem60", "gamma(1,0.5)[sd]",
                                      1.01705924819862, 0.188556329680152, 0.653466497337659, 0.354905759143606,
                                      2, "", "ind60", "gamma(1,0.5)[sd]", 0.113070826229762, 1.96662573975186,
                                      6.71986554274559, 3.75752125985714, 2, "", "dem60", "gamma(1,0.5)[sd]",
                                      1.24120186989403))
})

test_that("Regression Coefficients table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_reg"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.815953562643048, 2.60118593824993, 1.67581745535033, 1, "",
                                      "dem60", "normal(0,10)", "ind60", 0.441498861667027, -0.312456087676206,
                                      2.01243645531556, 0.907505231073783, 2, "", "dem60", "normal(0,10)",
                                      "ind60", 0.591523647528914))
})

test_that("Residual Variances table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_var"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0300708380067292, 0.101501895696027, 0.0613096065596296, 1,
                                      "", "x1", "gamma(1,0.5)[sd]", 0.0183001853668487, 5.42817560149972e-06,
                                      0.17569704600619, 0.0396057404498012, 1, "", "x2", "gamma(1,0.5)[sd]",
                                      0.053678867023321, 0.338223262849501, 0.897145163155488, 0.572127410666549,
                                      1, "", "x3", "gamma(1,0.5)[sd]", 0.148963892214124, 1.99043482092264,
                                      6.00745074394337, 3.61293055909161, 1, "", "y1", "gamma(1,0.5)[sd]",
                                      1.07626803497133, 3.04830021903427, 8.93372737433983, 5.24820633059636,
                                      1, "", "y2", "gamma(1,0.5)[sd]", 1.45965243312086, 3.70524788967191,
                                      10.1892162426505, 6.14698690459361, 1, "", "y3", "gamma(1,0.5)[sd]",
                                      1.67962429515206, 0.000230332847360387, 1.93276904045114, 0.527133225493819,
                                      1, "", "y4", "gamma(1,0.5)[sd]", 0.541132621965694, 0.0455200455430703,
                                      0.188025220992784, 0.0990973043032457, 2, "", "x1", "gamma(1,0.5)[sd]",
                                      0.0349141920617094, 0.00184458717029885, 0.546326755421821,
                                      0.215044935453852, 2, "", "x2", "gamma(1,0.5)[sd]", 0.147228996674541,
                                      0.25600807101895, 0.832881410783196, 0.491997990414816, 2, "",
                                      "x3", "gamma(1,0.5)[sd]", 0.14904847325761, 0.89663683584586,
                                      4.0316227990463, 2.1413674686689, 2, "", "y1", "gamma(1,0.5)[sd]",
                                      0.8257383799648, 3.17667191745081, 10.151887854535, 6.01790881181674,
                                      2, "", "y2", "gamma(1,0.5)[sd]", 1.8086995203598, 3.9860608811367,
                                      9.76672305837356, 6.45561409944685, 2, "", "y3", "gamma(1,0.5)[sd]",
                                      1.61981924307277, 0.915647047931681, 6.79590639973895, 3.26230684830837,
                                      2, "", "y4", "gamma(1,0.5)[sd]", 1.4249386705406))
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

test_that("Model Fit table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_fittab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1906.36346173569, 1905.83340954543, "Model 1", 75, 1, 1905.62733904596,
                                      0, 15, 15, 0, 1908.00800997856, 1907.12863445693, "Model 2",
                                      75, 2, 1907.00252475413, -0.642894015831216, 15, 15, 0.485106016309889
                                 ))
})

test_that("Factor Loadings table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_Model 1"]][["collection"]][["modelContainer_params_Model 1_ind"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 1, 1, "", "ind60", "", "x1", 0, 1.9532168575293, 2.56517267860099,
                                      2.23915232240683, "", "ind60", "normal(0,10)", "x2", 0.160873314749524,
                                      1.5402366210022, 2.16807785385677, 1.84369511512563, "", "ind60",
                                      "normal(0,10)", "x3", 0.154847216471292, 1, 1, 1, "", "dem60",
                                      "", "y1", 0, 1.06838755727553, 2.00612389474236, 1.49692306578284,
                                      "", "dem60", "normal(0,10)", "y2", 0.258672775324425, 0.774568619239911,
                                      1.58341442825733, 1.14647510898058, "", "dem60", "normal(0,10)",
                                      "y3", 0.194020553612609, 1.10029865408536, 2.0293567295824,
                                      1.52279668471034, "", "dem60", "normal(0,10)", "y4", 0.244980052550673
                                 ))
})

test_that("Factor Variances table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_Model 1"]][["collection"]][["modelContainer_params_Model 1_lvar"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.283932707489185, 0.664706603654407, 0.446681847345986, "", "ind60",
                                      "gamma(1,0.5)[sd]", 0.0950770068678501, 1.71418079014573, 5.56841007837074,
                                      3.41220758029049, "", "dem60", "gamma(1,0.5)[sd]", 1.02470540118021
                                 ))
})

test_that("Regression Coefficients table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_Model 1"]][["collection"]][["modelContainer_params_Model 1_reg"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.608355320401092, 2.08918040399425, 1.35613928209665, "", "dem60",
                                      "normal(0,10)", "ind60", 0.362601909681302))
})

test_that("Residual Variances table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_Model 1"]][["collection"]][["modelContainer_params_Model 1_var"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0537626650941761, 0.139315006891883, 0.0913901143044193, "",
                                      "x1", "gamma(1,0.5)[sd]", 0.022396773153692, 8.049983539546e-05,
                                      0.277406757498406, 0.104709707889598, "", "x2", "gamma(1,0.5)[sd]",
                                      0.0776319924126817, 0.347110879560647, 0.714105472715479, 0.506376270388377,
                                      "", "x3", "gamma(1,0.5)[sd]", 0.10049671962408, 1.50320897034426,
                                      3.95192676335891, 2.62279866493932, "", "y1", "gamma(1,0.5)[sd]",
                                      0.60869791140341, 4.44861907764544, 9.83134046264562, 6.88746763454273,
                                      "", "y2", "gamma(1,0.5)[sd]", 1.42268927130471, 3.94234538925821,
                                      8.08873898165505, 5.69624995761987, "", "y3", "gamma(1,0.5)[sd]",
                                      1.08915436269113, 0.432834786721287, 3.9046922943355, 2.10977838773889,
                                      "", "y4", "gamma(1,0.5)[sd]", 0.860767407353457))
})

test_that("Factor Loadings table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_Model 2"]][["collection"]][["modelContainer_params_Model 2_ind"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 1, 1, "", "ind60", "", "x1", 0, 1.96519068564153, 2.51673059616912,
                                      2.22745627318565, "", "ind60", "normal(0,10)", "x2", 0.147109264536351,
                                      1.52392573381841, 2.14033985258738, 1.83664770518682, "", "ind60",
                                      "normal(0,10)", "x3", 0.165151763792851, 1, 1, 1, "", "dem60",
                                      "", "y1", 0, 1.0914628169732, 2.01584566619698, 1.54522650863524,
                                      "", "dem60", "normal(0,10)", "y2", 0.244692844254203, 0.845114283621126,
                                      1.57775145985324, 1.16213045249259, "", "dem60", "normal(0,10)",
                                      "y3", 0.191840732689781, 1.14304702178927, 2.1012100225933,
                                      1.59967841227419, "", "dem60", "normal(0,10)", "y4", 0.254544832640968
                                 ))
})

test_that("Factor Covariances table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_Model 2"]][["collection"]][["modelContainer_params_Model 2_lcov"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.278349696338848, 1.04975295035336, 0.598487745834573, "", "ind60 - dem60",
                                      "lkj_corr(1)", 0.199283825243998))
})

test_that("Factor Variances table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_Model 2"]][["collection"]][["modelContainer_params_Model 2_lvar"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.303144015762355, 0.703077589685985, 0.462209301645044, "", "ind60",
                                      "gamma(1,0.5)[sd]", 0.103785215027818, 2.13184089890503, 6.65951675159269,
                                      3.95327378859623, "", "dem60", "gamma(1,0.5)[sd]", 1.15069819421129
                                 ))
})

test_that("Residual Variances table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_Model 2"]][["collection"]][["modelContainer_params_Model 2_var"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0494395895825296, 0.135638795976496, 0.0907033045381739, "",
                                      "x1", "gamma(1,0.5)[sd]", 0.0224170838077918, 0.00907833252556717,
                                      0.289710986087315, 0.112435158682542, "", "x2", "gamma(1,0.5)[sd]",
                                      0.0735661378454354, 0.337814332391283, 0.72121648559724, 0.504670906288005,
                                      "", "x3", "gamma(1,0.5)[sd]", 0.104252582418159, 1.52170064942189,
                                      4.20019273035171, 2.74205293975566, "", "y1", "gamma(1,0.5)[sd]",
                                      0.684940448197217, 4.59300415032351, 9.89943630594643, 6.87979883130566,
                                      "", "y2", "gamma(1,0.5)[sd]", 1.34858902968913, 3.7729627160678,
                                      8.36638015220564, 5.74414303407463, "", "y3", "gamma(1,0.5)[sd]",
                                      1.15205015243363, 0.0301222337991594, 4.03231453454266, 1.860334916366,
                                      "", "y4", "gamma(1,0.5)[sd]", 1.00210267084322))
})
