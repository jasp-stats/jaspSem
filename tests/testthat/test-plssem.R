context("Partial Least Squares Structural Equation Modeling")

model <- "
  ind60 =~ x1 + x2 + x3
  dem60 =~ y1 + y2 + y3 + y4
  dem60 ~ ind60
"
test_that("Basic PLSSEM works", {
  options <- jaspTools::analysisOptions("PLSSEM")
  options$models <- list(list(modelName = "Model1", syntax = list(model = model, columns = c("x1", "x2", "x3", "y1", "y2", "y3", "y4"))))
  options$groupingVariable <- ""
  options$innerWeightingScheme      <- "path"
  options$approachNonLinear         <- "sequential"
  options$approachWeights2          <- "PLS-PM"
  options$convergenceCriterion      <- "diff_absolute"
  options$approachCorrectionFactors <- "dist_squared_euclid"
  options$setSeed                   <- TRUE
  options$seed                      <- 1234
  results <- jaspTools::runAnalysis("PLSSEM", "poldem_grouped.csv", options)


  fittab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_fittab"]][["data"]]
  jaspTools::expect_equal_tables(fittab,
                                 list(1415.78715839976, 1434.32706330805, 312.55822949309, 13, "Model1",
                                      75, 5.64331990957278e-59))



  loadingTab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_loading"]][["data"]]
  jaspTools::expect_equal_tables(loadingTab,
                                 list(0.908813088046841, 0.999723336989396, 0.957287431560208, "ind60",
                                      1.65210538891334e-218, "x1", 0.0303605464910073, 31.5306390101955,
                                      0.873410971512945, 0.99726705992958, 0.945465443460148, "ind60",
                                      3.46285414770501e-124, "x2", 0.039939414022218, 23.6724916127761,
                                      0.751210109941494, 0.946795466978086, 0.855738665194547, "ind60",
                                      6.38822343733434e-59, "x3", 0.0530107796025797, 16.1427293016627,
                                      0.640183156580906, 0.975357666090481, 0.816776016230897, "dem60",
                                      1.75433570255258e-15, "y1", 0.103765836810973, 7.87133840320487,
                                      0.330558463156766, 0.90116220376633, 0.612293163277992, "dem60",
                                      5.07918639496688e-06, "y2", 0.138723251628015, 4.41377459144233,
                                      0.491651331633155, 0.948853896894929, 0.720042311300973, "dem60",
                                      6.23032965946502e-06, "y3", 0.164793237843109, 4.369368068285,
                                      0.881474436359286, 0.999071227484472, 0.952762344413074, "dem60",
                                      2.26447841798001e-188, "y4", 0.0325733979061509, 29.2497069896771
                                 ))


  pathTab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_path"]][["data"]]
  jaspTools::expect_equal_tables(pathTab,
                                 list(0.367669590406044, 0.653200646252389, 0.532168815733846, "dem60",
                                      9.00112755378276e-12, "ind60", 0.0791756345919768, 6.72137101870192
                                 ))


  totalTab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_total"]][["data"]]
  jaspTools::expect_equal_tables(totalTab,
                                 list(0.367669590406044, 0.653200646252389, 0.532168815733846, "dem60",
                                      9.00112755378276e-12, "ind60", 0.0791756345919768, 6.72137101870192
                                 ))


  weightTab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_weight"]][["data"]]
  jaspTools::expect_equal_tables(weightTab,
                                 list(0.342645202450133, 0.387344144567206, 0.366226089235645, "ind60",
                                      8.2929728489945e-141, "x1", 0.0145126762647775, 25.234910677673,
                                      0.336373992050745, 0.383741359170949, 0.361588098266446, "ind60",
                                      1.4730495651099e-129, "x2", 0.0149488153297261, 24.1884116092743,
                                      0.295048818266121, 0.360658817914631, 0.327131858233093, "ind60",
                                      4.14532073766624e-77, "x3", 0.0176359823987181, 18.5491145793427,
                                      0.242983109244946, 0.36544634104245, 0.307336195153043, "dem60",
                                      2.96925520347185e-20, "y1", 0.0336053860692634, 9.14544455819071,
                                      0.143505362893591, 0.316376988811909, 0.229708812865109, "dem60",
                                      1.57945369050043e-06, "y2", 0.0492915416028754, 4.66020752030424,
                                      0.202263994575525, 0.346674211809487, 0.270291097429354, "dem60",
                                      9.50662216270636e-06, "y3", 0.0632086259614037, 4.27617422967552,
                                      0.307835250305839, 0.443247387020814, 0.360431351743777, "dem60",
                                      2.06379254905237e-29, "y4", 0.0321844891032835, 11.1989148122598
                                 ))
})
