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
  results <- jaspTools::runAnalysis("PLSSEM", "poldem_grouped.csv", options)


  fittab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_fittab"]][["data"]]
  jaspTools::expect_equal_tables(fittab,
                                  list(1415.78715839976, 1434.32706330805, 312.55822949309, 13, "Model1",
                                      75, 5.64331990957278e-59))



  loadingTab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_loading"]][["data"]]
  jaspTools::expect_equal_tables(loadingTab,
                                  list(0.878153828087842, 0.999025039182358, 0.953824969655128, "ind60",
                                      1.6810436032475e-168, "x1", 0.0345047776748861, 27.6432724372937,
                                      0.844544545360538, 0.991599687172787, 0.941337964884426, "ind60",
                                      6.87821661156383e-114, "x2", 0.0415590388642241, 22.6506192301471,
                                      0.751048736717798, 0.971772518599437, 0.86864567889007, "ind60",
                                      1.45553080142854e-35, "x3", 0.0701009796383677, 12.3913486426464,
                                      0.593541599855371, 0.953342888176247, 0.789015766031364, "dem60",
                                      1.6321269668668e-09, "y1", 0.133330930828838, 5.91772487543986,
                                      0.191027569356954, 0.830605002444806, 0.548340145423321, "dem60",
                                      0.00219770834989698, "y2", 0.192515225513558, 2.84829495412925,
                                      0.510773042794821, 0.967457776367431, 0.761288338531433, "dem60",
                                      2.38978645270708e-05, "y3", 0.187225736371952, 4.06615219298173,
                                      0.782855281081119, 0.991976429316931, 0.925786115384094, "dem60",
                                      7.4367468432939e-50, "y4", 0.0625574543607991, 14.7989735970495
                                  ))


  pathTab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_path"]][["data"]]
  jaspTools::expect_equal_tables(pathTab,
                                  list(0.387639436240242, 0.676606105625154, 0.541009527830659, "dem60",
                                      1.23978163374192e-12, "ind60", 0.0772379639979521, 7.00445091800974
                                  ))


  totalTab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_total"]][["data"]]
  jaspTools::expect_equal_tables(totalTab,
                                  list(0.387639436240242, 0.676606105625154, 0.541009527830659, "dem60",
                                      1.23978163374192e-12, "ind60", 0.0772379639979521, 7.00445091800974
                                  ))


  weightTab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_weight"]][["data"]]
  jaspTools::expect_equal_tables(weightTab,
                                 list(0.334549894602283, 0.389577128427852, 0.363698498131332, "ind60",
                                      6.19734663548228e-136, "x1", 0.0146729800025821, 24.7869552106885,
                                      0.319338320007144, 0.393185363665554, 0.359074643079094, "ind60",
                                      2.20141290978141e-73, "x2", 0.0198579230660197, 18.0821852257818,
                                      0.298156780261209, 0.368461523472841, 0.330825036153308, "ind60",
                                      3.31186245520699e-50, "x3", 0.0222728467265738, 14.8532893084834,
                                      0.230183446676106, 0.365227210954829, 0.304305008995715, "dem60",
                                      2.34518315119544e-14, "y1", 0.0403573647612108, 7.54025964768135,
                                      0.0880170900884156, 0.312209546186919, 0.209325351971843, "dem60",
                                      0.00137289975502701, "y2", 0.0698951069216201, 2.99484987134477,
                                      0.199167224706968, 0.385687287929102, 0.287709867661853, "dem60",
                                      0.00136126823193088, "y3", 0.0959850603688332, 2.9974442538901,
                                      0.307219067712089, 0.426829531330196, 0.363694421901235, "dem60",
                                      7.2629257174068e-10, "y4", 0.0601193352030154, 6.04954164368394
                                  ))
})
