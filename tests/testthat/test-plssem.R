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
                                 list(0.877332169319391, 0.997553691579049, 0.949554216329399, "ind60",
                                      1.13465643555399e-149, "x1", 0.0364792923253741, 26.0299516739504,
                                      0.857830048718849, 0.995132921672411, 0.939491666920393, "ind60",
                                      1.11360151755777e-111, "x2", 0.0418941745098576, 22.4253533554966,
                                      0.740177244034861, 0.984707723882063, 0.866418969743743, "ind60",
                                      4.57077577001329e-38, "x3", 0.0674503539062686, 12.8452842656356,
                                      0.651832069989433, 0.955933428129885, 0.796119072378238, "dem60",
                                      1.1265333311975e-18, "y1", 0.0910490737980437, 8.74384591922498,
                                      0.127503446462884, 0.741973126907068, 0.560117940982156, "dem60",
                                      0.000153637315494029, "y2", 0.155196505114484, 3.60908862328423,
                                      0.487123728407951, 0.943874988558289, 0.753670488436454, "dem60",
                                      1.0542786972676e-05, "y3", 0.177206055182163, 4.2530741269631,
                                      0.83662206923296, 0.997949372041486, 0.934223157569736, "dem60",
                                      1.21592650510967e-77, "y4", 0.0501867717007415, 18.6149283149833
                                 ))


  pathTab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_path"]][["data"]]
  jaspTools::expect_equal_tables(pathTab,
                                 list(0.384844305185851, 0.702755195791058, 0.540255839878697, "dem60",
                                      4.23605621183583e-12, "ind60", 0.0790966255926938, 6.83032728426029
                                 ))

  totalTab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_total"]][["data"]]
  jaspTools::expect_equal_tables(totalTab,
                                 list(0.384844305185851, 0.702755195791058, 0.540255839878697, "dem60",
                                      4.23605621183583e-12, "ind60", 0.0790966255926938, 6.83032728426029
                                 ))


  weightTab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_weight"]][["data"]]
  jaspTools::expect_equal_tables(weightTab,
                                 list(0.337300855604375, 0.397783613324621, 0.363924413163503, "ind60",
                                      7.92410240075626e-105, "x1", 0.0167615427635713, 21.7118685491432,
                                      0.332734713059874, 0.389829524629347, 0.360000224529082, "ind60",
                                      1.87262369514304e-99, "x2", 0.0170328967569328, 21.1355842559519,
                                      0.295716880841832, 0.376629417597764, 0.331731322327227, "ind60",
                                      4.12663474055487e-48, "x3", 0.0228365644247833, 14.5263234940548,
                                      0.254896350536653, 0.391738520943328, 0.308102116070721, "dem60",
                                      1.57895224714032e-16, "y1", 0.037724610459296, 8.16713843614518,
                                      0.0537665248942941, 0.292491335936142, 0.214332480213437, "dem60",
                                      7.56118980611874e-05, "y2", 0.0565662677646452, 3.78905111974523,
                                      0.185384615946491, 0.366882464740143, 0.28873493082986, "dem60",
                                      3.31488669505353e-05, "y3", 0.0723792360684123, 3.98919561069904,
                                      0.300753960587727, 0.426426638819703, 0.362203681766653, "dem60",
                                      3.66778411406767e-29, "y4", 0.0324908939474746, 11.1478521444254
                                 ))
})
