context("Partial Least Squares Structural Equation Modeling")

model <- "
  ind60 =~ x1 + x2 + x3
  dem60 =~ y1 + y2 + y3 + y4
  dem60 ~ ind60
"

test_that("Basic PLSSEM works", {
  set.seed(123)
  options <- jaspTools::analysisOptions("PLSSEM")
  options$models <- list(list(modelName = "Model1", syntax = list(model = model, columns = c("x1", "x2", "x3", "y1", "y2", "y3", "y4"))))
  options$groupingVariable <- ""
  options$innerWeightingScheme      <- "path"
  options$approachNonLinear         <- "sequential"
  options$approachWeights2          <- "PLS-PM"
  options$convergenceCriterion      <- "diff_absolute"
  options$approachCorrectionFactors <- "dist_squared_euclid"
  options$setSeed                   <- TRUE
  options$seed                      <- 123
  results <- jaspTools::runAnalysis("PLSSEM", "poldem_grouped.csv", options)


  fittab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_fittab"]][["data"]]
  jaspTools::expect_equal_tables(fittab,
                                 list(1415.78715839976, 1434.32706330805, 312.55822949309, 13, "Model1",
                                      75, 5.64331990957278e-59))



  loadingTab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_loading"]][["data"]]
  jaspTools::expect_equal_tables(loadingTab,
                                 list(0.867539681350679, 0.995942100311173, 0.957599449244906, "ind60",
                                      2.18844398048209e-116, "x1", 0.0418118186495218, 22.9026021869981,
                                      0.823496080815603, 0.997648021014092, 0.942821873953847, "ind60",
                                      1.36633368047326e-109, "x2", 0.0424498084849335, 22.2102739118005,
                                      0.725015790085718, 0.945286633211369, 0.828051433626954, "ind60",
                                      1.9443427277183e-10, "x3", 0.132309737516661, 6.25843153473631,
                                      0.623557577409187, 0.979305682511502, 0.808683945085887, "dem60",
                                      3.26057216373881e-14, "y1", 0.107865257826108, 7.49716786835653,
                                      0.0195912436511869, 0.723375340804063, 0.508887870737397, "dem60",
                                      0.00279575874175447, "y2", 0.18365960524811, 2.77082088927464,
                                      0.482687927870186, 0.975324323467869, 0.759701100531397, "dem60",
                                      9.02293262907913e-07, "y3", 0.159127691857428, 4.77416024617551,
                                      0.850464773847159, 0.995582296227975, 0.942153929524718, "dem60",
                                      1.92908943021765e-104, "y4", 0.0434754572083612, 21.6709378123233
                                 ))


  pathTab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_path"]][["data"]]
  jaspTools::expect_equal_tables(pathTab,
                                 list(0.399458455723794, 0.660374769328702, 0.54619791924422, "dem60",
                                      8.65809334204698e-14, "ind60", 0.0741307485300629, 7.36803458854479
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
