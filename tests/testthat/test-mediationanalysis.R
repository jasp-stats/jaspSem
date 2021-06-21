context("Mediation Analysis")

test_that("Simple mediation analysis works", {
  options <- jaspTools::analysisOptions("MediationAnalysis")
  options$predictor <- "contcor1"
  options$mediators <- "contcor2"
  options$dependent <- "contNormal"
  options$mimic     <- "lavaan"
  options$estimator <- "ML"
  options$se        <- "standard"
  options$missing   <- "FIML"
  results <- jaspTools::runAnalysis("MediationAnalysis","test.csv", options)

  dir_tab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_dir"]][["data"]]
  ind_tab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_ind"]][["data"]]
  tot_tab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_tot"]][["data"]]

  expect_equal_tables(dir_tab, list(
    -0.00931725107194831, 0.524832903921214, 0.257757826424633, "contcor1",
    "<unicode><unicode><unicode>", 0.0585458735974023, "contNormal",
    0.136265298547951, 1.89158816787041
  ))

  expect_equal_tables(ind_tab, list(
    -0.265930503999881, 0.0873033053757795, -0.0893135993120506, "contcor2",
    "<unicode><unicode><unicode>", "<unicode><unicode><unicode>",
    0.321618995211592, 0.0901123214921099, "contcor1", "contNormal",
    -0.991136371066311
  ))
  expect_equal_tables(tot_tab, list(
    -0.0338982391107447, 0.37078669333591, 0.168444227112582, "contcor1",
    "<unicode><unicode><unicode>", 0.10276101683937, "contNormal",
    0.103237849174464, 1.63161309984214
  ))
})

test_that("Categorical confounders work", {
  options <- jaspTools::analysisOptions("MediationAnalysis")
  options$predictor <- "contcor1"
  options$mediators <- "contcor2"
  options$dependent <- "contNormal"
  options$confounds <- c("facGender", "facExperim")
  options$mimic     <- "lavaan"
  options$estimator <- "ML"
  options$se        <- "standard"
  options$missing   <- "FIML"
  results <- jaspTools::runAnalysis("MediationAnalysis","test.csv", options)

  ind_tab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_ind"]][["data"]]

  expect_equal_tables(ind_tab, list(
    -0.231781905561682, 0.111397204712701, -0.0601923504244907, "contcor2",
    "<unicode><unicode><unicode>", "<unicode><unicode><unicode>",
    0.491741929653112, 0.0875473000987102, "contcor1", "contNormal",
    -0.68754091053206
  ))
})

test_that("Multiple mediation with missing values works", {
  options <- jaspTools::analysisOptions("MediationAnalysis")
  options$predictor <- c("contcor1", "contOutlier")
  options$mediators <- c("contcor2", "debMiss1")
  options$dependent <- c("contNormal", "debMiss30")
  options$mimic     <- "lavaan"
  options$estimator <- "ML"
  options$se        <- "standard"
  options$missing   <- "FIML"
  results <- jaspTools::runAnalysis("MediationAnalysis","test.csv", options)

  dir_tab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_dir"]][["data"]]
  expect_equal_tables(dir_tab, list(0.0479536537219549, 0.579284062996402, 0.313618858359178, "contcor1",
                                    "<unicode>", 0.020681687391539, "contNormal", 0.135545962442553,
                                    2.31374548313894, -0.0588294146087172, 0.0807923963371572, 0.01098149086422,
                                    "contOutlier", "<unicode>", 0.757847260504974, "contNormal",
                                    0.0356184634123875, 0.308308944635741, -12.5813605100068, 3.62536845765606,
                                    -4.47799602617535, "contcor1", "<unicode>", 0.278766401651694,
                                    "debMiss30", 4.13444560601609, -1.08309467650496, -1.87453275467391,
                                    1.66705082621899, -0.103740964227459, "contOutlier", "<unicode>",
                                    0.908585002939855, "debMiss30", 0.903481800897482, -0.114823523976251)
                      )

  ind_tab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_ind"]][["data"]]
  expect_equal_tables(ind_tab, list(-0.286287438137467, 0.0716044322824627, -0.107341502927502, "contcor2",
                                    "<unicode>", "<unicode>", 0.239717584949295, 0.0913006242060912,
                                    "contcor1", "contNormal", -1.17569297976761, -0.0934068738657553,
                                    0.0235897422988643, -0.0349085657834455, "debMiss1", "<unicode>",
                                    "<unicode>", 0.242162592955928, 0.0298466239909187, "contcor1",
                                    "contNormal", -1.16959847097169, -0.0253089791564093, 0.00781462953868545,
                                    -0.00874717480886192, "contcor2", "<unicode>", "<unicode>",
                                    0.300593344191838, 0.00845005544907191, "contOutlier", "contNormal",
                                    -1.03516182368042, -0.0113560645466611, 0.0267224578961894,
                                    0.00768319667476416, "debMiss1", "<unicode>", "<unicode>", 0.428982821207132,
                                    0.00971408728507489, "contOutlier", "contNormal", 0.790933460786268,
                                    -3.22066204646898, 7.66726356288259, 2.22330075820681, "contcor2",
                                    "<unicode>", "<unicode>", 0.423453378696818, 2.77758308194287,
                                    "contcor1", "debMiss30", 0.800444376501476, -0.950829436560235,
                                    0.588622855547585, -0.181103290506325, "debMiss1", "<unicode>",
                                    "<unicode>", 0.644694053348693, 0.392724637863457, "contcor1",
                                    "debMiss30", -0.461145731756436, -0.291320700123487, 0.653670767046716,
                                    0.181175033461615, "contcor2", "<unicode>", "<unicode>", 0.452331433520963,
                                    0.241073681614605, "contOutlier", "debMiss30", 0.751533855741466,
                                    -0.143920756397419, 0.223640571186654, 0.0398599073946173, "debMiss1",
                                    "<unicode>", "<unicode>", 0.670768447428733, 0.0937673677892425,
                                    "contOutlier", "debMiss30", 0.425093594225754)
                      )

  tot_tab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_tot"]][["data"]]
  expect_equal_tables(tot_tab, list(-0.0319745939596935, 0.374712173256155, 0.171368789648231, "contcor1",
                                    "<unicode>", 0.0985812910530304, "contNormal", 0.103748530693355,
                                    1.65177076246735, -0.0609583635584311, 0.0807933890186755, 0.00991751273012219,
                                    "contOutlier", "<unicode>", 0.783889701281208, "contNormal",
                                    0.0361618258537469, 0.27425364997422, -8.17038021626784, 3.2987830993181,
                                    -2.43579855847487, "contcor1", "<unicode>", 0.405123017980409,
                                    "debMiss30", 2.92586073163926, -0.83250666449533, -1.59524858395851,
                                    1.82983653721605, 0.117293976628773, "contOutlier", "<unicode>",
                                    0.893212678301443, "debMiss30", 0.873762259967836, 0.134240149755485)
                      )

  tti_tab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_tti"]][["data"]]
  expect_equal_tables(tti_tab, list(-0.33094680692172, 0.0464466694998251, -0.142250068710948, "contcor1",
                                    "<unicode>", 0.139533736448037, "contNormal", 0.0962756151129247,
                                    -1.47752957531456, -0.0260621078056755, 0.0239341515374799,
                                    -0.00106397813409776, "contOutlier", "<unicode>", 0.933517114031927,
                                    "contNormal", 0.0127543821563864, -0.0834205938830991, -3.48111908446527,
                                    7.56551401986623, 2.04219746770048, "contcor1", "<unicode>",
                                    0.468648762264395, "debMiss30", 2.81807043176965, 0.724679356724968,
                                    -0.281938371807994, 0.724008253520458, 0.221034940856232, "contOutlier",
                                    "<unicode>", 0.389062316295845, "debMiss30", 0.25662375259526,
                                    0.861319104801817)
                      )
})


test_that("Bootstrapping works", {
  options                 <- jaspTools::analysisOptions("MediationAnalysis")
  options$predictor       <- "contcor1"
  options$mediators       <- "contcor2"
  options$dependent       <- "contNormal"
  options$mimic           <- "lavaan"
  options$estimator       <- "ML"
  options$se              <- "bootstrap"
  options$bootstrapNumber <- 100
  options$bootCItype      <- "bca.simple"
  options$missing         <- "FIML"

  set.seed(1)
  results <- jaspTools::runAnalysis("MediationAnalysis", "test.csv", options)

  # Direct effects table results match
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_dir"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0425663855373132, 0.792177915779829, 0.257757857221231, "contcor1",
                                      "<unicode>", 0.0585458466298006, "contNormal", 0.136265300259528,
                                      1.89158837011558))

  # Indirect effects table results match
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_ind"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.28761880151593, 0.07473726135656, -0.0893136271813779, "contcor2",
                                      "<unicode>", "<unicode>", 0.32161886441617, 0.0901123252539836,
                                      "contcor1", "contNormal", -0.991136638963043))

  # Total effects table results match
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_tot"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.0503046496398278, 0.527735697854857, 0.168444230039853, "contcor1",
                                      "<unicode>", 0.102761018524938, "contNormal", 0.103237851474511,
                                      1.63161309184588))
})
