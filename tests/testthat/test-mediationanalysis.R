context("Mediation Analysis")

test_that("Simple mediation analysis works", {
  options <- jaspTools::analysisOptions("MediationAnalysis")
  options$predictors             <- "contcor1"
  options$mediators              <- "contcor2"
  options$outcomes               <- "contNormal"
  options$emulation              <- "lavaan"
  options$estimator              <- "ml"
  options$errorCalculationMethod <- "standard"
  options$ciLevel                <- 0.95
  options$naAction               <- "fiml"
  results <- jaspTools::runAnalysis("MediationAnalysis", "test.csv", options)

  dir_tab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_dir"]][["data"]]
  ind_tab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_ind"]][["data"]]
  tot_tab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_tot"]][["data"]]
  path_tab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_path"]][["data"]]

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
  expect_equal_tables(path_tab, list(
    -0.406100650114518, 0.132139773510376, -0.136980438302071, "contcor2",
    "<unicode><unicode><unicode>", 0.318469030743472, "contNormal", 0.137308753597124,
    -0.997608926696571, -0.00931723177310873, 0.524832919891481,
    0.257757844059186, "contcor1", "<unicode><unicode><unicode>", 0.0585458547695155,
    "contNormal", 0.136265297698809, 1.89158830907129, 0.505381961413432,
    0.798652473013195, 0.652017217213314, "contcor1", "<unicode><unicode><unicode>",
    0, "contcor2", 0.0748152807686884, 8.71502733818777
  ))
})

test_that("Categorical confounders work", {
  options <- jaspTools::analysisOptions("MediationAnalysis")
  options$predictors     <- "contcor1"
  options$mediators      <- "contcor2"
  options$outcomes       <- "contNormal"
  options$confounds      <- c("facGender", "facExperim")
  options$emulation      <- "lavaan"
  options$estimator      <- "ml"
  options$errorCalculationMethod <- "standard"
  options$ciLevel                <- 0.95
  options$naAction       <- "fiml"
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
  options$predictors     <- c("contcor1", "contOutlier")
  options$mediators      <- c("contcor2", "debMiss1")
  options$outcomes       <- c("contNormal", "debMiss30")
  options$emulation      <- "lavaan"
  options$estimator      <- "ml"
  options$errorCalculationMethod <- "standard"
  options$ciLevel                <- 0.95
  options$naAction       <- "fiml"
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
  path_tab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_path"]][["data"]]
  expect_equal_tables(path_tab, list(-0.426273684109866, 0.104926133352088, -0.160673775378889, "contcor2",
                                  "<unicode>", 0.235751204710817, "contNormal", 0.135512647592504,
                                  -1.18567364916407, 0.00268153242569464, 0.0176588947741062,
                                  0.0101702135999004, "debMiss1", "<unicode>", 0.00777277196528736,
                                  "contNormal", 0.00382082590969811, 2.66178408549998, 0.0479536588500194,
                                  0.579284075594744, 0.313618867222382, "contcor1", "<unicode>",
                                  0.0206816855880658, "contNormal", 0.135545964348272, 2.31374551599758,
                                  -0.0588294152956745, 0.0807923976231493, 0.0109814911637374,
                                  "contOutlier", "<unicode>", 0.757847257421674, "contNormal",
                                  0.0356184639157002, 0.308308948688181, -4.78911489181894, 11.4449939950166,
                                  3.32793955159883, "contcor2", "<unicode>", 0.421643915240879,
                                  "debMiss30", 4.1414304076218, 0.803572491638193, -0.156467976004744,
                                  0.261992740626946, 0.0527623823111013, "debMiss1", "<unicode>",
                                  0.621128739432468, "debMiss30", 0.106752144409911, 0.494251264016787,
                                  -12.5813600726521, 3.62536886168353, -4.4779956054843, "contcor1",
                                  "<unicode>", 0.278766445822989, "debMiss30", 4.13444559751411,
                                  -1.0830945769795, -1.87453263568675, 1.66705093655934, -0.103740849563705,
                                  "contOutlier", "<unicode>", 0.90858510331437, "debMiss30", 0.90348179869162,
                                  -0.114823397343408, 0.523868679737975, 0.812273535879114, 0.668071107808544,
                                  "contcor1", "<unicode>", 0, "contcor2", 0.073574019322814, 9.08025841129203,
                                  0.00417845094299354, 0.104702729340293, 0.0544405901416431,
                                  "contOutlier", "<unicode>", 0.0337620645933085, "contcor2",
                                  0.0256444197929712, 2.12290200289751, -8.58710035891241, 1.72223533004461,
                                  -3.4324325144339, "contcor1", "<unicode>", 0.191853045593741,
                                  "debMiss1", 2.62998090022974, -1.30511689804822, -1.03358284479171,
                                  2.54450425574759, 0.755460705477943, "contOutlier", "<unicode>",
                                  0.407877013901804, "debMiss1", 0.912794094371833, 0.82763540007107)
                      )
})


# bootstrapped mediation analysis works
options                  <- jaspTools::analysisOptions("MediationAnalysis")
options$predictors       <- "contcor1"
options$mediators        <- "contcor2"
options$outcomes         <- "contNormal"
options$emulation        <- "lavaan"
options$estimator        <- "ml"
options$errorCalculationMethod   <- "bootstrap"
options$ciLevel                <- 0.95
options$bootstrapSamples <- 100
options$bootstrapCiType  <- "percentileBiasCorrected"
options$naAction         <- "fiml"

set.seed(1)
results <- jaspTools::runAnalysis("MediationAnalysis", "test.csv", options, makeTests = F)

test_that("Direct effects table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_dir"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0761400418471621, 0.67144328199062, 0.257757843176866, "contcor1",
                                      "<unicode>", 0.0602005168476014, "contNormal", 0.137154686851546,
                                      1.87932216604351))
})

test_that("Indirect effects table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_ind"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.299720820153127, 0.0732936772629952, -0.0893136104463748, "contcor2",
                                      "<unicode>", "<unicode>", 0.321618931631225, 0.0901123208860908,
                                      "contcor1", "contNormal", -0.991136501292364))
})

test_that("Path coefficients table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_path"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.475862350312371, 0.11058551725881, -0.136980447123533, "contcor2",
                                      "<unicode>", 0.309092736100577, "contNormal", 0.1346738091268,
                                      -1.01712759156134, 0.0761400418471621, 0.67144328199062, 0.257757843176866,
                                      "contcor1", "<unicode>", 0.0602005168476014, "contNormal", 0.137154686851546,
                                      1.87932216604351, 0.481651772564286, 0.75463988878285, 0.652017220865318,
                                      "contcor1", "<unicode>", 0, "contcor2", 0.0654581731453709,
                                      9.960822148478))
})

test_that("Total effects table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_tot"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.00126713143815475, 0.440665080659029, 0.168444232730491, "contcor1",
                                      "<unicode>", 0.102761008340492, "contNormal", 0.103237850066545,
                                      1.63161314016047))
})


# bootstrapped mediation analysis works with standardized CI
options                  <- jaspTools::analysisOptions("MediationAnalysis")
options$predictors       <- "contcor1"
options$mediators        <- "contcor2"
options$outcomes         <- "contNormal"
options$standardizedEstimate <- TRUE
options$standardizedEstimateType <- "all"
options$emulation        <- "lavaan"
options$estimator        <- "ml"
options$errorCalculationMethod   <- "bootstrap"
options$bootstrapSamples <- 100
options$bootstrapCiType  <- "percentile"
options$ciLevel                <- 0.95
options$naAction         <- "fiml"

set.seed(1)
results <- jaspTools::runAnalysis("MediationAnalysis", "test.csv", options, makeTests = F)

test_that("Direct effects table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_dir"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.00203609861592261, 0.515217972956754, 0.246415337277754, "contcor1",
                                      "<unicode>", 0.0465192566969286, "contNormal", 0.123786300663823,
                                      1.99065111370413))
})

test_that("Indirect effects table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_ind"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.285927353547018, 0.0726795446582816, -0.0853834093674345, "contcor2",
                                      "<unicode>", "<unicode>", 0.325721157816279, 0.0868804491259142,
                                      "contcor1", "contNormal", -0.982768968467117))
})

test_that("Path coefficients table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_path"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.42303242765298, 0.125618203587476, -0.129957536548202, "contcor2",
                                      "<unicode>", 0.302069524845541, "contNormal", 0.125927170167018,
                                      -1.0320055344358, 0.00203609861592261, 0.515217972956754, 0.246415337277754,
                                      "contcor1", "<unicode>", 0.0465192566969286, "contNormal", 0.123786300663823,
                                      1.99065111370413, 0.514802945031706, 0.794776935427497, 0.657010063712354,
                                      "contcor1", "<unicode>", 0, "contcor2", 0.0627045055841268,
                                      10.4778764714265))
})

test_that("Total effects table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_tot"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.0364756940559679, 0.347861392970991, 0.161031927910319, "contcor1",
                                      "<unicode>", 0.100547140107636, "contNormal", 0.0980582478184411,
                                      1.6422068667643))
})
