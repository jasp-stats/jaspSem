context("Mediation Analysis")

test_that("Simple mediation analysis works", {
  options <- jaspTools::analysisOptions("MediationAnalysis")
  options$predictors             <- "contcor1"
  options$mediators              <- "contcor2"
  options$outcomes               <- "contNormal"
  options$emulation              <- "lavaan"
  options$estimator              <- "ml"
  options$errorCalculationMethod <- "standard"
  options$naAction               <- "fiml"
  options$errorCalculationMethod <- "standard"
  options$ciLevel                <- 0.95
  results <- jaspTools::runAnalysis("MediationAnalysis","test.csv", options)

  dir_tab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_dir"]][["data"]]
  ind_tab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_ind"]][["data"]]
  tot_tab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_tot"]][["data"]]
  path_tab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_path"]][["data"]]

  expect_equal_tables(dir_tab, list(-0.0278608662153102, 0.543376554333683, 0.257757844059186, "contcor1",
                                    "<unicode>", 0.0769309295532143, "contNormal", 0.145726509531512,
                                    1.76877799988374))

  expect_equal_tables(ind_tab, list(-0.314706292009771, 0.136079083621019, -0.089313604194376, "contcor2",
                                    "<unicode>", "<unicode>", 0.437364726618501, 0.114998382415832,
                                    "contcor1", "contNormal", -0.776650960805862))
  expect_equal_tables(tot_tab, list(-0.0247650483164017, 0.361653528046022, 0.16844423986481, "contcor1",
                                    "<unicode>", 0.0874989250907576, "contNormal", 0.0985779788328879,
                                    1.70874105818665))
  expect_equal_tables(path_tab, list(-0.484366116379622, 0.21040523977548, -0.136980438302071, "contcor2",
                                     "<unicode>", 0.439611662237764, "contNormal", 0.177240847698062,
                                     -0.772849148946878, -0.0278608662153102, 0.543376554333683,
                                     0.257757844059186, "contcor1", "<unicode>", 0.0769309295532143,
                                     "contNormal", 0.145726509531512, 1.76877799988374, 0.484316826648432,
                                     0.819717607778195, 0.652017217213314, "contcor1", "<unicode>",
                                     2.53130849614536e-14, "contcor2", 0.085562995997723, 7.62031775080275
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
  options$naAction       <- "fiml"
  options$ciLevel        <- 0.95
  results <- jaspTools::runAnalysis("MediationAnalysis","test.csv", options)

  ind_tab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_ind"]][["data"]]

  expect_equal_tables(ind_tab, list(-0.293436900654361, 0.173052185553216, -0.0601923575505725, "contcor2",
                                    "<unicode>", "<unicode>", 0.612997771786017, 0.119004504645795,
                                    "contcor1", "contNormal", -0.505798984078199))
})

test_that("Multiple mediation with missing values works", {
  options <- jaspTools::analysisOptions("MediationAnalysis")
  options$predictors     <- c("contcor1", "contOutlier")
  options$mediators      <- c("contcor2", "debMiss1")
  options$outcomes       <- c("contNormal", "debMiss30")
  options$emulation      <- "lavaan"
  options$estimator      <- "ml"
  options$errorCalculationMethod <- "standard"
  options$naAction       <- "fiml"
  options$ciLevel        <- 0.95
  results <- jaspTools::runAnalysis("MediationAnalysis","test.csv", options)

  dir_tab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_dir"]][["data"]]
  expect_equal_tables(dir_tab, list(-0.0117587266478023, 0.638996461092566, 0.313618867222382, "contcor1",
                                    "<unicode>", 0.0588739541815493, "contNormal", 0.166012026974333,
                                    1.88913341363437, -0.123193418916241, 0.145156401243716, 0.0109814911637374,
                                    "contOutlier", "<unicode>", 0.872556176342391, "contNormal",
                                    0.0684578447044603, 0.160412458369756, -15.8407518266481, 6.88476061567945,
                                    -4.4779956054843, "contcor1", "<unicode>", 0.439871446586373,
                                    "debMiss30", 5.79743113179208, -0.772410314790593, -5.50776200256081,
                                    5.3002803034334, -0.103740849563705, "contOutlier", "<unicode>",
                                    0.969986374962637, "debMiss30", 2.75720431376461, -0.0376253761992921
  ))

  ind_tab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_ind"]][["data"]]
  expect_equal_tables(ind_tab, list(-0.359802695893585, 0.145119681667274, -0.107341507113156, "contcor2",
                                    "<unicode>", "<unicode>", 0.404654162351338, 0.128809095866971,
                                    "contcor1", "contNormal", -0.833337943960215, -0.103392515938771,
                                    0.0335753722606991, -0.0349085718390361, "debMiss1", "<unicode>",
                                    "<unicode>", 0.317765807957592, 0.0349414298629607, "contcor1",
                                    "contNormal", -0.999059625663476, -0.0332135918201788, 0.0157192415163538,
                                    -0.00874717515191253, "contcor2", "<unicode>", "<unicode>",
                                    0.483476731815384, 0.0124830950268751, "contOutlier", "contNormal",
                                    -0.700721666628392, -0.0336428977647662, 0.0490092912468505,
                                    0.00768319674104214, "debMiss1", "<unicode>", "<unicode>", 0.715567283785486,
                                    0.0210851295390034, "contOutlier", "contNormal", 0.36438935444194,
                                    -3.94548654204477, 8.39208706795777, 2.2233002629565, "contcor2",
                                    "<unicode>", "<unicode>", 0.479943709862794, 3.14739804081089,
                                    "contcor1", "debMiss30", 0.706393101262684, -1.18973653986432,
                                    0.827529906697092, -0.181103316583616, "debMiss1", "<unicode>",
                                    "<unicode>", 0.724899914763684, 0.514618243619106, "contcor1",
                                    "debMiss30", -0.351917793100354, -0.44989140912209, 0.812241395411601,
                                    0.181174993144755, "contcor2", "<unicode>", "<unicode>", 0.573644152011331,
                                    0.321978570649572, "contOutlier", "debMiss30", 0.562692705850723,
                                    -0.235348881342391, 0.315068694469274, 0.0398599065634415, "debMiss1",
                                    "<unicode>", "<unicode>", 0.776508719280326, 0.140415227053479,
                                    "contOutlier", "debMiss30", 0.283871681155068))

  tot_tab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_tot"]][["data"]]
  expect_equal_tables(tot_tab, list(-0.0523585499679635, 0.395096126508343, 0.17136878827019, "contcor1",
                                    "<unicode>", 0.133283993797962, "contNormal", 0.11414869865104,
                                    1.50127675825789, -0.1203541437202, 0.140189169225934, 0.00991751275286704,
                                    "contOutlier", "<unicode>", 0.881387118139766, "contNormal",
                                    0.0664663521884244, 0.14921102823203, -9.6328427113215, 4.76124539309866,
                                    -2.43579865911142, "contcor1", "<unicode>", 0.50711367399683,
                                    "debMiss30", 3.67202872551713, -0.663338672212699, -5.33671570335794,
                                    5.57130380364692, 0.117294050144492, "contOutlier", "<unicode>",
                                    0.966378302405106, "debMiss30", 2.78270917043525, 0.042151027276108
  ))

  tti_tab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_tti"]][["data"]]
  expect_equal_tables(tti_tab, list(-0.398084962465341, 0.113584804560957, -0.142250078952192, "contcor1",
                                    "<unicode>", 0.275807840576713, "contNormal", 0.13053040031916,
                                    -1.08978505087226, -0.0466649809541657, 0.044537024132425, -0.00106397841087039,
                                    "contOutlier", "<unicode>", 0.963525006225839, "contNormal",
                                    0.0232662451468447, -0.0457305596221092, -4.23452955699309,
                                    8.31892344973886, 2.04219694637288, "contcor1", "<unicode>",
                                    0.523672722610984, "debMiss30", 3.20247032745295, 0.637694260229765,
                                    -0.498930099066363, 0.940999898482757, 0.221034899708197, "contOutlier",
                                    "<unicode>", 0.547357664694887, "debMiss30", 0.367335830889523,
                                    0.601724310892704))
  path_tab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_path"]][["data"]]
  expect_equal_tables(path_tab, list(-0.539629545708885, 0.218281994951107, -0.160673775378889, "contcor2",
                                     "<unicode>", 0.405969801392503, "contNormal", 0.193348333601613,
                                     -0.831006776142958, 0.000636054840437975, 0.0197043723593629,
                                     0.0101702135999004, "debMiss1", "<unicode>", 0.0365532089951226,
                                     "contNormal", 0.00486445609953381, 2.090719577236, -0.0117587266478023,
                                     0.638996461092566, 0.313618867222382, "contcor1", "<unicode>",
                                     0.0588739541815493, "contNormal", 0.166012026974333, 1.88913341363437,
                                     -0.123193418916241, 0.145156401243716, 0.0109814911637374, "contOutlier",
                                     "<unicode>", 0.872556176342391, "contNormal", 0.0684578447044603,
                                     0.160412458369756, -5.61629693717515, 12.2721760403728, 3.32793955159883,
                                     "contcor2", "<unicode>", 0.465844839677631, "debMiss30", 4.563469818489,
                                     0.7292563956741, -0.206976037246958, 0.31250080186916, 0.0527623823111013,
                                     "debMiss1", "<unicode>", 0.690526665152527, "debMiss30", 0.132522036938863,
                                     0.398140441619096, -15.8407518266481, 6.88476061567945, -4.4779956054843,
                                     "contcor1", "<unicode>", 0.439871446586373, "debMiss30", 5.79743113179208,
                                     -0.772410314790593, -5.50776200256081, 5.3002803034334, -0.103740849563705,
                                     "contOutlier", "<unicode>", 0.969986374962637, "debMiss30",
                                     2.75720431376461, -0.0376253761992921, 0.47789449715066, 0.858247718466429,
                                     0.668071107808544, "contcor1", "<unicode>", 5.77249359423604e-12,
                                     "contcor2", 0.0970306659499731, 6.88515430939108, -0.0635060561561872,
                                     0.172387236439473, 0.0544405901416431, "contOutlier", "<unicode>",
                                     0.365645614903794, "contcor2", 0.0601779661402854, 0.904659855315358,
                                     -9.3720658801523, 2.50720085128449, -3.4324325144339, "contcor1",
                                     "<unicode>", 0.257367035876432, "debMiss1", 3.0304808723882,
                                     -1.13263625773323, -3.24455782017684, 4.75547923113272, 0.755460705477943,
                                     "contOutlier", "<unicode>", 0.71125789232424, "debMiss1", 2.04086327973698,
                                     0.370167229220422))
})


test_that("Bootstrapping works", {
  options                  <- jaspTools::analysisOptions("MediationAnalysis")
  options$predictors       <- "contcor1"
  options$mediators        <- "contcor2"
  options$outcomes         <- "contNormal"
  options$emulation        <- "lavaan"
  options$estimator        <- "ml"
  options$errorCalculationMethod   <- "bootstrap"
  options$bootstrapSamples <- 100
  options$bootstrapCiType  <- "percentileBiasCorrected"
  options$naAction         <- "fiml"
  options$ciLevel          <- 0.95

  set.seed(1)
  results <- jaspTools::runAnalysis("MediationAnalysis", "test.csv", options)

  # Direct effects table results match
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_dir"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0761400441341873, 0.671443275931252, 0.257757844059186, "contcor1",
                                      "<unicode>", 0.0769309295532143, "contNormal", 0.145726509531512,
                                      1.76877799988374),
                                 label = "Direct effects table results match")

  # Indirect effects table results match
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_ind"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.299720825954289, 0.0732936760821536, -0.089313604194376, "contcor2",
                                      "<unicode>", "<unicode>", 0.437364726618501, 0.114998382415832,
                                      "contcor1", "contNormal", -0.776650960805862),
                                 label = "Indirect effects table results match")

  # Total effects table results match
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_tot"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.00126714302931764, 0.440665080716848, 0.16844423986481, "contcor1",
                                      "<unicode>", 0.0874989250907576, "contNormal", 0.0985779788328879,
                                      1.70874105818665),
                                 label = "Total effects table results match")
  # Path coefficients table results match
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_path"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.475862353202891, 0.110585516049068, -0.136980438302071, "contcor2",
                                      "<unicode>", 0.439611662237764, "contNormal", 0.177240847698062,
                                      -0.772849148946878, 0.0761400441341873, 0.671443275931252, 0.257757844059186,
                                      "contcor1", "<unicode>", 0.0769309295532143, "contNormal", 0.145726509531512,
                                      1.76877799988374, 0.48165177793888, 0.754639896102569, 0.652017217213314,
                                      "contcor1", "<unicode>", 2.53130849614536e-14, "contcor2", 0.085562995997723,
                                      7.62031775080275),
                                 label = "Path coefficients table results match")
})
