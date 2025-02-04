context("PLS-SEM")



options <- jaspTools::analysisOptions("PLSSEM")
model <- "
  ind60 =~ x1 + x2 + x3
  dem60 =~ y1 + y2 + y3 + y4
  dem65 =~ y5 + y6 + y7 + y8
  dem60 ~ ind60
  dem65 ~ ind60 + dem60
"
options$models <- list(list(name = "Model1", syntax = list(model = model, columns = c("x1", "x2", "x3", "y1", "y2", "y3", "y4", "y5", "y6", "y7", "y8"))))
options$group                     <- ""
options$innerWeightingScheme      <- "path"
options$convergenceCriterion      <- "absoluteDifference"
options$overallModelFit <- TRUE
options$setSeed <- TRUE
options$seed <- 123
results <- jaspTools::runAnalysis("PLSSEM", testthat::test_path("poldem_grouped.csv"), options, makeTests = F)

test_that("Model Fit table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_fittab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.43026588019435, "dG", 0.295547730720455, 0.0560152218904532,
                                      "SRMR", 0.0529946519222452, 0.207096082194422, "dL", 0.185356586735755,
                                      1.97158372315007, "dML", 1.67522948035842))
})


test_that("Loadings table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_loading"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.991731547942878, "ind60", "x1", 0.961800526264931, "ind60",
                                      "x2", 0.80714480227883, "ind60", "x3", 0.847758580984988, "dem60",
                                      "y1", 0.727216064394063, "dem60", "y2", 0.694838538459375, "dem60",
                                      "y3", 0.897449246669676, "dem60", "y4", 0.83212789961779, "dem65",
                                      "y5", 0.771144494069751, "dem65", "y6", 0.817471968976973, "dem65",
                                      "y7", 0.822630667143518, "dem65", "y8"))
})


test_that("Total effects table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_total"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.438843974833827, "dem60", "ind60", 0.557409023483468, "dem65",
                                      "ind60", 0.908670674402449, "dem65", "dem60"))
})

test_that("Weights table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_weight"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("", 0.378464081729233, "ind60", "x1", "", 0.367041820676775, "ind60",
                                      "x2", "", 0.308022183070227, "ind60", "x3", "", 0.314031776023085,
                                      "dem60", "y1", "", 0.269379700042493, "dem60", "y2", "", 0.257386224304754,
                                      "dem60", "y3", "", 0.3324384879653, "dem60", "y4", "", 0.297524158061304,
                                      "dem65", "y5", "", 0.275719773903862, "dem65", "y6", "", 0.292283985935701,
                                      "dem65", "y7", "", 0.294128458797863, "dem65", "y8"))
})


# Multigroup PLSSEM works

options <- jaspTools::analysisOptions("PLSSEM")
model1 = "
  ind60 =~ x1 + x2 + x3
  dem60 =~ y1 + y2 + y3 + y4
  dem65 =~ y5 + y6 + y7 + y8
  dem60 ~ ind60
  dem65 ~ dem60
"
options$models <- list(list(name = "Model1", syntax = list(model = model1, columns = c("x1", "x2", "x3", "y1", "y2", "y3", "y4", "y5", "y6", "y7", "y8"))))
options$group                         <- "group"
options$innerWeightingScheme          <- "path"
options$convergenceCriterion          <- "absoluteDifference"
options$additionalFitMeasures         <- TRUE
options$rSquared                      <- TRUE
options$mardiasCoefficient            <- TRUE
options$reliabilityMeasures           <- TRUE
options$impliedConstructCorrelation   <- TRUE
options$impliedIndicatorCorrelation   <- TRUE
options$observedConstructCorrelation  <- TRUE
options$observedIndicatorCorrelation  <- TRUE
options$innerWeightingScheme          <- "centroid"
options$structuralModelIgnored        <- TRUE
options$handlingOfFlippedSigns        <- "none"

results <- jaspTools::runAnalysis("PLSSEM", testthat::test_path("poldem_grouped.csv"), options, makeTest = F)

test_that("Additional Fit Measures table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_addfit"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Comparative Fit Index (CFI)", 0.87051988624414, 0.682513115515102,
                                      "Goodness of fit index (GFI)", 0.496752300477218, 0.419309726581194,
                                      "Hoelter's critical N (CN)", 24.2921896537681, 16.4077818076546,
                                      "Bollen's Incremental Fit Index (IFI)", 0.874921175926589, 0.695397113795037,
                                      "Bentler-Bonett Non-normed Fit Index (NNFI)", 0.83044270817685,
                                      0.584243365555491, "Bentler-Bonett Normed Fit Index (NFI)",
                                      0.788344867447871, 0.614792587542818, "Root mean square error of approximation (RMSEA)",
                                      0.177868578652954, 0.250582502116038, "Root mean square residual covariance (RMS theta)",
                                      0.0766251796879127, 0.0795989971054803, "Standardized root mean square residual (SRMR)",
                                      0.0943761594914448, 0.0897114551867814, "Goodness of Fit (GoF)",
                                      0.651821788678243, 0.614476373672947, "Geodesic distance", 0.495814387492708,
                                      0.623860456924762, "Squared Euclidean distance", 0.587852725703405,
                                      0.531177582654172, "Maximum likelihood-based distance", 2.49543038009159,
                                      3.77238193053797))
})

test_that("1 table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_cors"]][["collection"]][["modelContainer_cors_impliedCon"]][["collection"]][["modelContainer_cors_impliedCon_1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("", "", 1, "ind60", 1, "", 0.521623484887408, "dem60", 0.94632437373424,
                                      1, 0.493625017661149, "dem65"))
})

test_that("2 table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_cors"]][["collection"]][["modelContainer_cors_impliedCon"]][["collection"]][["modelContainer_cors_impliedCon_2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("", "", 1, "ind60", 1, "", 0.319599163621636, "dem60", 0.996650485797297,
                                      1, 0.318528661683913, "dem65"))
})

test_that("1 table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_cors"]][["collection"]][["modelContainer_cors_impliedInd"]][["collection"]][["modelContainer_cors_impliedInd_1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("x1", 1, "", "", "", "", "", "", "", "", "", "", "x2", 0.96747843048152,
                                      1, "", "", "", "", "", "", "", "", "", "x3", 0.795398510820737,
                                      0.825044526322995, 1, "", "", "", "", "", "", "", "", "y1",
                                      0.414839952430787, 0.430301826564834, 0.353766472998025, 1,
                                      "", "", "", "", "", "", "", "y2", 0.415688469462732, 0.431181969440606,
                                      0.35449006983558, 0.679493964784794, 1, "", "", "", "", "",
                                      "", "y3", 0.310189790704042, 0.321751154245401, 0.264523095170475,
                                      0.507043389954196, 0.508080500651443, 1, "", "", "", "", "",
                                      "y4", 0.487291119411965, 0.50545338635573, 0.415551249650652,
                                      0.796537308724513, 0.798166552651171, 0.595597747115355, 1,
                                      "", "", "", "", "y5", 0.409464313554319, 0.424725827401921,
                                      0.349182245287324, 0.669319815905676, 0.670688848131929, 0.50047294720122,
                                      0.786215504138802, 1, "", "", "", "y6", 0.410128144463717, 0.425414400552049,
                                      0.349748345823468, 0.670404929229018, 0.671776180954938, 0.501284322944287,
                                      0.78749013085429, 0.738911844597158, 1, "", "", "y7", 0.390751828554134,
                                      0.405315892490962, 0.333224645782372, 0.638731956107242, 0.640038423674865,
                                      0.477601375229029, 0.750285521131483, 0.704002293707963, 0.705143634888086,
                                      1, "", "y8", 0.359080400446972, 0.372464010012978, 0.306215941942183,
                                      0.586961108860151, 0.588161683913348, 0.438890570789755, 0.68947297412361,
                                      0.646941119829515, 0.647989952408294, 0.617375964576475, 1
                                 ))
})

test_that("2 table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_cors"]][["collection"]][["modelContainer_cors_impliedInd"]][["collection"]][["modelContainer_cors_impliedInd_2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("x1", 1, "", "", "", "", "", "", "", "", "", "", "x2", 0.934303505690295,
                                      1, "", "", "", "", "", "", "", "", "", "x3", 0.790093644203597,
                                      0.658811598406884, 1, "", "", "", "", "", "", "", "", "y1",
                                      0.274137234138764, 0.228586561518095, 0.19330419751809, 1, "",
                                      "", "", "", "", "", "", "y2", 0.217865596146897, 0.181665025011161,
                                      0.153625005965657, 0.521843289400442, 1, "", "", "", "", "",
                                      "", "y3", 0.276356957074584, 0.230437455049635, 0.194869405404459,
                                      0.661944915025775, 0.526068718762583, 1, "", "", "", "", "",
                                      "y4", 0.292904521540612, 0.244235474405422, 0.206537698768669,
                                      0.701580523516689, 0.557568399933754, 0.707261307392206, 1,
                                      "", "", "", "", "y5", 0.281697230698017, 0.234890388227361,
                                      0.198635027796279, 0.674736223076412, 0.536234378902497, 0.680199645346271,
                                      0.720928301502733, 1, "", "", "", "y6", 0.236839922943984, 0.197486575605311,
                                      0.167004498271691, 0.567291608387322, 0.450844719575288, 0.571885038383767,
                                      0.606128086360512, 0.586860858515856, 1, "", "", "y7", 0.265705365927846,
                                      0.221555733445569, 0.187358578626843, 0.636431656119058, 0.505792518855538,
                                      0.641584921594313, 0.680001424521895, 0.658385956313478, 0.553544948859816,
                                      1, "", "y8", 0.306359780802382, 0.255455006325795, 0.216025494551558,
                                      0.733809277744428, 0.583181617981121, 0.73975102181084, 0.784045465677225,
                                      0.759122709303272, 0.638240438256752, 0.716027547589889, 1
                                 ))
})

test_that("1 table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_cors"]][["collection"]][["modelContainer_cors_observedCon"]][["collection"]][["modelContainer_cors_observedCon_1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("", "", 1, "ind60", 1, "", 0.521623484887408, "dem60", 0.94632437373424,
                                      1, 0.706803134908571, "dem65"))
})

test_that("2 table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_cors"]][["collection"]][["modelContainer_cors_observedCon"]][["collection"]][["modelContainer_cors_observedCon_2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("", "", 1, "ind60", 1, "", 0.319599163621636, "dem60", 0.996650485797297,
                                      1, 0.402495202076674, "dem65"))
})

test_that("1 table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_cors"]][["collection"]][["modelContainer_cors_observedInd"]][["collection"]][["modelContainer_cors_observedInd_1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("x1", 1, "", "", "", "", "", "", "", "", "", "", "x2", 0.936632480597606,
                                      1, "", "", "", "", "", "", "", "", "", "x3", 0.802018898050034,
                                      0.854833155000642, 1, "", "", "", "", "", "", "", "", "y1",
                                      0.435636471259361, 0.391654469928191, 0.297538560842895, 1,
                                      "", "", "", "", "", "", "", "y2", 0.36043466502006, 0.451056616923047,
                                      0.430432055900858, 0.569509025127605, 1, "", "", "", "", "",
                                      "", "y3", 0.260838300138224, 0.280074800299991, 0.172366407044565,
                                      0.748288584237668, 0.48480803952327, 1, "", "", "", "", "",
                                      "y4", 0.513167465417005, 0.562128880504207, 0.481034467720295,
                                      0.735145813265753, 0.8093147217146, 0.602715178667301, 1, "",
                                      "", "", "", "y5", 0.627963000084829, 0.624069696373029, 0.461389606907667,
                                      0.719883881782416, 0.616544458774234, 0.573479687781101, 0.716327751121613,
                                      1, "", "", "", "y6", 0.637698302737554, 0.609846128652746, 0.630817988447911,
                                      0.605557177407632, 0.683075171146645, 0.419941666854649, 0.714519572050633,
                                      0.670828718972408, 1, "", "", "y7", 0.47807714601197, 0.529238982970303,
                                      0.405796800553948, 0.723289586336763, 0.640791591329923, 0.690259546074704,
                                      0.747505468794052, 0.773887673838734, 0.625878964508004, 1,
                                      "", "y8", 0.543530042771138, 0.55133633247208, 0.380423995946671,
                                      0.604109707749136, 0.59462746878102, 0.453255459490693, 0.66170955960943,
                                      0.616519440519969, 0.738159724409321, 0.646941201777031, 1
                                 ))
})

test_that("2 table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_cors"]][["collection"]][["modelContainer_cors_observedInd"]][["collection"]][["modelContainer_cors_observedInd_2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("x1", 1, "", "", "", "", "", "", "", "", "", "", "x2", 0.844462190171461,
                                      1, "", "", "", "", "", "", "", "", "", "x3", 0.759443340174928,
                                      0.822979422277276, 1, "", "", "", "", "", "", "", "", "y1",
                                      0.228798893771052, 0.1673627845874, 0.0990214007881647, 1, "",
                                      "", "", "", "", "", "", "y2", 0.151047166840404, 0.100636161358447,
                                      0.0382347032598399, 0.693912354422896, 1, "", "", "", "", "",
                                      "", "y3", 0.402290635793068, 0.33745117598166, 0.264082997999289,
                                      0.619129644823373, 0.430540166939624, 1, "", "", "", "", "",
                                      "y4", 0.327744541436693, 0.265636839808341, 0.22237743313161,
                                      0.638936855818488, 0.720332884808176, 0.625254969353847, 1,
                                      "", "", "", "", "y5", 0.416772752181818, 0.337091821910455,
                                      0.314973431538363, 0.7429637976105, 0.523926428253201, 0.59787191426008,
                                      0.563485095874877, 1, "", "", "", "y6", 0.173882772345447, 0.172262926063391,
                                      0.103940159450445, 0.757801804543029, 0.722345337319345, 0.443619436764763,
                                      0.690021295526168, 0.519609085940291, 1, "", "", "y7", 0.32166269065689,
                                      0.28114796998464, 0.297044976903819, 0.636140897128672, 0.539182837545081,
                                      0.614027212992642, 0.643391219150212, 0.595245061008355, 0.606350074226451,
                                      1, "", "y8", 0.378769176379258, 0.339666986130413, 0.33764514816292,
                                      0.734843943897133, 0.634880950377465, 0.596867662479277, 0.813066001358268,
                                      0.645984661389981, 0.791972424098051, 0.771299433260701, 1
                                 ))
})


test_that("Mardia's coefficients table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_mardiasTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(330.897809673901, 26.471824773912, 286, "Skewness", 0.0347860345067629,
                                      "", "", 134.567190822067, "", "Kurtosis", 0.0308358026617142,
                                      -2.15918518879413))
})

test_that("Loadings table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_loading"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.823472500611673, 1, "dem60", "y1", 0.825156838000137, 1, "dem60",
                                      "y2", 0.615738096387633, 1, "dem60", "y3", 0.967290720859346,
                                      1, "dem60", "y4", 0.85890386330431, 1, "dem65", "y5", 0.860296333694986,
                                      1, "dem65", "y6", 0.819652028341773, 1, "dem65", "y7", 0.753217149752536,
                                      1, "dem65", "y8", 0.965771396825358, 1, "ind60", "x1", 1.00176753387165,
                                      1, "ind60", "x2", 0.823588805213466, 1, "ind60", "x3", 0.810325932156744,
                                      2, "dem60", "y1", 0.643991841667361, 2, "dem60", "y2", 0.816887240994446,
                                      2, "dem60", "y3", 0.865800409039581, 2, "dem60", "y4", 0.835471049666637,
                                      2, "dem65", "y5", 0.702431112065487, 2, "dem65", "y6", 0.788041616254904,
                                      2, "dem65", "y7", 0.908616414184754, 2, "dem65", "y8", 1.05852874275919,
                                      2, "ind60", "x1", 0.882643491810073, 2, "ind60", "x2", 0.746407359845629,
                                      2, "ind60", "x3"))
})

test_that("Regression Coefficients table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_path"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("", 0.521623484887408, 0.373798211601691, 1, "dem60", "ind60",
                                      "", 0.94632437373424, 8.57210950623997, 1, "dem65", "dem60",
                                      "", 0.319599163621636, 0.113763880589197, 2, "dem60", "ind60",
                                      "", 0.996650485797297, 148.525797772339, 2, "dem65", "dem60"
                                 ))
})

test_that("Total Effects table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_total"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.521623484887408, 1, "dem60", "ind60", 0.94632437373424, 1, "dem65",
                                      "dem60", 0.493625017661149, 1, "dem65", "ind60", 0.319599163621636,
                                      2, "dem60", "ind60", 0.996650485797297, 2, "dem65", "dem60",
                                      0.318528661683913, 2, "dem65", "ind60"))
})

test_that("Weights table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_weight"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("", 0.293345583919194, 1, "dem60", "y1", "", 0.293945595376005,
                                      1, "dem60", "y2", "", 0.219344363402489, 1, "dem60", "y3", "",
                                      0.344577944156403, 1, "dem60", "y4", "", 0.299325609271468,
                                      1, "dem65", "y5", "", 0.299810881332626, 1, "dem65", "y6", "",
                                      0.285646453876842, 1, "dem65", "y7", "", 0.2624940833262, 1,
                                      "dem65", "y8", "", 0.36210868343402, 1, "ind60", "x1", "", 0.375605162867342,
                                      1, "ind60", "x2", "", 0.308798395694023, 1, "ind60", "x3", "",
                                      0.304767776148618, 2, "dem60", "y1", "", 0.242208664013051,
                                      2, "dem60", "y2", "", 0.307235518354237, 2, "dem60", "y3", "",
                                      0.325632013959189, 2, "dem60", "y4", "", 0.299161554411588,
                                      2, "dem65", "y5", "", 0.251523237623159, 2, "dem65", "y6", "",
                                      0.282178245378945, 2, "dem65", "y7", "", 0.325353103425732,
                                      2, "dem65", "y8", "", 0.421090974654143, 2, "ind60", "x1", "",
                                      0.351122452536932, 2, "ind60", "x2", "", 0.296926658625392,
                                      2, "ind60", "x3"))
})

test_that("Reliability Measures table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_tabReliability"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 0.961000183958405, 0.952855148987773, 0.950346036120306, "ind60",
                                      1, 0.907216185107068, 0.887320081474863, 0.88513759231707, "dem60",
                                      1, 0.896551178426411, 0.894147556309882, 0.894174583403604,
                                      "dem65", 2, 0.955078330592321, 0.93004066282504, 0.927026758576996,
                                      "ind60", 2, 0.875816610760396, 0.866837550301532, 0.867792671415489,
                                      "dem60", 2, 0.892284153013129, 0.884815354534642, 0.883677432449498,
                                      "dem65"))
})

test_that("R-Squared table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_tabrsquared"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.251293661699972, 1, 0.272091059986084, "dem60", 0.892544958047031,
                                      1, 0.895529820323502, "dem65", 0.0772031705373062, 2, 0.102143625387649,
                                      "dem60", 0.993126418363321, 2, 0.993312190839988, "dem65"))
})

# Bootstrapping works

options <- jaspTools::analysisOptions("PLSSEM")
model <- "
  ind60 =~ x1 + x2 + x3
  dem60 =~ y1 + y2 + y3 + y4
  dem65 =~ y5 + y6 + y7 + y8
  dem60 ~ ind60
  dem65 ~ ind60 + dem60
"
options$models <- list(list(name = "Model1", syntax = list(model = model, columns = c("x1", "x2", "x3", "y1", "y2", "y3", "y4", "y5", "y6", "y7", "y8"))))
options$group <- ""
options$innerWeightingScheme      <- "path"
options$convergenceCriterion      <- "absoluteDifference"
options$setSeed                   <- TRUE
options$seed                      <- 123
options$errorCalculationMethod    <- "bootstrap"
options$bootstrapSamples          <- 200
options$handlingOfInadmissibles   <- "ignore"
options$handlingOfFlippedSigns    <- "none"
options$setSeed <- TRUE
options$seed <- 123

results <- jaspTools::runAnalysis("PLSSEM", testthat::test_path("poldem_grouped.csv"), options, makeTests = F)


test_that("Loadings table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_loading"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.853392561001313, 1.1180446425818, 0.989289467635534, "ind60",
                                      4.31808056376032e-49, "x1", 0.0673894669734411, 14.680179441476,
                                      0.83339230254863, 1.11006499351477, 0.957406943569191, "ind60",
                                      1.51004337219304e-45, "x2", 0.067823473135631, 14.1161591895272,
                                      0.636865351130356, 0.936863802547825, 0.804392175989385, "ind60",
                                      2.15883944130425e-24, "x3", 0.0794529079712384, 10.1241376373609,
                                      0.755257466691696, 0.921395025255003, 0.847163954739594, "dem60",
                                      2.34641900891912e-81, "y1", 0.0444294820163746, 19.0676081802477,
                                      0.601858594006427, 0.848556790488023, 0.724382982869384, "dem60",
                                      1.24962911923009e-26, "y2", 0.0682322287085268, 10.6164344413223,
                                      0.533814392776311, 0.83051931041522, 0.696074375322977, "dem60",
                                      3.13029888926552e-21, "y3", 0.0741647005134969, 9.38552128578072,
                                      0.787703296914169, 0.955341289010944, 0.892789811428192, "dem60",
                                      1.69038306636461e-107, "y4", 0.0405947818542633, 21.9927234646398,
                                      0.750457381112038, 0.914473467611394, 0.835991522387164, "dem65",
                                      1.47577857367684e-76, "y5", 0.0452358473608645, 18.4807309061357,
                                      0.680098055477842, 0.861657822207093, 0.770373752848573, "dem65",
                                      7.06355411130876e-55, "y6", 0.049517479147756, 15.5576125058758,
                                      0.727147168625325, 0.882386574198627, 0.815506515484913, "dem65",
                                      3.02191433099697e-92, "y7", 0.0400995142451285, 20.3370671898846,
                                      0.719402693170427, 0.903356785509169, 0.820453797634579, "dem65",
                                      1.13800477781802e-60, "y8", 0.0500599613331311, 16.3894213216577
                                 ))
})

test_that("Regression Coefficients table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_path"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.195578770108024, 0.650933570068278, 0.441527782000547, 0.238518982057254,
                                      "dem60", 4.56521605079441e-05, "ind60", 0.112847408750808, "",
                                      3.91260895476598, 0.05635700832898, 0.279303336761904, 0.159965138784183,
                                      0.898128069469413, "dem65", 0.00392150286783224, "ind60", 0.0601654384980987,
                                      1.23851898205725, 2.65875464016169, 0.825300443007273, 0.994277227844753,
                                      0.909788542478385, 29.4646949739788, "dem65", 1.15907171206722e-82,
                                      "dem60", 0.0473249624281238, 1.23851898205725, 19.2242845170803
                                 ))
})

test_that("Total effects table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_total"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.195578770108024, 0.650933570068278, 0.441527782000547, "dem60",
                                      4.56521605079441e-05, "ind60", 0.112847408750808, 3.91260895476598,
                                      0.32811299846643, 0.744911235477407, 0.559823543895544, "dem65",
                                      2.57010273096937e-08, "ind60", 0.102787981183385, 5.44639108045867,
                                      0.825300443007273, 0.994277227844753, 0.909788542478385, "dem65",
                                      1.15907171206722e-82, "dem60", 0.0473249624281238, 19.2242845170803
                                 ))
})

test_that("Weights table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_weight"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.330941657269293, 0.43101463354833, "", 0.379342885785455, "ind60",
                                      1.58150183746513e-43, "x1", 0.0275196851242272, 13.7844195554221,
                                      0.323386343086261, 0.4318601076441, "", 0.367007026369674, "ind60",
                                      2.72443697852608e-45, "x2", 0.0260760254837737, 14.0745002185264,
                                      0.251842045689824, 0.355556805589262, "", 0.308181097891855,
                                      "ind60", 4.42755082795622e-27, "x3", 0.0287673097541588, 10.7128925341134,
                                      0.274093632253853, 0.351889084777352, "", 0.314958928763463,
                                      "dem60", 1.24984889668148e-52, "y1", 0.02069007350364, 15.2227070971039,
                                      0.22874985017192, 0.310644947613172, "", 0.268732896935113,
                                      "dem60", 2.71140012870226e-37, "y2", 0.0211487958285135, 12.7067705941346,
                                      0.211428942709751, 0.295201043690409, "", 0.257953563681239,
                                      "dem60", 5.93024332298043e-34, "y3", 0.0213352783699776, 12.0904709658827,
                                      0.30131069033911, 0.364371113377643, "", 0.331786093130858,
                                      "dem60", 2.70310525860631e-79, "y4", 0.0176315784242949, 18.8177192731471,
                                      0.26631908667028, 0.331831383791989, "", 0.299006076343, "dem65",
                                      3.03083133618204e-65, "y5", 0.0175702069662422, 17.0177890856655,
                                      0.249640134133244, 0.2950591329893, "", 0.275143965720683, "dem65",
                                      6.80150410451226e-125, "y6", 0.0115893896919923, 23.7410228694609,
                                      0.259455884387387, 0.325884948985283, "", 0.291733641914686,
                                      "dem65", 3.90259681182693e-68, "y7", 0.0167632409054462, 17.4031766029148,
                                      0.265005859576053, 0.325451326210979, "", 0.293217297780113,
                                      "dem65", 1.71630155719206e-83, "y8", 0.0151744279798016, 19.3231203291755
                                 ))
})


# cSEM example matches output
options <- jaspTools::analysisOptions("PLSSEM")
model <- "
  # Structural model
  EXPE ~ IMAG
  QUAL ~ EXPE
  VAL  ~ EXPE + QUAL
  SAT  ~ IMAG + EXPE + QUAL + VAL
  LOY  ~ IMAG + SAT

  # Composite model
  IMAG <~ imag1 + imag2 + imag3
  EXPE <~ expe1 + expe2 + expe3
  QUAL <~ qual1 + qual2 + qual3 + qual4 + qual5
  VAL  <~ val1  + val2  + val3

    # Reflective measurement model
    SAT  =~ sat1  + sat2  + sat3  + sat4
    LOY  =~ loy1  + loy2  + loy3  + loy4
    "
options$models <- list(list(name = "Model1", syntax = list(model = model, columns = c("imag1", "imag2", "imag3",
                                                                                      "expe1", "expe2", "expe3",
                                                                                      "qual1", "qual2", "qual3", "qual4", "qual5",
                                                                                      "val1", "val2", "val3",
                                                                                      "sat1", "sat2", "sat3", "sat4",
                                                                                      "loy1", "loy2", "loy3", "loy4"))))
options$group                     <- ""
options$innerWeightingScheme      <- "path"
options$convergenceCriterion      <- "absoluteDifference"
options$additionalFitMeasures         <- TRUE
options$rSquared                      <- TRUE
options$mardiasCoefficient            <- TRUE
options$reliabilityMeasures           <- TRUE
options$setSeed <- TRUE
options$seed <- 123
results <- jaspTools::runAnalysis("PLSSEM", cSEM::satisfaction, options, makeTests = F)

test_that("Additional Fit Measures table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_addfit"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Comparative Fit Index (CFI)", 0.859882544280711, "Goodness of fit index (GFI)",
                                      0.728061232627151, "Hoelter's critical N (CN)", 75.1458784682054,
                                      "Bollen's Incremental Fit Index (IFI)", 0.861559817665155, "Bentler-Bonett Non-normed Fit Index (NNFI)",
                                      0.824091672439371, "Bentler-Bonett Normed Fit Index (NFI)",
                                      0.82299181963461, "Root mean square error of approximation (RMSEA)",
                                      0.108922002928605, "Root mean square residual covariance (RMS theta)",
                                      0.0506929896864307, "Standardized root mean square residual (SRMR)",
                                      0.0939687096223917, "Goodness of Fit (GoF)", 0.606126744677953,
                                      "Geodesic distance", 0.649343219969218, "Squared Euclidean distance",
                                      2.23401995218863, "Maximum likelihood-based distance", 2.92193232413732
                                 ))
})


test_that("Mardia's coefficients table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_mardiasTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(6620.64766637394, 158.895543992975, 2024, "Skewness", 0, "", "",
                                      764.431496500839, "", "Kurtosis", 0, 57.5192719976351))
})

test_that("Loadings table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_loading"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.630611581529705, "IMAG", "imag1", 0.92460948646301, "IMAG",
                                      "imag2", 0.957687129215542, "IMAG", "imag3", 0.752524393621429,
                                      "EXPE", "expe1", 0.934792825929098, "EXPE", "expe2", 0.729527691823171,
                                      "EXPE", "expe3", 0.786120979737557, "QUAL", "qual1", 0.924436629726835,
                                      "QUAL", "qual2", 0.755985724901802, "QUAL", "qual3", 0.76318065795902,
                                      "QUAL", "qual4", 0.78338153281586, "QUAL", "qual5", 0.95183503158758,
                                      "VAL", "val1", 0.805575313727215, "VAL", "val2", 0.676283618907474,
                                      "VAL", "val3", 0.924289865302974, "SAT", "sat1", 0.881336997275302,
                                      "SAT", "sat2", 0.712727779547715, "SAT", "sat3", 0.775595856561127,
                                      "SAT", "sat4", 0.909676025893971, "LOY", "loy1", 0.577531310061976,
                                      "LOY", "loy2", 0.904338032321655, "LOY", "loy3", 0.491738641829723,
                                      "LOY", "loy4"))
})

test_that("Regression Coefficients table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_path"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.471353677923676, 0.285635055190257, "EXPE", "IMAG", "", 0.834448527558827,
                                      2.29277019283586, "QUAL", "EXPE", "", 0.0457184790665393, 0.00140265011592659,
                                      "VAL", "EXPE", 3.29277019283586, 0.701315464456651, 0.330059801875975,
                                      "VAL", "QUAL", 3.29277019283586, 0.244966114148237, 0.146153749343165,
                                      "SAT", "IMAG", 1.72800425897561, -0.0172342465741804, 0.000378977922843456,
                                      "SAT", "EXPE", 3.29847682112087, 0.221545357537628, 0.0467867757143175,
                                      "SAT", "QUAL", 4.4151446231175, 0.526952593892149, 0.437277523980573,
                                      "SAT", "VAL", 2.67256749085723, 0.181941006626375, 0.0414081084059108,
                                      "LOY", "IMAG", 1.93450952389396, 0.628293646783788, 0.493798244508887,
                                      "LOY", "SAT", 1.93450952389396))
})

test_that("Total effects table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_total"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.471353677923676, "EXPE", "IMAG", 0.393320382502849, "QUAL",
                                      "IMAG", 0.834448527558827, "QUAL", "EXPE", 0.297391239992343,
                                      "VAL", "IMAG", 0.630930135736626, "VAL", "EXPE", 0.701315464456651,
                                      "VAL", "QUAL", 0.480692078722452, "SAT", "IMAG", 0.500104222401728,
                                      "SAT", "EXPE", 0.591105360669737, "SAT", "QUAL", 0.526952593892149,
                                      "SAT", "VAL", 0.483956785746983, "LOY", "IMAG", 0.314212305664752,
                                      "LOY", "EXPE", 0.371387742688635, "LOY", "QUAL", 0.331080966898675,
                                      "LOY", "VAL", 0.628293646783788, "LOY", "SAT"))
})

test_that("Weights table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_weight"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0156437093210086, "IMAG", "imag1", 1.72148398943408, 0.44728617775625,
                                      "IMAG", "imag2", 3.05148628819596, 0.602044065350609, "IMAG",
                                      "imag3", 2.53557446079279, 0.294610643934218, "EXPE", "expe1",
                                      1.49493339995316, 0.647338185763638, "EXPE", "expe2", 1.66231957025214,
                                      0.23737441876182, "EXPE", "expe3", 1.5212092336783, 0.237038612241018,
                                      "QUAL", "qual1", 1.84012870135929, 0.471225085890941, "QUAL",
                                      "qual2", 2.50052562170516, 0.183069588476778, "QUAL", "qual3",
                                      1.77961451705965, 0.10372504473348, "QUAL", "qual4", 2.15570460410088,
                                      0.204858416371178, "QUAL", "qual5", 2.02056454605736, 0.716330223392201,
                                      "VAL", "val1", 1.69123457143564, 0.220214717332445, "VAL", "val2",
                                      2.20494603455805, 0.208155654302742, "VAL", "val3", 1.67139667016484,
                                      0.320857337027855, "SAT", "sat1", "", 0.305946708478931, "SAT",
                                      "sat2", "", 0.247415822628863, "SAT", "sat3", "", 0.269239802888533,
                                      "SAT", "sat4", "", 0.383423535171163, "LOY", "loy1", "", 0.243426330113932,
                                      "LOY", "loy2", "", 0.381173599690884, "LOY", "loy3", "", 0.207265183497971,
                                      "LOY", "loy4", ""))
})

test_that("Reliability Measures table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_tabReliability"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.905102486354131, 0.895998833388936, 0.894010912676559, "SAT",
                                      0.876110242219458, 0.823701924294241, 0.819421641029961, "LOY"
                                 ))
})

test_that("R-Squared table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_tabrsquared"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.219037895698999, 0.222174289692176, "EXPE", 0.695079765891647,
                                      0.696304345145095, "QUAL", 0.543779108936317, 0.547443533764138,
                                      "VAL", 0.758514799820926, 0.762394080145088, "SAT", 0.583411462155354,
                                      0.586757554828805, "LOY"))
})

