context("Partial Least Squares Structural Equation Modeling")

test_that("Basic PLSSEM works", {

  options <- jaspTools::analysisOptions("PLSSEM")
  model <- "
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem60 ~ ind60
  "
  options$models <- list(list(modelName = "Model1", syntax = list(model = model, columns = c("x1", "x2", "x3", "y1", "y2", "y3", "y4"))))
  options$groupingVariable <- ""
  options$innerWeightingScheme      <- "path"
  options$approachNonLinear         <- "sequential"
  options$convergenceCriterion      <- "diff_absolute"
  options$approachCorrectionFactors <- "dist_squared_euclid"
  options$signFlippingHandling      <- "none"

  results <- jaspTools::runAnalysis("PLSSEM", "poldem_grouped.csv", options)


  fittab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_fittab"]][["data"]]
  jaspTools::expect_equal_tables(fittab,
                                 list(1415.78715839976, 1434.32706330805, 312.55822949309, 13, "Model1",
                                      75, 5.64331990957278e-59))



  loadingTab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_loading"]][["data"]]
  jaspTools::expect_equal_tables(loadingTab,
                                 list(1.01629013879267, "ind60", "x1", 0.958502396852076, "ind60", "x2",
                                      0.783184549863239, "ind60", "x3", 0.786583578405125, "dem60",
                                      "y1", 0.550164061107189, "dem60", "y2", 0.715154799753118, "dem60",
                                      "y3", 1.06636594277093, "dem60", "y4"))


  pathTab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_path"]][["data"]]
  jaspTools::expect_equal_tables(pathTab,
                                 list(0.441113920509059, "dem60", "ind60"))

  totalTab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_total"]][["data"]]
  jaspTools::expect_equal_tables(totalTab,
                                 list(0.441113920509059, "dem60", "ind60"))


  weightTab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_weight"]][["data"]]
  jaspTools::expect_equal_tables(weightTab,
                                 list(0.388110212588108, "ind60", "x1", 0.366041698929012, "ind60",
                                      "x2", 0.299089709267713, "ind60", "x3", 0.294254491291247, "dem60",
                                      "y1", 0.205811881117664, "dem60", "y2", 0.26753356868005, "dem60",
                                      "y3", 0.398918788333461, "dem60", "y4"))
})

test_that("Multigroup, multimodel PLSSEM works", {

  options <- jaspTools::analysisOptions("PLSSEM")
  model1 = "
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8
    dem60 ~ ind60
    dem65 ~ dem60
  "
  model2 = "
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8
    dem60 ~ ind60
    dem65 ~ ind60 + dem60
  "
  options$models <- list(list(modelName = "Model1", syntax = list(model = model1, columns = c("x1", "x2", "x3", "y1", "y2", "y3", "y4", "y5", "y6", "y7", "y8"))),
                         list(modelName = "Model2", syntax = list(model = model2, columns = c("x1", "x2", "x3", "y1", "y2", "y3", "y4", "y5", "y6", "y7", "y8"))))
  options$groupingVariable          <- "group"
  options$innerWeightingScheme      <- "path"
  options$approachNonLinear         <- "sequential"
  options$convergenceCriterion      <- "diff_absolute"
  options$approachCorrectionFactors <- "dist_squared_euclid"
  options$outputAdditionalFitMeasures <- TRUE
  options$outputRSquared <- TRUE
  options$outputMardiasCoefficients <- TRUE
  options$outputReliabilityMeasures <- TRUE
  options$outputImpliedConstructCorrelations <- TRUE
  options$outputImpliedIndicatorCorrelations <- TRUE
  options$outputObservedConstructCorrelations <- TRUE
  options$outputObservedIndicatorCorrelations <- TRUE
  options$innerWeightingScheme <- "centroid"
  options$ignoreStructuralModel <- TRUE
  options$signFlippingHandling      <- "none"

  results <- jaspTools::runAnalysis("PLSSEM", "poldem_grouped.csv", options)

  fittab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_fittab"]][["data"]]
  jaspTools::expect_equal_tables(fittab,
                                 list(837.114028880141, 858.055961744516, 89.8354936832977, 42, "Model1",
                                      37, 2.50270252185835e-05, "", "", "", 1, 983.444614918989, 1004.73323499543,
                                      139.578131429904, 42, "Model1", 38, 2.10842913855894e-12, "",
                                      "", "", 2, 834.070743364645, 856.623594141664, 84.9285131817342,
                                      41, "Model2", 37, 6.67910741734327e-05, 0.0267483614535254,
                                      4.90698050156354, 1, 1, 990.495831963802, 1013.42203819997,
                                      144.49642171038, 41, "Model2", 38, 1.86622493584378e-13, 0.0265738023190004,
                                      4.9182902804759, 1, 2))

  rsquaredtab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_tabrsquared"]][["data"]]
  jaspTools::expect_equal_tables(rsquaredtab,
                                 list(1, "dem60", 0.251293661699972, 0.251293661699972, 0.272091059986084,
                                      0.272091059986084, 1, "dem65", 0.892544958047032, 0.955489129838819,
                                      0.895529820323504, 0.957961955958885, 2, "dem60", 0.0772031705373062,
                                      0.0772031705373062, 0.102143625387649, 0.102143625387649, 2,
                                      "dem65", 0.993126418363321, 1.0012312007131, 0.993312190839988,
                                      1.0011646493232))

  addfittab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_addfit"]][["data"]]
  jaspTools::expect_equal_tables(addfittab,
                                 list("Comparative Fit Index (CFI)", 0.870519886244139, 0.682513115515104,
                                      0.881095219345823, 0.663256961343149, "Goodness of fit index (GFI)",
                                      0.496752300477216, 0.419309726581196, 0.496492992345079, 0.406276458556095,
                                      "Hoelter's critical N (CN)", 24.292189653768, 16.4077818076547,
                                      25.1370755296179, 15.5807646964114, "Bollen's Incremental Fit Index (IFI)",
                                      0.874921175926588, 0.695397113795039, 0.885436589164564, 0.677927787927771,
                                      "Bentler-Bonett Non-normed Fit Index (NNFI)", 0.830442708176849,
                                      0.584243365555493, 0.840493586927323, 0.548271533509102, "Bentler-Bonett Normed Fit Index (NFI)",
                                      0.78834486744787, 0.61479258754282, 0.799905861503856, 0.60121910111449,
                                      "Root mean square error of approximation (RMSEA)", 0.177868578652955,
                                      0.250582502116037, 0.172516275461992, 0.261198017900775, "Root mean square residual covariance (RMS theta)",
                                      0.0766251796879128, 0.0795989971054803, 0.0766251796879128,
                                      0.0795989971054803, "Standardized root mean square residual (SRMR)",
                                      0.0943761594914447, 0.0897114551867813, 0.0633682319348557,
                                      0.0857662455926121, "Goodness of Fit (GoF)", 0.651821788678243,
                                      0.614476373672947, 0.669021172943273, 0.616674789196147, "Geodesic distance",
                                      0.495814387492709, 0.623860456924759, 0.446431300714197, 0.627248478150673,
                                      "Squared Euclidean distance", 0.587852725703404, 0.531177582654172,
                                      0.265025166024278, 0.485486026281449, "Maximum likelihood-based dinstance",
                                      2.4954303800916, 3.77238193053796, 2.35912536615928, 3.90530869487514
                                 ))

  mardiastab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_mardiasTable"]][["data"]]
  jaspTools::expect_equal_tables(mardiastab,
                                 list(330.8978096739, 26.471824773912, 286, "Skewness", 0.0347860345067639,
                                      "", "", 134.567190822067, "", "Kurtosis", 0.0308358026617131,
                                      -2.15918518879414))

  reliabilitytab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_tabReliability"]][["data"]]
  jaspTools::expect_equal_tables(reliabilitytab,
                                 list(1, "ind60", 0.961000183958405, 0.961000183958405, 0.952855148987773,
                                      0.952855148987773, 0.950346036120306, 0.950346036120306, 1,
                                      "dem60", 0.907216185107067, 0.907216185107067, 0.887320081474863,
                                      0.887320081474863, 0.88513759231707, 0.88513759231707, 1, "dem65",
                                      0.896551178426411, 0.896551178426411, 0.894147556309883, 0.894147556309883,
                                      0.894174583403605, 0.894174583403605, 2, "ind60", 0.955078330592321,
                                      0.955078330592321, 0.93004066282504, 0.93004066282504, 0.927026758576996,
                                      0.927026758576996, 2, "dem60", 0.875816610760396, 0.875816610760396,
                                      0.866837550301531, 0.866837550301531, 0.867792671415489, 0.867792671415489,
                                      2, "dem65", 0.892284153013128, 0.892284153013128, 0.884815354534642,
                                      0.884815354534642, 0.883677432449498, 0.883677432449498))

  #parameter tables (model1)

  weighttab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_Model1"]][["collection"]][["modelContainer_params_Model1_weight"]][["data"]]
  jaspTools::expect_equal_tables(weighttab,
                                 list(0.293345583919194, 1, "dem60", "y1", 0.293945595376005, 1, "dem60",
                                      "y2", 0.219344363402489, 1, "dem60", "y3", 0.344577944156403,
                                      1, "dem60", "y4", 0.299325609271468, 1, "dem65", "y5", 0.299810881332626,
                                      1, "dem65", "y6", 0.285646453876842, 1, "dem65", "y7", 0.2624940833262,
                                      1, "dem65", "y8", 0.36210868343402, 1, "ind60", "x1", 0.375605162867342,
                                      1, "ind60", "x2", 0.308798395694023, 1, "ind60", "x3", 0.304767776148618,
                                      2, "dem60", "y1", 0.242208664013051, 2, "dem60", "y2", 0.307235518354237,
                                      2, "dem60", "y3", 0.325632013959189, 2, "dem60", "y4", 0.299161554411588,
                                      2, "dem65", "y5", 0.251523237623159, 2, "dem65", "y6", 0.282178245378945,
                                      2, "dem65", "y7", 0.325353103425732, 2, "dem65", "y8", 0.421090974654143,
                                      2, "ind60", "x1", 0.351122452536932, 2, "ind60", "x2", 0.296926658625392,
                                      2, "ind60", "x3"))

  loadingtab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_Model1"]][["collection"]][["modelContainer_params_Model1_loading"]][["data"]]
  jaspTools::expect_equal_tables(loadingtab,
                                 list(0.823472500611673, 1, "dem60", "y1", 0.825156838000137, 1, "dem60",
                                      "y2", 0.615738096387633, 1, "dem60", "y3", 0.967290720859345,
                                      1, "dem60", "y4", 0.85890386330431, 1, "dem65", "y5", 0.860296333694987,
                                      1, "dem65", "y6", 0.819652028341773, 1, "dem65", "y7", 0.753217149752536,
                                      1, "dem65", "y8", 0.965771396825359, 1, "ind60", "x1", 1.00176753387165,
                                      1, "ind60", "x2", 0.823588805213467, 1, "ind60", "x3", 0.810325932156744,
                                      2, "dem60", "y1", 0.643991841667361, 2, "dem60", "y2", 0.816887240994446,
                                      2, "dem60", "y3", 0.865800409039581, 2, "dem60", "y4", 0.835471049666637,
                                      2, "dem65", "y5", 0.702431112065487, 2, "dem65", "y6", 0.788041616254904,
                                      2, "dem65", "y7", 0.908616414184754, 2, "dem65", "y8", 1.05852874275919,
                                      2, "ind60", "x1", 0.882643491810074, 2, "ind60", "x2", 0.746407359845629,
                                      2, "ind60", "x3"))

  pathtab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_Model1"]][["collection"]][["modelContainer_params_Model1_path"]][["data"]]
  jaspTools::expect_equal_tables(pathtab,
                                 list(0.521623484887408, 1, "dem60", "ind60", 0.946324373734241, 1,
                                      "dem65", "dem60", 0.319599163621636, 2, "dem60", "ind60", 0.996650485797297,
                                      2, "dem65", "dem60"))

  totaltab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_Model1"]][["collection"]][["modelContainer_params_Model1_total"]][["data"]]
  jaspTools::expect_equal_tables(totaltab,
                                 list(0.521623484887408, 1, "dem60", "ind60", 0.946324373734241, 1,
                                      "dem65", "dem60", 0.493625017661149, 1, "dem65", "ind60", 0.319599163621636,
                                      2, "dem60", "ind60", 0.996650485797297, 2, "dem65", "dem60",
                                      0.318528661683913, 2, "dem65", "ind60"))

  #correlation tables (model2, group 2)

  obsindtab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_cors"]][["collection"]][["modelContainer_cors_Model2"]][["collection"]][["modelContainer_cors_Model2_observedInd"]][["collection"]][["modelContainer_cors_Model2_observedInd_2"]][["data"]]
  jaspTools::expect_equal_tables(obsindtab,
                                 list(1, "", "", "", "", "", "", "", "", "", "", 0.844462190171461,
                                      1, "", "", "", "", "", "", "", "", "", 0.759443340174928, 0.822979422277276,
                                      1, "", "", "", "", "", "", "", "", 0.228798893771052, 0.1673627845874,
                                      0.0990214007881648, 1, "", "", "", "", "", "", "", 0.151047166840404,
                                      0.100636161358447, 0.0382347032598399, 0.693912354422896, 1,
                                      "", "", "", "", "", "", 0.402290635793068, 0.33745117598166,
                                      0.264082997999289, 0.619129644823373, 0.430540166939625, 1,
                                      "", "", "", "", "", 0.327744541436693, 0.265636839808341, 0.22237743313161,
                                      0.638936855818488, 0.720332884808176, 0.625254969353847, 1,
                                      "", "", "", "", 0.416772752181817, 0.337091821910455, 0.314973431538363,
                                      0.7429637976105, 0.523926428253201, 0.59787191426008, 0.563485095874877,
                                      1, "", "", "", 0.173882772345447, 0.172262926063391, 0.103940159450445,
                                      0.757801804543029, 0.722345337319345, 0.443619436764764, 0.690021295526168,
                                      0.519609085940291, 1, "", "", 0.32166269065689, 0.28114796998464,
                                      0.297044976903819, 0.636140897128672, 0.539182837545081, 0.614027212992642,
                                      0.643391219150212, 0.595245061008356, 0.606350074226451, 1,
                                      "", 0.378769176379258, 0.339666986130413, 0.33764514816292,
                                      0.734843943897133, 0.634880950377465, 0.596867662479277, 0.813066001358268,
                                      0.645984661389981, 0.79197242409805, 0.771299433260701, 1))

  impindtab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_cors"]][["collection"]][["modelContainer_cors_Model2"]][["collection"]][["modelContainer_cors_Model2_impliedInd"]][["collection"]][["modelContainer_cors_Model2_impliedInd_2"]][["data"]]
  jaspTools::expect_equal_tables(impindtab,
                                 list(1, "", "", "", "", "", "", "", "", "", "", 0.934303505690295,
                                      1, "", "", "", "", "", "", "", "", "", 0.790093644203597, 0.658811598406884,
                                      1, "", "", "", "", "", "", "", "", 0.274137234138764, 0.228586561518095,
                                      0.19330419751809, 1, "", "", "", "", "", "", "", 0.217865596146897,
                                      0.181665025011161, 0.153625005965657, 0.521843289400442, 1,
                                      "", "", "", "", "", "", 0.276356957074584, 0.230437455049635,
                                      0.194869405404459, 0.661944915025775, 0.526068718762583, 1,
                                      "", "", "", "", "", 0.292904521540612, 0.244235474405422, 0.206537698768669,
                                      0.701580523516688, 0.557568399933754, 0.707261307392206, 1,
                                      "", "", "", "", 0.355954730085641, 0.296809253445637, 0.250996708521339,
                                      0.674736223076412, 0.536234378902497, 0.680199645346271, 0.720928301502733,
                                      1, "", "", "", 0.299272700111863, 0.249545515733112, 0.211028134561658,
                                      0.567291608387322, 0.450844719575288, 0.571885038383767, 0.606128086360512,
                                      0.586860858515855, 1, "", "", 0.335747290013451, 0.279959483812203,
                                      0.236747702911717, 0.636431656119058, 0.505792518855538, 0.641584921594313,
                                      0.680001424521895, 0.658385956313478, 0.553544948859816, 1,
                                      "", 0.387118513073036, 0.322794858864632, 0.272971432534776,
                                      0.733809277744428, 0.583181617981122, 0.73975102181084, 0.784045465677225,
                                      0.759122709303272, 0.638240438256752, 0.716027547589889, 1
                                 ))

  obscontab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_cors"]][["collection"]][["modelContainer_cors_Model2"]][["collection"]][["modelContainer_cors_Model2_observedCon"]][["collection"]][["modelContainer_cors_Model2_observedCon_2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("", "", 1, 1, "", 0.319599163621636, 0.996650485797297, 1, 0.402495202076674
                                 ))

  impcontab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_cors"]][["collection"]][["modelContainer_cors_Model2"]][["collection"]][["modelContainer_cors_Model2_impliedCon"]][["collection"]][["modelContainer_cors_Model2_impliedCon_2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("", "", 1, 1, "", 0.319599163621636, 0.996650485797297, 1, 0.402495202076674
                                 ))
})

test_that("Bootstrapping works", {

  options <- jaspTools::analysisOptions("PLSSEM")
  model <- "
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem60 ~ ind60
  "
  options$models <- list(list(modelName = "Model1", syntax = list(model = model, columns = c("x1", "x2", "x3", "y1", "y2", "y3", "y4"))))
  options$groupingVariable <- ""
  options$innerWeightingScheme      <- "path"
  options$approachNonLinear         <- "sequential"
  options$convergenceCriterion      <- "diff_absolute"
  options$approachCorrectionFactors <- "dist_squared_euclid"
  options$setSeed                   <- TRUE
  options$seed                      <- 123
  options$resamplingMethod          <- "bootstrap"
  options$nBootstraps               <- 200
  options$handleInadmissibles       <- "ignore"
  options$signFlippingHandling      <- "none"

  set.seed(123)
  results <- jaspTools::runAnalysis("PLSSEM", "poldem_grouped.csv", options)


  fittab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_fittab"]][["data"]]
  jaspTools::expect_equal_tables(fittab,
                                 list(1415.78715839976, 1434.32706330805, 312.55822949309, 13, "Model1",
                                      75, 5.64331990957278e-59))

  loadingtab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_loading"]][["data"]]
  jaspTools::expect_equal_tables(loadingtab,
                                 list(0.890021512353405, 1.20440950219456, 1.01379276122291, "ind60",
                                      7.15807055531443e-36, "x1", 0.0814413746098956, 12.4481292964292,
                                      0.798882816836385, 1.09333559128237, 0.948851846472176, "ind60",
                                      1.05575653902816e-37, "x2", 0.0742430978462834, 12.7803374858728,
                                      0.575744782709542, 0.937313767767564, 0.784399433202539, "ind60",
                                      9.39131750818054e-17, "x3", 0.0953143076287323, 8.22960846820529,
                                      0.433349612065233, 0.977354339017979, 0.763113657980061, "dem60",
                                      3.66569068785469e-09, "y1", 0.131954628466593, 5.78315188218852,
                                      0.00314794405334059, 0.863515076860006, 0.525102348713773, "dem60",
                                      0.0042112521880858, "y2", 0.199306595769215, 2.63464611739096,
                                      0.288510252769777, 0.943314639845184, 0.676961384886353, "dem60",
                                      6.26386302885132e-05, "y3", 0.176495987546554, 3.83556246403501,
                                      0.817241690478538, 1.31407379976973, 1.05121258871367, "dem60",
                                      1.60033913443091e-17, "y4", 0.124566033719138, 8.43899863652927
                                 ))

  pathtab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_path"]][["data"]]
  jaspTools::expect_equal_tables(pathtab,
                                 list(0.237373006286835, 0.656326030802514, 0.452831805859963, "dem60",
                                      6.54083247461508e-06, "ind60", 0.103890658431892, 4.35873458398407
                                 ))

  totaltab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_total"]][["data"]]
  jaspTools::expect_equal_tables(totaltab,
                                 list(0.237373006286835, 0.656326030802514, 0.452831805859963, "dem60",
                                      6.54083247461508e-06, "ind60", 0.103890658431892, 4.35873458398407
                                 ))

  weighttab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_weight"]][["data"]]
  jaspTools::expect_equal_tables(weighttab,
                                 list(0.34254486676368, 0.464008957197284, 0.389300620942958, "ind60",
                                      1.49297840679697e-30, "x1", 0.0340617111368756, 11.4292737490072,
                                      0.31300083079443, 0.415871189799314, 0.364151921178565, "ind60",
                                      1.21656076210304e-37, "x2", 0.0285177520800989, 12.7693066464621,
                                      0.22540626889694, 0.354884457227488, 0.300769637529741, "ind60",
                                      1.1683906384858e-18, "x3", 0.034414084034122, 8.73972520179598,
                                      0.184275464793891, 0.380144019589489, 0.294954288199695, "dem60",
                                      1.75683821448052e-08, "y1", 0.0534948032282675, 5.51369984372306,
                                      0.00171132923026484, 0.315410931631662, 0.197592948154488, "dem60",
                                      0.00338682586801573, "y2", 0.0729725177377539, 2.70777210763907,
                                      0.116806344670602, 0.354933302301699, 0.259556043900929, "dem60",
                                      6.57347731761408e-05, "y3", 0.0678810491191126, 3.82368933994345,
                                      0.309774585336782, 0.59281580844812, 0.408391863287812, "dem60",
                                      2.33111124882863e-10, "y4", 0.0655517133665078, 6.23007153153179
                                 ))
})
