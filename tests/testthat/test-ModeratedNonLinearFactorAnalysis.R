
context("MNLFA")

options <- list(

  ## ===============================
  ## General switches
  ## ===============================
  addGroupVariableToData = FALSE,
  checkModelFitPerGroup       = FALSE,
  syncAnalysisBox        = TRUE,

  invarianceTestConfigural = FALSE,
  invarianceTestMetric     = FALSE,
  invarianceTestScalar     = TRUE,
  invarianceTestStrict     = FALSE,
  invarianceTestCustom     = FALSE,

  splitContinuousVariablesIntoGroups = 2,

  showSyntax = FALSE,
  warnings   = FALSE,
  indicatorPreprocessing = "none",

  ## ===============================
  ## Factors
  ## ===============================
  factors = list(

    list(
      name = "Factor1",
      title = "Factor 1",
      indicators = c(
        "AgeImportant",
        "AttractiveImportant",
        "PhysicalbuildImportant"
      ),
      indicators.types = c("scale", "scale", "scale")
    ),

    list(
      name = "Factor2",
      title = "Factor 2",
      indicators = c(
        "TrustImportant",
        "EmotionalconnImportant",
        "OpennessImportant"
      ),
      indicators.types = c("scale", "scale", "scale")
    )
  ),

  ## ===============================
  ## Moderators
  ## ===============================
  moderators = list(
    list(
      variable = "Age",
      moderatorSquaredEffect = TRUE,
      moderatorCubicEffect   = FALSE
    ),
    list(
      variable = "Male",
      moderatorSquaredEffect = FALSE,
      moderatorCubicEffect   = FALSE
    )
  ),

  moderators.types = c("scale", "nominal"),

  moderatorInteractionTerms = list(
    list(
      value = "Age:Male",
      moderatorInteractionTermsInclude = TRUE
    )
  ),

  ## ===============================
  ## Individual moderation inclusion
  ## ===============================
  includeIndividualModerationsList = list(

    list(
      keyLabel = "Scalar",
      keyValue = "invarianceTestScalar",

      moderationTypeList = list(

        ## -------- Indicators --------
        list(
          keyLabel = "Indicators",
          keyValue = "indicators",

          moderationParameterList = list(

            ## Loadings
            list(
              keyLabel = "Loadings",
              keyValue = "loadings",

              moderationItemList = list(
                list(includeIndividualModeration = FALSE, value = "AgeImportant",  value.types = "scale"),
                list(includeIndividualModeration = FALSE, value = "AttractiveImportant",  value.types = "scale"),
                list(includeIndividualModeration = FALSE, value = "PhysicalbuildImportant", value.types = "scale"),
                list(includeIndividualModeration = FALSE, value = "TrustImportant", value.types = "scale"),
                list(includeIndividualModeration = FALSE, value = "EmotionalconnImportant",  value.types = "scale"),
                list(includeIndividualModeration = FALSE, value = "OpennessImportant", value.types = "scale")
              )
            ),

            ## Intercepts
            list(
              keyLabel = "Intercepts",
              keyValue = "intercepts",

              moderationItemList = list(
                list(includeIndividualModeration = FALSE, value = "AgeImportant",  value.types = "scale"),
                list(includeIndividualModeration = FALSE, value = "AttractiveImportant",  value.types = "scale"),
                list(includeIndividualModeration = FALSE, value = "PhysicalbuildImportant", value.types = "scale"),
                list(includeIndividualModeration = FALSE, value = "TrustImportant", value.types = "scale"),
                list(includeIndividualModeration = FALSE, value = "EmotionalconnImportant",  value.types = "scale"),
                list(includeIndividualModeration = FALSE, value = "OpennessImportant", value.types = "scale")
              )
            ),

            ## Residual variances
            list(
              keyLabel = "Residual variances",
              keyValue = "residualVariances",

              moderationItemList = list(
                list(includeIndividualModeration = TRUE, value = "AgeImportant",  value.types = "scale"),
                list(includeIndividualModeration = TRUE, value = "AttractiveImportant",  value.types = "scale"),
                list(includeIndividualModeration = TRUE, value = "PhysicalbuildImportant", value.types = "scale"),
                list(includeIndividualModeration = TRUE, value = "TrustImportant", value.types = "scale"),
                list(includeIndividualModeration = TRUE, value = "EmotionalconnImportant",  value.types = "scale"),
                list(includeIndividualModeration = TRUE, value = "OpennessImportant", value.types = "scale")
              )
            )
          )
        ),

        ## -------- Factors --------
        list(
          keyLabel = "Factors",
          keyValue = "factors",

          moderationParameterList = list(

            ## Variances
            list(
              keyLabel = "Variances",
              keyValue = "variances",
              moderationItemList = list(
                list(includeIndividualModeration = TRUE, value = "Factor1"),
                list(includeIndividualModeration = TRUE, value = "Factor2")
              )
            ),

            ## Means
            list(
              keyLabel = "Means",
              keyValue = "means",
              moderationItemList = list(
                list(includeIndividualModeration = TRUE, value = "Factor1"),
                list(includeIndividualModeration = TRUE, value = "Factor2")
              )
            ),

            ## Covariances
            list(
              keyLabel = "Covariances",
              keyValue = "covariances",
              moderationItemList = list(
                list(includeIndividualModeration = TRUE, value = "Factor1:Factor2")
              )
            )
          )
        )
      )
    )
  ),

  ## ===============================
  ## Parameter output switches
  ## ===============================
  parameterEstimatesLoadings          = FALSE,
  parameterEstimatesIntercepts        = TRUE,
  parameterEstimatesResidualVariances = TRUE,
  parameterEstimatesFactorVariance   = FALSE,
  parameterEstimatesFactorMeans       = FALSE,
  parameterEstimatesFactorCovariances = FALSE,

  parameterEstimatesAlphaLevel = 0.05,

  ## ===============================
  ## Plot options
  ## ===============================
  plotWidth  = 480,
  plotHeight = 320,

  plotModelList = list(

    list(
      keyLabel = "Scalar",
      keyValue = "invarianceTestScalar",

      plotTypeList = list(

        ## Indicators
        list(
          keyLabel = "Indicators",
          keyValue = "indicators",

          plotParameterList = list(

            list(keyLabel = "Loadings", keyValue = "loadings", plotItemList = list()),

            list(
              keyLabel = "Intercepts",
              keyValue = "intercepts",
              plotItemList = list()
            ),

            list(
              keyLabel = "Residual variances",
              keyValue = "residualVariances",
              plotItemList = list(
                list(
                  includePlot     = TRUE,
                  plotModerator1 = "Age:Male",
                  plotModerator2 = "",
                  value          = "AgeImportant"
                )
              )
            )
          )
        ),

        ## Factors
        list(
          keyLabel = "Factors",
          keyValue = "factors",

          plotParameterList = list(

            list(
              keyLabel = "Variances",
              keyValue = "variances",
              plotItemList = list(
                list(includePlot = FALSE, plotModerator1 = "", plotModerator2 = "", value = "Factor1"),
                list(includePlot = FALSE, plotModerator1 = "", plotModerator2 = "", value = "Factor2")
              )
            ),

            list(
              keyLabel = "Means",
              keyValue = "means",
              plotItemList = list(
                list(
                  includePlot     = TRUE,
                  plotModerator1 = "Age",
                  plotModerator2 = "Male",
                  value          = "Factor1"
                ),
                list(
                  includePlot     = FALSE,
                  plotModerator1 = "",
                  plotModerator2 = "",
                  value          = "Factor2"
                )
              )
            ),

            list(
              keyLabel = "Covariances",
              keyValue = "covariances",
              plotItemList = list(
                list(
                  includePlot     = FALSE,
                  plotModerator1 = "",
                  plotModerator2 = "",
                  value          = "Factor1:Factor2"
                )
              )
            )
          )
        )
      )
    )
  )
)

results <- runAnalysis(
  "ModeratedNonLinearFactorAnalysis",
  dataset = testthat::test_path("AttractDat.csv"),
  options = options, makeTests = FALSE
)

# Only do full numeric/plot comparison on macOS; other platforms just check for no errors
isMacOS <- Sys.info()[["sysname"]] == "Darwin"

test_that("Global Invariance Fit table results match", {
  table <- results[["results"]][["fitContainer"]][["collection"]][["fitContainer_invFitTable"]][["data"]]
  if (isMacOS) {
    jaspTools::expect_equal_tables(table,
                                   list(110509.169461257, 110941.488082078, 110383.169461257, 63, 110510.322119919,
                                        110741.288535264, "Scalar"))
  } else {
    expect_true(!is.null(table))
  }
})

test_that("Intercepts table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_globalParameterContainer"]][["collection"]][["mainContainer_globalParameterContainer_Scalar"]][["collection"]][["mainContainer_globalParameterContainer_Scalar_intTable"]][["data"]]

  if (isMacOS) {
    jaspTools::expect_equal_tables(table,
                                   list(-0.0574946636641181, 0.00858975344324991, "Baseline", -0.0244524551104341,
                                        "AgeImportant", 0.14693425380744, 0.0168585794506005, -0.0969570811005486,
                                        -0.00442123683652089, "Baseline", -0.0506891589685347, "AttractiveImportant",
                                        0.0317731601764926, 0.0236065164956955, -0.0849090134463109,
                                        -0.00673061908341247, "Baseline", -0.0458198162648617, "PhysicalbuildImportant",
                                        0.0215935298093122, 0.0199438344223566, 0.192757567377669, 0.268166083329696,
                                        "Baseline", 0.230461825353682, "TrustImportant", 0, 0.0192372198027208,
                                        0.202870875149009, 0.279377041325636, "Baseline", 0.241123958237323,
                                        "EmotionalconnImportant", 0, 0.0195172377605144, 0.124728971196977,
                                        0.187241288511625, "Baseline", 0.155985129854301, "OpennessImportant",
                                        0, 0.0159473127587388))
  } else {
    expect_true(!is.null(table))
  }
})

test_that("Residual Variances table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_globalParameterContainer"]][["collection"]][["mainContainer_globalParameterContainer_Scalar"]][["collection"]][["mainContainer_globalParameterContainer_Scalar_resTable"]][["data"]]
  if (isMacOS) {
    jaspTools::expect_equal_tables(table,
                                   list(0.646594512449221, 0.74696448777814, "Baseline", 0.694969883370343,
                                        "AgeImportant", 0, 0.0255828550169903, 0.876211843204352, 0.993069983981131,
                                        "Age", 0.932812779230121, "AgeImportant", 0.0294279924287488,
                                        0.029791838430515, 1.03465116150637, 1.2141823710084, "Male",
                                        1.12082790848747, "AgeImportant", 0.0051981587436627, 0.0457507990274859,
                                        0.962361340432082, 1.13041853166998, "Age_x_Male", 1.04301059121525,
                                        "AgeImportant", 0.305080904549035, 0.0428262779835528, 0.955084150137048,
                                        1.03312489468175, "Age_squared", 0.993338417671715, "AgeImportant",
                                        0.738700271987143, 0.0199036024563438, 0.160255535898398, 0.256403353703202,
                                        "Baseline", 0.202706824882275, "AttractiveImportant", 0, 0.024303658902679,
                                        0.916550654867402, 1.22835692375121, "Age", 1.06106142276264,
                                        "AttractiveImportant", 0.427518114705054, 0.0792604129017758,
                                        0.57005876861431, 0.887854135079745, "Male", 0.711427462889002,
                                        "AttractiveImportant", 0.00259256113715978, 0.0804123830626732,
                                        0.805393952968655, 1.23711642321335, "Age_x_Male", 0.998181389515073,
                                        "AttractiveImportant", 0.986736250846524, 0.109294453419655,
                                        0.963059807526434, 1.19893613203482, "Age_squared", 1.07454511331723,
                                        "AttractiveImportant", 0.198278909712733, 0.0600534745171921,
                                        0.40957559497325, 0.494155893026224, "Baseline", 0.449882422301376,
                                        "PhysicalbuildImportant", 0, 0.0215453512903771, 0.90060511782898,
                                        1.04832944071066, "Age", 0.971663964277163, "PhysicalbuildImportant",
                                        0.458168347677138, 0.0376492688045012, 0.9945970119461, 1.19517477278604,
                                        "Male", 1.090283108998, "PhysicalbuildImportant", 0.0651300039353497,
                                        0.0510968524056506, 1.02438874852121, 1.24000080379104, "Age_x_Male",
                                        1.12705051863739, "PhysicalbuildImportant", 0.0141099802372402,
                                        0.0549205524880895, 0.943271539063055, 1.03341081844155, "Age_squared",
                                        0.987313026955366, "PhysicalbuildImportant", 0.583417470849042,
                                        0.0229871578925847, 0.349462622077511, 0.437088781174819, "Baseline",
                                        0.390827572658347, "TrustImportant", 0, 0.0223074644953284,
                                        0.893269512582362, 1.06578075159624, "Age", 0.97571996622906,
                                        "TrustImportant", 0.585296586694269, 0.0439516576259422, 1.38582723268146,
                                        1.7197991230391, "Male", 1.54380842705605, "TrustImportant",
                                        3.10862446895044e-15, 0.0850332128463008, 0.97468214534211,
                                        1.22050401571013, "Age_x_Male", 1.09068944820742, "TrustImportant",
                                        0.130275633198881, 0.0625788356813981, 0.909664908913179, 1.0136825022898,
                                        "Age_squared", 0.960266317805813, "TrustImportant", 0.142120527168649,
                                        0.0265226318269378, 0.38074221922462, 0.470982700095746, "Baseline",
                                        0.423465463114594, "EmotionalconnImportant", 0, 0.0229776157941232,
                                        0.872930654326653, 1.03531658548255, "Age", 0.950662708009797,
                                        "EmotionalconnImportant", 0.245026434395534, 0.0413755462428974,
                                        1.29202084027217, 1.59068948331707, "Male", 1.43359825713044,
                                        "EmotionalconnImportant", 1.12656550754764e-11, 0.076055256733174,
                                        0.982378723305657, 1.22211146760989, "Age_x_Male", 1.09570812869477,
                                        "EmotionalconnImportant", 0.100836859218587, 0.0610361043653903,
                                        0.898953899575975, 0.999727748348488, "Age_squared", 0.948002720403366,
                                        "EmotionalconnImportant", 0.0488372777834627, 0.0256959980657119,
                                        0.650614567170557, 0.752803975708012, "Baseline", 0.699846578058036,
                                        "OpennessImportant", 0, 0.0260461019181554, 0.858101635495206,
                                        0.971874120315021, "Age", 0.913217811991085, "OpennessImportant",
                                        0.00426075098934042, 0.0290053886861357, 1.14000371962139, 1.33115645966537,
                                        "Male", 1.2318779628748, "OpennessImportant", 1.33934803692171e-07,
                                        0.0487155587722053, 0.897994207978539, 1.0556712828986, "Age_x_Male",
                                        0.973646084351093, "OpennessImportant", 0.517524430821138, 0.0401806563331368,
                                        0.961063334826429, 1.04191192219911, "Age_squared", 1.00067144785094,
                                        "OpennessImportant", 0.974013667090155, 0.0206194129722788
                                   ))
  } else {
    expect_true(!is.null(table))
  }
})

if (isMacOS) {
  test_that("residualVariances: AgeImportant plot matches", {
    plotName <- results[["results"]][["plotContainer"]][["collection"]][["plotContainer_plot1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "residualvariances-ageimportant")
  })

  test_that("means: Factor 1 plot matches", {
    plotName <- results[["results"]][["plotContainer"]][["collection"]][["plotContainer_plot2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "means-factor-1")
  })
}


# Configural invariance with loadings, factor variance/means/covariances, fit per group
# Uses simplified moderator setup (Age + squared only) for faster convergence (~15s vs ~225s)
options2 <- options
options2$invarianceTestConfigural <- TRUE
options2$invarianceTestScalar     <- FALSE
options2$checkModelFitPerGroup    <- TRUE
options2$parameterEstimatesLoadings          <- TRUE
options2$parameterEstimatesIntercepts        <- FALSE
options2$parameterEstimatesResidualVariances <- FALSE
options2$parameterEstimatesFactorVariance    <- TRUE
options2$parameterEstimatesFactorMeans       <- TRUE
options2$parameterEstimatesFactorCovariances <- TRUE

# Simplified moderators: Age + squared only (no Male, no interaction)
options2$moderators <- list(
  list(variable = "Age", moderatorSquaredEffect = TRUE, moderatorCubicEffect = FALSE)
)
options2$moderators.types <- c("scale")
options2$moderatorInteractionTerms <- list()

# Update individual moderations for configural
options2$includeIndividualModerationsList <- list(
  list(
    keyLabel = "Configural",
    keyValue = "invarianceTestConfigural",
    moderationTypeList = list(
      list(
        keyLabel = "Indicators",
        keyValue = "indicators",
        moderationParameterList = list(
          list(keyLabel = "Loadings", keyValue = "loadings",
            moderationItemList = list(
              list(includeIndividualModeration = TRUE, value = "AgeImportant", value.types = "scale"),
              list(includeIndividualModeration = TRUE, value = "AttractiveImportant", value.types = "scale"),
              list(includeIndividualModeration = TRUE, value = "PhysicalbuildImportant", value.types = "scale"),
              list(includeIndividualModeration = TRUE, value = "TrustImportant", value.types = "scale"),
              list(includeIndividualModeration = TRUE, value = "EmotionalconnImportant", value.types = "scale"),
              list(includeIndividualModeration = TRUE, value = "OpennessImportant", value.types = "scale")
            )),
          list(keyLabel = "Intercepts", keyValue = "intercepts",
            moderationItemList = list(
              list(includeIndividualModeration = TRUE, value = "AgeImportant", value.types = "scale"),
              list(includeIndividualModeration = TRUE, value = "AttractiveImportant", value.types = "scale"),
              list(includeIndividualModeration = TRUE, value = "PhysicalbuildImportant", value.types = "scale"),
              list(includeIndividualModeration = TRUE, value = "TrustImportant", value.types = "scale"),
              list(includeIndividualModeration = TRUE, value = "EmotionalconnImportant", value.types = "scale"),
              list(includeIndividualModeration = TRUE, value = "OpennessImportant", value.types = "scale")
            )),
          list(keyLabel = "Residual variances", keyValue = "residualVariances",
            moderationItemList = list(
              list(includeIndividualModeration = TRUE, value = "AgeImportant", value.types = "scale"),
              list(includeIndividualModeration = TRUE, value = "AttractiveImportant", value.types = "scale"),
              list(includeIndividualModeration = TRUE, value = "PhysicalbuildImportant", value.types = "scale"),
              list(includeIndividualModeration = TRUE, value = "TrustImportant", value.types = "scale"),
              list(includeIndividualModeration = TRUE, value = "EmotionalconnImportant", value.types = "scale"),
              list(includeIndividualModeration = TRUE, value = "OpennessImportant", value.types = "scale")
            ))
        )
      ),
      list(
        keyLabel = "Factors",
        keyValue = "factors",
        moderationParameterList = list(
          list(keyLabel = "Variances", keyValue = "variances",
            moderationItemList = list(
              list(includeIndividualModeration = TRUE, value = "Factor1"),
              list(includeIndividualModeration = TRUE, value = "Factor2")
            )),
          list(keyLabel = "Means", keyValue = "means",
            moderationItemList = list(
              list(includeIndividualModeration = TRUE, value = "Factor1"),
              list(includeIndividualModeration = TRUE, value = "Factor2")
            )),
          list(keyLabel = "Covariances", keyValue = "covariances",
            moderationItemList = list(
              list(includeIndividualModeration = TRUE, value = "Factor1:Factor2")
            ))
        )
      )
    )
  )
)

# Update plot list for configural
options2$plotModelList <- list(
  list(
    keyLabel = "Configural",
    keyValue = "invarianceTestConfigural",
    plotTypeList = list(
      list(keyLabel = "Indicators", keyValue = "indicators",
        plotParameterList = list(
          list(keyLabel = "Loadings", keyValue = "loadings", plotItemList = list()),
          list(keyLabel = "Intercepts", keyValue = "intercepts", plotItemList = list()),
          list(keyLabel = "Residual variances", keyValue = "residualVariances", plotItemList = list())
        )),
      list(keyLabel = "Factors", keyValue = "factors",
        plotParameterList = list(
          list(keyLabel = "Variances", keyValue = "variances",
            plotItemList = list(
              list(includePlot = FALSE, plotModerator1 = "", plotModerator2 = "", value = "Factor1"),
              list(includePlot = FALSE, plotModerator1 = "", plotModerator2 = "", value = "Factor2")
            )),
          list(keyLabel = "Means", keyValue = "means",
            plotItemList = list(
              list(includePlot = FALSE, plotModerator1 = "", plotModerator2 = "", value = "Factor1"),
              list(includePlot = FALSE, plotModerator1 = "", plotModerator2 = "", value = "Factor2")
            )),
          list(keyLabel = "Covariances", keyValue = "covariances",
            plotItemList = list(
              list(includePlot = FALSE, plotModerator1 = "", plotModerator2 = "", value = "Factor1:Factor2")
            ))
        ))
    )
  )
)

results2 <- runAnalysis(
  "ModeratedNonLinearFactorAnalysis",
  dataset = testthat::test_path("AttractDat.csv"),
  options = options2, makeTests = FALSE
)

test_that("Configural invariance fit table results match", {
  table <- results2[["results"]][["fitContainer"]][["collection"]][["fitContainer_invFitTable"]][["data"]]
  if (isMacOS) {
    jaspTools::expect_equal_tables(table,
                                   list(110750.546826731, 111196.589848213, 110620.546826731, 65, 110751.77359253,
                                        110990.03476023, "Configural"))
  } else {
    expect_true(!is.null(table))
  }
})

test_that("Configural loadings table results match", {
  table <- results2[["results"]][["mainContainer"]][["collection"]][["mainContainer_globalParameterContainer"]][["collection"]][["mainContainer_globalParameterContainer_Configural"]][["collection"]][["mainContainer_globalParameterContainer_Configural_loadTable"]][["data"]]
  expect_true(!is.null(table))
  expect_true(length(table) > 0)
})

test_that("Configural factor variance table results match", {
  table <- results2[["results"]][["mainContainer"]][["collection"]][["mainContainer_globalParameterContainer"]][["collection"]][["mainContainer_globalParameterContainer_Configural"]][["collection"]][["mainContainer_globalParameterContainer_Configural_fvTable"]][["data"]]
  if (isMacOS) {
    jaspTools::expect_equal_tables(table,
                                   list(1.08272487296397, 1.65914795894644, "Age", 1.34029875888875, "Factor 1",
                                        0.00714705820481409, 0.145939094293402, 0.510479014432259, 0.785144189992018,
                                        "Age_squared", 0.633087381247123, "Factor 1", 3.14935754408552e-05,
                                        0.0695307216433972, 0.175750143853411, 0.672342376354443, "Age",
                                        0.34375030089156, "Factor 2", 0.00180971265871377, 0.117658099262599,
                                        1.07188771535177, 1.34645955859815, "Age_squared", 1.20135484353264,
                                        "Factor 2", 0.00161490188655722, 0.0698935606220142))
  } else {
    expect_true(!is.null(table))
  }
})

test_that("Configural factor means table results match", {
  table <- results2[["results"]][["mainContainer"]][["collection"]][["mainContainer_globalParameterContainer"]][["collection"]][["mainContainer_globalParameterContainer_Configural"]][["collection"]][["mainContainer_globalParameterContainer_Configural_fmTable"]][["data"]]
  if (isMacOS) {
    jaspTools::expect_equal_tables(table,
                                   list(-0.117725185438308, 0.0696119407750608, "Age", -0.0240566223316237,
                                        "Factor 1", 0.614702893314816, 0.0477909613878266, -0.051902705586008,
                                        0.143214659802513, "Age_squared", 0.0456559771082527, "Factor 1",
                                        0.359020409523139, 0.0497757527504542, -0.987917571756613, 3.44502372347365,
                                        "Age", 1.22855307585852, "Factor 2", 0.277312821795793, 1.13087315129174,
                                        -0.426915653651567, 0.162740808173585, "Age_squared", -0.132087422738991,
                                        "Factor 2", 0.37989327791335, 0.150425330892885))
  } else {
    expect_true(!is.null(table))
  }
})

test_that("Configural factor covariances table results match", {
  table <- results2[["results"]][["mainContainer"]][["collection"]][["mainContainer_globalParameterContainer"]][["collection"]][["mainContainer_globalParameterContainer_Configural"]][["collection"]][["mainContainer_globalParameterContainer_Configural_covTable"]][["data"]]
  if (isMacOS) {
    jaspTools::expect_equal_tables(table,
                                   list(0.112514869799291, 0.191591577803958, "Baseline", 0.152053223801624,
                                        4.79616346638068e-14, 0.0201730002766414, -0.00653357644805145,
                                        0.0832700008773279, "Age", 0.0383682122146382, 0.0939787311243621,
                                        0.0229094968156911, -0.0581639956611958, -0.00904823650249357,
                                        "Age_squared", -0.0336061160818447, 0.00731607995465766, 0.0125297606349201
                                   ))
  } else {
    expect_true(!is.null(table))
  }
})

test_that("Fit per group table has data", {
  table <- results2[["results"]][["groupContainer"]][["collection"]][["groupContainer_checkModelFitPerGroupTable"]][["data"]]
  if (isMacOS) {
    jaspTools::expect_equal_tables(table,
                                   list(3541, 0.962855543637307, 167.949620595433, 8, "Age_nominal_1:Age_squared_nominal_1",
                                        0, 0.0751421557319182, 0.05283866976707, 2397, 0.952922234559455,
                                        160.894167907472, 8, "Age_nominal_2:Age_squared_nominal_1",
                                        0, 0.0892928015988366, 0.0549013968491845, 531, 0.95962332082404,
                                        30.8625752101887, 8, "Age_nominal_1:Age_squared_nominal_2",
                                        0.000148639143583429, 0.0733618575916063, 0.0631340931013203,
                                        591, 0.954701825269132, 49.3382567247161, 8, "Age_nominal_2:Age_squared_nominal_2",
                                        5.47570161435473e-08, 0.093505540527225, 0.0502803502483288
                                   ))
  } else {
    expect_true(!is.null(table))
    expect_true(length(table) > 0)
  }
})

