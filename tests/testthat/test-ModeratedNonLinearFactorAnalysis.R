
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
      moderatorCubicEffect   = TRUE
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
                                   list(110531.169461421, 111038.972285877, 110383.169461421, 74, 110532.758580963,
                                        110532.758580963, 42286, "Scalar"))
  } else {
    expect_true(!is.null(table))
  }
})

test_that("Intercepts table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_globalParameterContainer"]][["collection"]][["mainContainer_globalParameterContainer_Scalar"]][["collection"]][["mainContainer_globalParameterContainer_Scalar_intTable"]][["data"]]
  if (isMacOS) {
    jaspTools::expect_equal_tables(table,
                                   list(-0.0574959325724069, 0.00858529576511022, "Baseline", -0.0244553184036484,
                                        "AgeImportant", 0.146867426098622, 0.016857765973956, -0.0969593150713163,
                                        -0.00442882328463772, "Baseline", -0.050694069177977, "AttractiveImportant",
                                        0.0317467351348397, 0.0236051510427098, -0.0849095708331634,
                                        -0.00674023135599885, "Baseline", -0.0458249010945811, "PhysicalbuildImportant",
                                        0.0215638502029796, 0.0199415244600805, 0.192757363047004, 0.268162056302428,
                                        "Baseline", 0.230459709674716, "TrustImportant", 0, 0.0192362446070965,
                                        0.202870994340379, 0.279373564060625, "Baseline", 0.241122279200502,
                                        "EmotionalconnImportant", 0, 0.0195163202802931, 0.124731716073872,
                                        0.187235451523482, "Baseline", 0.155983583798677, "OpennessImportant",
                                        0, 0.0159451234672246))
  } else {
    expect_true(!is.null(table))
  }
})

test_that("Residual Variances table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_globalParameterContainer"]][["collection"]][["mainContainer_globalParameterContainer_Scalar"]][["collection"]][["mainContainer_globalParameterContainer_Scalar_resTable"]][["data"]]
  if (isMacOS) {
    jaspTools::expect_equal_tables(table,
                                   list(0.646613570928863, 0.746942520365854, "Baseline", 0.69496990609117,
                                        "AgeImportant", 0, 0.0255724162076593, 0.876203308137081, 0.993081035957556,
                                        "Age", 0.932813426658413, "AgeImportant", 0.0294566632287228,
                                        0.0297968254689069, 0.018696700344167, 59.948340621373, "Male",
                                        1.05869549953132, "AgeImportant", 0.977905004349961, 2.18032930370976,
                                        0.962347970751609, 1.13043501708274, "Age_x_Male", 1.04301095140758,
                                        "AgeImportant", 0.30516249516382, 0.0428338696493892, 0.955087497791614,
                                        1.03312141912085, "Age_squared", 0.993338487678319, "AgeImportant",
                                        0.738680907515042, 0.0199018631501982, 0.0186961206658338, 59.9495388938197,
                                        "Male_cubic", 1.05868966794805, "AgeImportant", 0.977907277101567,
                                        2.18033106599586, 0.16018515305409, 0.256518954835628, "Baseline",
                                        0.202707987118466, "AttractiveImportant", 0, 0.0243498242937901,
                                        0.916200146227112, 1.22883933257211, "Age", 1.06106681042816,
                                        "AttractiveImportant", 0.428699668156966, 0.0794706355415782,
                                        0.00243546316392896, 292.095974737424, "Male", 0.843438786637726,
                                        "AttractiveImportant", 0.954487967586071, 2.51631261990448,
                                        0.805002262117975, 1.23767378962502, "Age_x_Male", 0.998163413681483,
                                        "AttractiveImportant", 0.986634211205127, 0.109531052675441,
                                        0.962967460238558, 1.19903339492327, "Age_squared", 1.0745371762068,
                                        "AttractiveImportant", 0.198687544631483, 0.06010155460651,
                                        0.00243548473910476, 292.095320602336, "Male_cubic", 0.843441578113684,
                                        "AttractiveImportant", 0.954488808136095, 2.51631856001765,
                                        0.409521992757806, 0.494221387512306, "Baseline", 0.449882793044552,
                                        "PhysicalbuildImportant", 0, 0.0215756001720306, 0.900584049407487,
                                        1.0483567107155, "Age", 0.971665236467623, "PhysicalbuildImportant",
                                        0.458335009924217, 0.0376615648706567, 0.128489785196459, 8.48540624955039,
                                        "Male", 1.04416858136482, "PhysicalbuildImportant", 0.967748207677385,
                                        1.11617642981603, 1.02435090964077, 1.24004635617188, "Age_x_Male",
                                        1.12705040390454, "PhysicalbuildImportant", 0.014147242255802,
                                        0.0549417294464631, 0.943266671253661, 1.033420979503, "Age_squared",
                                        0.987315333284908, "PhysicalbuildImportant", 0.583548178928473,
                                        0.0229909879068547, 0.128490229187123, 8.48537925269738, "Male_cubic",
                                        1.04416872435385, "PhysicalbuildImportant", 0.96774805449042,
                                        1.11617481473402, 0.349478843293109, 0.437068912446706, "Baseline",
                                        0.390827759967549, "TrustImportant", 0, 0.0222983150407638,
                                        0.893368209301031, 1.06566255952306, "Age", 0.975719761263586,
                                        "TrustImportant", 0.584822512163115, 0.0438965424833342, "",
                                        "", "Male", 1.2425017983095, "TrustImportant", "", "", 0.974833108475544,
                                        1.22031936067618, "Age_x_Male", 1.09069139342935, "TrustImportant",
                                        0.129744669675369, 0.0624937554891678, 0.909671134324428, 1.01367438684366,
                                        "Age_squared", 0.960265759733051, "TrustImportant", 0.142060118759757,
                                        0.0265189787160148, "", "", "Male_cubic", 1.24250003473844,
                                        "TrustImportant", "", "", 0.38077583813856, 0.470940533770034,
                                        "Baseline", 0.423465201002049, "EmotionalconnImportant", 0,
                                        0.022958391144697, 0.8730436814598, 1.03518212287406, "Age",
                                        0.950662511901747, "EmotionalconnImportant", 0.24430673091729,
                                        0.0413126385926322, 0.0490288057585281, 29.240261814132, "Male",
                                        1.19733667646723, "EmotionalconnImportant", 0.912039665415409,
                                        1.95208985625982, 0.982557599187779, 1.22188646946155, "Age_x_Male",
                                        1.0957070027677, "EmotionalconnImportant", 0.100269639122296,
                                        0.0609336831510745, 0.898953691642321, 0.999725695311053, "Age_squared",
                                        0.948001637355947, "EmotionalconnImportant", 0.048828575639122,
                                        0.0256955280031787, 0.0490278389675028, 29.2401887440019, "Male_cubic",
                                        1.19732337533363, "EmotionalconnImportant", 0.912045305025684,
                                        1.95207343041251, 0.650635259367234, 0.75277839222326, "Baseline",
                                        0.699845814783678, "OpennessImportant", 0, 0.026034327953226,
                                        0.858112815220551, 0.971859474003081, "Age", 0.913216879626931,
                                        "OpennessImportant", 0.00425166421458867, 0.0289988129946106,
                                        0.0177754633832872, 69.3022026447576, "Male", 1.10990034034279,
                                        "OpennessImportant", 0.960574295034747, 2.34114361054104, 0.898010334324771,
                                        1.05565300271123, "Age_x_Male", 0.973646396745585, "OpennessImportant",
                                        0.517438125242917, 0.0401719076504857, 0.961056918230282, 1.04191987975002,
                                        "Age_squared", 1.00067192859369, "OpennessImportant", 0.973999680237143,
                                        0.0206230769303233, 0.0177755064039616, 69.3020212528661, "Male_cubic",
                                        1.10990023091619, "OpennessImportant", 0.960574308281833, 2.34114195335388
                                   ))
  } else {
    expect_true(!is.null(table))
  }
})

test_that("means: Factor 1 plot matches", {
  plotName <- results[["results"]][["plotContainer"]][["collection"]][["plotContainer_plot2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  if (isMacOS) {
    jaspTools::expect_equal_plots(testPlot, "means-factor-1")
  } else {
    expect_true(!is.null(testPlot))
  }
})

test_that("residualVariances: AgeImportant plot matches", {
  plotName <- results[["results"]][["plotContainer"]][["collection"]][["plotContainer_plot2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  if (isMacOS) {
    jaspTools::expect_equal_plots(testPlot, "residualvariances-ageimportant")
  } else {
    expect_true(!is.null(testPlot))
  }
})
