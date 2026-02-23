
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
                                   list(110509.169461257, 110941.488082078, 110383.169461257, 63, 110510.32211992,
                                        110741.288535264, "Scalar"))
  } else {
    expect_true(!is.null(table))
  }
})

test_that("Intercepts table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_globalParameterContainer"]][["collection"]][["mainContainer_globalParameterContainer_Scalar"]][["collection"]][["mainContainer_globalParameterContainer_Scalar_intTable"]][["data"]]

  if (isMacOS) {
    jaspTools::expect_equal_tables(table, list(
      -0.0574861929045923, 0.00858128860752704, "Baseline", -0.0244524521485326,
      "AgeImportant", 0.146830715974896, 0.016854259066302, -0.0969402021426738,
      -0.00443810976979217, "Baseline", -0.050689155956233, "AttractiveImportant",
      0.0317108822473435, 0.0235979061611658, -0.0848970131371122,
      -0.00674261421971015, "Baseline", -0.0458198136784112, "PhysicalbuildImportant",
      0.0215533717641752, 0.0199377130227581, 0.192754727487206, 0.268168924536394,
      "Baseline", 0.2304618260118, "TrustImportant", 0, 0.0192386690888316,
      0.202874248917621, 0.279373669739374, "Baseline", 0.241123959328497,
      "EmotionalconnImportant", 0, 0.0195155169751003, 0.124729118354982,
      0.187241143728145, "Baseline", 0.155985131041563, "OpennessImportant",
      0, 0.0159472382825015
    ))
  } else {
    expect_true(!is.null(table))
  }
})

test_that("Residual Variances table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_globalParameterContainer"]][["collection"]][["mainContainer_globalParameterContainer_Scalar"]][["collection"]][["mainContainer_globalParameterContainer_Scalar_resTable"]][["data"]]
  if (isMacOS) {
    jaspTools::expect_equal_tables(table, list(0.646606652457164, 0.746950459656657, "Baseline", 0.694969881555978,
                                               "AgeImportant", 0, 0.0255761966839589, 0.876198923546318, 0.993084629277921,
                                               "Age", 0.932812780338964, "AgeImportant", 0.0294662256658471,
                                               0.0297988566783437, 1.03466637732652, 1.21416452100736, "Male",
                                               1.12082791115723, "AgeImportant", 0.00518990522715801, 0.0457423906327308,
                                               0.962345856508274, 1.13043672000159, "Age_x_Male", 1.04301059128771,
                                               "AgeImportant", 0.305177586274157, 0.0428348402552443, 0.955083751609445,
                                               1.0331253265621, "Age_squared", 0.993338418050797, "AgeImportant",
                                               0.738702963667453, 0.0199038141357453, 0.160179026727878, 0.25652583474152,
                                               "Baseline", 0.202706829040004, "AttractiveImportant", 0, 0.0243530498628984,
                                               0.916624372511569, 1.22825814099167, "Age", 1.06106142506865,
                                               "AttractiveImportant", 0.42726415472749, 0.0792168740980419,
                                               0.570159359998413, 0.887697466345509, "Male", 0.711427451876695,
                                               "AttractiveImportant", 0.00257212360357029, 0.0803483311790748,
                                               0.805737247787792, 1.23658933885069, "Age_x_Male", 0.998181391596376,
                                               "AttractiveImportant", 0.986709877434933, 0.109077420735927,
                                               0.96295578273375, 1.1990656400831, "Age_squared", 1.07454510933481,
                                               "AttractiveImportant", 0.198721334092077, 0.0601126943374251,
                                               0.409574331977041, 0.49415741251731, "Baseline", 0.449882420331446,
                                               "PhysicalbuildImportant", 0, 0.0215460580056503, 0.900593780583289,
                                               1.04834263452848, "Age", 0.971663962785821, "PhysicalbuildImportant",
                                               0.458242820464494, 0.0376555088378515, 0.994712677701923, 1.19503580763242,
                                               "Male", 1.09028311376437, "PhysicalbuildImportant", 0.0647902493017707,
                                               0.0510321670803993, 1.02438374681922, 1.24000685834447, "Age_x_Male",
                                               1.12705051866917, "PhysicalbuildImportant", 0.0141149050224374,
                                               0.05492336019721, 0.943260411130804, 1.03342301147815, "Age_squared",
                                               0.98731302770647, "PhysicalbuildImportant", 0.583514805265072,
                                               0.022993101038091, 0.349503241275549, 0.437037976472422, "Baseline",
                                               0.390827569827947, "TrustImportant", 0, 0.0222842866765284,
                                               0.893392206579228, 1.06563438138005, "Age", 0.975719965660134,
                                               "TrustImportant", 0.584712191201693, 0.0438832837679643, 1.38615365582676,
                                               1.71939416566908, "Male", 1.54380844295832, "TrustImportant",
                                               2.66453525910038e-15, 0.0848477123883031, 0.974863651163081,
                                               1.22027677478423, "Age_x_Male", 1.09068944805369, "TrustImportant",
                                               0.129639432711541, 0.0624752164183595, 0.909661973010481, 1.01368577395058,
                                               "Age_squared", 0.960266317822583, "TrustImportant", 0.142144302386552,
                                               0.0265242131004608, 0.380792286960501, 0.470920774568827, "Baseline",
                                               0.423465463438607, "EmotionalconnImportant", 0, 0.0229492061710526,
                                               0.87293239449885, 1.03531453023845, "Age", 0.950662711975443,
                                               "EmotionalconnImportant", 0.24501546761672, 0.041374581519101,
                                               1.29224614409905, 1.59041213541992, "Male", 1.43359825248384,
                                               "EmotionalconnImportant", 1.04087849450707e-11, 0.0759277158352719,
                                               0.982411767300974, 1.22207033715356, "Age_x_Male", 1.09570811792609,
                                               "EmotionalconnImportant", 0.100731910444214, 0.061017294147974,
                                               0.898967854396623, 0.999712236285522, "Age_squared", 0.948002723660458,
                                               "EmotionalconnImportant", 0.048771354040301, 0.0256884914579814,
                                               0.650607824676498, 0.752811785554339, "Baseline", 0.69984658189516,
                                               "OpennessImportant", 0, 0.0260498044623598, 0.858097909980456,
                                               0.971878339477629, "Age", 0.913217811839557, "OpennessImportant",
                                               0.004263428126424, 0.0290074115057278, 1.14003952241099, 1.33111464504171,
                                               "Male", 1.23187795832527, "OpennessImportant", 1.32382763640138e-07,
                                               0.0486958173517397, 0.898000773195294, 1.05566356409664, "Age_x_Male",
                                               0.97364608395088, "OpennessImportant", 0.517486564884311, 0.0401770242701764,
                                               0.961050252152221, 1.04192610395085, "Age_squared", 1.00067144704241,
                                               "OpennessImportant", 0.974022450885988, 0.0206263626404878))
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
                                   list(109630.101875, 110028.109494168, 109514.101875, 58, 109631.079449632,
                                        109843.798800276, "Configural"))
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
                                   list(0.532676683524698, 0.77837493292571, "Age", 0.643911622670088,
                                        "Factor 1", 5.38198031918569e-06, 0.0623051274748882, 0.112103086409582,
                                        6.3894530930541, "Male", 0.846331738859306, "Factor 1", 0.871490435084536,
                                        0.872900377580528, 0.711065357738871, 1.19878443686349, "Age_x_Male",
                                        0.92326273857994, "Factor 1", 0.549025374478376, 0.12301736471709,
                                        1.01857363320427, 1.18888253846439, "Age_squared", 1.10043827934909,
                                        "Factor 1", 0.0152430729707058, 0.0434037038641479, "", "",
                                        "Age", 0.359898335678741, "Factor 2", "", "", "", "", "Male",
                                        0.456223975800164, "Factor 2", "", "", "", "", "Age_x_Male",
                                        0.987560238529586, "Factor 2", "", "", "", "", "Age_squared",
                                        1.29247653291302, "Factor 2", "", ""))
  } else {
    expect_true(!is.null(table))
  }
})

test_that("Configural factor means table results match", {
  table <- results2[["results"]][["mainContainer"]][["collection"]][["mainContainer_globalParameterContainer"]][["collection"]][["mainContainer_globalParameterContainer_Configural"]][["collection"]][["mainContainer_globalParameterContainer_Configural_fmTable"]][["data"]]
  if (isMacOS) {
    jaspTools::expect_equal_tables(table,
                                   list("", "", "Age", 1.86976762318969, "Factor 1", "", "", "", "", "Male",
                                        -1.70257279018573, "Factor 1", "", "", -2.16817681506917, 1.06637907952546,
                                        "Age_x_Male", -0.550898867771854, "Factor 1", 0.504370330395776,
                                        0.825156972298571, -0.171566542276445, 0.123473953353127, "Age_squared",
                                        -0.0240462944616587, "Factor 1", 0.749362029127892, 0.0752668156039635,
                                        "", "", "Age", 0.781167148919985, "Factor 2", "", "", "", "",
                                        "Male", 1.30503314816448, "Factor 2", "", "", "", "", "Age_x_Male",
                                        -0.164820615408313, "Factor 2", "", "", "", "", "Age_squared",
                                        -0.0594753746606537, "Factor 2", "", ""))
  } else {
    expect_true(!is.null(table))
  }
})

test_that("Configural factor covariances table results match", {
  table <- results2[["results"]][["mainContainer"]][["collection"]][["mainContainer_globalParameterContainer"]][["collection"]][["mainContainer_globalParameterContainer_Configural"]][["collection"]][["mainContainer_globalParameterContainer_Configural_covTable"]][["data"]]
  if (isMacOS) {
    jaspTools::expect_equal_tables(table,
                                   list(0.0561400914299918, 0.199429211144, "Baseline", 0.127784651286996,
                                        0.000472685785173788, 0.0365540185544875, "", "", "Age", -0.0263198772566827,
                                        "", "", "", "", "Male", -0.0154432858963612, "", "", -0.106516821915126,
                                        0.0547054736300884, "Age_x_Male", -0.0259056741425186, 0.528782526524825,
                                        0.0411288923717259, "", "", "Age_squared", 0.0250047188491346,
                                        "", ""))
  } else {
    expect_true(!is.null(table))
  }
})

test_that("Fit per group table has data", {
  table <- results2[["results"]][["groupContainer"]][["collection"]][["groupContainer_checkModelFitPerGroupTable"]][["data"]]
  if (isMacOS) {
    jaspTools::expect_equal_tables(table,
                                   list(1628, 0.97778795824664, 49.0228529406467, 8, "Male_0:Age_nominal_1:Age_x_Male_nominal_1:Age_squared_nominal_1",
                                        6.2939756739766e-08, 0.0561229817199524, 0.0407322258776629,
                                        643, 0.982043505706005, 23.8330889295756, 8, "Male_0:Age_nominal_2:Age_x_Male_nominal_1:Age_squared_nominal_1",
                                        0.00244418442668215, 0.0554794753664046, 0.047719532797256,
                                        317, 0.97809319056567, 14.8789242756421, 8, "Male_0:Age_nominal_1:Age_x_Male_nominal_1:Age_squared_nominal_2",
                                        0.0615429008069905, 0.0520817585380339, 0.0503364248041806,
                                        97, 0.940577053943037, 16.0630238785912, 8, "Male_0:Age_nominal_2:Age_x_Male_nominal_1:Age_squared_nominal_2",
                                        0.0414868785936976, 0.101933775996507, 0.0695680121995393, 1754,
                                        0.947009442037605, 128.003790771617, 8, "Male_1:Age_nominal_2:Age_x_Male_nominal_2:Age_squared_nominal_1",
                                        0, 0.0924778436482069, 0.0546765358580967, 1913, 0.97198620904555,
                                        74.3387163683241, 8, "Male_1:Age_nominal_1:Age_x_Male_nominal_1:Age_squared_nominal_1",
                                        6.69020394639119e-13, 0.0658386706429718, 0.0461067737321225,
                                        214, 0.940578327178694, 22.8259958861586, 8, "Male_1:Age_nominal_1:Age_x_Male_nominal_1:Age_squared_nominal_2",
                                        0.00359498010500803, 0.0930593591533234, 0.0827842870984544,
                                        494, 0.951760716536376, 44.5920261766308, 8, "Male_1:Age_nominal_2:Age_x_Male_nominal_2:Age_squared_nominal_2",
                                        4.39663623064135e-07, 0.0962243001293451, 0.0508178877899054))
  } else {
    expect_true(!is.null(table))
    expect_true(length(table) > 0)
  }
})

