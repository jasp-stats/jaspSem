
context("MNLFA")
options <- list(

  ## ===============================
  ## General switches
  ## ===============================
  addGroupVariableToData = FALSE,
  checkFitPerGroup       = FALSE,
  syncAnalysisBox        = TRUE,

  configuralInvariance = FALSE,
  metricInvariance     = FALSE,
  scalarInvariance     = TRUE,
  strictInvariance     = FALSE,
  customInvariance     = FALSE,

  continuousVariableSplit = 2,

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
      moderatorSquaredEffect = FALSE,
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
      moderatorInteractionTermsInclude = FALSE
    )
  ),

  ## ===============================
  ## Individual moderation inclusion
  ## ===============================
  includeIndividualModerationsList = list(

    list(
      keyLabel = "Scalar",
      keyValue = "scalarInvariance",

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
  loadingEstimates          = FALSE,
  interceptEstimates        = TRUE,
  residualVarianceEstimates = TRUE,
  factorVarianceEstimates   = FALSE,
  factorMeanEstimates       = FALSE,
  factorCovarianceEstimates = FALSE,

  parameterAlphaLevel = 0.05,

  ## ===============================
  ## Plot options
  ## ===============================
  plotWidth  = 480,
  plotHeight = 320,

  plotModelList = list(

    list(
      keyLabel = "Scalar",
      keyValue = "scalarInvariance",

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
                  includePlot     = FALSE,
                  plotModerator1 = "",
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
  options = options, makeTests = F
)


test_that("Global Invariance Fit table results match", {
  table <- results[["results"]][["fitContainer"]][["collection"]][["fitContainer_invFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(110563.562680751, 110844.912894301, 110481.562680751, 41, 110564.053418853,
                                      110564.053418853, 42319, "Scalar"))
})

test_that("Intercepts table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_globalParameterContainer"]][["collection"]][["mainContainer_globalParameterContainer_Scalar"]][["collection"]][["mainContainer_globalParameterContainer_Scalar_intTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.054869697912462, 0.00464425622310493, "Baseline", -0.0251127208446785,
                                      "AgeImportant", 0.0981139369331607, 0.0151824101373815, -0.0930382063712298,
                                      -0.0152648503937016, "Baseline", -0.0541515283824657, "AttractiveImportant",
                                      0.00634608422144978, 0.0198405064049632, -0.0821945802745531,
                                      -0.015565523834727, "Baseline", -0.04888005205464, "PhysicalbuildImportant",
                                      0.00403111472362938, 0.0169975206088958, 0.217962188262746,
                                      0.281449770750464, "Baseline", 0.249705979506605, "TrustImportant",
                                      0, 0.016196109466424, 0.226928780768059, 0.292063146781522,
                                      "Baseline", 0.25949596377479, "EmotionalconnImportant", 0, 0.0166162150241623,
                                      0.140128027302089, 0.197244111821308, "Baseline", 0.168686069561699,
                                      "OpennessImportant", 0, 0.0145706974642758))
})

test_that("Residual Variances table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_globalParameterContainer"]][["collection"]][["mainContainer_globalParameterContainer_Scalar"]][["collection"]][["mainContainer_globalParameterContainer_Scalar_resTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.658266300999997, 0.744258127740461, "Baseline", 0.69994288676784,
                                      "AgeImportant", 0, 0.021923320847036, 0.926279384087875, 0.99705164287982,
                                      "Age", 0.961014246341085, "AgeImportant", 0.0342455795276513,
                                      0.0180504020811187, 1.03099268181534, 1.2076883600745, "Male",
                                      1.11584849381553, "AgeImportant", 0.00660126554249341, 0.0450292934278078,
                                      0.164547199973699, 0.251397203629099, "Baseline", 0.203388067345127,
                                      "AttractiveImportant", 0, 0.0219910495279841, 0.919453376017568,
                                      1.17423769281281, "Age", 1.03906535449115, "AttractiveImportant",
                                      0.539116556935572, 0.0648354519693629, 0.548317810806964, 0.889772925361919,
                                      "Male", 0.698482886368561, "AttractiveImportant", 0.00366522084635412,
                                      0.0862626579160926, 0.432106967722182, 0.504164693061706, "Baseline",
                                      0.466747337165921, "PhysicalbuildImportant", 0, 0.0183642040622242,
                                      1.0120080241379, 1.10305629714759, "Age", 1.05655185570288,
                                      "PhysicalbuildImportant", 0.0123110808744891, 0.0232198461146851,
                                      0.972040473566882, 1.1609075803424, "Male", 1.06228487429851,
                                      "PhysicalbuildImportant", 0.182230716477718, 0.0481180346934268,
                                      0.348151567767944, 0.422874458827597, "Baseline", 0.383698326566391,
                                      "TrustImportant", 0, 0.0190323175203308, 0.974701289016805,
                                      1.0803467568156, "Age", 1.02616537480724, "TrustImportant",
                                      0.325174228573886, 0.0269389816004733, 1.36600693933502, 1.69038712441676,
                                      "Male", 1.51956590581516, "TrustImportant", 1.37667655053519e-14,
                                      0.0825952467464258, 0.372459688629779, 0.448927378274475, "Baseline",
                                      0.408909955282937, "EmotionalconnImportant", 0, 0.0194791091228891,
                                      0.952648177180978, 1.05295405152283, "Age", 1.00154618357745,
                                      "EmotionalconnImportant", 0.951760280017268, 0.0255780213102518,
                                      1.29048129779859, 1.58366921329249, "Male", 1.42957878469617,
                                      "EmotionalconnImportant", 7.7706729939564e-12, 0.0746637474664503,
                                      0.655119812180591, 0.738870882675624, "Baseline", 0.695736267477959,
                                      "OpennessImportant", 0, 0.0213525825013775, 0.865949064629713,
                                      0.933761789569418, "Age", 0.899216407915583, "OpennessImportant",
                                      3.32959231297281e-08, 0.0172953856703595, 1.14417860944263,
                                      1.33324645925353, "Male", 1.23510002825399, "OpennessImportant",
                                      6.2225004926475e-08, 0.0481855110035902))
})

test_that("means: Factor 1 plot matches", {
  plotName <- results[["results"]][["plotContainer"]][["collection"]][["plotContainer_plot1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "means-factor-1")
})
