
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
    jaspTools::expect_equal_tables(table,
                                   list(-0.0574912403950574, 0.0085863252558806, "Baseline", -0.0244524575695884,
                                        "AgeImportant", 0.146892305808439, 0.0168568315979654, -0.0969545190672014,
                                        -0.0044237993915731, "Baseline", -0.0506891592293873, "AttractiveImportant",
                                        0.0317636982064078, 0.0236052091787142, -0.084905310096424,
                                        -0.00673432500692499, "Baseline", -0.0458198175516745, "PhysicalbuildImportant",
                                        0.0215811196053493, 0.0199419442668594, 0.192748630590698, 0.268175017585384,
                                        "Baseline", 0.230461824088041, "TrustImportant", 0, 0.0192417788259478,
                                        0.202865833315281, 0.279382080532382, "Baseline", 0.241123956923832,
                                        "EmotionalconnImportant", 0, 0.0195198095017694, 0.124725597400328,
                                        0.18724466012686, "Baseline", 0.155985128763594, "OpennessImportant",
                                        0, 0.0159490335586965))
  } else {
    expect_true(!is.null(table))
  }
})

test_that("Residual Variances table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_globalParameterContainer"]][["collection"]][["mainContainer_globalParameterContainer_Scalar"]][["collection"]][["mainContainer_globalParameterContainer_Scalar_resTable"]][["data"]]
  if (isMacOS) {
    jaspTools::expect_equal_tables(table,
                                   list(0.646617419386937, 0.746938028965407, "Baseline", 0.694969884765935,
                                        "AgeImportant", 0, 0.0255702941728258, 0.876205188422703, 0.993077525138524,
                                        "Age", 0.932812778660516, "AgeImportant", 0.0294476767937393,
                                        0.0297954528256756, 1.03465817386015, 1.21417413439009, "Male",
                                        1.12082790500427, "AgeImportant", 0.00519435321756578, 0.0457469213270295,
                                        0.962347305988228, 1.13043501512127, "Age_x_Male", 1.04301059026106,
                                        "AgeImportant", 0.305168541777731, 0.0428340381564082, 0.955092413003307,
                                        1.03311595781027, "Age_squared", 0.993338418192528, "AgeImportant",
                                        0.738644824084827, 0.0198992180682056, 0.160221999696647, 0.256457025346488,
                                        "Baseline", 0.202706826321335, "AttractiveImportant", 0, 0.0243253052565871,
                                        0.916729172371656, 1.22811772040752, "Age", 1.0610614220695,
                                        "AttractiveImportant", 0.426902848770568, 0.0791549800028521,
                                        0.569994814922505, 0.887953798040529, "Male", 0.711427481036435,
                                        "AttractiveImportant", 0.00260561978905893, 0.0804531186044653,
                                        0.805665741222132, 1.23669909442734, "Age_x_Male", 0.998181392623879,
                                        "AttractiveImportant", 0.986715389375137, 0.109122620820344,
                                        0.963081659979181, 1.19890890467061, "Age_squared", 1.07454510285701,
                                        "AttractiveImportant", 0.198185979213279, 0.0600410286477228,
                                        0.409549273802495, 0.494187649364298, "Baseline", 0.449882421216155,
                                        "PhysicalbuildImportant", 0, 0.0215601021791471, 0.900606808793065,
                                        1.04832748218099, "Age", 0.971663968816942, "PhysicalbuildImportant",
                                        0.458157334303579, 0.0376483404726608, 0.994587066966276, 1.1951867182278,
                                        "Male", 1.09028310661004, "PhysicalbuildImportant", 0.0651592465528246,
                                        0.0511024133204324, 1.02436964742578, 1.2400239082974, "Age_x_Male",
                                        1.12705051073239, "PhysicalbuildImportant", 0.0141287917292776,
                                        0.0549312704896051, 0.943275749242377, 1.03340620853736, "Age_squared",
                                        0.987313028188021, "PhysicalbuildImportant", 0.583380692146222,
                                        0.0229849101704226, 0.349480484201915, 0.437066463984075, "Baseline",
                                        0.390827582779892, "TrustImportant", 0, 0.0222972782611187,
                                        0.89342839877627, 1.06559121099502, "Age", 0.975719964636035,
                                        "TrustImportant", 0.584539508785214, 0.0438631162089887, 1.38599731877586,
                                        1.71958799238274, "Male", 1.54380839058532, "TrustImportant",
                                        2.88657986402541e-15, 0.0849365252000292, 0.974904678959461,
                                        1.22022544303957, "Age_x_Male", 1.09068945800565, "TrustImportant",
                                        0.129495675520032, 0.062451802534707, 0.90965536446832, 1.01369312616757,
                                        "Age_squared", 0.9602663120942, "TrustImportant", 0.142197716761232,
                                        0.0265277693704309, 0.380776959532815, 0.470939716534985, "Baseline",
                                        0.423465457133681, "EmotionalconnImportant", 0, 0.0229578994183047,
                                        0.872956751430413, 1.03528564289116, "Age", 0.950662711807301,
                                        "EmotionalconnImportant", 0.244861081763754, 0.0413610477967341,
                                        1.29217293033968, 1.59050228345216, "Male", 1.43359826880487,
                                        "EmotionalconnImportant", 1.06801234522891e-11, 0.0759691668776525,
                                        0.982408585051868, 1.22207431464, "Age_x_Male", 1.09570812640672,
                                        "EmotionalconnImportant", 0.100742012124158, 0.0610191098206668,
                                        0.898946007886583, 0.999736525885989, "Age_squared", 0.94800272092627,
                                        "EmotionalconnImportant", 0.0488745964310127, 0.0257002445028373,
                                        0.650623034028101, 0.752794177890028, "Baseline", 0.699846577485023,
                                        "OpennessImportant", 0, 0.0260414548428243, 0.858107619562192,
                                        0.971867340980081, "Age", 0.913217811093637, "OpennessImportant",
                                        0.00425645200450653, 0.0290021389545522, 1.14003822524085, 1.33111618156254,
                                        "Male", 1.23187796847656, "OpennessImportant", 1.32438962241466e-07,
                                        0.0486965380819102, 0.89800284028914, 1.0556611331897, "Age_x_Male",
                                        0.973646083537137, "OpennessImportant", 0.517474635362337, 0.040175880542415,
                                        0.961058627573315, 1.04191702293311, "Age_squared", 1.00067144663239,
                                        "OpennessImportant", 0.974016863515232, 0.0206219130164052
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

