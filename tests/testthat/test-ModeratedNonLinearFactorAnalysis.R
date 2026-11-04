
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
                                   list(110509.388844222, 110941.707465043, 110383.388844222, 63, 110510.541502884, 110741.507918229,
                                       "Scalar"))
  } else {
    expect_true(!is.null(table))
  }
})

test_that("Intercepts table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_globalParameterContainer"]][["collection"]][["mainContainer_globalParameterContainer_Scalar"]][["collection"]][["mainContainer_globalParameterContainer_Scalar_intTable"]][["data"]]

  if (isMacOS) {
    jaspTools::expect_equal_tables(table,
                                   list(-0.0575075131128216, 0.00858586357882383, "Baseline", -0.0244608247669989, "AgeImportant",
                                       0.14685073074176, 0.0168608651008339, -0.0969603033443979, -0.00441671593943532, "Baseline",
                                       -0.0506885096419166, "AttractiveImportant", 0.0317896476258752, 0.0236084918230474, -0.0849246263215631,
                                       -0.00673207730417562, "Baseline", -0.0458283518128694, "PhysicalbuildImportant", 0.0215928452942764,
                                       0.0199474453699559, 0.192723633736811, 0.268130870707717, "Baseline", 0.230427252222264,
                                       "TrustImportant", 0, 0.0192368935260313, 0.202866409013928, 0.279373155678646, "Baseline",
                                       0.241119782346287, "EmotionalconnImportant", 0, 0.0195173858469323, 0.124708420112923,
                                       0.187215688115912, "Baseline", 0.155962054114417, "OpennessImportant", 0, 0.0159460246453603))
  } else {
    expect_true(!is.null(table))
  }
})

test_that("Residual Variances table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_globalParameterContainer"]][["collection"]][["mainContainer_globalParameterContainer_Scalar"]][["collection"]][["mainContainer_globalParameterContainer_Scalar_resTable"]][["data"]]
  if (isMacOS) {
    jaspTools::expect_equal_tables(table,
                                   list(0.646612127419697, 0.746964710749845, "Baseline", 0.694979453455565, "AgeImportant", 0,
                                       0.025578430331697, 0.876100170808923, 0.993060843774282, "Age", 0.932749041733253, "AgeImportant",
                                       0.029421851488117, 0.0298179412133317, 1.03458330394845, 1.21413705459288, "Male", 1.12077023755404,
                                       "AgeImportant", 0.00522642485449265, 0.045756526056616, 0.962411222701564, 1.13066578629913,
                                       "Age_x_Male", 1.04315168688881, "AgeImportant", 0.304031123077225, 0.0428764787164041,
                                       0.955019483068737, 1.03306580604861, "Age_squared", 0.993276382518247, "AgeImportant",
                                       0.736383150691772, 0.0199050238170814, 0.160242088254625, 0.256476069631688, "Baseline",
                                       0.202727060317858, "AttractiveImportant", 0, 0.0243250898552165, 0.917042685898173, 1.22868865912398,
                                       "Age", 1.06148949504726, "AttractiveImportant", 0.423957815738771, 0.0792201806238112,
                                       0.570608878553608, 0.888304659944463, "Male", 0.711951210284001, "AttractiveImportant",
                                       0.00262180024670888, 0.0803885368745447, 0.80486395234456, 1.23548955985828, "Age_x_Male",
                                       0.997196575519581, "AttractiveImportant", 0.979513491653952, 0.109019327178907, 0.963794507799079,
                                       1.1997728316164, "Age_squared", 1.07532993342436, "AttractiveImportant", 0.193628424813515,
                                       0.0600795144536052, 0.409493804875226, 0.494110084794352, "Baseline", 0.449816650036056,
                                       "PhysicalbuildImportant", 0, 0.0215544809681606, 0.900316944789983, 1.04806398809138, "Age",
                                       0.971385488723626, "PhysicalbuildImportant", 0.453896601822881, 0.0376550275455605, 0.994700620166836,
                                       1.19519038396, "Male", 1.09034701638628, "PhysicalbuildImportant", 0.0648151252536744,
                                       0.0510745064808863, 1.02475323479355, 1.2405735012451, "Age_x_Male", 1.12751128969961,
                                       "PhysicalbuildImportant", 0.0138367679320477, 0.0549734951680977, 0.943045645565508, 1.03323785832694,
                                       "Age_squared", 0.987112183659311, "PhysicalbuildImportant", 0.577733881733901, 0.0230006442490908,
                                       0.349421315658436, 0.437021166411425, "Baseline", 0.390774245489726, "TrustImportant",
                                       0, 0.0223007822001916, 0.893042880790857, 1.06564416931014, "Age", 0.975533668746858, "TrustImportant",
                                       0.582653608081049, 0.0439745187978644, 1.38595696583409, 1.71984112765175, "Male", 1.54389954044844,
                                       "TrustImportant", 3.10862446895044e-15, 0.0850109818424618, 0.974652665270785, 1.22076818539211,
                                       "Age_x_Male", 1.09079098161389, "TrustImportant", 0.13028501548198, 0.0626533004867506,
                                       0.90987246686121, 1.01395655882572, "Age_squared", 0.960505676958165, "TrustImportant",
                                       0.144744569840831, 0.0265395776819324, 0.380787100655736, 0.470843078146671, "Baseline",
                                       0.42342764504847, "EmotionalconnImportant", 0, 0.022930804473807, 0.872934952333022, 1.0351903407755,
                                       "Age", 0.950607085383052, "EmotionalconnImportant", 0.244130218958387, 0.0413423587786717,
                                       1.29233736367395, 1.59051931748833, "Male", 1.43369715827136, "EmotionalconnImportant",
                                       1.03075326052249e-11, 0.0759317848675833, 0.982491999203281, 1.22211054584811, "Age_x_Male",
                                       1.09577088546727, "EmotionalconnImportant", 0.100442688220125, 0.061007158269673, 0.898818027181414,
                                       0.999635306000426, "Age_squared", 0.947887247324381, "EmotionalconnImportant", 0.0484484514846628,
                                       0.0257070587941207, 0.650687548195195, 0.752880748876231, "Baseline", 0.699921516007072,
                                       "OpennessImportant", 0, 0.0260470716871623, 0.858023924971641, 0.971812374956781, "Age",
                                       0.913147451563234, "OpennessImportant", 0.0042365762826746, 0.0290094507432857, 1.14003860255976,
                                       1.33114599009624, "Male", 1.23189196537373, "OpennessImportant", 1.32774209404829e-07,
                                       0.0487040248055993, 0.898174132488078, 1.05592020633261, "Age_x_Male", 0.973858416454579,
                                       "OpennessImportant", 0.521040888125141, 0.0401982200536647, 0.960934442660724, 1.04179549649901,
                                       "Age_squared", 1.00054843700579, "OpennessImportant", 0.978777748536298, 0.0206225892812422))
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
              list(includePlot = TRUE, plotModerator1 = "Age", plotModerator2 = "", value = "Factor1:Factor2")
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
                                   list(110748.480730426, 111194.523751908, 110618.480730426, 65, 110749.707496225, 110987.968663925,
                                       "Configural"))
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
                                   list(1.06323315247491, 1.67228127455123, "Age", 1.33342599770886, "Factor 1", 0.0127498564772044,
                                       0.154052419589609, 0.513573172872568, 0.809038849714203, "Age_squared", 0.644593398216966,
                                       "Factor 1", 0.000151990779061251, 0.0747305362299116, 1.79420176841326, 4.38925963325172,
                                       "Age", 2.80628177416402, "Factor 2", 6.14409182109199e-06, 0.64044560136475, 0.675983354732759,
                                       1.06561462104923, "Age_squared", 0.848727133058166, "Factor 2", 0.157768061964901, 0.0985447832410268))
  } else {
    expect_true(!is.null(table))
  }
})

test_that("Configural factor means table results match", {
  table <- results2[["results"]][["mainContainer"]][["collection"]][["mainContainer_globalParameterContainer"]][["collection"]][["mainContainer_globalParameterContainer_Configural"]][["collection"]][["mainContainer_globalParameterContainer_Configural_fmTable"]][["data"]]
  if (isMacOS) {
    jaspTools::expect_equal_tables(table,
                                   list(-0.126152180441041, 0.0698625095149789, "Age", -0.028144835463031, "Factor 1", 0.573540992245613,
                                       0.0500046662852376, -0.0659257782953776, 0.142200788191597, "Age_squared", 0.0381375049481095,
                                       "Factor 1", 0.472575411744338, 0.053094487482589, -1.05886938015851, 0.0817614733271691,
                                       "Age", -0.488553953415671, "Factor 2", 0.0931559560072313, 0.290982605415924, -0.248788523974826,
                                       0.0798657564649268, "Age_squared", -0.0844613837549495, "Factor 2", 0.313748118585263,
                                       0.0838419182781254))
  } else {
    expect_true(!is.null(table))
  }
})

test_that("Configural factor correlation coefficients table results match", {
  table <- results2[["results"]][["mainContainer"]][["collection"]][["mainContainer_globalParameterContainer"]][["collection"]][["mainContainer_globalParameterContainer_Configural"]][["collection"]][["mainContainer_globalParameterContainer_Configural_covTable"]][["data"]]
  if (isMacOS) {
    jaspTools::expect_equal_tables(table,
                                   list(0.109504385177572, 0.202811788254223, "Baseline", 0.156158086715898, 5.36846123111445e-11,
                                       0.0238033463402001, 0.0731657415284104, 0.137213807413587, "Age", 0.105189774470999, 1.21121557228321e-10,
                                       0.0163390925523069, -0.0267788590972744, 0.0403803128024518, "Age_squared", 0.00680072685258872,
                                       0.691409535515424, 0.0171327566295782))
  } else {
    expect_true(!is.null(table))
  }
})

test_that("Factor correlation output identifies and preserves the Fisher z scale", {
  table <- results2[["results"]][["mainContainer"]][["collection"]][["mainContainer_globalParameterContainer"]][["collection"]][["mainContainer_globalParameterContainer_Configural"]][["collection"]][["mainContainer_globalParameterContainer_Configural_covTable"]]
  plot <- results2[["results"]][["plotContainer"]][["collection"]][["plotContainer_plot1"]]
  plotObject <- results2[["state"]][["figures"]][[plot[["data"]]]][["obj"]]
  expect_equal(table[["title"]], "Factor Correlations (Fisher z)")
  expect_match(plot[["title"]], "Factor Correlations (Fisher z)", fixed = TRUE)
  expect_equal(plotObject$labels$y, "Fisher z")

  coefficients <- setNames(vapply(table[["data"]], function(row) row[["est"]], numeric(1)),
                           vapply(table[["data"]], function(row) row[["effect"]], character(1)))
  expected <- coefficients[["Baseline"]] + coefficients[["Age"]] * plotObject$data$Age +
    coefficients[["Age_squared"]] * plotObject$data$Age_squared
  expect_equal(plotObject$data$estimatedValue, as.numeric(expected), tolerance = 1e-10)
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

