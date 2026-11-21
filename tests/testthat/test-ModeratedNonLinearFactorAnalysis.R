context("Moderated Non-Linear Factor Analysis")

# Curated tests of the MNLFA (estimated with MoFIA). Each test runs one configuration through the analysis form and
# records what JASP shows, as JASP's generated tests do. Data in tests/testthat/mnlfa/:
#   continuous.csv     2 factors x 4 continuous items (y1-y8), Age, Male (0/1), Region (south, north, east); N = 400
#   ordinal.csv        1 factor, 5 ordinal items (o1-o5, levels 1-4; level 3 never observed in o3), Age; N = 400
#   saved-0.97.1.jasp  an MNLFA saved by jaspSem 0.97.1 (the OpenMx version) on continuous.csv
# The recorded tables come from running these configurations (as jaspTools::makeTestsFromExamples() records them).
# When this file was written they were compared with independent fits (relative tolerance 1e-8): tests 1-5 and 7
# with hand-written MoFIA fits (n(par), -2LL, information criteria, model comparisons, every recorded parameter
# table), test 8 with lavaan::cfa() per group. Test 6 checks the shown syntax against the fit table itself.

mnlfaData <- function(file) utils::read.csv(testthat::test_path("mnlfa", file))

mnlfaFactor <- function(k, indicators, type = "scale") {
  list(name = paste0("Factor", k), title = paste("Factor", k), indicators = indicators,
       indicators.types = rep(type, length(indicators)))
}

mnlfaModerator <- function(variable, square = FALSE, cube = FALSE) {
  list(variable = variable, moderatorSquaredEffect = square, moderatorCubicEffect = cube)
}

mnlfaOptions <- function(factors, moderators, moderatorTypes, models, ...) {
  options <- jaspTools::analysisOptions("ModeratedNonLinearFactorAnalysis")
  options$factors          <- factors
  options$moderators       <- moderators
  options$moderators.types <- moderatorTypes
  options$syncAnalysisBox  <- TRUE
  for (model in models)
    options[[model]] <- TRUE
  extra <- list(...)
  for (name in names(extra))
    options[[name]] <- extra[[name]]
  return(options)
}

# Two factors, Age with its square and cube, Male (0/1) and the interaction Age:Male.
continuousOptions <- function(models, ...) {
  items <- paste0("y", 1:8)
  mnlfaOptions(factors        = list(mnlfaFactor(1, items[1:4]), mnlfaFactor(2, items[5:8])),
               moderators     = list(mnlfaModerator("Age", square = TRUE, cube = TRUE), mnlfaModerator("Male")),
               moderatorTypes = c("scale", "scale"),
               models         = models,
               moderatorInteractionTerms = list(list(value = "Age:Male", moderatorInteractionTermsInclude = TRUE)),
               ...)
}

ordinalData <- function() {
  data <- mnlfaData("ordinal.csv")
  for (item in paste0("o", 1:5))
    data[[item]] <- ordered(data[[item]], levels = 1:4)
  return(data)
}

# The moderation boxes of a Custom model with one factor (Factor1): the ticked indicators and factor parameters.
customModel <- function(items, loadings, intercepts, means, variances) {
  rows      <- function(values, ticked) {
    lapply(values, function(v) list(value = v, includeIndividualModeration = v %in% ticked))
  }
  parameter <- function(key, label, rows) list(keyValue = key, keyLabel = label, moderationItemList = rows)
  list(list(keyValue = "invarianceTestCustom", keyLabel = "Custom", moderationTypeList = list(
    list(keyValue = "indicators", keyLabel = "Indicators", moderationParameterList = list(
      parameter("loadings",          "Loadings",           rows(items, loadings)),
      parameter("intercepts",        "Intercepts",         rows(items, intercepts)),
      parameter("residualVariances", "Residual variances", rows(items, character(0))))),
    list(keyValue = "factors", keyLabel = "Factors", moderationParameterList = list(
      parameter("variances", "Variances", rows("Factor1", if (variances) "Factor1")),
      parameter("means",     "Means",     rows("Factor1", if (means) "Factor1")))))))
}

# A plot request: one parameter of one indicator against Moderator 1, in one model.
plotRequest <- function(model, label, parameter, indicator, moderator) {
  request <- list(value = indicator, plotModerator1 = moderator, plotModerator2 = "", includePlot = TRUE)
  item  <- function(key) if (key == parameter) list(request) else list()
  entry <- function(key, label) list(keyValue = key, keyLabel = label, plotItemList = item(key))
  list(list(keyValue = model, keyLabel = label, plotTypeList = list(
    list(keyValue = "indicators", keyLabel = "Indicators", plotParameterList = list(
      entry("loadings", "Loadings"), entry("intercepts", "Intercepts"),
      entry("residualVariances", "Residual variances"))),
    list(keyValue = "factors", keyLabel = "Factors", plotParameterList = list(
      entry("variances", "Variances"), entry("means", "Means"), entry("covariances", "Correlations"))))))
}

runMnlfa <- function(dataset, options) {
  set.seed(1)
  jaspTools::runAnalysis("ModeratedNonLinearFactorAnalysis", dataset, options, view = FALSE, quiet = TRUE)
}

fitTable <- function(results) results[["results"]][["mofiaFitTable"]][["data"]]

fitTableNotes <- function(results) vapply(results[["results"]][["mofiaFitTable"]][["footnotes"]], `[[`, "", "text")

parameterTable <- function(results, model, type) {
  models    <- results[["results"]][["mofiaParameterContainer"]][["collection"]]
  container <- models[[paste0("mofiaParameterContainer_", model)]]
  container[["collection"]][[paste0("mofiaParameterContainer_", model, "_", type)]][["data"]]
}

firstPlot <- function(results, model) {
  container <- results[["results"]][["mofiaPlotContainer"]][["collection"]][[paste0("mofiaPlotContainer_", model)]]
  container[["collection"]][[1]]
}

test_that("Continuous items, Configural and Metric, default SEs: fit table, loadings, Configural against MoFIA", {
  data    <- mnlfaData("continuous.csv")
  options <- continuousOptions(c("invarianceTestConfigural", "invarianceTestMetric"), parameterEstimatesLoadings = TRUE)
  results <- runMnlfa(data, options)
  expect_identical(results[["status"]], "complete")
  jaspTools::expect_equal_tables(fitTable(results),
    list(2, 6482.1855371198, 7080.905219186, "", "", "", 6182.1855371198,
        "Configural", 150, "", 6664.11324796318, 6604.94527586046, "",
        "", "", 2, 6441.39848656829, 6920.37423222124, "Configural",
        30, 19.2129494484834, 6201.39848656829, "Metric", 120, 0.936374783964641,
        6545.48450807366, 6539.60627756081, 1.00223526630851, 19.1700991716742,
        "SB2001"))
  jaspTools::expect_equal_tables(parameterTable(results, "invarianceTestMetric", "loadings"),
    list("Baseline", 0.877785178151725, "Factor 1", "y1", 0.762350894768247,
        3.10635767837288e-50, 0.0588961247727043, 0.993219461535203,
        14.9039547430214, "Baseline", 0.868960823477008, "Factor 1",
        "y2", 0.764293416764468, 1.56371695120473e-59, 0.0534027194061437,
        0.973628230189547, 16.2718459497971, "Baseline", 0.838215605597909,
        "Factor 1", "y3", 0.725944467787092, 1.72787989131743e-48, 0.057282245335321,
        0.950486743408725, 14.6330787260718, "Baseline", 0.898891575086574,
        "Factor 1", "y4", 0.776707299687082, 3.92062262658198e-47, 0.0623400615334087,
        1.02107585048607, 14.4191640652271, "Baseline", 0.671650590329338,
        "Factor 2", "y5", 0.574744025027754, 4.96036056681822e-42, 0.0494430336812161,
        0.768557155630923, 13.5843321156182, "Baseline", 0.634900209145341,
        "Factor 2", "y6", 0.547519964595118, 5.09964444241114e-46, 0.0445825766388907,
        0.722280453695563, 14.240994061153, "Baseline", 0.72300392299806,
        "Factor 2", "y7", 0.623744121225652, 3.07546626860129e-46, 0.050643686595956,
        0.822263724770467, 14.2762893382211, "Baseline", 0.666349815787348,
        "Factor 2", "y8", 0.576518949989857, 6.89150209995187e-48, 0.0458329165770725,
        0.756180681584838, 14.5386736335406))
  # nothing to report: the Model Specification section stays empty and untitled
  expect_identical(results[["results"]][["mofiaContainer"]][["title"]], "")

  # The Configural model written directly with MoFIA, not through JASP: as in JASP's Configural preset, the first
  # indicator of each factor (y1, y5) keeps its loading and intercept unmoderated and everything else is moderated.
  # It must give the -2LL and number of parameters of the fit table's Configural row.
  items <- paste0("y", 1:8)
  free  <- setdiff(items, c("y1", "y5"))
  model <- MoFIA::mofia_model(data[, c(items, "Age", "Male")],
                              items          = items,
                              loadings       = list(Factor1 = items[1:4], Factor2 = items[5:8]),
                              moderators     = ~ Age + Male + I(Age^2) + I(Age^3) + Age:Male,
                              standardize    = TRUE,
                              continuous     = items,
                              loading_mod    = free,
                              threshold_mod  = free,
                              resvar_mod     = items,
                              mean_mod       = list(Factor1 = TRUE, Factor2 = TRUE),
                              var_mod        = list(Factor1 = TRUE, Factor2 = TRUE),
                              cor_mod        = TRUE,
                              identification = "variance",
                              cov_param      = "fisherz",
                              sparse         = 5)
  fit        <- MoFIA::mofia(model, link = "logit")
  rows       <- fitTable(results)
  configural <- rows[[which(vapply(rows, `[[`, "", "model") == "Configural")]]
  expect_equal(configural[["minus2LL"]], -2 * fit[["loglik"]], tolerance = 1e-8)
  expect_identical(as.integer(configural[["npar"]]), as.integer(fit[["npar"]]))
})

test_that("Continuous items, the other two standard-error choices: same estimates, own standard errors and tests", {
  data     <- mnlfaData("continuous.csv")
  models   <- c("invarianceTestConfigural", "invarianceTestMetric")
  reduced  <- runMnlfa(data, continuousOptions(models, parameterEstimatesLoadings = TRUE,
                                               errorCalculationMethod = "robustHuberWhiteReducedInformation"))
  standard <- runMnlfa(data, continuousOptions(models, parameterEstimatesLoadings = TRUE,
                                               errorCalculationMethod = "standard"))
  jaspTools::expect_equal_tables(fitTable(reduced),
    list(2, 6482.1855371198, 7080.905219186, "", "", "", 6182.1855371198,
        "Configural", 150, "", 6664.11324796318, 6604.94527586046, "",
        "", "", 2, 6441.39848656829, 6920.37423222124, "Configural",
        30, 19.2129494484834, 6201.39848656829, "Metric", 120, 0.932261390958454,
        6545.48450807366, 6539.60627756081, 0.99266232498308, 19.3549699277757,
        "SB2001"))
  jaspTools::expect_equal_tables(parameterTable(reduced, "invarianceTestMetric", "loadings"),
    list("Baseline", 0.877785178151725, "Factor 1", "y1", 0.762817384132897,
        1.25396447949199e-50, 0.0586581156213479, 0.992752972170553,
        14.9644285169002, "Baseline", 0.868960823477008, "Factor 1",
        "y2", 0.765075300909354, 2.09996523519245e-60, 0.0530037915936666,
        0.972846346044661, 16.3943143943091, "Baseline", 0.838215605597909,
        "Factor 1", "y3", 0.726952915840693, 2.43748767749842e-49, 0.0567677215677644,
        0.949478295355125, 14.7657080898926, "Baseline", 0.898891575086574,
        "Factor 1", "y4", 0.778083206463525, 3.58293949901163e-48, 0.0616380553805937,
        1.01969994370962, 14.5833863436513, "Baseline", 0.671650590329338,
        "Factor 2", "y5", 0.574647890934335, 5.96107330179902e-42, 0.0494920825893479,
        0.768653289724342, 13.5708694237469, "Baseline", 0.634900209145341,
        "Factor 2", "y6", 0.546839257606706, 2.44951854267292e-45, 0.0449298825046013,
        0.722961160683975, 14.130911850934, "Baseline", 0.72300392299806,
        "Factor 2", "y7", 0.622790901328298, 2.13777862300205e-45, 0.0511300322149944,
        0.823216944667821, 14.1404941807573, "Baseline", 0.666349815787348,
        "Factor 2", "y8", 0.575619366594657, 5.60090620099372e-47, 0.0462918961309292,
        0.757080264980038, 14.3945241279961))
  jaspTools::expect_equal_tables(fitTable(standard),
    list(2, 6482.1855371198, 7080.905219186, "", "", "", 6182.1855371198,
        "Configural", 150, "", 6664.11324796318, 6604.94527586046, "",
        "", "", 2, 6441.39848656829, 6920.37423222124, "Configural",
        30, 19.2129494484834, 6201.39848656829, "Metric", 120, 0.935436699670949,
        6545.48450807366, 6539.60627756081, "", 19.2129494484834, "LR"))
  jaspTools::expect_equal_tables(parameterTable(standard, "invarianceTestMetric", "loadings"),
    list("Baseline", 0.877785178151725, "Factor 1", "y1", 0.74994384734715,
        2.78092154025675e-41, 0.0652263673276503, 1.0056265089563, 13.4575205413842,
        "Baseline", 0.868960823477008, "Factor 1", "y2", 0.744445719969096,
        1.37203308574879e-42, 0.0635292814001027, 0.993475926984919,
        13.6781151041888, "Baseline", 0.838215605597909, "Factor 1",
        "y3", 0.717623398856809, 2.90826531328289e-42, 0.0615277666795489,
        0.958807812339008, 13.6233712165035, "Baseline", 0.898891575086574,
        "Factor 1", "y4", 0.768394963985379, 1.54905171693648e-41, 0.0665811270668926,
        1.02938818618777, 13.5006962886567, "Baseline", 0.671650590329338,
        "Factor 2", "y5", 0.565977987662411, 1.27381715028261e-35, 0.0539155839089182,
        0.777323192996265, 12.4574481371469, "Baseline", 0.634900209145341,
        "Factor 2", "y6", 0.532898919269794, 3.12106175133313e-34, 0.0520424307181763,
        0.736901499020887, 12.1996647809072, "Baseline", 0.72300392299806,
        "Factor 2", "y7", 0.61320716875594, 4.15158617041138e-38, 0.0560197815409785,
        0.832800677240179, 12.9062253209463, "Baseline", 0.666349815787348,
        "Factor 2", "y8", 0.563757141716259, 4.01990222822087e-37, 0.0523441628929545,
        0.768942489858436, 12.7301647205641))
  estimates <- function(results) {
    vapply(parameterTable(results, "invarianceTestMetric", "loadings"), `[[`, 0, "estimate")
  }
  expect_equal(estimates(reduced), estimates(standard), tolerance = 1e-10)
})

test_that("Ordinal items with the probit link: fit table, thresholds and a threshold plot", {
  options <- mnlfaOptions(factors        = list(mnlfaFactor(1, paste0("o", 1:5), type = "ordinal")),
                          moderators     = list(mnlfaModerator("Age")),
                          moderatorTypes = "scale",
                          models         = c("invarianceTestConfigural", "invarianceTestScalar"),
                          categoricalIndicatorLink     = "probit",
                          parameterEstimatesIntercepts = TRUE,
                          plotModelList  = plotRequest(model = "invarianceTestConfigural", label = "Configural",
                                                       parameter = "intercepts", indicator = "o2", moderator = "Age"))
  results <- runMnlfa(ordinalData(), options)
  jaspTools::expect_equal_tables(fitTable(results),
    list(2, 2746.38168616274, 2862.13415802887, "", "", "", 2688.38168616274,
        "Configural", 29, "", 2751.08438886544, 2770.1152356526, "",
        "", "", 2, 2734.90980406183, 2818.7305595511, "Configural",
        8, 4.5281178990881, 2692.90980406183, "Scalar", 21, 0.805264603418458,
        2737.35424850627, 2752.09616748552, 0.997048270968194, 4.54152324509928,
        "SB2001"))
  jaspTools::expect_equal_tables(parameterTable(results, "invarianceTestScalar", "intercepts"),
    list("Threshold 1 (1 | 2)", -4.11514077251821, "o1", -5.30755779150589,
        1.34218321453221e-11, 0.608387209353497, -2.92272375353053,
        -6.76401592481072, "Threshold 2 (2 | 3)", 0.452341144997533,
        "o1", -0.16043763981212, 0.147951114806923, 0.312647982127822,
        1.06511992980719, 1.44680653915943, "Threshold 3 (3 | 4)", 4.78250263509245,
        "o1", 3.34406603582102, 7.19798722479776e-11, 0.733909709881217,
        6.22093923436387, 6.51647276320475, "Threshold 1 (1 | 2)", -4.14522930935415,
        "o2", -5.199218741357, 1.2747307673703e-14, 0.537759591664228,
        -3.0912398773513, -7.70833170362565, "Threshold 2 (2 | 3)",
        0.366683524242342, "o2", -0.224061203327606, 0.223765164015392,
        0.301405909613476, 0.957428251812291, 1.21657708938945, "Threshold 3 (3 | 4)",
        5.00269839052741, "o2", 3.85358183687716, 1.42926069148716e-17,
        0.586294729247237, 6.15181494417765, 8.53273642243132, "Threshold 1 (1 | 2)",
        -3.01713409662835, "o3", -3.69521046096074, 2.75862528440145e-18,
        0.345963685904928, -2.33905773229596, -8.72095604119984, "Threshold 2 (2 | 4)",
        0.339460590365228, "o3", -0.089445197727545, 0.120847130577718,
        0.218833504837807, 0.768366378458, 1.55122768159668, "Threshold 1 (1 | 2)",
        -3.27044248254795, "o4", -4.05802134619794, 3.99203816831937e-16,
        0.401833334623652, -2.48286361889796, -8.13880333151298, "Threshold 2 (2 | 3)",
        0.633144479252604, "o4", 0.133888150591431, 0.0129341363492837,
        0.254727297337728, 1.13240080791378, 2.48557765842094, "Threshold 3 (3 | 4)",
        3.72851312243672, "o4", 2.91677136517773, 2.20520647578817e-19,
        0.414161568101205, 4.54025487969572, 9.00255699612769, "Threshold 1 (1 | 2)",
        -3.81713820190554, "o5", -4.67917416151389, 3.99902234358179e-18,
        0.439822346945137, -2.95510224229719, -8.67881822835546, "Threshold 2 (2 | 3)",
        0.507561631480617, "o5", 0.00733333405263903, 0.046734656619718,
        0.255223208882263, 1.00778992890859, 1.9886970064496, "Threshold 3 (3 | 4)",
        3.97732528393077, "o5", 3.09323398192685, 1.171154166804e-18,
        0.451075279432434, 4.86141658593468, 8.81743129203453))
  plot <- firstPlot(results, "invarianceTestConfigural")
  expect_identical(plot[["status"]], "complete")
  expect_identical(plot[["title"]], "Intercepts: o2")
  expect_s3_class(results[["state"]][["figures"]][[plot[["data"]]]][["obj"]], "ggplot")
})

test_that("Nominal moderator and missing moderator values: cases excluded with a note, dummy-coded effects", {
  data <- mnlfaData("continuous.csv")
  data$Region <- factor(data$Region, levels = c("south", "north", "east"))
  data$Age[c(3, 17)] <- NA
  data$Region[5]     <- NA
  options <- mnlfaOptions(factors        = list(mnlfaFactor(1, paste0("y", 1:4)), mnlfaFactor(2, paste0("y", 5:8))),
                          moderators     = list(mnlfaModerator("Age"), mnlfaModerator("Region")),
                          moderatorTypes = c("scale", "nominal"),
                          models         = "invarianceTestScalar",
                          parameterEstimatesFactorMeans = TRUE)
  results <- runMnlfa(data, options)
  jaspTools::expect_equal_tables(fitTable(results),
    list(2, 6351.60508420933, 6606.57700617331, "", "", "", 6223.60508420933,
        "Scalar", 64, "", 6376.66532517319, 6403.50316974801, "", "",
        ""))
  jaspTools::expect_equal_tables(parameterTable(results, "invarianceTestScalar", "factorMeans"),
    list("Baseline", 0, "Factor 1", "", "", "", "", "", "Age", 0.101873902825831,
        "Factor 1", -0.00292031570678503, 0.0567351897381743, 0.0534674205032439,
        0.206668121358448, 1.90534538354344, "Region (north)", -0.00213563334877999,
        "Factor 1", -0.251208993034617, 0.986591903539077, 0.127080579873148,
        0.246937726337057, -0.0168053478423829, "Region (east)", -0.0458687062093067,
        "Factor 1", -0.31024849332344, 0.733822675509139, 0.134890125124506,
        0.218511080904826, -0.340044952638075, "Baseline", 0, "Factor 2",
        "", "", "", "", "", "Age", -0.218604786418627, "Factor 2", -0.326420547461903,
        7.06819686128101e-05, 0.0550090521528525, -0.11078902537535,
        -3.97397842469988, "Region (north)", 0.147997380626658, "Factor 2",
        -0.0967397487987243, 0.235926928651426, 0.124868176841941, 0.39273451005204,
        1.18522897002007, "Region (east)", 0.0321675285201875, "Factor 2",
        -0.246171865148065, 0.820803270040769, 0.142012504241791, 0.31050692218844,
        0.226511944789164))
  note <- results[["results"]][["mofiaContainer"]][["collection"]][["mofiaContainer_dataNotes"]][["rawtext"]]
  expect_identical(note, paste("3 cases were excluded: 3 with a missing moderator value and 0 without any observed",
                               "indicator."))
})

test_that("Custom model: the ticked boxes are fitted; a model that is not identified is refused with the reason", {
  data   <- mnlfaData("continuous.csv")
  items  <- paste0("y", 1:4)
  custom <- function(boxes) {
    mnlfaOptions(factors = list(mnlfaFactor(1, items)), moderators = list(mnlfaModerator("Age")),
                 moderatorTypes = "scale", models = "invarianceTestCustom",
                 includeIndividualModerationsList = boxes, parameterEstimatesLoadings = TRUE)
  }
  fitted <- runMnlfa(data, custom(customModel(items, loadings = "y2", intercepts = "y3", means = TRUE,
                                              variances = TRUE)))
  jaspTools::expect_equal_tables(fitTable(fitted),
    list(1, 3241.21488059776, 3305.07831335149, "", "", "", 3209.21488059776,
        "Custom", 16, "", 3242.63524613301, 3254.3092527301, "", "",
        ""))
  jaspTools::expect_equal_tables(parameterTable(fitted, "invarianceTestCustom", "loadings"),
    list("Baseline", 0.812634741136868, "Factor 1", "y1", 0.738589862902537,
        1.2443132851639e-102, 0.0377786932915032, 0.886679619371199,
        21.5103983313165, "Baseline", 0.799748166358896, "Factor 1",
        "y2", 0.7314586248531, 1.36245746896124e-116, 0.0348422430434719,
        0.868037707864692, 22.953406454374, "Age", 0.00592804543849329,
        "Factor 1", "y2", -0.0505020521437479, 0.836871815240739, 0.0287913951620308,
        0.0623581430207345, 0.205896428607635, "Baseline", 0.767680204217958,
        "Factor 1", "y3", 0.695402638617284, 3.0155846400926e-96, 0.0368769866032186,
        0.839957769818633, 20.8173247038291, "Baseline", 0.841896350685597,
        "Factor 1", "y4", 0.767267674706831, 2.49797081152877e-108,
        0.0380765547568361, 0.916525026664363, 22.1106230871491))
  # the factor mean and the intercepts of all its indicators moderated: no anchor, so JASP does not fit the model
  refused <- runMnlfa(data, custom(customModel(items, loadings = character(0), intercepts = items, means = TRUE,
                                               variances = FALSE)))
  jaspTools::expect_equal_tables(fitTable(refused),
    list(1, "", "", "", "", "", "", "Custom", "", "", "", "", "", "", ""))
  reason <- paste("Model Custom is not identified: the mean of factor 'Factor 1' and the intercepts or thresholds",
                  "of all its indicators depend on the moderators (Age), and a shift of the mean is offset exactly by",
                  "shifts of the intercepts or thresholds. Keep at least one anchor indicator (leave its intercepts",
                  "unchecked) or uncheck the factor mean.")
  expect_true(reason %in% fitTableNotes(refused))
})

test_that("Show syntax: the shown code refits the model outside JASP with the same -2LL and number of parameters", {
  data    <- mnlfaData("continuous.csv")
  results <- runMnlfa(data, continuousOptions("invarianceTestConfigural", showSyntax = TRUE))
  syntax  <- results[["results"]][["mofiaSyntaxContainer"]][["collection"]]
  html    <- syntax[["mofiaSyntaxContainer_invarianceTestConfiguralSyntax"]][["rawtext"]]
  code    <- gsub("&amp;", "&", gsub("&gt;", ">", gsub("&lt;", "<", html, fixed = TRUE), fixed = TRUE), fixed = TRUE)
  env     <- new.env()
  env$data        <- data
  env$library     <- function(...) invisible(NULL)   # the code starts with library(MoFIA); use its namespace instead
  env$mofia_model <- MoFIA::mofia_model
  env$mofia       <- MoFIA::mofia
  eval(parse(text = code), envir = env)
  row <- fitTable(results)[[1]]
  expect_equal(-2 * env$fit_configural$loglik, row$minus2LL, tolerance = 1e-8)
  expect_identical(as.integer(env$fit_configural$npar), as.integer(row$npar))
})

test_that("An analysis saved with jaspSem 0.97.1 (OpenMx) opens and is refitted with MoFIA, its plot included", {
  jaspFile <- testthat::test_path("mnlfa", "saved-0.97.1.jasp")
  options  <- jaspTools::analysisOptions(jaspFile)
  dataset  <- jaspTools::extractDatasetFromJASPFile(jaspFile)
  results  <- runMnlfa(dataset, options)
  expect_identical(results[["status"]], "complete")
  jaspTools::expect_equal_tables(fitTable(results),
    list(1, 3308.66373021817, 3596.04917760995, "", "", "", 3164.66373021817,
        "Configural", 72, "", 3340.810519209, 3367.58840481369, "",
        "", "", 1, 3287.3230038381, 3514.83648302325, "", "", "", 3173.3230038381,
        "Metric", 57, "", 3306.65633717143, 3333.97170455955, "", "",
        ""))
  jaspTools::expect_equal_tables(parameterTable(results, "invarianceTestMetric", "loadings"),
    list("Baseline", 0.817627218680242, "Factor 1", "y1", 0.690817965810937,
        1.31580129653777e-36, 0.0646997872764803, 0.944436471549546,
        12.6372474021637, "Baseline", 0.814642047972873, "Factor 1",
        "y2", 0.696200990329669, 2.03153594079278e-41, 0.0604302214619513,
        0.933083105616077, 13.480705982284, "Baseline", 0.783191601227985,
        "Factor 1", "y3", 0.65835146262077, 9.52378132435128e-35, 0.0636951186817402,
        0.9080317398352, 12.2959438248524, "Baseline", 0.852410864595947,
        "Factor 1", "y4", 0.728711054648522, 1.4406463390802e-41, 0.063113307654198,
        0.976110674543371, 13.506040109106))
  plot <- firstPlot(results, "invarianceTestConfigural")
  expect_identical(plot[["status"]], "complete")
  expect_identical(plot[["title"]], "Loadings: y2")
})

test_that("Fit per group: one lavaan CFA per group of the binned moderators", {
  options <- mnlfaOptions(factors        = list(mnlfaFactor(1, paste0("y", 1:4)), mnlfaFactor(2, paste0("y", 5:8))),
                          moderators     = list(mnlfaModerator("Age"), mnlfaModerator("Male")),
                          moderatorTypes = c("scale", "scale"),
                          models         = "invarianceTestScalar",
                          checkModelFitPerGroup = TRUE)
  results <- runMnlfa(mnlfaData("continuous.csv"), options)
  group   <- results[["results"]][["groupContainer"]][["collection"]]
  table   <- group[["groupContainer_checkModelFitPerGroupTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(98, 0.999010505099724, 19.4809280592622, 19, "Age_nominal_1:Male_nominal_1",
        0.42639794663789, 0.0160712708306746, 0.0484327716818694, 89,
        0.971409078001804, 31.5850430722906, 19, "Age_nominal_1:Male_nominal_2",
        0.0347853119875157, 0.0862691560238694, 0.0512309422509571,
        97, 1, 18.0886341043773, 19, "Age_nominal_2:Male_nominal_2",
        0.516525116240924, 0, 0.0404636522004913, 116, 1, 18.1806828932366,
        19, "Age_nominal_2:Male_nominal_1", 0.510400836917032, 0, 0.0391186586499299))
})
