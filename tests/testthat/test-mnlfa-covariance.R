# Evaluate generated OpenMx models at fixed parameters: no optimization or
# simulation is needed to check their implied latent and observed covariances.
.mnlfaCovarianceTestModel <- function(moderatorData, varianceSlopes, correlationIntercept,
                                    correlationSlopes, moderateVariances,
                                    moderateCorrelation) {
  factors <- list(F1 = c("a", "b", "c"), F2 = c("d", "e", "f"))
  indicators <- unlist(factors, use.names = FALSE)
  dataset <- as.data.frame(matrix(0, nrow(moderatorData), length(indicators),
                                 dimnames = list(NULL, indicators)))
  dataset <- cbind(dataset, moderatorData)

  fixedVariances <- names(factors)[!moderateVariances]
  removeMod <- data.frame(paramValue = rep("variances", length(fixedVariances)),
                         itemValue = fixedVariances)
  if (!moderateCorrelation)
    removeMod <- rbind(removeMod,
                      data.frame(paramValue = "covariances", itemValue = "F1:F2"))

  syntax <- jaspSem:::.generateSyntax(factors, names(moderatorData),
                                     "invarianceTestCustom", removeMod)
  model <- mxsem::mxsem(syntax$model, dataset,
                       scale_loadings = FALSE, scale_latent_variances = FALSE)

  values <- OpenMx::omxGetParameters(model)
  values[] <- 0
  loadingValues <- c(0.7, 0.8, 0.9, 0.6, 0.75, 0.85)
  values[paste0("load_", indicators, "_0")] <- loadingValues
  values[paste0("res_", indicators, "_0")] <- log(0.3)
  values["rho_0"] <- correlationIntercept
  for (factorIndex in which(moderateVariances))
    values[paste0("var_", names(factors)[factorIndex], "_", seq_len(ncol(moderatorData)))] <-
      varianceSlopes[, factorIndex]
  if (moderateCorrelation)
    values[paste0("rho_", seq_len(ncol(moderatorData)))] <- correlationSlopes

  model <- OpenMx::omxSetParameters(model, labels = names(values),
                                   values = unname(values), free = FALSE)
  loadings <- matrix(0, length(indicators), length(factors))
  loadings[1:3, 1] <- loadingValues[1:3]
  loadings[4:6, 2] <- loadingValues[4:6]
  list(model = model, loadings = loadings)
}

.mnlfaExpectCovarianceMoments <- function(model, loadings, moderatorData, varianceSlopes,
                                         correlationIntercept, correlationSlopes,
                                         moderateVariances, moderateCorrelation) {
  for (row in seq_len(nrow(moderatorData))) {
    moderatorValues <- as.numeric(moderatorData[row, ])
    variances <- rep(1, 2)
    variances[moderateVariances] <- exp(
      moderatorValues %*% varianceSlopes[, moderateVariances, drop = FALSE])
    correlation <- tanh(correlationIntercept +
                          if (moderateCorrelation) sum(moderatorValues * correlationSlopes) else 0)

    # Independent reference: construct two correlated factors from independent
    # unit-variance variables, then propagate them through the measurement model.
    mixing <- rbind(c(sqrt(variances[1]), 0),
                    c(correlation * sqrt(variances[2]),
                      sqrt(1 - correlation^2) * sqrt(variances[2])))
    expectedLatent <- tcrossprod(mixing)
    expectedObserved <- loadings %*% expectedLatent %*% t(loadings) + diag(0.3, 6)
    actualLatent <- OpenMx::mxEval(S, model, compute = TRUE, defvar.row = row)[
      c("F1", "F2"), c("F1", "F2")]
    actualObserved <- OpenMx::mxGetExpected(model, "covariance", defvar.row = row)
    scenario <- paste("row", row, "variance moderation", paste(moderateVariances, collapse = "/"),
                      "correlation moderation", moderateCorrelation, "intercept", correlationIntercept)

    expect_equal(unname(actualLatent), expectedLatent, tolerance = 1e-10, info = scenario)
    expect_equal(unname(actualObserved), expectedObserved, tolerance = 1e-10, info = scenario)
    expect_equal(unname(stats::cov2cor(actualLatent)[1, 2]), correlation,
                 tolerance = 1e-10, info = scenario)
    expect_true(all(eigen(actualLatent, symmetric = TRUE, only.values = TRUE)$values > 0),
                info = scenario)
  }
}

test_that("Two-factor MNLFA has the intended covariance for every variance moderation combination", {
  oldOptions <- OpenMx::mxSetDefaultOptions()
  withr::defer(options(oldOptions))
  moderatorData <- data.frame(x = c(-2, -1, 0, 1, 2))
  varianceSlopes <- matrix(c(0.8, -0.2), nrow = 1)
  combinations <- expand.grid(first = c(FALSE, TRUE), second = c(FALSE, TRUE))

  for (combination in seq_len(nrow(combinations))) {
    moderateVariances <- unname(as.logical(combinations[combination, ]))
    for (moderateCorrelation in c(FALSE, TRUE)) {
      for (correlationIntercept in atanh(c(-0.65, 0, 0.65))) {
        fitted <- .mnlfaCovarianceTestModel(moderatorData, varianceSlopes,
                                           correlationIntercept, 0.6,
                                           moderateVariances, moderateCorrelation)
        .mnlfaExpectCovarianceMoments(fitted$model, fitted$loadings, moderatorData,
                                     varianceSlopes, correlationIntercept, 0.6,
                                     moderateVariances, moderateCorrelation)
      }
    }
  }
})

test_that("Two-factor MNLFA covariance uses the same polynomial and interaction terms as variances", {
  oldOptions <- OpenMx::mxSetDefaultOptions()
  withr::defer(options(oldOptions))
  moderatorData <- data.frame(x = c(-2, -1, 0, 1, 2), group = c(0, 1, 0, 1, 1))
  moderatorData$x_squared <- moderatorData$x^2
  moderatorData$x_x_group <- moderatorData$x * moderatorData$group
  varianceSlopes <- cbind(c(0.6, -0.25, 0.1, 0.12), c(-0.4, 0.2, 0.05, -0.18))
  correlationSlopes <- c(0.7, 0.2, -0.2, 0.1)
  fitted <- .mnlfaCovarianceTestModel(moderatorData, varianceSlopes, 0.2,
                                     correlationSlopes, c(TRUE, TRUE), TRUE)
  .mnlfaExpectCovarianceMoments(fitted$model, fitted$loadings, moderatorData,
                               varianceSlopes, 0.2, correlationSlopes, c(TRUE, TRUE), TRUE)
})
