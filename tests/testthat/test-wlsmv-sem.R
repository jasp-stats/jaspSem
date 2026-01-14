#
# Copyright (C) 2024 Utrecht University
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.
#

context("Structural Equation Modeling w/ WLSMV Estimation")

## Load the testing data:
bfi_ae <- readRDS(testthat::test_path("bfi_ae.rds"))

###-Basic WLSMV Estimation-------------------------------------------------------------------------------------------###

## Estimate the model in lavaan:
mod1 <- '
agree =~ A1 + A2 + A3 + A4 + A5
extra =~ E1 + E2 + E3 + E4 + E5
'

semOut  <- lavaan::sem(mod1, data = bfi_ae, estimator = "wlsmv", missing = "listwise")
semPars <- lavaan::parameterEstimates(semOut)

## Specify the JASP options:
options <- jaspTools::analysisOptions("SEM")
options$models <- list(
  list(
    name = "Model1",
    syntax = list(
      model = mod1,
      columns = c(paste0("A", 1:5), paste0("E", 1:5))
    )
  )
)
options$emulation                  <- "lavaan"
options$estimator                  <- "wlsmv"
options$group                      <- ""
options$samplingWeights            <- ""
options$informationMatrix          <- "expected"
options$naAction                   <- "listwise"
options$modelTest                  <- "default"
options$errorCalculationMethod     <- "default"
options$meanStructure              <- TRUE
options$latentInterceptFixedToZero <- TRUE
options$additionalFitMeasures      <- TRUE

## Estimate the model is JASP:
results <- jaspTools::runAnalysis("SEM", data = bfi_ae, options = options)

jaspPars    <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]]
jaspLambda  <- jaspPars[["modelContainer_params_ind"]][["data"]]
jaspThresh  <- jaspPars[["modelContainer_params_thr"]][["data"]]
jaspResVars <- jaspPars[["modelContainer_params_var"]][["data"]]
jaspLvCov   <- jaspPars[["modelContainer_params_lcov"]][["data"]]

jaspFit0 <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_fittab"]][["data"]]
jaspFit1 <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_addfit"]][["collection"]][["modelContainer_addfit_fitMeasures"]][["data"]]

## Compare lavaan and JASP results:
testthat::test_that("WLSMV model fit matches", {
  tmp <- do.call(rbind.data.frame, jaspFit1)

  jasp <- jaspFit0[[1]][c("Chisq", "Df", "PrChisq")] |> unlist() |>
    c(tmp[grep("PNFI|ECVI", tmp$index, invert = TRUE), "value"]) |>
    as.numeric()

  fm1 <- c("chisq", "df", "pvalue", "cfi", "tli", "nnfi", "nfi", "rfi", "ifi", "rni", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue")
  fm2 <- c("srmr", "cn_05", "cn_01", "gfi", "mfi")

  lavaan <- lavaan::fitMeasures(semOut, fit.measures = c(paste(fm1, "scaled", sep = "."), fm2)) |> as.numeric()

  testthat::expect_equal(lavaan, jasp)
})

testthat::test_that("WLSMV factor loadings match", {
  lavaan <- semPars |>
    dplyr::filter(op == "=~") |>
    dplyr::select(-op) |>
    as.data.frame()
  jasp <- do.call(rbind.data.frame, jaspLambda)[colnames(lavaan)] |>
    dplyr::mutate(z = as.numeric(z), pvalue = as.numeric(pvalue))

  testthat::expect_equal(lavaan, jasp)
})

testthat::test_that("WLSMV thresholds match", {
  lavaan <- semPars |>
    dplyr::filter(op == "|") |>
    dplyr::select(-op) |>
    as.data.frame()
  jasp <- do.call(rbind.data.frame, jaspThresh)[colnames(lavaan)] |>
    dplyr::mutate(z = as.numeric(z), pvalue = as.numeric(pvalue))

  testthat::expect_equal(lavaan, jasp)
})

testthat::test_that("WLSMV residual variances match", {
  lavaan <- semPars |>
    dplyr::filter(op == "~~", lhs != "agree", lhs != "extra") |>
    dplyr::select(-op) |>
    as.data.frame()
  jasp <- do.call(rbind.data.frame, jaspResVars) |>
    dplyr::rename(rhs = col8) |>
    dplyr::mutate(z = as.numeric(z), pvalue = as.numeric(pvalue))

  testthat::expect_equal(lavaan, jasp[colnames(lavaan)])
})

testthat::test_that("WLSMV latent covariance matches", {
  lavaan <- semPars |>
    dplyr::filter(op == "~~", lhs == "agree", rhs == "extra") |>
    dplyr::select(-lhs, -rhs, -op) |>
    as.data.frame()
  jasp <- do.call(rbind.data.frame, jaspLvCov) |>
    dplyr::select(-lhs) |>
    dplyr::mutate(z = as.numeric(z), pvalue = as.numeric(pvalue))

  testthat::expect_equal(lavaan, jasp[colnames(lavaan)])
})

###-Latent Regression w/ WLSMV Estimation----------------------------------------------------------------------------###

## Estimate the model in lavaan:
mod2 <- '
agree =~ A1 + A2 + A3 + A4 + A5
extra =~ E1 + E2 + E3 + E4 + E5

agree ~ extra + age
'

semOut  <- lavaan::sem(mod2, data = bfi_ae, estimator = "wlsmv", missing = "listwise")
semPars <- lavaan::parameterEstimates(semOut)

## Specify the JASP options:
options$models <- list(
  list(
    name = "Model1",
    syntax = list(
      model = mod2,
      columns = c(paste0("A", 1:5), paste0("E", 1:5), "age")
    )
  )
)
options$exogenousCovariateConditional <- TRUE

## Estimate the model is JASP:
results <- jaspTools::runAnalysis("SEM", data = bfi_ae, options = options)

jaspPars    <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]]
jaspLambda  <- jaspPars[["modelContainer_params_ind"]][["data"]]
jaspThresh  <- jaspPars[["modelContainer_params_thr"]][["data"]]
jaspResVars <- jaspPars[["modelContainer_params_var"]][["data"]]
jaspLvCov   <- jaspPars[["modelContainer_params_lcov"]][["data"]]
jaspReg     <- jaspPars[["modelContainer_params_reg"]][["data"]]

jaspFit0 <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_fittab"]][["data"]]
jaspFit1 <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_addfit"]][["collection"]][["modelContainer_addfit_fitMeasures"]][["data"]]

## Compare lavaan and JASP results:
testthat::test_that("WLSMV model fit matches", {
  tmp <- do.call(rbind.data.frame, jaspFit1)

  jasp <- jaspFit0[[1]][c("Chisq", "Df", "PrChisq")] |> unlist() |>
    c(tmp[grep("PNFI|ECVI", tmp$index, invert = TRUE), "value"]) |>
    as.numeric()

  fm1 <- c("chisq", "df", "pvalue", "cfi", "tli", "nnfi", "nfi", "rfi", "ifi", "rni", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue")
  fm2 <- c("srmr", "cn_05", "cn_01", "gfi", "mfi")

  lavaan <- lavaan::fitMeasures(semOut, fit.measures = c(paste(fm1, "scaled", sep = "."), fm2)) |> as.numeric()

  testthat::expect_equal(lavaan, jasp)
})

testthat::test_that("WLSMV factor loadings match in latent regression", {
  lavaan <- semPars |>
    dplyr::filter(op == "=~") |>
    dplyr::select(-op) |>
    as.data.frame()
  jasp <- do.call(rbind.data.frame, jaspLambda)[colnames(lavaan)] |>
    dplyr::mutate(z = as.numeric(z), pvalue = as.numeric(pvalue))

  testthat::expect_equal(lavaan, jasp)
})

testthat::test_that("WLSMV thresholds match in latent regression", {
  lavaan <- semPars |>
    dplyr::filter(op == "|") |>
    dplyr::select(-op) |>
    as.data.frame()
  jasp <- do.call(rbind.data.frame, jaspThresh)[colnames(lavaan)] |>
    dplyr::mutate(z = as.numeric(z), pvalue = as.numeric(pvalue))

  testthat::expect_equal(lavaan, jasp)
})

testthat::test_that("WLSMV residual variances match in latent regression", {
  lavaan <- semPars |>
    dplyr::filter(op == "~~", lhs != "agree", lhs != "extra") |>
    dplyr::select(-op) |>
    as.data.frame()
  jasp <- do.call(rbind.data.frame, jaspResVars) |>
    dplyr::rename(rhs = col8) |>
    dplyr::mutate(z = as.numeric(z), pvalue = as.numeric(pvalue))

  testthat::expect_equal(lavaan, jasp[colnames(lavaan)])
})

testthat::test_that("WLSMV latent regressions match in latent regression", {
  lavaan <- semPars |>
    dplyr::filter(op == "~") |>
    dplyr::select(-op) |>
    as.data.frame()
  jasp <- lapply(jaspReg, "[", x = colnames(lavaan)) |>
    do.call(what = rbind.data.frame, args = _)

  testthat::expect_equal(lavaan, jasp)
})

###-Latent Regression w/ WLSMV Estimation & Random Covariates--------------------------------------------------------###

## Estimate the model in lavaan:
semOut  <- lavaan::sem(mod2, data = bfi_ae, estimator = "wlsmv", missing = "listwise", conditional.x = FALSE)
semPars <- lavaan::parameterEstimates(semOut)

## Specify the JASP options:
options$exogenousCovariateConditional <- FALSE
options$exogenousCovariateFixed       <- FALSE

## Estimate the model is JASP:
results <- jaspTools::runAnalysis("SEM", data = bfi_ae, options = options)

jaspPars    <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]]
jaspLambda  <- jaspPars[["modelContainer_params_ind"]][["data"]]
jaspThresh  <- jaspPars[["modelContainer_params_thr"]][["data"]]
jaspMu      <- jaspPars[["modelContainer_params_mu"]][["data"]]
jaspResVars <- jaspPars[["modelContainer_params_var"]][["data"]]
jaspLvCov   <- jaspPars[["modelContainer_params_lcov"]][["data"]]
jaspReg     <- jaspPars[["modelContainer_params_reg"]][["data"]]

jaspFit0 <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_fittab"]][["data"]]
jaspFit1 <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_addfit"]][["collection"]][["modelContainer_addfit_fitMeasures"]][["data"]]

## Compare lavaan and JASP results:
testthat::test_that("WLSMV model fit matches", {
  tmp <- do.call(rbind.data.frame, jaspFit1)

  jasp <- jaspFit0[[1]][c("Chisq", "Df", "PrChisq")] |> unlist() |>
    c(tmp[grep("PNFI|ECVI", tmp$index, invert = TRUE), "value"]) |>
    as.numeric()

  fm1 <- c("chisq", "df", "pvalue", "cfi", "tli", "nnfi", "nfi", "rfi", "ifi", "rni", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue")
  fm2 <- c("srmr", "cn_05", "cn_01", "gfi", "mfi")

  lavaan <- lavaan::fitMeasures(semOut, fit.measures = c(paste(fm1, "scaled", sep = "."), fm2)) |> as.numeric()

  testthat::expect_equal(lavaan, jasp)
})

testthat::test_that("WLSMV factor loadings match w/ random covariates", {
  lavaan <- semPars |>
    dplyr::filter(op == "=~") |>
    dplyr::select(-op) |>
    as.data.frame()
  jasp <- do.call(rbind.data.frame, jaspLambda)[colnames(lavaan)] |>
    dplyr::mutate(z = as.numeric(z), pvalue = as.numeric(pvalue))

  testthat::expect_equal(lavaan, jasp)
})

testthat::test_that("WLSMV thresholds match w/ random covariates", {
  lavaan <- semPars |>
    dplyr::filter(op == "|") |>
    dplyr::select(-op) |>
    as.data.frame()
  jasp <- do.call(rbind.data.frame, jaspThresh)[colnames(lavaan)] |>
    dplyr::mutate(z = as.numeric(z), pvalue = as.numeric(pvalue))

  testthat::expect_equal(lavaan, jasp)
})

testthat::test_that("WLSMV residual variances match w/ random covariates", {
  lavaan <- semPars |>
    dplyr::filter(op == "~~", lhs != "agree", lhs != "extra") |>
    dplyr::select(-op) |>
    as.data.frame()
  jasp <- do.call(rbind.data.frame, jaspResVars) |>
    dplyr::rename(rhs = col8) |>
    dplyr::mutate(z = as.numeric(z), pvalue = as.numeric(pvalue))

  testthat::expect_equal(lavaan, jasp[colnames(lavaan)])
})

testthat::test_that("WLSMV latent regressions match w/ random covariates", {
  lavaan <- semPars |>
    dplyr::filter(op == "~") |>
    dplyr::select(-op) |>
    as.data.frame()
  jasp <- lapply(jaspReg, "[", x = colnames(lavaan)) |>
    do.call(what = rbind.data.frame, args = _)

  testthat::expect_equal(lavaan, jasp)
})

###-Latent Regression w/ WLSMV Estimation & Bootstrapping------------------------------------------------------------###

## Specify the JASP options:
options$estimator              <- "dwls"
options$errorCalculationMethod <- "bootstrap"
options$bootstrapSamples       <- 10
options$bootstrapCiType        <- "percentile"
options$modelTest              <- "default"
options$userGaveSeed           <- TRUE
options$bootSeed               <- 235711

## Estimate model is JASP:
results1 <- suppressWarnings(
  jaspTools::runAnalysis("SEM", data = bfi_ae, options = options)
)
results2 <- suppressWarnings(
  jaspTools::runAnalysis("SEM", data = bfi_ae, options = options)
)

## Check reproducability:
testthat::test_that("Bootstrapped WLSMV models replicated with the same seed",
  testthat::expect_equal(results1[["results"]], results2[["results"]])
)

## Specify the JASP options:
options$userGaveSeed <- FALSE

## Estimate model is JASP:
results1 <- suppressWarnings(
  jaspTools::runAnalysis("SEM", data = bfi_ae, options = options)
)
results2 <- suppressWarnings(
  jaspTools::runAnalysis("SEM", data = bfi_ae, options = options)
)

## Check non-reproducability:
testthat::test_that("Bootstrapped WLSMV models don't replicated with different seeds", {
  lambda1 <- results1[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_ind"]][["data"]]
  lambda2 <- results2[["results"]][["modelContainer"]][["collection"]][["modelContainer_params"]][["collection"]][["modelContainer_params_ind"]][["data"]]

  testthat::expect_true(
    crossprod(do.call(rbind.data.frame, lambda1)$se - do.call(rbind.data.frame, lambda2)$se) > .Machine$double.eps
  )
})

###-Error Handling---------------------------------------------------------------------------------------------------###

options <- jaspTools::analysisOptions("SEM")
options$models <- list(
  list(
    name = "Model1",
    syntax = list(
      model = mod1,
      columns = c(paste0("A", 1:5), paste0("E", 1:5))
    )
  )
)
options$emulation                  <- "lavaan"
options$estimator                  <- "wlsmv"
options$group                      <- ""
options$samplingWeights            <- ""
options$informationMatrix          <- "expected"
options$naAction                   <- "listwise"
options$modelTest                  <- "default"
options$errorCalculationMethod     <- "bootstrap"
options$meanStructure              <- TRUE
options$latentInterceptFixedToZero <- TRUE
options$exogenousCovariateConditional <- TRUE

results <- jaspTools::runAnalysis("SEM", bfi_ae, options)

test_that("Bootstrapping with conditional covariates throws the correct error",
  testthat::expect_identical(
    results[["status"]],
    "validationError",
    label = "Bootstrapped standard errors are not yet available when conditioning estimation on exogenous covariates"
  )
)

options$errorCalculationMethod <- "default"
options$exogenousCovariateFixed <- FALSE

results <- jaspTools::runAnalysis("SEM", bfi_ae, options)

test_that("Conditioning on random covariates throws the correct error",
  testthat::expect_identical(
    results[["status"]],
    "validationError",
    label = "When conditioning estimation on exogenous covariates, the exogenous covariates must be fixed"
  )
)

