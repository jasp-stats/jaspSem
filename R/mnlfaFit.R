#
# Copyright (C) 2013-2026 University of Amsterdam
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
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# MNLFA with MoFIA: fitting, the convergence policy, the check of the factor correlation matrix and the standard
# errors per SE choice.
# Pure functions on fit objects; tests call them via jaspSem:::.

#' Fit one MoFIA model
#'
#' One mofia() call with the chosen link; every other argument stays at MoFIA's default (automatic quadrature,
#' no restarts, one thread). Warnings are kept verbatim, an error as its message.
#' `fitter` is a seam for tests; production always uses MoFIA::mofia.
#' @noRd
.mnlfaFitModel <- function(mofiaModel, link, fitter = MoFIA::mofia) {
  warnings <- character(0)
  fit <- tryCatch(
    withCallingHandlers(
      fitter(mofiaModel, link = link),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )
  if (inherits(fit, "error"))
    return(list(fit = NULL, error = conditionMessage(fit), errorClass = class(fit), warnings = warnings))
  return(list(fit = fit, error = NULL, errorClass = NULL, warnings = warnings))
}

# MoFIA's estimation failure (class "mofia_estimation_error", since MoFIA 2553b5b).
.mnlfaIsEstimationError <- function(fitResult) {
  return(!is.null(fitResult[["error"]]) && "mofia_estimation_error" %in% fitResult[["errorClass"]])
}

#' What JASP shows for one fit
#'
#' `outcome` is one of "error", "converged", "notConverged", "saddle", "singular", "failed" (factor correlation
#' matrix not positive definite) or "unknown". `estimates`: estimates and -2LL can be shown; `inference`: standard
#' errors, intervals, p-values and tests can be shown. `footnotes` are always shown: the status, the correlation-matrix
#' check, the quadrature footnote and MoFIA's quadrature warnings. `statusFootnotes` are only the status and the
#' correlation-matrix notes, i.e. why inference is withheld (the parameter tables carry these).
#' @noRd
.mnlfaFitOutcome <- function(fitResult, label) {
  out <- list(outcome = "error", estimates = FALSE, inference = FALSE, footnotes = character(0), error = NULL,
              statusFootnotes = character(0))

  # Quadrature warnings are always shown, also when the fit then failed.
  warnings           <- fitResult[["warnings"]]
  quadratureWarnings <- unique(warnings[grepl("quadrature", warnings, ignore.case = TRUE)])
  quadratureNotes    <- if (length(quadratureWarnings) > 0)
    gettextf("MoFIA reported a quadrature problem: %s", quadratureWarnings) else character(0)

  if (.mnlfaIsEstimationError(fitResult)) {
    # MoFIA's own text is long and advises restarts that JASP does not offer; it is listed under "Show warnings".
    out[["error"]]     <- gettextf("Model %s could not be fitted: the estimation did not reach a valid solution. The model may not be identified by these data; check the anchor indicators.",
                                   label)
    out[["footnotes"]] <- quadratureNotes
    return(out)
  }
  if (!is.null(fitResult[["error"]]) || is.null(fitResult[["fit"]])) {
    out[["error"]]     <- gettextf("Model %1$s could not be fitted. MoFIA reported: %2$s", label,
                                   if (is.null(fitResult[["error"]])) gettext("no result") else fitResult[["error"]])
    out[["footnotes"]] <- quadratureNotes
    return(out)
  }

  fit    <- fitResult[["fit"]]
  status <- if (is.null(fit[["status"]])) NA_character_ else as.character(fit[["status"]])[1]
  out[["estimates"]] <- TRUE
  if (isTRUE(fit[["converged"]]) && identical(status, "ok")) {
    out[["outcome"]]   <- "converged"
    out[["inference"]] <- TRUE
  } else if (identical(status, "ok")) {
    out[["outcome"]]   <- "notConverged"
    out[["footnotes"]] <- gettextf("The model did not converge (Newton decrement %s). Standard errors and tests are not shown.",
                                   format(fit[["newton_decrement"]], digits = 3))
  } else if (identical(status, "saddle")) {
    out[["outcome"]]   <- "saddle"
    out[["footnotes"]] <- gettext("The solution is a saddle point, not a maximum. Standard errors and tests are not available.")
  } else if (identical(status, "singular")) {
    out[["outcome"]]   <- "singular"
    out[["footnotes"]] <- gettext("The information matrix is singular: the model is not identified by these data. Standard errors and tests are not available. Check the anchor indicators.")
  } else {
    # Never silent: a status this version does not know withholds inference with a footnote.
    out[["outcome"]]   <- "unknown"
    out[["footnotes"]] <- gettextf("MoFIA reported the fit status '%s', which this version does not recognize. Standard errors and tests are not shown.",
                                   status)
  }

  # With three or more factors the constant correlations must form a positive definite matrix.
  if (length(fit[["model"]][["factors"]]) >= 3) {
    R <- .mnlfaCorrelationMatrix(fit)
    if (!.mnlfaIsPositiveDefinite(R)) {
      out[["outcome"]]   <- "failed"
      out[["inference"]] <- FALSE
      out[["footnotes"]] <- c(out[["footnotes"]], gettext("The estimated factor correlation matrix is not positive definite."))
    }
  }

  out[["statusFootnotes"]] <- out[["footnotes"]]
  out[["footnotes"]] <- c(out[["footnotes"]], .mnlfaQuadratureFootnote(fit), quadratureNotes)
  return(out)
}

#' The factor correlation matrix from the baseline Fisher-z correlations
#'
#' Built structurally from `model$par` (block "r0", column `pair`) and `model$pairs`; never from names.
#' @noRd
.mnlfaCorrelationMatrix <- function(fit) {
  model <- fit[["model"]]
  par   <- model[["par"]]
  if (length(fit[["coefficients"]]) != nrow(par))
    stop(gettext("The coefficients of the MoFIA fit do not match its parameter table."))
  R <- diag(length(model[["factors"]]))
  for (i in which(par[["block"]] == "r0")) {
    factors <- model[["pairs"]][par[["pair"]][i], ]
    R[factors[1], factors[2]] <- R[factors[2], factors[1]] <- tanh(unname(fit[["coefficients"]][i]))
  }
  return(R)
}

.mnlfaIsPositiveDefinite <- function(R) {
  if (!all(is.finite(R)))
    return(FALSE)
  return(min(eigen(R, symmetric = TRUE, only.values = TRUE)[["values"]]) > 1e-8)
}

#' The quadrature footnote
#'
#' Refits = position of the fitted node count in the recorded sequence minus one; 0 when no check was
#' recorded. Models with only continuous indicators use an exact rule.
#' @noRd
.mnlfaQuadratureFootnote <- function(fit) {
  if (all(fit[["model"]][["cont"]]))
    return(gettext("Quadrature: exact (continuous indicators)."))
  quadrature <- fit[["quadrature"]]
  refits <- if (is.null(quadrature) || NROW(quadrature) == 0) 0L else match(fit[["nq"]], quadrature[["nq"]]) - 1L
  if (is.na(refits))
    return(gettextf("Quadrature: %d nodes per factor; the number of refits after the accuracy check could not be determined.",
                    as.integer(fit[["nq"]])))
  return(sprintf(ngettext(as.integer(refits),
                          "Quadrature: %1$d nodes per factor; %2$d refit after the accuracy check.",
                          "Quadrature: %1$d nodes per factor; %2$d refits after the accuracy check."),
                 as.integer(fit[["nq"]]), as.integer(refits)))
}

# The covariance matrix of each SE choice; MLF is never used.
.mnlfaSeSource <- function(seChoice) {
  switch(seChoice,
         robustHuberWhite                   = list(information = "exact",   type = "MLR"),
         robustHuberWhiteReducedInformation = list(information = "reduced", type = "MLR"),
         standard                           = list(information = "exact",   type = "ML"),
         stop(gettextf("Unknown standard error choice: %s", seChoice)))
}

#' Standard errors of one fit for the selected SE choice
#'
#' Returns `available`, the covariance matrix `vcov`, the standard errors `se` (aligned with the
#' coefficients; NA where the variance is negative or not finite), `footnotes` (why no standard errors
#' are available, e.g. a reduced information matrix that is not positive definite) and
#' `varianceFootnotes` (single parameters without a standard error). Inference is never taken from
#' another SE type: when the selected matrix is unavailable there are no standard errors.
#' @noRd
.mnlfaStandardErrors <- function(fitResult, outcome, seChoice) {
  out <- list(available = FALSE, vcov = NULL, se = NULL, footnotes = character(0), varianceFootnotes = character(0))
  if (!isTRUE(outcome[["inference"]]))
    return(out)        # the status footnote of the outcome explains why

  fit    <- fitResult[["fit"]]
  source <- .mnlfaSeSource(seChoice)
  block  <- fit[["vcov"]][[source[["information"]]]]
  if (is.null(block)) {
    out[["footnotes"]] <- if (source[["information"]] == "reduced")
      gettext("Standard errors with reduced information are not available for this model, because the reduced information matrix is not positive definite. The default (exact information) standard errors are available.")
    else
      gettext("Standard errors are not available for this model.")
    return(out)
  }
  V <- block[[source[["type"]]]]
  k <- length(fit[["coefficients"]])
  if (!is.matrix(V) || nrow(V) != k || ncol(V) != k) {
    out[["footnotes"]] <- gettext("Standard errors are not available for this model.")
    return(out)
  }

  variances <- diag(V)
  se        <- rep(NA_real_, k)
  ok        <- is.finite(variances) & variances >= 0
  se[ok]    <- sqrt(variances[ok])
  names(se) <- names(fit[["coefficients"]])
  if (any(!ok))
    out[["varianceFootnotes"]] <- sprintf(ngettext(sum(!ok),
      "The estimated sampling variance of %s is negative or not finite; its standard error is not shown.",
      "The estimated sampling variances of %s are negative or not finite; their standard errors are not shown."),
      paste(names(fit[["coefficients"]])[!ok], collapse = ", "))
  out[["available"]] <- TRUE
  out[["vcov"]]      <- V
  out[["se"]]        <- se
  return(out)
}
