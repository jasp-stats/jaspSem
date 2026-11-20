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

# MNLFA with MoFIA: "Show syntax". Runnable R code that reproduces one model from a
# data frame `data` with the original (decoded) column names.

.mnlfaQuote <- function(x) encodeString(x, quote = "\"")

.mnlfaChr <- function(x) {
  if (length(x) == 0)
    return("character(0)")
  return(paste0("c(", paste(.mnlfaQuote(x), collapse = ", "), ")"))
}

.mnlfaColumn <- function(x) {
  ifelse(make.names(x) == x, paste0("data$", x), paste0("data[[", .mnlfaQuote(x), "]]"))
}

.mnlfaListName <- function(x) .mnlfaFormulaName(x)

.mnlfaFactorList <- function(factors) {
  if (length(factors) == 0)
    return("list()")
  return(paste0("list(", paste0(.mnlfaListName(factors), " = TRUE", collapse = ", "), ")"))
}

.mnlfaSeArguments <- function(seChoice) {
  switch(seChoice,
         robustHuberWhite                   = c(se = "MLR", information = "exact"),
         robustHuberWhiteReducedInformation = c(se = "MLR", information = "reduced"),
         standard                           = c(se = "ML",  information = "exact"))
}

#' R code for one model; the tests evaluate it on the fixture data
#'
#' Only for models whose moderation settings were read without errors. With `fit = FALSE` (a model
#' JASP refuses) the code reproduces the specification and stops before any fit.
#' @noRd
.mnlfaSyntax <- function(spec, prepared, model, fit = TRUE) {
  m       <- spec[["models"]][[model]]
  dec     <- function(x) unname(spec[["decoded"]][x])
  items   <- spec[["indicators"]]
  modVars <- spec[["moderatorVariables"]]
  tag     <- tolower(sub("^invarianceTest", "", model))

  lines <- c("library(MoFIA)",
             sprintf("data <- data[, %s]", .mnlfaChr(dec(c(items, modVars)))))

  nominal <- Filter(function(info) info[["class"]] == "nominal", prepared[["moderatorInfo"]])
  for (info in nominal) {
    column <- .mnlfaColumn(dec(info[["variable"]]))
    lines  <- c(lines, sprintf("%s <- factor(%s, levels = %s)   # nominal moderator: level order of the data",
                               column, column, .mnlfaChr(info[["inputLevels"]])))
  }
  for (item in spec[["categorical"]]) {
    column <- .mnlfaColumn(dec(item))
    lines  <- c(lines, sprintf("%s <- as.integer(factor(%s, levels = %s))   # ordinal indicator: level order",
                               column, column, .mnlfaChr(prepared[["categoryLevels"]][[item]])))
  }

  lines <- c(lines,
             sprintf("keep <- stats::complete.cases(data[, %s, drop = FALSE]) &", .mnlfaChr(dec(modVars))),
             sprintf("  rowSums(!is.na(data[, %s, drop = FALSE])) > 0", .mnlfaChr(dec(items))),
             "data <- data[keep, , drop = FALSE]")
  for (info in nominal) {
    column <- .mnlfaColumn(dec(info[["variable"]]))
    lines  <- c(lines,
                sprintf("%s <- droplevels(%s)", column, column),
                sprintf("contrasts(%s) <- contr.treatment(levels(%s))   # reference = first level", column, column))
  }
  if (spec[["preprocessing"]] != "none") {
    for (item in spec[["continuous"]]) {
      column <- .mnlfaColumn(dec(item))
      lines  <- c(lines, if (spec[["preprocessing"]] == "center")
        sprintf("%s <- %s - mean(%s, na.rm = TRUE)", column, column, column)
      else
        sprintf("%s <- (%s - mean(%s, na.rm = TRUE)) / sd(%s, na.rm = TRUE)", column, column, column, column))
    }
  }

  loadings <- vapply(seq_along(spec[["factors"]]), function(k) {
    paste0(.mnlfaListName(spec[["factorNames"]][k]), " = ", .mnlfaChr(dec(spec[["factors"]][[k]][["indicators"]])))
  }, "")
  formula <- .mnlfaFormulaText(prepared[["formulaParts"]], dec)
  se      <- .mnlfaSeArguments(spec[["seChoice"]])
  corMod  <- length(spec[["factors"]]) == 2 && isTRUE(m[["covariances"]])

  lines <- c(lines,
             sprintf("model_%s <- mofia_model(data,", tag),
             sprintf("  items = %s,", .mnlfaChr(dec(items))),
             sprintf("  loadings = list(%s),", paste(loadings, collapse = ", ")),
             sprintf("  moderators = %s,", formula),
             sprintf("  standardize = TRUE, continuous = %s,", .mnlfaChr(dec(spec[["continuous"]]))),
             sprintf("  loading_mod = %s,", .mnlfaChr(dec(m[["loadings"]]))),
             sprintf("  threshold_mod = %s,", .mnlfaChr(dec(m[["intercepts"]]))),
             sprintf("  resvar_mod = %s,", .mnlfaChr(dec(m[["residualVariances"]]))),
             sprintf("  mean_mod = %s, var_mod = %s, cor_mod = %s,", .mnlfaFactorList(m[["means"]]),
                     .mnlfaFactorList(m[["variances"]]), if (corMod) "TRUE" else "FALSE"),
             "  identification = \"variance\", cov_param = \"fisherz\", sparse = 5)")
  if (fit) {
    lines <- c(lines,
               sprintf("fit_%s <- mofia(model_%s, link = %s)", tag, tag, .mnlfaQuote(spec[["link"]])),
               sprintf("summary(fit_%s, se = %s, information = %s)", tag, .mnlfaQuote(se[["se"]]),
                       .mnlfaQuote(se[["information"]])))
  } else {
    lines <- c(paste("#", gettext("JASP refuses to fit this model (see the table 'Model Fit (MoFIA)'); this code only reproduces its specification.")),
               lines)
  }
  return(paste(lines, collapse = "\n"))
}

# Escape for display in a jasp-code HTML element.
.mnlfaHtmlEscape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  return(gsub(">", "&gt;", x, fixed = TRUE))
}
