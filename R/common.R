#
# Copyright (C) 2013-2020 University of Amsterdam
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

# This is a temporary fix
# TODO: remove it when R will solve this problem!
gettextf <- function(fmt, ..., domain = NULL)  {
  return(sprintf(gettext(fmt, domain = domain), ...))
}

# Function commonly used in the various procedures within the SEM module

lavBootstrap <- function(fit, samples = 1000) {
  # Run bootstrap, track progress with progress bar
  # Notes: faulty runs are simply ignored
  # recommended: add a warning if not all boot samples are successful
  # fit <- lavBootstrap(fit, samples = 1000)
  # if (nrow(fit@boot$coef) < 1000)
  #  tab$addFootnote(gettextf("Not all bootstrap samples were successful: CI based on %.0f samples.", nrow(fit@boot$coef)),
  #                  "<em>Note.</em>")


  coef_with_callback <- function(lav_object) {
    # Progress bar is ticked every time coef() is evaluated, which happens once on the main object:
    # https://github.com/yrosseel/lavaan/blob/77a568a574e4113245e2f6aff1d7c3120a26dd90/R/lav_bootstrap.R#L107
    # and then every time on a successful bootstrap:
    # https://github.com/yrosseel/lavaan/blob/77a568a574e4113245e2f6aff1d7c3120a26dd90/R/lav_bootstrap.R#L375
    # i.e., samples + 1 times
    progressbarTick()
    return(lavaan::coef(lav_object))
  }
  startProgressbar(samples + 1)

  bootres <- lavaan::bootstrapLavaan(object = fit, R = samples, FUN = coef_with_callback)

  # Add the bootstrap samples to the fit object
  fit@boot       <- list(coef = bootres)
  fit@Options$se <- "bootstrap"

  return(fit)
}


# Function to create a misfit plot
.resCorToMisFitPlot <- function(rescor) {
  ggmisfit <- reshape2::melt(abs(t(rescor)))
  ggmisfit$labels <- substr(round(ggmisfit$value, 2), 2, 4)
  ggmisfit$labels[ggmisfit$labels == ""] <- "0"

  levels(ggmisfit$Var1) <- .unv(levels(ggmisfit$Var1))
  levels(ggmisfit$Var2) <- .unv(levels(ggmisfit$Var2))

  misfitplot <-
    ggplot2::ggplot(ggmisfit, ggplot2::aes(x = Var1, y = Var2, fill = value,
                                           label = labels)) +
    ggplot2::geom_tile(na.rm = TRUE) +
    ggplot2::geom_text(color = ifelse(ggmisfit$value > .5, "white", "black"),
                       na.rm = TRUE) +
    ggplot2::scale_y_discrete(limits = rev(levels(ggmisfit$Var1))) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::scale_fill_continuous(low = "#FFFFFF", high = "#000000",
                                   na.value = "transparent",
                                   limits = c(0, 1)) +
    ggplot2::coord_fixed() +
    ggplot2::labs(x = "", y = "") +
    ggplot2::theme(axis.ticks.x = ggplot2::element_blank()) +
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank()) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                       hjust = 0)) +
    jaspGraphs::themeJaspRaw()

  return(misfitplot)
}

.withWarnings <- function(expr) {
  myWarnings <- NULL
  wHandler <- function(w) {
    myWarnings <<- c(myWarnings, list(w))
    invokeRestart("muffleWarning")
  }
  val <- withCallingHandlers(expr, warning = wHandler)
  list(value = val, warnings = myWarnings)
}

.decodeVarsInMessage <- function(encodedVars, message) {
  if (length(encodedVars) == 0 || !is.character(encodedVars) || !is.character(message))
    return(message)

  decodedVars <- .unv(encodedVars)
  names(decodedVars) <- encodedVars
  stringr::str_replace_all(message, decodedVars)
}

# plotting stuff
.lavToPlotObj <- function(lavResult) {
  # Create semplot model and decode the names of the manifest variables
  # Sorry, this code is really ugly but all it does is replace names for plot.
  semPlotMod <- semPlot::semPlotModel(list(lavResult), list(mplusStd = "std"))[[1]]

  manifests <- semPlotMod@Vars$name[semPlotMod@Vars$manifest]
  semPlotMod@Vars$name[semPlotMod@Vars$manifest] <- decodeColNames(manifests)

  thresholds <- semPlotMod@Thresholds$lhs
  semPlotMod@Thresholds$lhs <- decodeColNames(thresholds)

  lhsAreManifest <- semPlotMod@Pars$lhs %in% manifests
  if (any(lhsAreManifest))
    semPlotMod@Pars$lhs[lhsAreManifest] <- decodeColNames(semPlotMod@Pars$lhs[lhsAreManifest])

  rhsAreManifest <- semPlotMod@Pars$rhs %in% manifests
  if (any(rhsAreManifest))
    semPlotMod@Pars$rhs[rhsAreManifest] <- decodeColNames(semPlotMod@Pars$rhs[rhsAreManifest])

  if(.hasSlot(semPlotMod, "Thresholds"))
    semPlotMod@Thresholds$lhs <- ifelse(nchar(semPlotMod@Thresholds$lhs) > 0, decodeColNames(semPlotMod@Thresholds$lhs), "")

  return(semPlotMod)
}

