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

  levels(ggmisfit$Var1) <- levels(ggmisfit$Var1)
  levels(ggmisfit$Var2) <- levels(ggmisfit$Var2)

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

  decodedVars <- encodedVars
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

.sa.aco <- function (data = NULL, sample.cov, sample.nobs, model, sens.model,
                     opt.fun, d = NULL, paths = NULL, verbose = TRUE, max.value = Inf,
                     max.iter = 1000, e = 1e-10, n.of.ants = 10, k = 100, q = 1e-04,
                     sig.level = 0.05, rate.of.conv = 0.1, measurement = FALSE,
                     xi = 0.5, seed = NULL, ...) {
  for.n.of.sens.pars <- lavaan::lavaanify(sens.model, fixed.x = TRUE)
  n.of.sens.pars <- length(for.n.of.sens.pars[which(for.n.of.sens.pars$lhs !=
                                                      "phantom" & for.n.of.sens.pars$rhs == "phantom"), ]$lhs)
  if (n.of.sens.pars < 2)
    stop("Sensitivity model must have at least two sensitivity parameters or phantom coefficients.")
  if (is.null(data)) {
    old.out = lavaan::sem(model = model, sample.cov = sample.cov,
                          sample.nobs = sample.nobs, ...)
  }
  else {
    old.out = lavaan::sem(model = model, data = data, ...)
  }
  old.par = lavaan::standardizedSolution(old.out, type = "std.all")
  old.fit <- lavaan::fitMeasures(old.out)
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if (is.null(paths)) {
    paths <- old.par
  }
  if (is.character(paths)) {
    paths <- lavaan::lavaanify(paths, fixed.x = TRUE)
  }
  e.abs <- e
  e.rel <- e
  eval <- 0
  iter <- 0
  last.impr <- max.iter
  nl <- matrix(NA, k, k - 1)
  sens.pars <- data.frame()
  outcome <- vector()
  model.results <- data.frame()
  max.X <- rep(NA, n.of.sens.pars)
  max.y <- -Inf
  p.X <- vector()
  sens.fit <- vector()
  p <- data.frame(v = numeric(), sd = numeric(), gr = numeric())
  if (is.null(d)) {
    d <- list(rep(c(-1, 1), n.of.sens.pars))
  }
  else {
    if (!is.list(d))
      stop("d (domain) must be in a list format; e.g.,\n    d = list(-1, 1,\n             -1, 1,\n             -1, 1,\n             -1, 1)")
  }
  if (rate.of.conv <= 0 | rate.of.conv > 1)
    stop("Convergence rate (rate.of.conv) must be in (0, 1]")
  for (i in 1:(round(1/rate.of.conv * k, 0))) {
    X <- vector()
    for (j in 1:n.of.sens.pars) {
      X <- c(X, stats::runif(1, d[[1]][2 * j - 1], d[[1]][2 *
                                                            j]))
    }
    X <- t(X)
    new.model = sens.model
    for (l in 1:n.of.sens.pars) {
      new.model = gsub(paste("phantom", l, sep = ""), paste(X[l]),
                       new.model, ignore.case = FALSE, perl = FALSE,
                       fixed = FALSE, useBytes = FALSE)
    }
    iter <- iter + 1
    if((2 * k) < max.iter)
      progressbarTick()

    warnings <- options(warn = 2)
    if (is.null(data)) {
      new.out = try(lavaan::sem(model = new.model, sample.cov = sample.cov,
                                sample.nobs = sample.nobs, ...), silent = TRUE)
    }
    else {
      new.out = try(lavaan::sem(model = new.model, data = data,
                                ...), silent = TRUE)
    }
    if (isTRUE(class(new.out) == "try-error")) {
      next
    }
    on.exit(options(warnings))
    new.par = lavaan::standardizedSolution(new.out, type = "std.all")
    eval <- eval + 1
    if((2 * k) >= max.iter)
      progressbarTick()

    new.par$lines <- 1:length(new.par[, 1])
    new.par$evals <- eval
    model.results <- rbind(model.results, new.par)
    if (eval == 1) {
      sens.out <- new.out
      model.1 <- model.results
      model.1$path <- paste(model.1$lhs, model.1$op, model.1$rhs,
                            sep = "")
      phan.names <- model.1[which(model.1$evals == 1 &
                                    model.1$op == "~" & model.1$rhs == "phantom"),
      ]$path
      if (is.data.frame(paths)) {
        if (measurement) {
          paths <- which(model.1$lhs %in% paths$lhs &
                           model.1$rhs %in% paths$rhs)
        }
        else {
          paths <- which(model.1$lhs %in% paths$lhs &
                           model.1$op == "~" & model.1$rhs %in% paths$rhs)
        }
      }
    }
    sens.par <- c(X, eval = eval)
    sens.pars <- rbind(sens.pars, sens.par)
    fit <- c(lavaan::fitMeasures(new.out), eval = eval)
    sens.fit <- rbind(sens.fit, fit)
    if (!is.numeric(opt.fun)) {
      y <- eval(opt.fun)
    }
    else if (opt.fun == 1) {
      y <- mean(abs(old.par$est[paths]), na.rm = TRUE)/mean(abs(new.par$est[paths]),
                                                            na.rm = TRUE)
    }
    else if (opt.fun == 2) {
      y <- stats::sd(new.par$est[paths] - old.par$est[paths],
                     na.rm = TRUE)/mean(abs(old.par$est[paths]), na.rm = TRUE)
    }
    else if (opt.fun == 3) {
      y <- mean(abs(new.par$pvalue[paths] - old.par$pvalue[paths]),
                na.rm = TRUE)
    }
    else if (opt.fun == 4) {
      y <- 1/mean(abs(new.par$pvalue[paths] - rep(sig.level,
                                                  length(paths))), na.rm = TRUE)
    }
    else if (opt.fun == 5) {
      y <- abs(unname(lavaan::fitmeasures(new.out)["rmsea"]) -
                 unname(lavaan::fitmeasures(old.out)["rmsea"]))
    }
    else if (opt.fun == 6) {
      y <- 1/abs(unname(lavaan::fitmeasures(new.out)["rmsea"]) -
                   0.05)
    }
    outcome <- c(outcome, y)
    p.X <- rbind(p.X, X)
    p <- rbind(p, data.frame(v = y, sd = 0, gr = 0))
    if (eval == k) {
      break
    }
  }
  if (length(p.X) == 0 | length(p$v) < k)
    .quitAnalysis("Sensitivity analysis models do not reach the specified convergence rate.\n   Please set a lower convergence rate threshhold or reduce model complexicity")
  p$gr <- rank(-p$v, ties.method = "random")
  for (i in 1:k) {
    nl[i, ] <- (1:k)[1:k != i]
  }
  while (TRUE) {
    dist.mean <- p.X
    if (sum(apply(dist.mean, 2, stats::sd)) == 0) {
      colnames(sens.pars) <- c(phan.names, "eval")
      return(list(n.eval = eval, n.iter = iter, max.y = max.y,
                  phantom.coef = max.X, old.model.par = old.par,
                  old.model.fit = old.fit, model = model, sens.model = sens.model,
                  sens.fit = sens.fit, outcome = outcome, sens.pars = sens.pars,
                  model.results = model.results, old.out = old.out,
                  sens.out = sens.out))
    }
    dist.rank <- p$gr
    dim(dist.mean) <- c(length(p$v), n.of.sens.pars)
    o.X <- vector()
    o.X <- SEMsens::gen.sens.pars(dist.mean, dist.rank, n.of.ants,
                         nl, q, k, xi)
    if (length(o.X) == 0) {
      colnames(sens.pars) <- c(phan.names, "eval")
      return(list(n.eval = eval, n.iter = iter, max.y = max.y,
                  phantom.coef = max.X, old.model.par = old.par,
                  old.model.fit = old.fit, model = model, sens.model = sens.model,
                  sens.fit = sens.fit, outcome = outcome, sens.pars = sens.pars,
                  model.results = model.results, old.out = old.out,
                  sens.out = sens.out))
    }
    X <- o.X
    dim(X) <- c(length(X)/n.of.sens.pars, n.of.sens.pars)
    for (j in 1:dim(X)[1]) {
      X.sens <- X[j, ]
      X.model <- as.vector(X.sens)
      new.model = sens.model
      for (i in 1:dim(X)[2]) {
        new.model = gsub(paste("phantom", i, sep = ""),
                         paste(X.model[i]), new.model, ignore.case = FALSE,
                         perl = FALSE, fixed = FALSE, useBytes = FALSE)
      }
      iter <- iter + 1
      if((2 * k) < max.iter)
        progressbarTick()

      warnings <- options(warn = 2)
      on.exit(options(warnings))
      if (is.null(data)) {
        new.out = try(lavaan::sem(model = new.model,
                                  sample.cov = sample.cov, sample.nobs = sample.nobs,
                                  ...), TRUE)
      }
      else {
        new.out = try(lavaan::sem(model = new.model,
                                  data = data, ...), TRUE)
      }
      if (isTRUE(class(new.out) != "try-error")) {
        new.par <- lavaan::standardizedSolution(new.out,
                                                type = "std.all")
        eval <- eval + 1
        if((2 * k) >= max.iter)
          progressbarTick()

        p.X <- rbind(p.X, X.sens)
        new.par$lines <- 1:length(new.par[, 1])
        new.par$evals <- eval
        model.results <- rbind(model.results, new.par)
        fit <- c(lavaan::fitMeasures(new.out), eval = eval)
        sens.fit <- rbind(sens.fit, fit)
        sens.par <- c(X.sens, eval = eval)
        sens.pars <- rbind(sens.pars, sens.par)
        if (!is.numeric(opt.fun)) {
          y <- eval(opt.fun)
        }
        else if (opt.fun == 1) {
          y <- mean(abs(old.par$est[paths]), na.rm = TRUE)/mean(abs(new.par$est[paths]),
                                                                na.rm = TRUE)
        }
        else if (opt.fun == 2) {
          y <- stats::sd(new.par$est[paths] - old.par$est[paths],
                         na.rm = TRUE)/mean(abs(old.par$est[paths]),
                                            na.rm = TRUE)
        }
        else if (opt.fun == 3) {
          y <- mean(abs(new.par$pvalue[paths] - old.par$pvalue[paths]),
                    na.rm = TRUE)
        }
        else if (opt.fun == 4) {
          y <- 1/mean(abs(new.par$pvalue[paths] - rep(sig.level,
                                                      length(paths))), na.rm = TRUE)
        }
        else if (opt.fun == 5) {
          y <- abs(unname(lavaan::fitmeasures(new.out)["rmsea"]) -
                     unname(lavaan::fitmeasures(old.out)["rmsea"]))
        }
        else if (opt.fun == 6) {
          y <- abs(unname(lavaan::fitmeasures(new.out)["rmsea"]) -
                     0.05)
        }
        outcome <- c(outcome, y)
        p <- rbind(p, data.frame(v = y, sd = 0, gr = 0))
        p$gr <- rank(-p$v, ties.method = "random")
        idx.final <- p$gr <= k
        p <- p[idx.final, ]
        p.X <- p.X[idx.final, ]
        dim(p.X) <- c(length(p.X)/n.of.sens.pars, n.of.sens.pars)
      }
    }
    p$gr <- rank(-p$v, ties.method = "random")
    for (i in 1:k) {
      nl[i, ] <- (1:k)[1:k != i]
    }
    if (max(outcome, na.rm = TRUE) > max.y) {
      max.y <- max(outcome, na.rm = TRUE)
      max.X <- sens.pars[which.max(outcome), ]
      colnames(max.X) <- c(phan.names, "eval")
      last.impr <- eval
    }
    if ((abs(max.y - max.value) < abs(e.rel * max.value +
                                      e.abs)) | (max.y > max.value)) {
      colnames(sens.pars) <- c(phan.names, "eval")
      return(list(n.eval = eval, n.iter = iter, max.y = max.y,
                  phantom.coef = max.X, old.model.par = old.par,
                  old.model.fit = old.fit, model = model, sens.model = sens.model,
                  sens.fit = sens.fit, outcome = outcome, sens.pars = sens.pars,
                  model.results = model.results, old.out = old.out,
                  sens.out = sens.out))
    }
    if (max.iter > 0 & iter >= max.iter) {
      colnames(sens.pars) <- c(phan.names, "eval")
      return(list(n.eval = eval, n.iter = iter, max.y = max.y,
                  phantom.coef = max.X, old.model.par = old.par,
                  old.model.fit = old.fit, model = model, sens.model = sens.model,
                  sens.fit = sens.fit, outcome = outcome, sens.pars = sens.pars,
                  model.results = model.results, old.out = old.out,
                  sens.out = sens.out))
    }
  }
}

.sa.tabu <- function (model, sens.model, data = NULL, sample.cov = NULL,
                      sample.nobs = NULL, opt.fun = 1, sig.level = 0.05, ...) {
  init.model <- model
  init.model.par.table <- lavaan::lavaanify(init.model, auto = T,
                                            model.type = "sem", fixed.x = TRUE)
  non.phan.path.ids <- which(init.model.par.table$op == "~")
  non.phan.path.names <- character(length(non.phan.path.ids))
  for (i in seq_along(non.phan.path.ids)) {
    j <- non.phan.path.ids[i]
    non.phan.path.names[i] <- paste(init.model.par.table$lhs[j],
                                    init.model.par.table$op[j], init.model.par.table$rhs[j])
  }
  sens.model.par.table <- lavaan::lavaanify(sens.model, auto = T,
                                            model.type = "sem", fixed.x = TRUE)
  phan.path.ids <- which(sens.model.par.table$label != "")
  phan.path.names <- character(length(phan.path.ids))
  for (i in seq_along(phan.path.ids)) {
    j <- phan.path.ids[i]
    phan.path.names[i] <- paste(sens.model.par.table$lhs[j],
                                sens.model.par.table$op[j], sens.model.par.table$rhs[j])
  }
  init.model.sem <- lavaan::sem(model = init.model.par.table,
                                data = data, sample.cov = sample.cov, sample.nobs = sample.nobs)
  init.model.params <- lavaan::standardizedSolution(init.model.sem,
                                                    type = "std.all")
  sens.model.template <- sens.model
  f <- function(phantom.coef) {
    for (j in 1:length(phantom.coef)) {
      sens.model.template <- gsub(paste0("phantom", j),
                                  paste(phantom.coef[j]), sens.model.template)
    }
    sens.model.template.par.table <- lavaan::lavaanify(sens.model.template,
                                                       auto = T, model.type = "sem", fixed.x = TRUE)
    sens.model.sem <- try(lavaan::sem(model = sens.model.template.par.table,
                                      data = data, sample.cov = sample.cov, sample.nobs = sample.nobs),
                          silent = TRUE)
    sens.model.params <- lavaan::standardizedSolution(sens.model.sem,
                                                      type = "std.all")
    if (opt.fun == 1) {
      y <- mean(abs(sens.model.params$est[non.phan.path.ids] -
                      init.model.params$est[non.phan.path.ids]), na.rm = TRUE)/mean(abs(init.model.params$est[non.phan.path.ids]),
                                                                                    na.rm = TRUE)
    }
    else if (opt.fun == 2) {
      y <- stats::sd(sens.model.params$est[non.phan.path.ids] -
                       init.model.params$est[non.phan.path.ids], na.rm = TRUE)/mean(abs(init.model.params$est[non.phan.path.ids]),
                                                                                    na.rm = TRUE)
    }
    else if (opt.fun == 3) {
      y <- mean(abs(sens.model.params$pvalue[non.phan.path.ids] -
                      init.model.params$pvalue[non.phan.path.ids]),
                na.rm = TRUE)
    }
    else if (opt.fun == 4) {
      y <- mean(abs(sens.model.params$pvalue[non.phan.path.ids] -
                      rep(sig.level, length(non.phan.path.ids))), na.rm = TRUE)
    }
    else if (opt.fun == 5) {
      y <- abs(unname(lavaan::fitmeasures(sens.model.sem)["rmsea"]) -
                 unname(lavaan::fitmeasures(init.model.sem)["rmsea"]))
    }
    else if (opt.fun == 6) {
      y <- abs(unname(lavaan::fitmeasures(sens.model.sem)["rmsea"]) -
                 0.05)
    }
    return(list(y = y, model = sens.model.params))
  }
  res <- .sa.tabu.helper(length(phan.path.ids), f, maximum = TRUE,
                         ...)
  colnames(res$best.param) <- phan.path.names
  out <- list(model = model, old.model.par = init.model.params,
              model.results = res$model.history, best.param = res$best.param[1,
              ], best.obj = res$best.obj, sens.par = NULL, outcome = NULL)
  return(out)
}

.sa.tabu.helper <- function (n.var, f, maximum = FALSE, max.len = 1, max.tabu.size = 5,
                             neigh.size = NULL, max.iter = NULL, max.iter.obj = NULL,
                             range = c(-1, 1), r = 1e-05, verbose = TRUE, seed = NULL)
{
  if (is.null(neigh.size)) {
    neigh.size <- min(n.var * 2, 10)
  }
  if (is.null(max.iter)) {
    max.iter <- n.var * 50
  }
  if (is.null(max.iter.obj)) {
    max.iter.obj <- n.var * 5
  }
  options(warn = 2)
  tabu.list <- list()
  n.iter <- 1
  n.iter.obj <- 1
  model.history <- list()
  max.attempts <- 50
  if (!is.null(seed)) {
    set.seed(seed)
  }
  for (i in 1:max.attempts) {
    best.param <- current.param <- t(stats::runif(n.var,
                                                  -1, 1))
    best.obj <- try(f(best.param), silent = TRUE)
    if (class(best.obj)[1] != "try-error") {
      break
    }
  }
  if (class(best.obj)[1] == "try-error") {
    .quitAnalysis("Can't find a valid set of initial parameters for the sensitivity analysis! Maybe try a different seed?")
  }
  best.obj <- best.obj$y
  tabu.list[[1]] <- current.param
  if (verbose) {
    cat("  n   curr_obj   best_obj\n")
  }
  while ((n.iter <= max.iter) & (n.iter.obj <= max.iter.obj)) {
    best.neighbor <- SEMsens::gen.neighbors.tabu(current.param, maximum,
                                        neigh.size, tabu.list, max.len, range, r, f)
    current.param <- best.neighbor$best.param
    current.obj <- best.neighbor$best.obj
    best.neighbor$best.model$evals <- n.iter
    model.history[[n.iter]] <- best.neighbor$best.model
    if ((maximum & current.obj > best.obj) || (!maximum &
                                               current.obj < best.obj)) {
      best.obj <- current.obj
      best.param <- current.param
      n.iter.obj <- 1
    }
    else {
      n.iter.obj <- n.iter.obj + 1
    }
    tabu.list <- append(tabu.list, list(current.param))
    if (length(tabu.list) > max.tabu.size) {
      tabu.list <- tabu.list[-1]
    }
    if (verbose) {
      cat(sprintf("%3d %10f %10f\n", n.iter, current.obj,
                  best.obj))
    }
    n.iter <- n.iter + 1
    progressbarTick()
  }
  return(list(best.param = best.param, best.obj = best.obj,
              model.history = do.call(rbind, model.history)))
}

