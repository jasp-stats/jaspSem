generateLoadings <- function(factorList,
                             moderators,
                             removeModeration = FALSE,
                             removeModerationFor = NULL,
                             removeModerationForVariables = c(),
                             removeSpecificModeration = list(),
                             removeModerator = NULL) {
  loadingsBlock <- ""
  moderators <- paste0("data.", moderators)
  if (!is.null(removeModerator)) {
    moderators <- setdiff(moderators, paste0("data.", removeModerator))
  }

  for (factorName in names(factorList)) {
    indicators <- factorList[[factorName]]
    loadingsBlock <- paste0(loadingsBlock, "  ", factorName, " =~ ")

    for (i in seq_along(indicators)) {
      var <- indicators[i]

      if (removeModeration || (!is.null(removeModerationFor) && factorName %in% removeModerationFor) || var %in% removeModerationForVariables) {
        if (i == 1) {
          loadExpr <- paste0("1 * ", var)
        } else {
          loadExpr <- var
        }
      } else {
        moderatedTerms <- moderators
        if (var %in% names(removeSpecificModeration)) {
          moderatedTerms <- setdiff(moderatedTerms, paste0("data.", removeSpecificModeration[[var]]))
        }

        moderationTerms <- paste0(
          paste0(moderatedTerms, " * load_", var, "_", seq_along(moderatedTerms)),
          collapse = " + "
        )
        loadExpr <- paste0("{load_", var, " := load_", var, "_0 + ", moderationTerms, "} * ", var)
      }

      if (i < length(indicators)) {
        loadExpr <- paste0(loadExpr, " + \n")
      }

      loadingsBlock <- paste0(loadingsBlock, loadExpr)
    }
    loadingsBlock <- paste0(loadingsBlock, "\n")
  }

  return(loadingsBlock)
}

# Apply the same changes to all other functions
generateFactorVariances <- function(factorList,
                                    moderators,
                                    removeModerationFor = NULL,
                                    removeSpecificModeration = list(),
                                    removeModerator = NULL) {
  variancesBlock <- ""
  moderators <- paste0("data.", moderators)
  if (!is.null(removeModerator)) {
    moderators <- setdiff(moderators, paste0("data.", removeModerator))
  }

  for (factorName in names(factorList)) {
    if (!is.null(removeModerationFor) && factorName %in% removeModerationFor) {
      variancesBlock <- paste0(variancesBlock, "  ", factorName, " ~~ 1 * ", factorName, "\n")
    } else {
      moderatedTerms <- moderators
      if (factorName %in% names(removeSpecificModeration)) {
        moderatedTerms <- setdiff(moderatedTerms, paste0("data.", removeSpecificModeration[[factorName]]))
      }

      moderationTerms <- paste0(
        paste0(moderatedTerms, " * var_", factorName, "_", seq_along(moderatedTerms)),
        collapse = " + "
      )
      variancesBlock <- paste0(variancesBlock, "  ", factorName, " ~~ {var_", factorName, " := exp(var_", factorName, "_0 + ", moderationTerms, ")} * ", factorName, "\n")
    }
  }
  return(variancesBlock)
}

# Apply the same pattern to other functions (generateFactorMeans, generateIntercepts, generateResidualVariances)
generateFactorMeans <- function(factorList,
                                moderators,
                                removeModerationFor = NULL,
                                removeSpecificModeration = list(),
                                removeModerator = NULL) {
  meansBlock <- ""
  moderators <- paste0("data.", moderators)
  if (!is.null(removeModerator)) {
    moderators <- setdiff(moderators, paste0("data.", removeModerator))
  }

  for (factorName in names(factorList)) {
    if (!is.null(removeModerationFor) && factorName %in% removeModerationFor) {
      meansBlock <- paste0(meansBlock, "  ", factorName, " ~ 0 * 1\n")
    } else {
      moderatedTerms <- moderators
      if (factorName %in% names(removeSpecificModeration)) {
        moderatedTerms <- setdiff(moderatedTerms, paste0("data.", removeSpecificModeration[[factorName]]))
      }

      moderationTerms <- paste0(
        paste0(moderatedTerms, " * mean_", factorName, "_", seq_along(moderatedTerms)),
        collapse = " + "
      )
      meansBlock <- paste0(meansBlock, "  ", factorName, " ~ {mean_", factorName, " := mean_", factorName, "_0 + ", moderationTerms, "} * 1\n")
    }
  }
  return(meansBlock)
}

generateIntercepts <- function(factorList,
                               moderators,
                               removeModeration = FALSE,
                               removeModerationFor = NULL,
                               removeModerationForVariables = c(),
                               removeSpecificModeration = list(),
                               removeModerator = NULL) {
  interceptsBlock <- ""
  moderators <- paste0("data.", moderators)
  if (!is.null(removeModerator)) {
    moderators <- setdiff(moderators, paste0("data.", removeModerator))
  }

  for (factorName in names(factorList)) {
    indicators <- factorList[[factorName]]

    for (i in seq_along(indicators)) {
      var <- indicators[i]

      if (removeModeration || (!is.null(removeModerationFor) && factorName %in% removeModerationFor) || var %in% removeModerationForVariables) {
        interceptExpr <- paste0(var, " ~ int_", var, " * 1")
      } else {
        moderatedTerms <- moderators
        if (var %in% names(removeSpecificModeration)) {
          moderatedTerms <- setdiff(moderatedTerms, paste0("data.", removeSpecificModeration[[var]]))
        }

        moderationTerms <- paste0(
          paste0(moderatedTerms, " * int_", var, "_", seq_along(moderatedTerms)),
          collapse = " + "
        )
        interceptExpr <- paste0(var, " ~ {int_", var, " := int_", var, "_0 + ", moderationTerms, "} * 1")
      }

      interceptsBlock <- paste0(interceptsBlock, "  ", interceptExpr, "\n")
    }
  }
  return(interceptsBlock)
}


generateResidualVariances <- function(factorList,
                                      moderators,
                                      removeModeration = FALSE,
                                      removeModerationFor = NULL,
                                      removeModerationForVariables = c(),
                                      removeSpecificModeration = list(),
                                      removeModerator = NULL) {
  residualsBlock <- ""
  moderators <- paste0("data.", moderators)
  if (!is.null(removeModerator)) {
    moderators <- setdiff(moderators, paste0("data.", removeModerator))
  }

  for (factorName in names(factorList)) {
    indicators <- factorList[[factorName]]

    for (i in seq_along(indicators)) {
      var <- indicators[i]

      if (removeModeration || (!is.null(removeModerationFor) && factorName %in% removeModerationFor) || var %in% removeModerationForVariables) {
        residualExpr <- paste0(var, " ~~ res_", var, " * ", var)
      } else {
        moderatedTerms <- moderators
        if (var %in% names(removeSpecificModeration)) {
          moderatedTerms <- setdiff(moderatedTerms, paste0("data.", removeSpecificModeration[[var]]))
        }

        moderationTerms <- paste0(
          paste0(moderatedTerms, " * res_", var, "_", seq_along(moderatedTerms)),
          collapse = " + "
        )
        residualExpr <- paste0(var, " ~~ {res_", var, " := exp(res_", var, "_0 + ", moderationTerms, ")} * ", var)
      }

      residualsBlock <- paste0(residualsBlock, "  ", residualExpr, "\n")
    }
  }
  return(residualsBlock)
}
