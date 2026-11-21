# jaspSem Changelog

> **HOW TO READ AND UPDATE THIS CHANGELOG:**
> 
> This document follows a modified [Keep a Changelog](https://keepachangelog.com/) format adapted for the R/JASP ecosystem. Releases are listed in reverse chronological order (newest first).
> As an example see [jaspModuleTemplate](https://github.com/jasp-stats/jaspModuleTemplate/blob/master/NEWS.md)
> * **Adding New Changes (For Contributors):** All new commits should be logged at the very top of the file under the `# jaspSem (development version)` header. Place your bullet point under the appropriate category (`## Added`, `## Fixed`, etc.). 
> * **Issue References:** Please reference the relevant GitHub Issue (if any) at the end of your line (e.g., `([Issue #19](https://github.com/jasp-stats/jaspSem/issues/19)`). 
> * **Format Categories:**
>   * **Added:** New features, analyses, or options.
>   * **Changed:** Updates to default configurations, existing behaviour, or dependencies.
>   * **Fixed:** Bug fixes.
>   * **Deprecated / Removed:** Removed features or legacy code.


---

# jaspSem (development version)

## Added
* MNLFA: models are now estimated with the MoFIA package (marginal maximum likelihood with adaptive quadrature) instead of OpenMx/mxsem. The results are shown in the new **Model Fit (MoFIA)** table, the **Parameter Estimates** tables and the **Parameter Plots**.
* MNLFA: ordinal indicators (binary or ordered categories) are supported; **Categorical indicator link** chooses Logit (default) or Probit.
* MNLFA: **Standard errors** (Estimation Options): Huber-White sandwich (default), Huber-White sandwich (reduced information), or Standard (from the inverse observed information). If the reduced information matrix of a model is not positive definite, that model shows its estimates without standard errors, confidence intervals, p-values or scaled tests, and a footnote gives the reason and points to the default standard errors.
* MNLFA: **Save factor scores to data** adds one column per factor with the EAP scores of the chosen **Model** (name: **Column name prefix**, default `FS_`, followed by the factor title). Cases excluded from the analysis get empty cells. The scores include the moderated factor means and variances; their posterior standard deviation does not include the uncertainty of the parameter estimates. Scores are written only for a converged model.
* MNLFA: **Show syntax** shows, for each fitted model, R code for the MoFIA package that reproduces the fit from the original data, including the exclusions, coding and preprocessing.
* MNLFA: **Parameter Plots** show each moderated parameter as a function of the moderators, with confidence bands for the chosen standard errors. Ordinal indicators are plotted as effective thresholds, one line per threshold. An interaction term chosen as Moderator 1 is plotted with its first moderator on the x-axis and lines for the second.
* MNLFA: new notes under the tables, always shown: cases excluded because of a missing moderator value or no observed indicator; how the moderators are coded; the quadrature (nodes and refits); fits that did not converge, ended at a saddle point or have a singular information matrix; identification warnings and errors checked before fitting; the reason when a test cannot be computed. **Show warnings** lists the remaining MoFIA and JASP warnings per model.
* MIMIC: residual variances and predictor covariances tables ([PR #365](https://github.com/jasp-stats/jaspSem/pull/365))
* MIMIC: option to freely estimate exogenous predictor variances and covariances (`fixed.x`) ([PR #365](https://github.com/jasp-stats/jaspSem/pull/365), [#4120](https://github.com/jasp-stats/jasp-issues/issues/4120))
* All analyses: added contextual help text to all output tables, containers, and QML controls ([PR #360](https://github.com/jasp-stats/jaspSem/pull/360))

## Fixed
* SEM: sampling weights dropdown now only offers scale variables, and negative or missing weights give a clear error.
* MNLFA: unmoderated residual variances (Strict and Custom models) are displayed on the variance scale; they were shown exponentiated.
* MNLFA: missing moderator values no longer stop the analysis: those cases are excluded and a note says how many.
* MNLFA: moderator names that contain ":" no longer stop the analysis when an interaction term is included.
* MNLFA: moderation choices are kept when a factor is renamed, an indicator is added or removed, or the moderators are reordered; when the first indicator of a factor changes, its loading and intercept boxes follow the new default anchor. An indicator removed and assigned again in the same session gets its last choices back.
* SEM: indirect effects table showed encoded column names instead of variable names in path models ([#4219](https://github.com/jasp-stats/jasp-issues/issues/4219))
* SEM: path plot for simple regression model across SEM, MIMIC, LGCM, and Mediation Analysis ([PR #365](https://github.com/jasp-stats/jaspSem/pull/365), [#4109](https://github.com/jasp-stats/jasp-issues/issues/4109))
* SEM: AVE and CR values regression ([PR #365](https://github.com/jasp-stats/jaspSem/pull/365), [#4201](https://github.com/jasp-stats/jasp-issues/issues/4201))
* PLSSEM: fixed untranslated column headers ([PR #360](https://github.com/jasp-stats/jaspSem/pull/360))
* PLSSEM: removed memory cap in parallel model fitting ([PR #360](https://github.com/jasp-stats/jaspSem/pull/360))

## Changed
* MNLFA: removed dead dependencies (`factorsUncorrelated`, `interceptsFixedToZero`, `packageMimiced`, `estimator`, `naAction`) from the main container — they were listed but never read anywhere in MNLFA code.
* MNLFA: new defaults of the boxes under **Include Individual Moderations** (new analyses; saved analyses keep their boxes). Configural moderates every parameter except the loading and intercept (threshold) of each factor's anchor indicator, by default its first indicator; Metric also leaves the loadings unmoderated, Scalar also the intercepts, Strict also the residual variances; Custom starts with nothing moderated. The models are therefore nested.
* MNLFA: the fit table compares each model with the smallest larger model in which it is nested. With a sandwich standard-error choice it shows the Satorra–Bentler (2001) scaled likelihood-ratio test, or the 2010 version when the 2001 scaling factor is not positive or not finite; if neither can be computed, no p-value is shown and a note says why. With Standard errors it shows the ordinary likelihood-ratio test. Models that are not nested get no test.
* MNLFA: continuous moderators are standardized on the analysed cases, so their effects are per standard deviation; moderators with two values (0/1) are used as coded; nominal moderators are dummy coded against their first level. Moderator terms are labelled, e.g., `Age`, `Age²`, `Age³`, `Gender (female)`, `Age × Gender (female)`.
* MNLFA: **Indicator preprocessing** (Center / Z-standardize) applies only to continuous indicators and uses the mean and standard deviation of the analysed cases; ordinal indicators are never transformed.
* MNLFA: variance tables show baseline values on the variance scale and moderation effects as multiplicative ratios; the test in a baseline variance row is of variance = 1. With two factors the factor correlation can be moderated (coefficients on the Fisher z scale); with three or more factors each pair has a constant correlation.
* MNLFA: analyses saved with an earlier version of the module are fitted again with MoFIA when they run. Expected differences:
  * continuous indicators, one or two factors, nominal moderators (if any) coded 0/1, no missing data: the same maximum-likelihood solution, up to numerical precision;
  * Metric (and the other presets) in saved files: the saved boxes are restored, so the same model is fitted and gives the same fit;
  * nominal moderators with three or more levels: now dummy coded (different estimates and fit);
  * 0/1 moderators stored as scale variables: now used as coded, with effects per unit and the baseline at 0 (before: standardized);
  * missing moderator values: the analysis runs with those cases excluded and a note (before: an error);
  * three or more factors with moderated factor variances: constant correlations between the factors (before: constant covariances);
  * indicators that load on more than one factor: a free loading per factor (before: forced equal);
  * unmoderated residual variances (Strict and Custom): corrected display (see Fixed);
  * pairs of models that are not nested, such as Configural vs Metric with the boxes of the earlier version: no test is shown (before: a test);
  * plots, syntax, warnings and convergence notes look different.
* MNLFA: analyses saved with an earlier version have no standard-error choice, so they open with the new default, Huber-White sandwich. The earlier version reported standard errors from the inverse observed information: choose **Standard** under Estimation Options → **Standard errors** to get that type again. The estimates, −2LL and information criteria do not depend on this choice; standard errors, confidence intervals, p-values and the fit-table tests do.

## Removed
* MNLFA: the OpenMx/mxsem code and the old **Global Invariance Fit**, parameter tables, plots and syntax; OpenMx and mxsem are no longer direct dependencies of jaspSem (OpenMx is still installed with semPlot, which the path diagrams of other analyses use).


---

# jaspSem 0.19.2

## Added
* SEM: full support for ordinal data ([PR #248](https://github.com/jasp-stats/jaspSem/pull/248))
* SEM: expanded estimator and test options ([PR #248](https://github.com/jasp-stats/jaspSem/pull/248))
* SEM: CIs for standardized estimates, including bootstrapped CIs ([PR #248](https://github.com/jasp-stats/jaspSem/pull/248))
* SEM: thresholds table ([PR #248](https://github.com/jasp-stats/jaspSem/pull/248))

## Changed
* SEM: restructured estimation options for conciseness ([PR #248](https://github.com/jasp-stats/jaspSem/pull/248))
* SEM: restructured additional fit measures table ([PR #248](https://github.com/jasp-stats/jaspSem/pull/248))
* SEM: lavaan warnings surfaced in output ([PR #248](https://github.com/jasp-stats/jaspSem/pull/248))

## Fixed
* SEM: various bug fixes ([PR #248](https://github.com/jasp-stats/jaspSem/pull/248)):
  [#2644](https://github.com/jasp-stats/jasp-issues/issues/2644),
  [#1259](https://github.com/jasp-stats/jasp-issues/issues/1259),
  [#959](https://github.com/jasp-stats/jasp-issues/issues/959),
  [#1484](https://github.com/jasp-stats/jasp-issues/issues/1484),
  [#2740](https://github.com/jasp-stats/jasp-issues/issues/2740),
  [#1102](https://github.com/jasp-stats/jasp-issues/issues/1102),
  [#2182](https://github.com/jasp-stats/INTERNAL-jasp/issues/2182),
  [#2879](https://github.com/jasp-stats/jasp-issues/issues/2879),
  [#2905](https://github.com/jasp-stats/jasp-issues/issues/2905),
  [#2908](https://github.com/jasp-stats/jasp-issues/issues/2908)
