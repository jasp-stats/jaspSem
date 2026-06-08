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
* MNLFA: new **Estimation Options** section with an **Indicator preprocessing** option (None / Center / Z-standardize) that transforms all indicator variables before the mxsem script is built.
* MNLFA: new **Show warnings** checkbox in the Output Options section that prints captured OpenMx warning text above the Global Invariance Fit table.
* MNLFA: Global Invariance Fit table now adds a per-model footnote whenever the OpenMx optimizer status is anything other than OK (e.g. "Non-convex Hessian", "OK/Gradient").
* MIMIC: residual variances and predictor covariances tables ([PR #365](https://github.com/jasp-stats/jaspSem/pull/365))
* MIMIC: option to freely estimate exogenous predictor variances and covariances (`fixed.x`) ([PR #365](https://github.com/jasp-stats/jaspSem/pull/365), [#4120](https://github.com/jasp-stats/jasp-issues/issues/4120))
* All analyses: added contextual help text to all output tables, containers, and QML controls ([PR #360](https://github.com/jasp-stats/jaspSem/pull/360))

## Fixed
* MNLFA: fixed a bug where the estimated-parameter count `k` silently under-counted when the Hessian was singular (some SEs were `NA`). The wrong `k` propagated into AIC, BIC, SABIC, and Δdf, producing incorrect LRT p-values. `k` now matches `summary(mxRun(...))$estimatedParameters`.
* MNLFA: OpenMx warnings from `mxRun()` are no longer silently swallowed.
* SEM: indirect effects table showed encoded column names instead of variable names in path models ([#4219](https://github.com/jasp-stats/jasp-issues/issues/4219))
* SEM: path plot for simple regression model across SEM, MIMIC, LGCM, and Mediation Analysis ([PR #365](https://github.com/jasp-stats/jaspSem/pull/365), [#4109](https://github.com/jasp-stats/jasp-issues/issues/4109))
* SEM: AVE and CR values regression ([PR #365](https://github.com/jasp-stats/jaspSem/pull/365), [#4201](https://github.com/jasp-stats/jasp-issues/issues/4201))
* PLSSEM: fixed untranslated column headers ([PR #360](https://github.com/jasp-stats/jaspSem/pull/360))
* PLSSEM: removed memory cap in parallel model fitting ([PR #360](https://github.com/jasp-stats/jaspSem/pull/360))

## Changed
* MNLFA: removed dead dependencies (`factorsUncorrelated`, `interceptsFixedToZero`, `packageMimiced`, `estimator`, `naAction`) from the main container — they were listed but never read anywhere in MNLFA code.


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
