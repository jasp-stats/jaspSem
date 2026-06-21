# jaspSem Changelog

> **HOW TO READ AND UPDATE THIS CHANGELOG:**
>
> This document follows a modified [Keep a Changelog](https://keepachangelog.com/) format adapted for the R/JASP ecosystem. Releases are listed in reverse chronological order (newest first).
> As an example see [jaspModuleTemplate](https://github.com/jasp-stats/jaspModuleTemplate/blob/master/NEWS.md)
> * **Adding New Changes (For Contributors):** All new commits should be logged at the very top of the file under the `# jaspSem (development version)` header. Place your bullet point under the appropriate category (`## Added`, `## Fixed`, etc.).
> * **Issue References:** Please reference the relevant GitHub Issue (if any) at the end of your line (e.g., `([Issue #19](https://github.com/jasp-stats/jaspSem/issues/19))`).
> * **Format Categories:**
>   * **Added:** New analyses, QML options, or features.
>   * **Changed:** Updates to default configurations, behavior, or dependencies.
>   * **Fixed:** Bug fixes.
>   * **Deprecated / Removed:** Outdated components or legacy code.


---

# jaspSem (development version)

## Added
* New analysis: Bayesian structural equation modeling (BSEM).
* MNLFA: new **Estimation Options** section with an **Indicator preprocessing** option (None / Center / Z-standardize) that transforms all indicator variables before the mxsem script is built.
* MNLFA: new **Show warnings** checkbox in the Output Options section that prints captured OpenMx warning text above the Global Invariance Fit table.
* MNLFA: Global Invariance Fit table now adds a per-model footnote whenever the OpenMx optimizer status is anything other than OK (e.g. "Non-convex Hessian", "OK/Gradient").

## Fixed
* MNLFA: fixed a bug where the estimated-parameter count `k` silently under-counted when the Hessian was singular (some SEs were `NA`). The wrong `k` propagated into AIC, BIC, SABIC, and Δdf, producing incorrect LRT p-values. `k` now matches `summary(mxRun(...))$estimatedParameters`.
* MNLFA: OpenMx warnings from `mxRun()` are no longer silently swallowed.

## Changed
* MNLFA: removed dead dependencies (`factorsUncorrelated`, `interceptsFixedToZero`, `packageMimiced`, `estimator`, `naAction`) from the main container — they were listed but never read anywhere in MNLFA code.

---

# jaspSem 0.20.0

## Added
* SEM: full support for ordinal data ([Pull Request #248](https://github.com/jasp-stats/jaspSem/pull/248)).
* SEM: more options for estimators and tests.
* SEM: CIs for standardized estimates, especially for bootstrapping.
* SEM: thresholds table.
* SEM: lavaan warnings shown when relevant.

## Changed
* SEM: estimation options restructured to be more concise.
* SEM: additional fit measures table restructured.

## Fixed
* SEM: [jasp-issues #2644](https://github.com/jasp-stats/jasp-issues/issues/2644).
* SEM: [jasp-issues #1259](https://github.com/jasp-stats/jasp-issues/issues/1259).
* SEM: [jasp-issues #959](https://github.com/jasp-stats/jasp-issues/issues/959).
* SEM: [jasp-issues #1484](https://github.com/jasp-stats/jasp-issues/issues/1484).
* SEM: [jasp-issues #2740](https://github.com/jasp-stats/jasp-issues/issues/2740).
* SEM: [jasp-issues #1102](https://github.com/jasp-stats/jasp-issues/issues/1102).
* SEM: [jasp-issues #2879](https://github.com/jasp-stats/jasp-issues/issues/2879).
* SEM: [jasp-issues #2905](https://github.com/jasp-stats/jasp-issues/issues/2905).
* SEM: [jasp-issues #2908](https://github.com/jasp-stats/jasp-issues/issues/2908).
* SEM: [INTERNAL-jasp #2182](https://github.com/jasp-stats/INTERNAL-jasp/issues/2182).
