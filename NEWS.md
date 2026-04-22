# jaspSem Changelog

> **HOW TO READ AND UPDATE THIS CHANGELOG:**
> 
> This document follows a modified [Keep a Changelog](https://keepachangelog.com/) format adapted for the R/JASP ecosystem. Releases are listed in reverse chronological order (newest first).
> As an example see [jaspModuleTemplate](https://github.com/jasp-stats/jaspModuleTemplate/blob/master/NEWS.md)
> * **Adding New Changes (For Contributors):** All new commits should be logged at the very top of the file under the `# jaspModuleTemplate (development version)` header. Place your bullet point under the appropriate category (`## Added`, `## Fixed`, etc.). 
> * **Issue References:** Please reference the relevant GitHub Issue (if any) at the end of your line (e.g., `([Issue #19](https://github.com/jasp-stats/jaspModuleTemplate/issues/19)`). 
> * **Format Categories:** >   * **Added:** New template features, QML examples, or build tools.
>   * **Changed:** Updates to default configurations, boilerplate code, or dependencies. 
>   * **Fixed:** Bug fixes in the build pipeline, R wrappers, or QML layouts.
>   * **Deprecated / Removed:** Outdated template components or legacy code.


---

# jaspSem (development version)

## Added
* MNLFA: new **Estimation Options** section with an **Indicator preprocessing** option (None / Center / Z-standardize) that transforms all indicator variables before the mxsem script is built.
* MNLFA: new **Show warnings** checkbox in the Output Options section that prints captured OpenMx warning text above the Global Invariance Fit table.
* MNLFA: Global Invariance Fit table now adds a per-model footnote whenever the OpenMx optimizer status is anything other than OK (e.g. "Non-convex Hessian", "OK/Gradient").

## Fixed
* MNLFA: fixed a bug where the estimated-parameter count `k` silently under-counted when the Hessian was singular (some SEs were `NA`). The wrong `k` propagated into AIC, BIC, SABIC, and Δdf, producing incorrect LRT p-values. `k` now matches `summary(mxRun(...))$estimatedParameters`.
* MNLFA: OpenMx warnings from `mxRun()` are no longer silently swallowed.

## Changed
* MNLFA: removed dead dependencies (`factorsUncorrelated`, `interceptsFixedToZero`, `packageMimiced`, `estimator`, `naAction`) from the main container — they were listed but never read anywhere in MNLFA code.

---

# jaspModuleTemplate 0.2.0
## Added
* Added NEWS.md
* Added workflow to remind users to update their `NEWS.md`.
* Added workflow to auto-bump version when user does not do so.

---

# jaspModuleTemplate 0.1.0

## Added
* Initial examples to showcase JASP module development

## Changed
* Use best practices for checking input ([Issue #19](https://github.com/jasp-stats/jaspModuleTemplate/issues/19)).
* The main results table now defaults to displaying 95% Confidence Intervals for effect sizes.

## Fixed
* Remove deprecated dependencies from qml files ([Issue #14](https://github.com/jasp-stats/jaspModuleTemplate/issues/14)).
