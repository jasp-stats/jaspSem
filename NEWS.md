# jaspSem (development version)

## [Pull Request #360](https://github.com/jasp-stats/jaspSem/pull/360)
- Replace help files with inline `info` fields on all QML controls across SEM, Mediation Analysis, MIMIC, PLS-SEM, and MNLFA
- Standardize all table, plot, and container titles to Title Case across all R files
- Wrap missing user-visible strings in `gettext()` for internationalization
- Fix `initCollapsed` parameter placement in PLS-SEM correlation containers
- Enable `preloadData` for all analyses
- Fix "Residuals" → "Residual" in covariance matrix titles for consistency

## [Pull Request #248](https://github.com/jasp-stats/jaspSem/pull/248):
- Major changes to SEM: 
  - ordinal data fully supported
  - more options for estimators, and tests
  - structure of estimation options is more concise
  - display warnings from lavaan if necessary
  - CIs for standardized estimates are now available, especially for bootstrapping
  - restructured additional fit measures table
  - adds thresholds table
- Bug Fixes:
  - fixes https://github.com/jasp-stats/jasp-issues/issues/2644
  - fixes https://github.com/jasp-stats/jasp-issues/issues/1259
  - fixes https://github.com/jasp-stats/jasp-issues/issues/959
  - fixes https://github.com/jasp-stats/jasp-issues/issues/1484
  - fixes https://github.com/jasp-stats/jasp-issues/issues/2740
  - fixes https://github.com/jasp-stats/jasp-issues/issues/1102
  - fixes https://github.com/jasp-stats/INTERNAL-jasp/issues/2182
  - fixes https://github.com/jasp-stats/jasp-issues/issues/2879
  - fixes https://github.com/jasp-stats/jasp-issues/issues/2905
  - fixes https://github.com/jasp-stats/jasp-issues/issues/2908



