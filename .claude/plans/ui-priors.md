# Plan: R Implementation of UI Priors for Bayesian SEM

## Context

The QML (`inst/qml/BayesianSEM.qml`) already has full UI for Unit Information (UI) priors under "Non-default" prior specification. The R backend does not implement these options — they're absent from `.bsemModelDeps` and `.bayesiansemBuildDpriors` ignores them.

The manuscript (Pfadt, Merkle & Wagenmakers, 2025) defines UI priors: per-parameter Normals centered at 0 with dispersion `N * V[i,i]` (one observation's worth of info). The BMA R files (`functionsAll.R`) have a complete standalone implementation. This plan adapts that for JASP.

## Prior Types (this step: ui0 and ui0Half only)

| QML value | Location params (λ, β, ν, α, τ) | Variance params (θ, ψ) | Correlation params (ρ) |
|-----------|----------------------------------|------------------------|------------------------|
| `ui0` | Normal(0, √(N·V)) | Gamma(shape,rate) @ ML SD | Beta(a,b) @ ρ=0 |
| `ui0Half` | Normal(0, √(N·V/4)) | Gamma @ ML SD, halved disp | Beta(a,b) @ ρ=0, halved disp |

Directional variants (`ui0Positive`, `ui0Negative`, `ui0HalfPositive`, `ui0HalfNegative`) require the encompassing prior approach (blavaan does not support truncated Stan distributions directly): fit with base prior + compute P_C_prior (from `prisamp=TRUE`) and P_C_post from posterior. Deferred to a future step.

## File to Modify

**`R/bayesiansem.R`** only.

---

## Step 1 — Update `.bsemModelDeps` (~line 116)

Add to the dependency vector:
```r
"priorSpecification",
"priorUiScope", "priorUiGlobal",
"priorUiGroupLoadings", "priorUiGroupRegressions",
"priorUiGroupObsIntercepts", "priorUiGroupLatIntercepts",
"priorUiGroupThresholds", "priorUiGroupResidual",
"priorUiGroupLatent", "priorUiGroupCorrelations",
"priorUiParameters"
```

---

## Step 2 — Modify `.bayesiansemOptionsToBlavOptions` (~line 500)

Only apply user-configured default priors when `priorSpecification == "default"`:
```r
if (options[["priorSpecification"]] == "default")
  blavaanOptions[["dp"]] <- .bayesiansemBuildDpriors(options)
# else: blavaan defaults cover any params without inline priors
```

---

## Step 3 — Modify `.bayesiansemComputeResults` fit loop

After translating syntax, add UI prior syntax building:
```r
translatedSyntax <- .bayesiansemTranslateModel(options[["models"]][[i]][["syntax"]], dataset)

if (options[["priorSpecification"]] == "nonDefault") {
  if (options[["group"]] != "") {
    modelContainer$setError(gettext("UI priors are not supported for multigroup models."))
    break
  }
  uiSyntax <- try(.bsemBuildUiModelSyntax(translatedSyntax, dataset, options))
  if (isTryError(uiSyntax)) {
    modelContainer$setError(gettextf("UI prior construction failed: %s", .extractErrorMessage(uiSyntax)))
    break
  }
  blavaanArgs[["model"]] <- uiSyntax
} else {
  blavaanArgs[["model"]] <- translatedSyntax
}
```

---

## Step 4 — New Helper Functions

All added to `R/bayesiansem.R`.

### 4a. Math helpers (from `functionsAll.R`)

```r
.bsemGammaMatch(mu, sigma2)
# → c(shape = mu^2/sigma2, rate = mu/sigma2)

.bsemBetaMatchCorrUi(rho_hat, var_rho, eps=1e-8, min_shape=1, max_n0=10)
# Converts correlation mean/variance to Beta(a,b) on x=(ρ+1)/2 ∈ [0,1]
# Caps n0 at 10, enforces unimodality via n0_min = min_shape/min(x, 1-x)
```

### 4b. Parameter info extractors (adapted from `functionsAll.R`)

Each returns `data.frame(par, lhs, rhs, mu, var_ui)` where `var_ui = N * V[free, free]`:

```r
.bsemExtractLoadingInfo(fit_lav, N)      # op "=~"
.bsemExtractRegressionInfo(fit_lav, N)  # op "~", rhs != "1"
.bsemExtractInterceptInfo(fit_lav, N)   # op "~1"
.bsemExtractResidVarInfo(fit_lav, N)    # op "~~", lhs==rhs, lhs ∈ ov
.bsemExtractFactorVarInfo(fit_lav, N)   # op "~~", lhs==rhs, lhs ∈ lv
.bsemExtractCorrInfo(fit_lav, N)        # op "~~", lhs!=rhs, both ∈ lv
```

For correlations, `std.lv=TRUE` → variance of rho = `V[idx,idx]` directly.
`std.lv=FALSE` → delta method (from `functionsAll.R`):
```r
grad <- c(1/√(ψ_ii·ψ_jj), -0.5·ψ_ij/(ψ_ii^1.5·√ψ_jj), -0.5·ψ_ij/(√ψ_ii·ψ_jj^1.5))
var_rho_hat <- t(grad) %*% V[idx3, idx3] %*% grad
var_ui <- N * var_rho_hat
```

### 4c. Prior string formatter

```r
.bsemUiPriorStr(priorType, paramClass, mu, var_ui)
# priorType: "ui0" or "ui0Half"
# paramClass: "location", "variance", "correlation"
# Returns blavaan inline prior string, e.g. 'prior("normal(0,2.3)")'
```

Logic:
- **location**: `sd <- sqrt(var_ui * if(priorType=="ui0Half") 0.25 else 1)` → `'prior("normal(0,sd)")'`
- **variance**: SD-scale matching via `mu_sd=√max(mu,1e-6)`, `var_sd≈var_ui/(4*mu)`, scale by 0.25 if Half → `.bsemGammaMatch(mu_sd, var_sd)` → `'prior("gamma(shape,rate)[sd]")'`
- **correlation**: center at ρ=0 (x=0.5), `var_eff = var_ui * if(Half) 0.25 else 1` → `.bsemBetaMatchCorrUi(0, var_eff)` → `'prior("beta(a,b)")'`

### 4d. Prior assignment builder

```r
.bsemBuildPriorAssignment(fit_lav, N, scope, options)
# Returns named list: paramKey -> priorStr (or NULL for blavaan default)
# paramKey format: paste0(lhs, op, rhs), e.g. "f1=~x1", "f1~f2", "x1~~x1", "x1~1"
```

Per scope:
- **"global"**: single type from `options[["priorUiGlobal"]]` → all free params
- **"group"**: per-class from `priorUiGroupLoadings`, ..., `priorUiGroupCorrelations`
  - Map: loadings→lambda, regressions→beta, obsIntercepts/latIntercepts→location, thresholds→location, residual→variance, latent→variance, correlations→correlation
- **"parameter"**: iterate `options[["priorUiParameters"]]`; normalize name (strip whitespace); match to PT; others → `NULL`

### 4e. Inline prior syntax builder

```r
.bsemBuildInlinePriorSyntax(fit_lav, priorAssignment)
# Builds complete blavaan model syntax from param table with embedded priors
```

Sections (mirrors `build_blavaan_model_from_fit` from `functionsAll.R`):
1. **Loadings**: `f1 =~ 1*x1 + prior*x2 + ...` (fixed first loading as `1*` when `std.lv=FALSE`)
2. **Factor variances**: only when `std.lv=FALSE` and `free > 0` → `f1 ~~ prior*f1`
3. **Factor covariances/correlations**: `f1 ~~ prior*f2`
4. **Regressions**: `f2 ~ prior*f1` (fixed regressions written as `0*f1`)
5. **Residual variances**: `x1 ~~ prior*x1`
6. **Intercepts**: `x1 ~ prior*1` (if any free `~1` rows)

Term format: if key in `priorAssignment` and not `NULL`, prepend `priorStr*`; else bare name.

### 4f. Lavaan ML fitter

```r
.bsemFitLavaan(translatedSyntax, dataset, options)
# Fits lavaan::sem() mirroring blavaan options
# Returns fit or stop() on non-convergence
```

Uses: `std.lv`, `auto.fix.first`, `meanstructure`, `int.ov.free`, `int.lv.free`, `orthogonal` from options.

### 4g. Orchestrator

```r
.bsemBuildUiModelSyntax(translatedSyntax, dataset, options)
```
1. Fit lavaan: `.bsemFitLavaan(translatedSyntax, dataset, options)`
2. `N <- nrow(dataset)`
3. Build assignment: `.bsemBuildPriorAssignment(fit_lav, N, options[["priorUiScope"]], options)`
4. Build syntax: `.bsemBuildInlinePriorSyntax(fit_lav, priorAssignment)`
5. Return syntax string

---

## Key Notes

- **Prior column in output table**: populated automatically from blavaan's `parameterTable()` when inline priors are used — no extra work needed
- **Variance UI matching**: `var_ui` from `vcov()` is for the variance estimate; convert to SD scale via `var_sd ≈ var_ui / (4 * mu_var)` (delta method for `√x`)
- **paramKey normalization**: remove all whitespace, e.g. `"f1 =~ x1"` → `"f1=~x1"`
- **std.lv detection**: `isTRUE(lavaan::lavInspect(fit_lav, "options")$std.lv)`
- **Prior predictive sampling**: the same blavaan options path is used, so nonDefault priors also apply to prior predictive fits automatically

---

## Out of Scope (for now)

- Directional variants for correlation parameters (would need truncated Beta)
- UI priors in multigroup context
- Caching lavaan ML fit (fast enough without caching)

---

## Verification

```r
# Basic smoke test
options <- jaspTools::analysisOptions("BayesianSEM")
options$models <- list(list(name="M1", syntax="f1 =~ x1+x2+x3\nf2 =~ y1+y2+y3\nf2~f1"))
options$priorSpecification <- "nonDefault"
options$priorUiScope <- "global"
options$priorUiGlobal <- "ui0"
options$runAnalysis <- TRUE
options$mcmcBurnin <- 100; options$mcmcSamples <- 200; options$mcmcChains <- 1
results <- jaspTools::runAnalysis("BayesianSEM", "debug.csv", options)
# Check: results$status == "complete"
# Check: Prior column in parameter tables shows normal(0,...) entries
```
