
### Help on Unit Information Priors

Unit information (UI) priors are weakly informative priors whose scale is derived from the model's expected Fisher information matrix. To construct them, JASP first fits the model using maximum likelihood (ML) and extracts the ML estimates and their sampling variances. The UI variance for each parameter is then N × Var(θ̂), corresponding to one observation's worth of information (Kass & Wasserman, 1995).

Prior families are matched to parameter type: normal priors for loadings (λ) and regressions (β), gamma priors on the SD scale for variance-type parameters (θ, ψ), and beta priors for correlations (ρ). Hyperparameters are set by moment matching to the UI mean and variance targets.

Four prior variants are available for the focal structural parameters (regressions and correlations that distinguish competing models):

- **UI-0**: Focal parameters centered at zero rather than at their ML estimates, with UI dispersion unchanged. Non-focal parameters keep the standard UI centering at their ML estimates. Variance parameters retain a gamma prior centered at a positive ML estimate on the SD scale.
- **UI-0-half**: Same as UI-0 but with focal prior standard deviations halved (i.e., 1/4 the UI variance). Provides a sensitivity check on prior concentration.
- **UI-0-positive / UI-0-negative**: Directional variants of UI-0 that impose an order constraint (β > 0 or β < 0, ρ > 0 or ρ < 0) on focal parameters via the encompassing-prior approach. The constrained model's marginal likelihood is obtained by adjusting for the prior and posterior probability mass in the constrained region.
- **UI-0-half-positive / UI-0-half-negative**: Half-dispersion variants with directional constraints.

**References**

Kass, R. E., & Wasserman, L. (1995). A reference Bayesian test for nested hypotheses and its relationship to the Schwarz criterion. *Journal of the American Statistical Association, 90*(431), 928–934. https://doi.org/10.1080/01621459.1995.10476592

Pfadt, J. M., Merkle, E. C., & Wagenmakers, E.-J. (2026). Bayes factors for structural equation models with bridge sampling and blavaan. *PsyArXiv*. https://doi.org/10.31234/osf.io/pt2bc_v1
