# Changelog

## chaoticds (development version)

### Dynamics Expansion

- Added globally coupled Kuramoto oscillators with matching R and C++
  RK4 implementations, linear-time order-parameter coupling, transient
  handling, and synchronization diagnostics.

### Dynamics Expansion

- Added a periodic nearest-neighbour coupled logistic-map lattice with
  matching R and C++ implementations, deterministic initialization,
  optional Gaussian perturbations, and parity tests.

### Statistical Inference

- Added native profile-likelihood confidence intervals for Poisson
  point-process fits from
  [`fit_ppp()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/fit_ppp.md).
- Added PPL return-level profiling on the fitted block-maximum GEV
  scale.
- Fixed profile-likelihood support for
  [`fit_gev_rlargest()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/fit_gev_rlargest.md)
  so it uses the joint r-largest likelihood instead of a block-maxima
  approximation.

### Repository and Consistency Cleanup

- Moved `extRemes` from `Suggests` to `Imports` to match mandatory
  runtime use.
- Fixed Rd escaping for recurrence documentation (`10\%`) to avoid
  parser issues.
- Removed tracked compiled artifacts from `src/` (`*.o`, `*.dll`,
  `symbols.rds`) and added ignore rules.
- Improved check-output ignore rules in `.gitignore` (including
  `..Rcheck/`).
- Removed obsolete Python-side files and references; repository is now
  R-only.
- Archived non-package research folders under `archive/` (`analysis/`,
  `simulations/`, `extremal-index/`).
- Moved internal development documents to `docs/dev/` and excluded them
  from package builds.

## chaoticds 0.1.0

### Major Features

- **Simulation Functions**: Complete implementation of chaotic map
  simulators
  - [`simulate_logistic_map()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_logistic_map.md):
    Generates logistic map time series
  - [`simulate_henon_map()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_henon_map.md):
    Produces Hénon map trajectories
- **Extreme Value Analysis**: Comprehensive extreme value theory tools
  - [`block_maxima()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/block_maxima.md):
    Extract block maxima from time series
  - [`exceedances()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/exceedances.md):
    Identify threshold exceedances
  - [`fit_gev()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/fit_gev.md):
    Fit Generalized Extreme Value distribution
  - [`fit_gpd()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/fit_gpd.md):
    Fit Generalized Pareto Distribution
- **Threshold Diagnostics**: Tools for threshold selection
  - [`mean_residual_life()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/mean_residual_life.md):
    Calculate mean residual life
  - [`mrl_plot()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/mrl_plot.md):
    Create MRL diagnostic plots
  - [`hill_estimates()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/hill_estimates.md):
    Compute Hill estimator for different k values
  - [`hill_plot()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/hill_plot.md):
    Visualize Hill plot diagnostics
  - [`threshold_diagnostics()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/threshold_diagnostics.md):
    Comprehensive threshold analysis
- **Extremal Index Estimation**: Multiple methods for extremal index
  - [`extremal_index_runs()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/extremal_index_runs.md):
    Runs-based estimator
  - [`extremal_index_intervals()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/extremal_index_intervals.md):
    Intervals-based estimator
  - [`hitting_times()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/hitting_times.md):
    Calculate hitting time statistics
  - [`plot_hts()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/plot_hts.md):
    Visualize hitting time survival functions
- **Cluster Analysis**: Tools for studying extreme event clustering
  - [`cluster_sizes()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/cluster_sizes.md):
    Compute cluster size distributions
  - [`cluster_summary()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/cluster_summary.md):
    Summary statistics for clusters
  - [`cluster_histogram()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/cluster_histogram.md):
    Visualize cluster size distributions
- **Statistical Diagnostics**: Dependence and mixing analysis
  - [`acf_decay()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/acf_decay.md):
    Autocorrelation function decay
  - [`mixing_coefficients()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/mixing_coefficients.md):
    Simple mixing coefficients
  - [`d_check()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/d_check.md):
    Test Leadbetter’s D conditions
- **Bootstrap Methods**: Uncertainty quantification
  - [`bootstrap_extremal_index()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/bootstrap_extremal_index.md):
    Bootstrap confidence intervals
- **High-Level Analysis**: End-to-end workflows
  - [`run_demo()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/run_demo.md):
    Complete extreme value analysis workflow

### Example Datasets

- `logistic_ts`: 5000 observations from logistic map (r=3.8)
- `henon_ts`: 3000 observations from Hénon map (a=1.4, b=0.3)  
- `ar1_ts`: 4000 observations from AR(1) model for comparison

### Documentation

- **Comprehensive Vignettes**:
  - “Estimating the Extremal Index for the Logistic Map”
  - “Block Maxima vs Peaks-over-Threshold for the Hénon Map”
- **Example Scripts**:
  - Basic extreme value analysis workflow
  - Comparative analysis across different systems
- **Package Website**: Complete pkgdown documentation site

### Infrastructure

- **Testing**: Comprehensive test suite with 43+ tests
- **CI/CD**: GitHub Actions for automated checking
- **Documentation**: Full roxygen2 documentation for all functions
- **Data**: Reproducible dataset generation scripts

### Dependencies

- **Imports**: assertthat, ggplot2, evd, rmarkdown
- **Suggests**: testthat, knitr, evir, ismev, pkgdown

### Author

Diogo Ribeiro (ESMAD – Instituto Politécnico do Porto)  
ORCID: [0009-0001-2022-7072](https://orcid.org/0009-0001-2022-7072)
