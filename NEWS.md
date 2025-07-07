# chaoticds (development version)

# chaoticds 0.1.0

## Major Features

* **Simulation Functions**: Complete implementation of chaotic map simulators
  - `simulate_logistic_map()`: Generates logistic map time series
  - `simulate_henon_map()`: Produces Hénon map trajectories

* **Extreme Value Analysis**: Comprehensive extreme value theory tools
  - `block_maxima()`: Extract block maxima from time series
  - `exceedances()`: Identify threshold exceedances
  - `fit_gev()`: Fit Generalized Extreme Value distribution
  - `fit_gpd()`: Fit Generalized Pareto Distribution

* **Threshold Diagnostics**: Tools for threshold selection
  - `mean_residual_life()`: Calculate mean residual life
  - `mrl_plot()`: Create MRL diagnostic plots
  - `hill_estimates()`: Compute Hill estimator for different k values
  - `hill_plot()`: Visualize Hill plot diagnostics
  - `threshold_diagnostics()`: Comprehensive threshold analysis

* **Extremal Index Estimation**: Multiple methods for extremal index
  - `extremal_index_runs()`: Runs-based estimator
  - `extremal_index_intervals()`: Intervals-based estimator
  - `hitting_times()`: Calculate hitting time statistics
  - `plot_hts()`: Visualize hitting time survival functions

* **Cluster Analysis**: Tools for studying extreme event clustering
  - `cluster_sizes()`: Compute cluster size distributions
  - `cluster_summary()`: Summary statistics for clusters
  - `cluster_histogram()`: Visualize cluster size distributions

* **Statistical Diagnostics**: Dependence and mixing analysis
  - `acf_decay()`: Autocorrelation function decay
  - `mixing_coefficients()`: Simple mixing coefficients
  - `d_check()`: Test Leadbetter's D conditions

* **Bootstrap Methods**: Uncertainty quantification
  - `bootstrap_extremal_index()`: Bootstrap confidence intervals

* **High-Level Analysis**: End-to-end workflows
  - `run_demo()`: Complete extreme value analysis workflow

## Example Datasets

* `logistic_ts`: 5000 observations from logistic map (r=3.8)
* `henon_ts`: 3000 observations from Hénon map (a=1.4, b=0.3)  
* `ar1_ts`: 4000 observations from AR(1) model for comparison

## Documentation

* **Comprehensive Vignettes**:
  - "Estimating the Extremal Index for the Logistic Map"
  - "Block Maxima vs Peaks-over-Threshold for the Hénon Map"

* **Example Scripts**:
  - Basic extreme value analysis workflow
  - Comparative analysis across different systems

* **Package Website**: Complete pkgdown documentation site

## Infrastructure

* **Testing**: Comprehensive test suite with 43+ tests
* **CI/CD**: GitHub Actions for automated checking
* **Documentation**: Full roxygen2 documentation for all functions
* **Data**: Reproducible dataset generation scripts

## Dependencies

* **Imports**: assertthat, ggplot2, evd, rmarkdown
* **Suggests**: testthat, knitr, evir, ismev, pkgdown

## Author

Diogo Ribeiro (ESMAD – Instituto Politécnico do Porto)  
ORCID: [0009-0001-2022-7072](https://orcid.org/0009-0001-2022-7072)