# chaoticds <img src="man/figures/logo.png" align="right" height="139" alt="chaoticds logo" />

> **Professional Tools for Extreme Value Analysis of Chaotic Dynamical Systems**

<!-- badges: start -->
[![R-CMD-check](https://github.com/DiogoRibeiro7/chaotic-dynamical-systems/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/DiogoRibeiro7/chaotic-dynamical-systems/actions/workflows/R-CMD-check.yaml)
[![Codecov](https://codecov.io/gh/DiogoRibeiro7/chaotic-dynamical-systems/branch/main/graph/badge.svg)](https://codecov.io/gh/DiogoRibeiro7/chaotic-dynamical-systems)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![CRAN status](https://www.r-pkg.org/badges/version/chaoticds)](https://CRAN.R-project.org/package=chaoticds)
<!-- badges: end -->

---

## Why chaoticds?

Analyzing extreme events in chaotic systems is challenging. Traditional statistical methods often fail to capture the complex dependence structures in chaotic dynamics. **chaoticds** provides a comprehensive, statistically rigorous toolkit specifically designed for this purpose.

### Key Features

🎯 **Simulation Tools** - Generate dynamics from logistic, Hénon, tent, Lozi, and Arnold cat maps
📊 **Extreme Value Analysis** - Block maxima and peaks-over-threshold methods
🔬 **Extremal Index Estimation** - Multiple methods (runs, intervals) with bootstrap confidence intervals
⚡ **High Performance** - C++ implementations for computationally intensive operations
📈 **Diagnostic Tools** - Threshold selection, mixing conditions, and model validation
🎨 **Visualization** - Publication-ready plots with ggplot2
📚 **Well Documented** - Comprehensive vignettes and examples

---

## Installation

### From GitHub (Latest Development Version)

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install chaoticds
devtools::install_github("DiogoRibeiro7/chaotic-dynamical-systems")
```

### From CRAN (Stable Release)

```r
# Coming soon!
install.packages("chaoticds")
```

### System Requirements

- **R** ≥ 4.0.0
- **C++ compiler** (for building from source)
- **Suggested packages**: `ggplot2`, `evd`, `ismev` for full functionality

---

## Quick Start: 5-Minute Tour

### 1. Simulate Chaotic Dynamics

```r
library(chaoticds)

# Generate a chaotic time series from the logistic map
series <- simulate_logistic_map(n = 1000, r = 3.8, x0 = 0.2)

# Visualize
plot(series, type = "l", main = "Logistic Map Trajectory (r = 3.8)",
     xlab = "Iteration", ylab = "x")
```

<img src="man/figures/README-simulate-1.png" width="100%" />

### 2. Extreme Value Analysis

```r
# Extract block maxima
block_maxima_values <- block_maxima(series, block_size = 50)

# Fit Generalized Extreme Value distribution
gev_fit <- fit_gev(block_maxima_values)
summary(gev_fit)
```

### 3. Estimate Extremal Index

```r
# Define high threshold
threshold <- quantile(series, 0.95)

# Estimate extremal index (measures clustering of extremes)
theta <- extremal_index_runs(series, threshold, run_length = 2)
print(theta)
#> Extremal Index (runs method)
#> ========================================
#> Estimate:     0.6234
#> Threshold:    0.8956
#> Exceedances:  51
#> Clusters:     32
```

### 4. Bootstrap Confidence Intervals

```r
# Get uncertainty estimates
boot_result <- bootstrap_extremal_index(
  series,
  threshold,
  run_length = 2,
  B = 1000
)

cat("95% CI: [", boot_result$ci[1], ",", boot_result$ci[2], "]\n")
```

### 5. Comprehensive Analysis

```r
# Run complete analysis workflow
results <- run_demo(n = 2000, r = 3.8, x0 = 0.2)

# Access results
str(results, max.level = 1)
```

---

## Common Use Cases

### 📊 Case 1: Comparing Extreme Value Methods

```r
# Generate Hénon map data
henon_data <- simulate_henon_map(n = 5000, a = 1.4, b = 0.3)
x_series <- henon_data$x

# Method 1: Block Maxima
bm <- block_maxima(x_series, block_size = 100)
gev <- fit_gev(bm)

# Method 2: Peaks Over Threshold
threshold <- quantile(x_series, 0.95)
exceedances_data <- exceedances(x_series, threshold)
gpd <- fit_gpd(x_series, threshold)

# Compare estimates
print(gev)
print(gpd)
```

See `vignette("block-maxima-vs-pot-henon")` for detailed comparison.

### 🔬 Case 2: Threshold Selection

```r
# Diagnostic plots for threshold selection
thresholds <- quantile(series, seq(0.85, 0.99, by = 0.01))

# Mean Residual Life plot
mrl_data <- mean_residual_life(series, thresholds)
mrl_plot(mrl_data)

# Hill plot
k_values <- 10:100
hill_data <- hill_estimates(series, k_values)
hill_plot(hill_data)
```

See `vignette("threshold-selection")` for comprehensive guide.

### ⚡ Case 3: Performance Optimization

```r
# Use C++ implementation for speed
system.time({
  r_version <- simulate_logistic_map(100000, 3.8, 0.2)
})

system.time({
  cpp_version <- simulate_logistic_map_cpp(100000, 3.8, 0.2)
})

# C++ is ~3-5x faster!
```

See `vignette("performance-optimization")` for benchmarks.

---

## Learning Resources

### 📚 Vignettes

| Vignette | Description | Level |
|----------|-------------|-------|
| [Getting Started](articles/getting-started.html) | Introduction and basic workflows | Beginner |
| [Extremal Index Estimation](articles/estimating-theta-logistic.html) | Detailed guide to θ estimation | Intermediate |
| [Block Maxima vs POT](articles/block-maxima-vs-pot-henon.html) | Comparing extreme value methods | Intermediate |
| [Interactive Analysis](articles/getting-started-interactive.html) | Using the Shiny app | Beginner |
| [Performance Guide](articles/performance-optimization.html) | Optimization techniques | Advanced |
| [Multivariate Analysis](articles/multivariate-analysis.html) | Multi-dimensional systems | Advanced |

### 📖 Function Reference

Browse all functions: [Reference](reference/index.html)

**Core Functions by Category:**

- **Simulation**: `simulate_logistic_map()`, `simulate_henon_map()`, `logistic_bifurcation()`
- **Extreme Value**: `block_maxima()`, `exceedances()`, `fit_gev()`, `fit_gpd()`
- **Extremal Index**: `extremal_index_runs()`, `extremal_index_intervals()`, `bootstrap_extremal_index()`
- **Diagnostics**: `threshold_diagnostics()`, `mrl_plot()`, `hill_plot()`
- **Utilities**: `run_demo()`, `launch_explorer()`, `clean_extreme_data()`

---

## Advanced Features

### Interactive Explorer

Launch a Shiny app for interactive analysis:

```r
launch_explorer()
```

<img src="man/figures/shiny-app.png" width="100%" />

### Recurrence Analysis

```r
# Analyze recurrence patterns
rp <- recurrence_plot(series, embed = 3, delay = 1, eps = 0.1)
image(rp, main = "Recurrence Plot")

# Quantify recurrence
ra <- recurrence_analysis(series, embed = 3)
print(ra)
#> $recurrence_rate
#> [1] 0.0823
#>
#> $determinism
#> [1] 0.2156
```

### Chaos Diagnostics

```r
# Estimate Lyapunov exponent
lambda <- estimate_lyapunov_exponent(series)
cat("Lyapunov exponent:", lambda, "\n")
#> Lyapunov exponent: 0.418

# Positive λ indicates chaos!

# Estimate correlation dimension
cd <- estimate_correlation_dimension(series)
cat("Correlation dimension:", cd$dimension, "\n")
```

---

## Getting Help

### 📝 Documentation

- **Function help**: `?simulate_logistic_map`
- **Vignettes**: `browseVignettes("chaoticds")`
- **Website**: [https://diogoribeiro7.github.io/chaotic-dynamical-systems/](https://diogoribeiro7.github.io/chaotic-dynamical-systems/)

### 💬 Support

- **Report bugs**: [GitHub Issues](https://github.com/DiogoRibeiro7/chaotic-dynamical-systems/issues)
- **Ask questions**: [GitHub Discussions](https://github.com/DiogoRibeiro7/chaotic-dynamical-systems/discussions)
- **Email**: dfr@esmad.ipp.pt

### 🤝 Contributing

Contributions are welcome! Please see our [Contributing Guide](CONTRIBUTING.md).

---

## Citation

If you use `chaoticds` in your research, please cite:

```r
citation("chaoticds")
```

```
To cite chaoticds in publications use:

  Ribeiro, D. (2025). chaoticds: Chaotic Dynamical Systems Utilities.
  R package version 0.1.0.
  https://github.com/DiogoRibeiro7/chaotic-dynamical-systems

A BibTeX entry for LaTeX users is:

  @Manual{,
    title = {chaoticds: Chaotic Dynamical Systems Utilities},
    author = {Diogo Ribeiro},
    year = {2025},
    note = {R package version 0.1.0},
    url = {https://github.com/DiogoRibeiro7/chaotic-dynamical-systems},
  }
```

---

## Theoretical Background

This package implements methods from:

📕 **Coles, S.** (2001). *An Introduction to Statistical Modeling of Extreme Values*. Springer. [DOI: 10.1007/978-1-4471-3675-0](https://doi.org/10.1007/978-1-4471-3675-0)

📕 **Embrechts, P., Klüppelberg, C., & Mikosch, T.** (1997). *Modelling Extremal Events*. Springer. [DOI: 10.1007/978-3-642-33483-2](https://doi.org/10.1007/978-3-642-33483-2)

📄 **Freitas, A. C. M., Freitas, J. M., & Todd, M.** (2010). Hitting time statistics and extreme value theory. *Probability Theory and Related Fields*, 147(3-4), 675-710. [DOI: 10.1007/s00440-009-0221-y](https://doi.org/10.1007/s00440-009-0221-y)

---

## Related Packages

- **evd**: Extreme value distributions ([CRAN](https://CRAN.R-project.org/package=evd))
- **ismev**: Introduction to statistical modelling of extreme values ([CRAN](https://CRAN.R-project.org/package=ismev))
- **extRemes**: Extreme value analysis ([CRAN](https://CRAN.R-project.org/package=extRemes))
- **nonlinearTseries**: Nonlinear time series analysis ([CRAN](https://CRAN.R-project.org/package=nonlinearTseries))

**Why choose chaoticds?**
- Specialized for chaotic systems
- Integrated workflow from simulation to analysis
- Modern R practices (tidyverse-compatible, S3 classes)
- High-performance C++ implementations
- Comprehensive testing (90%+ coverage)

---

## License

MIT © [Diogo Ribeiro](https://orcid.org/0009-0001-2022-7072)

---

## Acknowledgments

Development supported by ESMAD – Instituto Politécnico do Porto.

Special thanks to the R community and contributors to the extreme value analysis literature.

---

<p align="center">
  <strong>⭐ Star this repository if you find it useful!</strong><br>
  <a href="https://github.com/DiogoRibeiro7/chaotic-dynamical-systems">GitHub</a> •
  <a href="https://diogoribeiro7.github.io/chaotic-dynamical-systems/">Documentation</a> •
  <a href="https://github.com/DiogoRibeiro7/chaotic-dynamical-systems/issues">Issues</a>
</p>
