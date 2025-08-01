# Development Agents for chaoticds Package

This document outlines the development workflow and agent responsibilities for the `chaoticds` R package - a toolkit for extreme value analysis in chaotic dynamical systems.

## Project Overview

The `chaoticds` package provides tools for:
- Simulating chaotic maps (logistic, Hénon, and other dynamical systems)
- Extreme value analysis using block maxima and peaks-over-threshold methods
- Extremal index estimation with multiple algorithms
- Bootstrap confidence intervals and uncertainty quantification
- Threshold selection diagnostics and validation
- Cluster analysis of extreme events
- Interactive exploration tools for parameter sensitivity analysis

## Development Environment Setup

### Prerequisites
- R (>= 4.0.0)
- System build tools (gcc, make)
- Git for version control

### Quick Setup
Run the setup script to prepare the development environment:
```bash
chmod +x setup.sh
./setup.sh
```

For minimal installation (faster setup):
```bash
./setup.sh --minimal
```

This will automatically:
- Install R if not present (Linux/macOS)
- Install R package dependencies
- Set up renv for reproducible environments
- Install development tools (devtools, testthat, roxygen2, pkgdown)
- Initialize package structure and directories
- Set up CI/CD configuration
- Create documentation templates

## Agent Roles and Responsibilities

### 1. Core Development Agent
**Focus**: Package architecture, core algorithms, and API design

**Responsibilities**:
- Maintain package metadata (`DESCRIPTION`, `NAMESPACE`)
- Implement core simulation functions for chaotic systems
- Develop extremal index estimators with multiple methods
- Create block maxima and peaks-over-threshold analysis functions
- Ensure consistent function signatures, parameter validation, and error handling
- Implement mathematical algorithms with numerical stability considerations

**Key Files**:
- `R/simulate.R` - Chaotic map simulation functions
- `R/extremal-index.R` - Extremal index estimation methods
- `R/block-maxima.R` - Block maxima approach and GEV fitting
- `R/peaks-over-threshold.R` - POT methods and GPD fitting
- `R/threshold-selection.R` - Threshold selection diagnostics
- `R/utils.R` - Internal utility functions
- `src/*.cpp` - C++ implementations for performance-critical code

**Code Quality Standards**:
```r
# Parameter validation using checkmate
extremal_index_runs <- function(x, threshold, run_length = 3) {
  checkmate::assert_numeric(x, finite = TRUE, min.len = 10)
  checkmate::assert_number(threshold, finite = TRUE)
  checkmate::assert_integerish(run_length, lower = 1)
  
  # Implementation with error handling
  tryCatch({
    # Core algorithm
  }, error = function(e) {
    stop("Extremal index estimation failed: ", e$message, call. = FALSE)
  })
}
```

### 2. Testing and Quality Assurance Agent
**Focus**: Comprehensive testing, code quality, and package validation

**Responsibilities**:
- Write unit tests with edge case coverage using testthat
- Implement integration tests for complete analysis workflows
- Set up and maintain continuous integration (GitHub Actions)
- Perform regular `R CMD check` validation across platforms
- Monitor code coverage and performance regression
- Validate statistical correctness against known benchmarks
- Test package installation across different R versions and platforms

**Key Files**:
- `tests/testthat/test-simulate.R` - Simulation function tests
- `tests/testthat/test-extremal-index.R` - Extremal index estimation tests
- `tests/testthat/test-block-maxima.R` - Block maxima method tests
- `tests/testthat/test-threshold-selection.R` - Threshold diagnostics tests
- `tests/testthat/test-integration.R` - End-to-end workflow tests
- `.github/workflows/R-CMD-check.yaml` - CI configuration
- `tests/testthat.R` - Test runner configuration

**Testing Strategy**:
```r
# Statistical correctness tests
test_that("logistic map produces expected statistical properties", {
  # Test multiple parameter regimes
  series_periodic <- simulate_logistic_map(1000, r = 3.2, x0 = 0.2)
  series_chaotic <- simulate_logistic_map(1000, r = 3.8, x0 = 0.2)
  
  # Validate basic properties
  expect_length(series_chaotic, 1000)
  expect_true(all(series_chaotic >= 0 & series_chaotic <= 1))
  expect_equal(series_chaotic[1], 0.2)
  
  # Statistical properties should differ between regimes
  expect_lt(var(series_periodic), var(series_chaotic))
})

# Extremal index bounds testing
test_that("extremal index estimators return valid bounds", {
  # Test with known theoretical cases
  x_iid <- rnorm(5000)  # Should have θ ≈ 1
  x_dependent <- simulate_logistic_map(5000, 3.9, 0.2)
  
  threshold <- quantile(x_iid, 0.95)
  theta_iid <- extremal_index_runs(x_iid, threshold)
  theta_dep <- extremal_index_runs(x_dependent, threshold)
  
  expect_true(theta_iid > 0 && theta_iid <= 1)
  expect_true(theta_dep > 0 && theta_dep <= 1)
  expect_lt(theta_dep, theta_iid)  # Dependent series should have lower θ
})
```

### 3. Documentation Agent
**Focus**: Comprehensive documentation, tutorials, and user experience

**Responsibilities**:
- Write detailed roxygen2 documentation with mathematical notation
- Create comprehensive vignettes demonstrating real-world applications
- Maintain clear README with installation instructions and quick examples
- Generate and update pkgdown website with attractive styling
- Ensure all examples are executable and pedagogically sound
- Create getting-started guides for different user types
- Maintain bibliography and citations for statistical methods

**Key Files**:
- `man/*.Rd` (auto-generated from roxygen2 comments)
- `vignettes/estimating-theta-logistic.Rmd` - Extremal index tutorial
- `vignettes/block-maxima-vs-pot-henon.Rmd` - Method comparison
- `vignettes/threshold-selection-guide.Rmd` - Threshold selection best practices
- `README.md` - Main package introduction
- `_pkgdown.yml` - Website configuration
- `NEWS.md` - Changelog and version history

**Documentation Standards**:
```r
#' Estimate Extremal Index Using Runs Method
#'
#' Estimates the extremal index θ ∈ (0,1] using the runs estimator of 
#' Smith and Weissman (1994). The extremal index measures the degree of 
#' local dependence in the extremes of a stationary sequence.
#'
#' @param x Numeric vector. The time series data.
#' @param threshold Numeric. Threshold value for defining exceedances.
#' @param run_length Integer. Minimum run length for clustering (default: 3).
#'   Values typically range from 2-5 depending on the application.
#'
#' @return Numeric value between 0 and 1 representing the extremal index estimate.
#'   Values close to 1 indicate weak dependence (nearly independent extremes),
#'   while smaller values indicate stronger clustering of extreme events.
#'
#' @details
#' The runs estimator is based on the lengths of runs of consecutive 
#' exceedances above the threshold. For a sequence with extremal index θ,
#' the expected number of clusters is approximately θ times the number
#' of exceedances.
#'
#' @references
#' Smith, R. L., & Weissman, I. (1994). Estimating the extremal index. 
#' Journal of the Royal Statistical Society, 56(3), 515-528.
#'
#' @examples
#' # Independent data (θ ≈ 1)
#' x_iid <- rnorm(1000)
#' theta_iid <- extremal_index_runs(x_iid, quantile(x_iid, 0.95))
#' print(paste("IID extremal index:", round(theta_iid, 3)))
#'
#' # Dependent chaotic data (θ < 1)
#' x_logistic <- simulate_logistic_map(1000, r = 3.8, x0 = 0.2)
#' theta_dep <- extremal_index_runs(x_logistic, quantile(x_logistic, 0.95))
#' print(paste("Chaotic extremal index:", round(theta_dep, 3)))
#'
#' @seealso [extremal_index_intervals()], [bootstrap_extremal_index()]
#' @export
extremal_index_runs <- function(x, threshold, run_length = 3) {
  # Implementation...
}
```

### 4. Performance Optimization Agent
**Focus**: Computational efficiency, memory optimization, and scalability

**Responsibilities**:
- Implement C++ versions of computationally intensive algorithms using Rcpp
- Manage Rcpp integration, exports, and compilation
- Benchmark R vs C++ implementations across different data sizes
- Optimize memory usage for large datasets (>1M observations)
- Implement intelligent method selection based on data size
- Profile code to identify performance bottlenecks
- Ensure numerical stability in extreme value computations

**Key Files**:
- `src/simulate.cpp` - C++ implementations of map iterations
- `src/extremal_index.cpp` - Fast extremal index estimators
- `src/cluster_analysis.cpp` - Efficient cluster detection
- `R/RcppExports.R` (auto-generated)
- `R/performance-wrappers.R` - Smart method selection
- `inst/benchmarks/` - Performance comparison scripts

**Performance Standards**:
```r
# Automatic method selection based on data size
simulate_logistic_map <- function(n, r, x0, method = "auto") {
  checkmate::assert_integerish(n, lower = 1)
  
  # Use C++ for large datasets, R for small ones
  if (method == "auto") {
    method <- if (n > 10000) "cpp" else "r"
  }
  
  switch(method,
    "cpp" = simulate_logistic_map_cpp(n, r, x0),
    "r" = simulate_logistic_map_r(n, r, x0),
    stop("Invalid method. Use 'auto', 'cpp', or 'r'")
  )
}

# Benchmark function for continuous monitoring
benchmark_extremal_index <- function() {
  data_sizes <- c(1000, 10000, 100000)
  results <- microbenchmark::microbenchmark(
    small = extremal_index_runs(rnorm(1000), 1.96),
    medium = extremal_index_runs(rnorm(10000), 1.96),
    large = extremal_index_runs(rnorm(100000), 1.96),
    times = 10
  )
  return(results)
}
```

### 5. Interactive Tools Agent
**Focus**: Shiny applications, visualizations, and user interfaces

**Responsibilities**:
- Develop comprehensive Shiny app for interactive extreme value exploration
- Create high-quality diagnostic and exploratory plots using ggplot2
- Implement responsive UI for parameter sensitivity analysis
- Design intuitive workflows for non-expert users
- Ensure accessibility and mobile compatibility
- Create exportable reports and visualizations

**Key Files**:
- `inst/shiny/app.R` - Main Shiny application
- `inst/shiny/ui.R` - User interface components
- `inst/shiny/server.R` - Server logic and reactivity
- `R/launch-explorer.R` - App launcher function
- `R/plotting-functions.R` - ggplot2 visualization functions
- `R/diagnostic-plots.R` - Statistical diagnostic visualizations

**Interactive Features**:
```r
#' Launch Interactive Extreme Value Explorer
#'
#' Opens a Shiny application for interactive exploration of extreme value
#' analysis methods applied to chaotic dynamical systems.
#'
#' @param launch_browser Logical. Whether to open in browser (default: TRUE).
#' @param port Integer. Port number for the application (default: auto-select).
#'
#' @examples
#' \dontrun{
#' # Launch the interactive explorer
#' launch_explorer()
#' }
#'
#' @export
launch_explorer <- function(launch_browser = TRUE, port = NULL) {
  app_dir <- system.file("shiny", package = "chaoticds")
  if (app_dir == "") {
    stop("Shiny app not found. Please reinstall the package.")
  }
  
  shiny::runApp(
    appDir = app_dir,
    launch.browser = launch_browser,
    port = port
  )
}
```

### 6. Data and Examples Agent
**Focus**: Datasets, reproducible examples, and educational materials

**Responsibilities**:
- Curate and document example datasets for different analysis scenarios
- Create reproducible example workflows and case studies
- Maintain data generation scripts for synthetic examples
- Ensure all examples follow best practices
- Create educational materials for different skill levels
- Validate reproducibility across different environments using renv

**Key Files**:
- `data/logistic_series.rda` - Example logistic map time series
- `data/henon_attractor.rda` - Hénon map trajectory data
- `data/extreme_events.rda` - Preprocessed extreme event datasets
- `R/data.R` - Data documentation
- `data-raw/generate-examples.R` - Data generation scripts
- `inst/examples/` - Standalone example scripts

**Data Documentation**:
```r
#' Example Logistic Map Time Series
#'
#' A time series generated from the logistic map in the chaotic regime,
#' demonstrating extreme value clustering behavior.
#'
#' @format A numeric vector with 5000 observations from the logistic map
#'   x_{n+1} = 3.9 * x_n * (1 - x_n) with initial condition x_0 = 0.2.
#'
#' @details
#' This dataset illustrates the extreme value properties of deterministic
#' chaotic systems. The extremal index is approximately 0.65, indicating
#' moderate clustering of extreme events.
#'
#' @examples
#' data(logistic_series)
#' plot(logistic_series[1:200], type = "l", 
#'      main = "Logistic Map Time Series",
#'      xlab = "Time", ylab = "Value")
#'
#' # Analyze extreme value properties
#' threshold <- quantile(logistic_series, 0.95)
#' theta <- extremal_index_runs(logistic_series, threshold)
#' print(paste("Extremal index:", round(theta, 3)))
#'
"logistic_series"
```

## Development Workflow

### 1. Feature Development Cycle
```bash
# Start new feature
git checkout -b feature/new-diagnostic-method
git pull origin main

# Set up development environment
R -e "renv::restore()"

# Develop iteratively
R -e "devtools::load_all()"           # Load package
R -e "devtools::test()"               # Run tests
R -e "devtools::check()"              # Full package check

# Document changes
R -e "devtools::document()"           # Update documentation
R -e "pkgdown::build_site()"          # Update website

# Commit and push
git add .
git commit -m "feat: add new diagnostic method"
git push origin feature/new-diagnostic-method
```

### 2. Code Review Process
**Requirements for all pull requests**:
- **Functionality**: Code works correctly and handles edge cases
- **Performance**: No significant performance regressions
- **Documentation**: Complete roxygen2 docs with examples
- **Testing**: New code has >90% test coverage
- **Style**: Consistent with package style guide
- **Backward compatibility**: No breaking changes without major version bump

**Review checklist**:
- [ ] All tests pass (`devtools::check()`)
- [ ] Documentation is complete and accurate
- [ ] Examples run without errors
- [ ] Performance is acceptable
- [ ] Code follows style guidelines
- [ ] No new warnings or notes

### 3. Release Process
```bash
# Prepare release
R -e "usethis::use_version('minor')"    # Bump version
R -e "devtools::spell_check()"          # Check spelling
R -e "urlchecker::url_check()"          # Validate URLs

# Comprehensive testing
R -e "devtools::check_rhub()"           # Test on R-hub
R -e "devtools::check_win_devel()"      # Test on Windows
R -e "devtools::check_mac_release()"    # Test on macOS

# Update documentation
# Edit NEWS.md with changes
# Update README.md if needed
R -e "pkgdown::build_site()"

# Final checks and release
git tag v0.1.0
git push origin v0.1.0
```

## Quality Standards and Metrics

### Code Quality Requirements
- **Documentation**: 100% of exported functions documented with examples
- **Testing**: >90% code coverage for core functions
- **Validation**: Zero errors, warnings, or notes in `R CMD check`
- **Style**: Consistent formatting using `styler::style_pkg()`
- **Performance**: Core functions handle 1M+ observations efficiently

### Statistical Validation
- **Accuracy**: Results match published benchmarks within statistical tolerance
- **Robustness**: Functions handle edge cases and degenerate inputs gracefully
- **Numerical stability**: Algorithms remain stable across parameter ranges
- **Theoretical consistency**: Results align with extreme value theory predictions

### User Experience Standards
- **Installation**: Package installs cleanly on major platforms
- **Documentation**: Clear examples for typical use cases
- **Error messages**: Informative error messages with suggested solutions
- **Performance**: Reasonable execution times for interactive use
- **Reproducibility**: All examples and vignettes are reproducible

## Communication and Coordination

### Development Communication
- **Daily standups** (async via GitHub issues/discussions)
- **Weekly integration meetings** to coordinate between agents
- **Monthly planning sessions** for roadmap updates
- **Quarterly reviews** for major feature planning

### Issue Management
- **Bug reports**: Triaged within 24 hours
- **Feature requests**: Evaluated monthly
- **Performance issues**: Addressed in next patch release
- **Documentation gaps**: Fixed within one week

### Community Engagement
- **User support**: Respond to questions within 48 hours
- **Conference presentations**: Share developments at R/statistics conferences
- **Academic collaboration**: Work with researchers using the package
- **Teaching integration**: Support educational use cases

## Tools and Infrastructure

### Development Environment
- **IDE**: RStudio (recommended) or VS Code with R extension
- **Version control**: Git with GitHub for collaboration
- **Package management**: renv for reproducible environments
- **Documentation**: roxygen2 + pkgdown for comprehensive docs
- **Testing**: testthat for unit tests, covr for coverage

### Continuous Integration
- **GitHub Actions**: Automated testing across R versions and platforms
- **R-hub**: Additional platform testing before releases
- **Codecov**: Test coverage monitoring and reporting
- **Performance monitoring**: Automated benchmarking of key functions

### Quality Assurance Tools
```r
# Regular quality checks
devtools::check()                    # Package validation
styler::style_pkg()                  # Code formatting
lintr::lint_package()               # Code style checking
goodpractice::gp()                  # Best practices validation
```

This comprehensive agent-based development approach ensures high-quality, well-documented, and thoroughly tested code while maintaining clear responsibilities and efficient collaboration.
