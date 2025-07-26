# Development Agents for chaoticds Package

This document outlines the development workflow and agent responsibilities for the `chaoticds` R package - a toolkit for extreme value analysis in chaotic dynamical systems.

## Project Overview

The `chaoticds` package provides tools for:
- Simulating chaotic maps (logistic, Hénon)
- Extreme value analysis (block maxima, peaks-over-threshold)
- Extremal index estimation
- Bootstrap confidence intervals
- Threshold selection diagnostics
- Cluster analysis and mixing condition checks

## Development Environment Setup

Run the setup script to prepare the development environment:
```bash
./setup.sh
```

This will:
- Install R dependencies
- Set up renv for reproducible environments
- Install development tools (devtools, testthat, roxygen2)
- Initialize package structure
- Set up CI/CD configuration

## Agent Roles and Responsibilities

### 1. Core Development Agent
**Focus**: Package architecture, core functions, and API design

**Responsibilities**:
- Maintain package structure (`DESCRIPTION`, `NAMESPACE`)
- Implement core simulation functions (`simulate_logistic_map`, `simulate_henon_map`)
- Develop extremal index estimators (`extremal_index_runs`, `extremal_index_intervals`)
- Create block maxima and POT analysis functions
- Ensure consistent function signatures and error handling

**Key Files**:
- `R/simulate.R`
- `R/extremal-index.R`
- `R/block-maxima.R`
- `R/peaks-over-threshold.R`
- `R/RcppExports.R` (C++ interfaces)

### 2. Testing and Quality Assurance Agent
**Focus**: Test coverage, code quality, and package validation

**Responsibilities**:
- Write comprehensive unit tests using testthat
- Implement integration tests for end-to-end workflows
- Set up continuous integration (GitHub Actions)
- Perform `R CMD check` validation
- Monitor code coverage and performance

**Key Files**:
- `tests/testthat/test-*.R`
- `.github/workflows/R-CMD-check.yaml`
- `tests/testthat.R`

**Testing Priorities**:
```r
# Core simulation functions
test_that("logistic map generates expected sequence", {
  series <- simulate_logistic_map(100, 3.8, 0.2)
  expect_length(series, 100)
  expect_true(all(series >= 0 & series <= 1))
})

# Extremal index estimation
test_that("extremal index runs estimator works", {
  x <- rnorm(1000)
  threshold <- quantile(x, 0.95)
  theta <- extremal_index_runs(x, threshold, 3)
  expect_true(theta > 0 & theta <= 1)
})
```

### 3. Documentation Agent
**Focus**: Documentation, vignettes, and user experience

**Responsibilities**:
- Write comprehensive roxygen2 documentation
- Create vignettes demonstrating package usage
- Maintain README with clear installation and usage instructions
- Generate pkgdown website
- Ensure examples in documentation are executable

**Key Files**:
- `man/*.Rd` (generated from roxygen2)
- `vignettes/*.Rmd`
- `README.md`
- `_pkgdown.yml`

**Documentation Standards**:
```r
#' Simulate the logistic map
#'
#' Generates a time series following the logistic map equation:
#' x[n+1] = r * x[n] * (1 - x[n])
#'
#' @param n Integer. Number of iterations to generate.
#' @param r Numeric. Growth rate parameter (typically 0-4).
#' @param x0 Numeric. Initial value in (0, 1).
#'
#' @return Numeric vector of length n containing the orbit trajectory.
#'
#' @examples
#' # Chaotic regime
#' series <- simulate_logistic_map(1000, r = 3.8, x0 = 0.2)
#' plot(series[1:100], type = "l")
#'
#' @export
simulate_logistic_map <- function(n, r, x0) {
  # Implementation...
}
```

### 4. Performance Optimization Agent
**Focus**: C++ integration, performance monitoring, and scalability

**Responsibilities**:
- Implement C++ versions of computationally intensive functions
- Manage Rcpp integration and exports
- Benchmark R vs C++ implementations
- Optimize memory usage for large datasets
- Implement fast wrapper functions with automatic method selection

**Key Files**:
- `src/*.cpp` (C++ implementations)
- `R/RcppExports.R`
- `R/fast-functions.R`

**Performance Targets**:
- C++ implementations should be 5-10x faster for large datasets (n > 10,000)
- Memory usage should scale linearly with input size
- Automatic fallback to R implementations when C++ unavailable

### 5. Interactive Tools Agent
**Focus**: Shiny applications and user interfaces

**Responsibilities**:
- Develop interactive Shiny app for extreme value exploration
- Create visualization functions using ggplot2
- Implement diagnostic plots and exploratory tools
- Ensure responsive UI for parameter exploration

**Key Files**:
- `inst/shiny/`
- `R/launch-explorer.R`
- `R/cluster-statistics.R` (plotting functions)

### 6. Data and Examples Agent
**Focus**: Datasets, examples, and reproducible research

**Responsibilities**:
- Curate example datasets (`data/`)
- Create realistic test cases and benchmarks
- Maintain example scripts and workflows
- Ensure reproducibility with renv

**Key Files**:
- `data/`
- `R/data.R` (data documentation)
- `data-raw/` (data generation scripts)

## Development Workflow

### 1. Feature Development
```bash
# Create feature branch
git checkout -b feature/new-estimator

# Develop and test
R -e "devtools::load_all(); devtools::test()"

# Document
R -e "devtools::document()"

# Check package
R -e "devtools::check()"
```

### 2. Code Review Process
- All changes require agent review
- Focus on: correctness, performance, documentation, testing
- Ensure backward compatibility
- Validate examples and vignettes

### 3. Release Process
```bash
# Version bump
R -e "usethis::use_version()"

# Final checks
R -e "devtools::check_rhub()"
R -e "devtools::check_win_devel()"

# Update NEWS.md
# Tag release
git tag v0.1.0
```

## Quality Standards

### Code Quality
- All functions must have roxygen2 documentation
- Test coverage > 90% for core functions
- No warnings or errors in `R CMD check`
- Consistent coding style (use `styler` package)

### Performance Requirements
- Core functions should handle datasets up to 1M observations
- C++ implementations for computationally intensive operations
- Memory-efficient algorithms for large-scale analysis

### Documentation Requirements
- Complete function documentation with examples
- At least 2 comprehensive vignettes
- README with installation and basic usage
- Changelog in NEWS.md

## Communication and Coordination

### Daily Standups
- Progress updates from each agent
- Identify blockers and dependencies
- Coordinate integration efforts

### Weekly Reviews
- Code quality assessment
- Performance benchmarking
- Documentation completeness check
- User feedback integration

### Monthly Planning
- Roadmap updates
- Feature prioritization
- Technical debt assessment
- Release planning

## Tools and Infrastructure

### Development Tools
- **RStudio**: Primary IDE
- **devtools**: Package development workflow
- **testthat**: Unit testing framework
- **roxygen2**: Documentation generation
- **renv**: Dependency management
- **pkgdown**: Website generation

### CI/CD Pipeline
- **GitHub Actions**: Automated testing and checks
- **R-hub**: Multi-platform testing
- **codecov**: Test coverage monitoring

### Monitoring
- Package performance benchmarks
- Download statistics (CRAN)
- User feedback and issue tracking

This agent-based development approach ensures comprehensive coverage of all aspects of the package while maintaining high quality standards and clear responsibilities.
