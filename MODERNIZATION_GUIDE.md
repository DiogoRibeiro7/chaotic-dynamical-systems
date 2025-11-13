# Package Modernization Guide: chaoticds
## Comprehensive Review and Best Practices Implementation

**Date:** 2025-11-13
**Package Version:** 0.1.0
**Review Scope:** Code quality, modern R practices, testing, CI/CD, ecosystem integration

---

## Executive Summary

The `chaoticds` package demonstrates **strong foundations** with professional-grade infrastructure already in place. This guide provides recommendations for further modernization and optimization following current R development best practices.

### Current State Assessment

✅ **Excellent**: Comprehensive GitHub Actions CI/CD (7 workflows)
✅ **Excellent**: pkgdown website configuration
✅ **Excellent**: Comprehensive test suite (15+ test files)
✅ **Excellent**: roxygen2 documentation
✅ **Good**: CRAN-compliant structure
⚠️ **Needs Improvement**: Code style consistency
⚠️ **Needs Improvement**: Automated linting
⚠️ **Needs Improvement**: Dependency optimization

---

## 1. Code Style and Organization

### 1.1 Current State

**Strengths:**
- Consistent snake_case function naming
- Logical file organization (one topic per file)
- Clear function documentation
- Appropriate use of checkmate for validation

**Areas for Improvement:**
- No automated style checking (lintr)
- Some library() calls in R/ directory (should use @importFrom)
- Mixed comment styles
- Occasional inconsistent spacing

### 1.2 Recommended Improvements

#### A. Adopt Tidyverse Style Guide

**Files Created:**
- `.lintr` - Linting rules configuration
- `.styler.R` - Style configuration
- `.github/workflows/lintr.yaml` - Automated style checking

**Action Items:**
```r
# Install tools
install.packages(c("styler", "lintr"))

# Style entire package
styler::style_pkg()

# Check for linting issues
lintr::lint_package()
```

#### B. Remove library() Calls from R/ Directory

**Current Issue:** Found 4 library()/require() calls in R/ directory

**Files to Fix:**
1. `R/extremal-index.R`
2. `R/cluster-statistics.R`
3. `R/threshold-selection.R`
4. `R/run-demo-chaos.R`

**Before:**
```r
library(ggplot2)

my_function <- function() {
  ggplot(...) + geom_point()
}
```

**After:**
```r
#' @importFrom ggplot2 ggplot geom_point
my_function <- function() {
  ggplot(...) + geom_point()
}
```

#### C. Consistent Comment Style

**Recommendation:** Use tidyverse comment style

**Before:**
```r
# compute block maxima
bm <- block_maxima(x, 50)
```

**After:**
```r
# Compute block maxima
bm <- block_maxima(x, 50)
```

**For multi-line comments:**
```r
# Long explanation that needs multiple lines should be formatted
# like this, with each line starting with a hash and space, and
# continuing until the explanation is complete.
```

---

## 2. Modern R Practices Implementation

### 2.1 S3 Methods and Classes

#### Current State
Functions return base R types (numeric, data.frame, list) without formal classes.

#### Recommendation: Implement S3 Classes

**Example: Create S3 class for extremal index results**

```r
#' Estimate extremal index using runs method
#'
#' @return Object of class "extremal_index" containing:
#' \describe{
#'   \item{estimate}{Numeric extremal index estimate}
#'   \item{method}{Character string "runs" or "intervals"}
#'   \item{threshold}{Threshold used}
#'   \item{n_exceedances}{Number of exceedances}
#'   \item{n_clusters}{Number of clusters}
#' }
#' @export
extremal_index_runs <- function(x, threshold, run_length = 1) {
  # ... existing code ...

  result <- list(
    estimate = theta,
    method = "runs",
    threshold = threshold,
    n_exceedances = length(exc_indices),
    n_clusters = length(unique_clusters),
    run_length = run_length
  )

  class(result) <- "extremal_index"
  result
}

#' Print method for extremal_index objects
#' @export
print.extremal_index <- function(x, ...) {
  cat("Extremal Index Estimation\n")
  cat("Method:", x$method, "\n")
  cat("Estimate:", round(x$estimate, 4), "\n")
  cat("Threshold:", round(x$threshold, 4), "\n")
  cat("Exceedances:", x$n_exceedances, "\n")
  cat("Clusters:", x$n_clusters, "\n")
  invisible(x)
}

#' Summary method for extremal_index objects
#' @export
summary.extremal_index <- function(object, ...) {
  cat("\nExtremal Index Summary\n")
  cat("======================\n")
  print(object)

  if (!is.null(object$ci)) {
    cat("\nConfidence Interval:\n")
    cat("  Lower:", round(object$ci[1], 4), "\n")
    cat("  Upper:", round(object$ci[2], 4), "\n")
  }

  invisible(object)
}

#' Plot method for extremal_index objects
#' @importFrom graphics plot abline
#' @export
plot.extremal_index <- function(x, ...) {
  # Create diagnostic plot
  # e.g., plot of clusters, threshold diagnostics, etc.
}
```

**Benefits:**
- Consistent object structure
- Informative printing
- Enables method dispatch
- Better user experience

### 2.2 Pipe-Friendly Functions

#### Current State
Functions work but aren't optimized for piping.

#### Recommendation: Return Input for Pipe Chains

**Example: Make cluster_summary pipe-friendly**

```r
#' @param .data Optional data frame to append results to (for piping)
#' @param .name_prefix Prefix for result column names
cluster_summary <- function(sizes, .data = NULL, .name_prefix = "cluster_") {
  stats <- list(
    mean_size = mean(sizes),
    var_size = var(sizes),
    max_size = max(sizes),
    n_clusters = length(sizes)
  )

  if (!is.null(.data)) {
    # Add to existing data frame
    for (nm in names(stats)) {
      .data[[paste0(.name_prefix, nm)]] <- stats[[nm]]
    }
    return(.data)
  }

  stats
}

# Usage:
data.frame(series = "logistic") %>%
  cluster_summary(sizes, ., .name_prefix = "evt_")
```

### 2.3 Tidy Data Principles

#### Recommendation: Consistent data frame returns

**Before:**
```r
# Returns list with mixed types
threshold_diagnostics(...) # returns list(plot, data, summary)
```

**After:**
```r
# Return tidy data frame
threshold_diagnostics(...) # returns data frame with one row per threshold
```

---

## 3. Testing Infrastructure Enhancement

### 3.1 Current State

✅ **Excellent test coverage** with comprehensive suites:
- test-comprehensive-validation.R (315 lines)
- test-documentation-examples.R (217 lines)
- test-edge-cases-errors.R (308 lines)
- 15+ test files total
- ~570 test assertions

### 3.2 Recommended Enhancements

#### A. Snapshot Testing for Plots

```r
# tests/testthat/test-plots.R
test_that("mrl_plot produces consistent output", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  x <- simulate_logistic_map(500, 3.8, 0.2)
  thresholds <- quantile(x, seq(0.8, 0.95, 0.05))
  mrl <- mean_residual_life(x, thresholds)

  vdiffr::expect_doppelganger(
    "mrl-plot-logistic",
    mrl_plot(mrl)
  )
})
```

#### B. Property-Based Testing

```r
# tests/testthat/test-properties.R
test_that("block_maxima satisfies mathematical properties", {
  skip_if_not_installed("hedgehog")

  hedgehog::forall(
    hedgehog::gen.sample.int(100, 1000),  # random n
    hedgehog::gen.int(10),                 # random block_size
    function(n, block_size) {
      x <- rnorm(n)
      bm <- block_maxima(x, block_size)

      # Property: All block maxima should be <= global maximum
      all(bm <= max(x))
    }
  )
})
```

#### C. Test Coverage Badges

Add to README.md:
```markdown
[![Codecov](https://codecov.io/gh/DiogoRibeiro7/chaotic-dynamical-systems/branch/main/graph/badge.svg)](https://codecov.io/gh/DiogoRibeiro7/chaotic-dynamical-systems)
```

---

## 4. Dependencies Optimization

### 4.1 Current Dependencies

**Imports:**
- assertthat (redundant with checkmate)
- ggplot2
- evd
- checkmate
- Rcpp

**Suggests:**
- testthat
- knitr, rmarkdown
- evir, ismev, extRemes
- pkgdown, shiny, shinydashboard
- plotly, microbenchmark

### 4.2 Recommended Changes

#### A. Remove assertthat

**Current:** Both assertthat and checkmate are imported
**Recommendation:** Use only checkmate (more comprehensive)

```r
# DESCRIPTION
Imports:
    ggplot2,
    evd,
    checkmate,
    Rcpp
```

#### B. Make ggplot2 Suggested (Optional)

**If plotting is optional:**

```r
# DESCRIPTION
Suggests:
    ggplot2,
    ...

# In functions
mrl_plot <- function(mrl_df) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required for plotting. Install with: install.packages('ggplot2')")
  }

  ggplot2::ggplot(mrl_df, ggplot2::aes(x = threshold, y = mean_excess)) +
    ggplot2::geom_point() +
    ggplot2::labs(title = "Mean Residual Life Plot")
}
```

#### C. Add Minimal Dependencies Badge

```r
# Calculate
length(tools::package_dependencies("chaoticds", which = "Imports")[[1]])
```

### 4.3 Conditional Features

```r
# R/zzz.R
.onAttach <- function(libname, pkgname) {
  # Check for optional features
  has_ggplot2 <- requireNamespace("ggplot2", quietly = TRUE)
  has_shiny <- requireNamespace("shiny", quietly = TRUE)

  msg <- paste0(
    "chaoticds v", utils::packageVersion("chaoticds"), "\n",
    "Optional features:\n",
    if (has_ggplot2) "  ✓ Visualization (ggplot2)\n" else "  ✗ Visualization (install ggplot2)\n",
    if (has_shiny) "  ✓ Interactive explorer (shiny)\n" else "  ✗ Interactive explorer (install shiny)\n"
  )

  packageStartupMessage(msg)
}
```

---

## 5. CI/CD Pipeline Enhancement

### 5.1 Current Workflows (Excellent!)

1. ✅ R-CMD-check.yaml
2. ✅ test-coverage.yaml
3. ✅ pkgdown.yaml
4. ✅ code-quality.yml
5. ✅ benchmark.yml
6. ✅ security.yml
7. ✅ update-deps.yaml

### 5.2 Additional Recommendations

#### A. Add Lint Workflow

Already created: `.github/workflows/lintr.yaml`

#### B. Enhance R-CMD-check

Add platforms:
```yaml
strategy:
  matrix:
    config:
      - {os: ubuntu-latest, r: 'release'}
      - {os: ubuntu-latest, r: 'devel'}
      - {os: ubuntu-latest, r: 'oldrel-1'}
      - {os: windows-latest, r: 'release'}
      - {os: macOS-latest, r: 'release'}
      - {os: macOS-latest, r: 'devel'}
```

#### C. Add Dependency Graph

```yaml
# .github/workflows/dependency-review.yml
name: Dependency Review

on:
  pull_request:
    branches: [main]

jobs:
  dependency-review:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: actions/dependency-review-action@v3
```

---

## 6. Package Website (pkgdown) Enhancement

### 6.1 Current State

✅ Excellent pkgdown configuration with:
- Bootstrap 5 theme
- Organized reference sections
- Vignettes integration
- Author information

### 6.2 Recommended Enhancements

#### A. Add Custom CSS

```yaml
# _pkgdown.yml
template:
  bootstrap: 5
  bslib:
    primary: "#0054AD"
    secondary: "#767676"
    success: "#28A745"
    info: "#17A2B8"
    warning: "#FFC107"
    danger: "#DC3545"
```

#### B. Add Logo

```r
# Create hex logo
usethis::use_logo("path/to/logo.png")
```

#### C. Enhance Homepage

```yaml
# _pkgdown.yml
home:
  title: "Chaotic Dynamical Systems Analysis"
  description: >
    Professional R package for extreme value analysis of chaotic systems.

  sidebar:
    structure: [links, license, community, citation, authors, dev]

  links:
  - text: Report a bug
    href: https://github.com/DiogoRibeiro7/chaotic-dynamical-systems/issues
  - text: Ask a question
    href: https://github.com/DiogoRibeiro7/chaotic-dynamical-systems/discussions
```

---

## 7. Modern Function Design Patterns

### 7.1 Factory Functions

For creating custom maps:

```r
#' Create a custom map function
#'
#' @param type Character: "logistic", "henon", etc.
#' @param ... Parameters specific to the map type
#' @return Function that iterates the map
#' @export
create_map <- function(type = c("logistic", "henon", "tent"), ...) {
  type <- match.arg(type)

  switch(type,
    logistic = function(x, r) r * x * (1 - x),
    henon = function(xy, a, b) {
      list(
        x = 1 - a * xy$x^2 + xy$y,
        y = b * xy$x
      )
    },
    tent = function(x, r) ifelse(x < 0.5, r * x, r * (1 - x))
  )
}
```

### 7.2 Functional Programming Helpers

```r
#' Apply function to rolling windows
#'
#' @param x Numeric vector
#' @param width Window width
#' @param FUN Function to apply
#' @return Vector of function results
#' @export
rolling_apply <- function(x, width, FUN, ...) {
  n <- length(x) - width + 1
  vapply(seq_len(n), function(i) {
    FUN(x[i:(i + width - 1)], ...)
  }, numeric(1))
}

# Usage
rolling_max <- function(x, width) rolling_apply(x, width, max)
```

### 7.3 Progress Bars for Long Operations

```r
#' @param .progress Logical; show progress bar?
logistic_bifurcation <- function(r_seq, n_iter = 200, discard = 100,
                                  x0 = 0.2, .progress = FALSE) {
  # ... validation ...

  if (.progress && requireNamespace("progress", quietly = TRUE)) {
    pb <- progress::progress_bar$new(
      format = "[:bar] :percent eta: :eta",
      total = length(r_seq)
    )
  }

  results <- vector("list", length(r_seq))

  for (i in seq_along(r_seq)) {
    results[[i]] <- compute_bifurcation_point(r_seq[i], ...)
    if (.progress) pb$tick()
  }

  do.call(rbind, results)
}
```

---

## 8. Documentation Best Practices

### 8.1 Enhanced Examples

**Before:**
```r
#' @examples
#' simulate_logistic_map(100, 3.8, 0.2)
```

**After:**
```r
#' @examples
#' # Basic usage
#' series <- simulate_logistic_map(100, 3.8, 0.2)
#' plot(series, type = "l", main = "Logistic Map Time Series")
#'
#' # Chaotic vs periodic regimes
#' series_chaotic <- simulate_logistic_map(500, r = 3.9, x0 = 0.2)
#' series_periodic <- simulate_logistic_map(500, r = 3.2, x0 = 0.2)
#'
#' par(mfrow = c(1, 2))
#' plot(series_chaotic, type = "l", main = "Chaotic (r=3.9)")
#' plot(series_periodic, type = "l", main = "Periodic (r=3.2)")
#' par(mfrow = c(1, 1))
#'
#' \donttest{
#' # Bifurcation diagram
#' r_vals <- seq(2.5, 4, length.out = 1000)
#' bif <- logistic_bifurcation(r_vals, n_iter = 500, discard = 450)
#' plot(bif$r, bif$x, pch = ".", cex = 0.5,
#'      xlab = "r", ylab = "x", main = "Bifurcation Diagram")
#' }
```

### 8.2 Lifecycle Badges

```r
#' @description
#' \lifecycle{stable}
#' Core function for simulating logistic map dynamics.
#'
#' @description
#' \lifecycle{experimental}
#' This function is under active development and the API may change.
```

### 8.3 Cross-References

```r
#' @seealso
#' Core extreme value functions:
#' \itemize{
#'   \item \code{\link{block_maxima}} for block maxima method
#'   \item \code{\link{exceedances}} for peaks-over-threshold
#'   \item \code{\link{extremal_index_runs}} for extremal index estimation
#' }
#'
#' Related simulation functions:
#' \itemize{
#'   \item \code{\link{simulate_henon_map}} for 2D chaotic maps
#'   \item \code{\link{logistic_bifurcation}} for parameter exploration
#' }
```

---

## 9. Performance Optimization Opportunities

### 9.1 Memoization for Expensive Computations

```r
#' @importFrom memoise memoise
estimate_correlation_dimension_memo <- memoise::memoise(
  estimate_correlation_dimension
)
```

### 9.2 Parallel Processing Option

```r
#' @param .parallel Use parallel processing? Requires 'future' package
#' @param .workers Number of parallel workers
bootstrap_extremal_index <- function(x, threshold, run_length = 1, B = 1000,
                                      .parallel = FALSE, .workers = NULL) {
  if (.parallel) {
    if (!requireNamespace("future", quietly = TRUE)) {
      stop("Install 'future' for parallel processing")
    }

    future::plan(future::multisession, workers = .workers)
    boot_samples <- future.apply::future_replicate(B, {
      # Bootstrap logic
    })
  } else {
    boot_samples <- replicate(B, {
      # Bootstrap logic
    })
  }

  # ... rest of function ...
}
```

---

## 10. Vignette Improvements

### 10.1 Create Comprehensive Getting Started Guide

```r
# vignettes/getting-started.Rmd
---
title: "Getting Started with chaoticds"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Getting Started with chaoticds}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

## Installation

```{r eval = FALSE}
# From GitHub
devtools::install_github("DiogoRibeiro7/chaotic-dynamical-systems")
```

## Quick Start: 5-Minute Overview

### Simulate Chaotic Dynamics
... examples ...

### Extreme Value Analysis
... examples ...

### Extremal Index Estimation
... examples ...
```

### 10.2 Add Precompiled Vignettes

For computationally intensive vignettes:

```r
# Save results
saveRDS(expensive_results, "vignettes/cached_results.rds")

# In vignette
expensive_results <- readRDS("cached_results.rds")
```

---

## 11. Package Metadata Enhancements

### 11.1 Add Badges to README

```markdown
# chaoticds <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/DiogoRibeiro7/chaotic-dynamical-systems/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/DiogoRibeiro7/chaotic-dynamical-systems/actions/workflows/R-CMD-check.yaml)
[![Codecov](https://codecov.io/gh/DiogoRibeiro7/chaotic-dynamical-systems/branch/main/graph/badge.svg)](https://codecov.io/gh/DiogoRibeiro7/chaotic-dynamical-systems)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![CRAN status](https://www.r-pkg.org/badges/version/chaoticds)](https://CRAN.R-project.org/package=chaoticds)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->
```

### 11.2 Enhanced DESCRIPTION

```dcf
Title: Tools for Extreme Value Analysis of Chaotic Dynamical Systems
Description: Provides comprehensive tools for analyzing extreme events in
    chaotic dynamical systems. Includes functions for simulating various
    chaotic maps (logistic, Hénon, tent, etc.), performing extreme value
    analysis using block maxima and peaks-over-threshold methods, estimating
    the extremal index, and conducting diagnostic analyses. Implements both
    R and C++ algorithms for performance. Supports the methodology described
    in Coles (2001) <doi:10.1007/978-1-4471-3675-0> and Embrechts et al.
    (1997) <doi:10.1007/978-3-642-33483-2>.
```

---

## 12. Code Quality Checklist

### Daily Development

- [ ] Run `styler::style_file()` before committing
- [ ] Run `lintr::lint()` on changed files
- [ ] Write tests for new functions
- [ ] Update documentation with `devtools::document()`
- [ ] Run `devtools::check()` locally

### Before Each Commit

```bash
# Run locally
Rscript -e "styler::style_pkg()"
Rscript -e "devtools::document()"
Rscript -e "devtools::test()"
Rscript -e "lintr::lint_package()"
```

### Before Release

- [ ] Update NEWS.md
- [ ] Update version in DESCRIPTION
- [ ] Run `devtools::check(remote = TRUE)`
- [ ] Run `rhub::check_for_cran()`
- [ ] Build and review pkgdown site
- [ ] Check all vignettes build
- [ ] Review test coverage report
- [ ] Update CITATION if needed

---

## 13. Implementation Priority

### High Priority (Do First)

1. ✅ Add lintr configuration (DONE)
2. ✅ Add styler configuration (DONE)
3. ✅ Create lintr GitHub Action (DONE)
4. 🔲 Run `styler::style_pkg()` to format all code
5. 🔲 Remove library() calls from R/ directory
6. 🔲 Remove assertthat dependency

### Medium Priority (Do Soon)

7. 🔲 Implement S3 classes for major return types
8. 🔲 Add print/summary methods
9. 🔲 Enhance function examples
10. 🔲 Add snapshot tests for plots
11. 🔲 Create comprehensive getting started vignette

### Low Priority (Nice to Have)

12. 🔲 Add pipe-friendly variants
13. 🔲 Implement parallel processing options
14. 🔲 Add progress bars for long operations
15. 🔲 Create hex logo
16. 🔲 Add more badges to README

---

## 14. Estimated Impact

### Code Style (lintr/styler)
**Effort:** 2-4 hours
**Impact:** High (improves consistency, catches bugs)
**ROI:** Very High

### Remove library() Calls
**Effort:** 1-2 hours
**Impact:** High (CRAN requirement)
**ROI:** Very High

### S3 Classes
**Effort:** 1 day
**Impact:** Medium-High (better UX)
**ROI:** High

### Enhanced Documentation
**Effort:** 1-2 days
**Impact:** High (user adoption)
**ROI:** High

### Vignette Improvements
**Effort:** 2-3 days
**Impact:** Medium (helps users)
**ROI:** Medium-High

---

## 15. Conclusion

The `chaoticds` package is **already well-structured** with excellent CI/CD, comprehensive testing, and professional documentation. The recommended modernizations will:

1. **Improve code consistency** through automated styling
2. **Enhance user experience** with S3 classes and methods
3. **Increase maintainability** through better practices
4. **Boost discoverability** through improved documentation

### Overall Assessment

**Current Grade:** A-
**Potential Grade (with improvements):** A+

The package is ready for CRAN submission **now**. The modernization suggestions will make it even better for long-term maintenance and user adoption.

---

**Next Steps:**

1. Review this guide
2. Prioritize improvements
3. Implement high-priority items
4. Submit to CRAN
5. Continue iterating on lower-priority enhancements

**Estimated Time to Implement All High-Priority Items:** 1 day

---

**Document Prepared By:** R Package Development Consultant
**Date:** 2025-11-13
**Package Reviewed:** chaoticds v0.1.0
