# Quick Start: Modernize chaoticds in 1 Day

This guide provides a step-by-step plan to implement the highest-impact modernization improvements in approximately 8 hours.

---

## Morning Session (4 hours)

### 1. Code Style Automation (1 hour)

```r
# Install required packages
install.packages(c("styler", "lintr"))

# Style the entire package
styler::style_pkg()

# Check for linting issues
lints <- lintr::lint_package()
print(lints)

# Review and fix any critical lints
# Focus on: unused imports, T/F usage, trailing whitespace
```

**Deliverable:** Consistently formatted code base

---

### 2. Remove library() Calls from R/ Directory (30 minutes)

**Files to fix:**
1. R/extremal-index.R
2. R/cluster-statistics.R
3. R/threshold-selection.R
4. R/run-demo-chaos.R

**Process for each file:**

```r
# BEFORE (in R/extremal-index.R)
library(ggplot2)

plot_hitting_times <- function(...) {
  ggplot(...) + geom_point()
}

# AFTER
#' @importFrom ggplot2 ggplot geom_point
plot_hitting_times <- function(...) {
  ggplot(...) + geom_point()
}
```

Then run:
```r
devtools::document()  # Regenerate NAMESPACE
```

**Deliverable:** Clean NAMESPACE, proper imports

---

### 3. Remove assertthat Dependency (30 minutes)

```r
# 1. Open DESCRIPTION
# 2. Remove 'assertthat' from Imports
# 3. Check if any code uses assertthat:: calls
grep -r "assertthat::" R/

# If none found, you're done!
# Otherwise, replace with checkmate equivalents
```

**Deliverable:** One fewer dependency

---

### 4. Add README Badges (30 minutes)

Edit README.md:

```markdown
# chaoticds

<!-- badges: start -->
[![R-CMD-check](https://github.com/DiogoRibeiro7/chaotic-dynamical-systems/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/DiogoRibeiro7/chaotic-dynamical-systems/actions/workflows/R-CMD-check.yaml)
[![Codecov](https://codecov.io/gh/DiogoRibeiro7/chaotic-dynamical-systems/branch/main/graph/badge.svg)](https://codecov.io/gh/DiogoRibeiro7/chaotic-dynamical-systems)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

R package for extreme value analysis of chaotic dynamical systems.
```

**Deliverable:** Professional README with CI/CD status badges

---

### 5. Enhanced DESCRIPTION (30 minutes)

```r
# Current Description is good, but can be enhanced:

Description: Provides comprehensive tools for analyzing extreme events in
    chaotic dynamical systems. Includes functions for simulating various
    chaotic maps (logistic, Hénon, tent, etc.), performing extreme value
    analysis using block maxima and peaks-over-threshold methods, estimating
    the extremal index, and conducting diagnostic analyses. Implements both
    R and C++ algorithms for performance. Based on methodology from Coles
    (2001) "An Introduction to Statistical Modeling of Extreme Values"
    <doi:10.1007/978-1-4471-3675-0>.

# Add Keywords
Keywords: chaos, extreme-value-theory, time-series, dynamical-systems

# Update Authors if needed (already good)
```

**Deliverable:** Enhanced package metadata

---

### 6. Verify CI/CD (1 hour)

```bash
# Check all workflows run
git add .
git commit -m "Modernize: code style, imports, badges"
git push

# Monitor GitHub Actions
# https://github.com/DiogoRibeiro7/chaotic-dynamical-systems/actions

# Fix any failures
```

**Deliverable:** All CI/CD pipelines green

---

## Afternoon Session (4 hours)

### 7. Implement S3 Class for extremal_index (2 hours)

Create new file: `R/s3-extremal-index.R`

```r
#' Create extremal_index object
#' @keywords internal
new_extremal_index <- function(estimate, method, threshold, n_exceedances,
                                n_clusters, run_length = NULL, ...) {
  structure(
    list(
      estimate = estimate,
      method = method,
      threshold = threshold,
      n_exceedances = n_exceedances,
      n_clusters = n_clusters,
      run_length = run_length,
      ...
    ),
    class = "extremal_index"
  )
}

#' @export
print.extremal_index <- function(x, digits = 4, ...) {
  cat("\nExtremal Index (", x$method, " method)\n", sep = "")
  cat(strrep("=", 40), "\n")
  cat("Estimate:    ", format(round(x$estimate, digits)), "\n")
  cat("Threshold:   ", format(round(x$threshold, digits)), "\n")
  cat("Exceedances: ", x$n_exceedances, "\n")
  cat("Clusters:    ", x$n_clusters, "\n\n")
  invisible(x)
}

#' @export
summary.extremal_index <- function(object, ...) {
  cat("\nExtremal Index Summary\n")
  cat(strrep("=", 40), "\n\n")

  print(object)

  cat("Cluster Statistics:\n")
  cat("  Average cluster size: ",
      round(object$n_exceedances / object$n_clusters, 2), "\n")

  invisible(object)
}
```

Update `R/extremal-index.R`:

```r
extremal_index_runs <- function(x, threshold, run_length = 1) {
  # ... existing validation and computation ...

  # REPLACE: return(theta)
  # WITH:
  new_extremal_index(
    estimate = theta,
    method = "runs",
    threshold = threshold,
    n_exceedances = length(exc_indices),
    n_clusters = length(unique(clusters)),
    run_length = run_length
  )
}
```

Test:
```r
devtools::load_all()
x <- simulate_logistic_map(1000, 3.8, 0.2)
result <- extremal_index_runs(x, quantile(x, 0.95), 2)
print(result)
summary(result)
```

**Deliverable:** S3 class with print/summary methods

---

### 8. Enhance Key Function Documentation (1.5 hours)

Pick 5 most important functions and enhance:
1. simulate_logistic_map
2. block_maxima
3. extremal_index_runs
4. fit_gev
5. run_demo

For each, add:
- More detailed @description
- @details section
- @section with background
- Enhanced @examples
- @references if applicable
- @family tags

Example template:
```r
#' Function Title
#'
#' @description
#' One-line summary of what function does.
#'
#' @details
#' Detailed explanation of how it works, when to use it,
#' what methods it implements, etc.
#'
#' @section Mathematical Background:
#' Equations, theory, citations.
#'
#' @param x Description with type, constraints, typical values
#'
#' @return Detailed description of return value structure
#'
#' @references
#' Author (Year). Title. Journal. DOI.
#'
#' @seealso
#' Related functions with \code{\link{function_name}}
#'
#' @examples
#' # Basic example
#' result <- my_function(...)
#'
#' # Advanced example
#' ...
#'
#' \donttest{
#' # Long-running example
#' ...
#' }
#'
#' @family simulation functions
#' @export
```

**Deliverable:** Enhanced documentation for key functions

---

### 9. Create Comprehensive Getting Started Vignette (30 minutes)

Create `vignettes/quickstart.Rmd`:

```rmd
---
title: "Quick Start Guide"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Quick Start Guide}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

## Installation

```{r eval=FALSE}
# From GitHub
devtools::install_github("DiogoRibeiro7/chaotic-dynamical-systems")
```

## 5-Minute Overview

### Simulate Chaotic Dynamics

```{r}
library(chaoticds)

# Generate logistic map trajectory
series <- simulate_logistic_map(n = 500, r = 3.8, x0 = 0.2)
plot(series, type = "l", main = "Logistic Map (r=3.8)")
```

### Extreme Value Analysis

```{r}
# Block maxima method
bm <- block_maxima(series, block_size = 25)
hist(bm, main = "Distribution of Block Maxima")

# Peaks over threshold
threshold <- quantile(series, 0.95)
exc <- exceedances(series, threshold)
```

### Extremal Index Estimation

```{r}
theta <- extremal_index_runs(series, threshold, run_length = 2)
print(theta)
```

## What's Next?

- See `vignette("estimating-theta-logistic")` for detailed extremal index tutorial
- See `vignette("block-maxima-vs-pot-henon")` for method comparison
- Run `run_demo()` for a complete analysis workflow
```

Build:
```r
devtools::build_vignettes()
```

**Deliverable:** User-friendly getting started guide

---

## End of Day: Verification (30 minutes)

### Final Checks

```r
# 1. Run full check
devtools::check()

# 2. Verify no errors/warnings
# 3. Build pkgdown site locally
pkgdown::build_site()

# 4. Review site at docs/index.html
# 5. Commit everything
git add .
git commit -m "feat: Modernize package - style, S3 classes, enhanced docs"
git push

# 6. Watch CI/CD pass
# 7. Celebrate! 🎉
```

---

## Summary of Achievements

After 8 hours, you will have:

✅ Consistently styled code (styler)
✅ Automated linting (lintr + GitHub Action)
✅ Clean imports (no library() in R/)
✅ One fewer dependency (removed assertthat)
✅ Professional README with badges
✅ S3 class system for main results
✅ Enhanced documentation for key functions
✅ Quick start vignette
✅ All CI/CD passing

---

## Next Steps (Future Work)

### Week 2: Advanced Features (8 hours)
- Implement more S3 classes
- Add progress bars to long-running functions
- Create additional vignettes
- Add more plot methods

### Week 3: Performance (4 hours)
- Profile code for bottlenecks
- Optimize hot paths
- Consider memoization for expensive operations

### Week 4: Community (4 hours)
- Create hex logo
- Write blog post announcing package
- Submit to rOpenSci for review
- Submit to CRAN

---

## Maintenance Going Forward

### Before Each Commit
```bash
Rscript -e "styler::style_pkg()"
Rscript -e "devtools::document()"
Rscript -e "devtools::test()"
```

### Before Each Release
```r
# 1. Update NEWS.md
# 2. Bump version in DESCRIPTION
# 3. Run comprehensive checks
devtools::check(remote = TRUE)
rhub::check_for_cran()

# 4. Build and review website
pkgdown::build_site()

# 5. Create GitHub release
# 6. Submit to CRAN (when ready)
```

---

**Ready to start? Begin with Morning Session Step 1!**
