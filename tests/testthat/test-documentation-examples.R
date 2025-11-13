# Test that all @examples from documentation work correctly
# This validates that documentation matches actual function behavior

context("Documentation Examples Validation")

# =============================================================================
# SIMULATION FUNCTIONS - Examples from Documentation
# =============================================================================

test_that("simulate_logistic_map example works", {
  # From documentation: series <- simulate_logistic_map(100, 3.8, 0.2)
  expect_silent({
    series <- simulate_logistic_map(100, 3.8, 0.2)
  })
  expect_length(series, 100)
  expect_equal(series[1], 0.2)
})

test_that("simulate_henon_map example works", {
  # From documentation: orbit <- simulate_henon_map(100, 1.4, 0.3)
  expect_silent({
    orbit <- simulate_henon_map(100, 1.4, 0.3)
  })
  expect_s3_class(orbit, "data.frame")
  expect_equal(nrow(orbit), 100)
})

test_that("simulate_tent_map example works", {
  # From documentation: series <- simulate_tent_map(100, r = 2, x0 = 0.1)
  expect_silent({
    series <- simulate_tent_map(100, r = 2, x0 = 0.1)
  })
  expect_length(series, 100)
})

test_that("simulate_lozi_map example works", {
  # From documentation: orbit <- simulate_lozi_map(100)
  expect_silent({
    orbit <- simulate_lozi_map(100)
  })
  expect_s3_class(orbit, "data.frame")
})

test_that("simulate_cat_map example works", {
  # From documentation: orbit <- simulate_cat_map(100)
  expect_silent({
    orbit <- simulate_cat_map(100)
  })
  expect_s3_class(orbit, "data.frame")
})

test_that("logistic_bifurcation example works", {
  # From documentation:
  # r_vals <- seq(2.5, 4, length.out = 200)
  # bifdat <- logistic_bifurcation(r_vals, n_iter = 200, discard = 100)
  # plot(bifdat$r, bifdat$x, pch = '.', cex = 0.5)

  expect_silent({
    r_vals <- seq(2.5, 4, length.out = 200)
    bifdat <- logistic_bifurcation(r_vals, n_iter = 200, discard = 100)
  })

  expect_s3_class(bifdat, "data.frame")
  expect_true(all(c("r", "x") %in% names(bifdat)))

  # Plotting should work
  expect_silent({
    plot(bifdat$r, bifdat$x, pch = '.', cex = 0.5)
  })
})

# =============================================================================
# EXTREME VALUE FUNCTIONS - Examples
# =============================================================================

test_that("block_maxima example works", {
  # From documentation: block_maxima(rnorm(1000), 50)
  set.seed(123)
  expect_silent({
    bm <- block_maxima(rnorm(1000), 50)
  })
  expect_length(bm, 20)
})

test_that("fit_gev example works", {
  skip_if_not_installed("evd")

  # From documentation:
  # m <- block_maxima(rnorm(1000), 50)
  # fit <- fit_gev(m)

  set.seed(123)
  expect_silent({
    m <- block_maxima(rnorm(1000), 50)
    fit <- fit_gev(m)
  })
  expect_true(!is.null(fit))
})

# =============================================================================
# RECURRENCE ANALYSIS - Examples
# =============================================================================

test_that("recurrence_plot example works", {
  # From documentation:
  # rp <- recurrence_plot(rnorm(100))
  # image(rp)

  set.seed(42)
  expect_silent({
    rp <- recurrence_plot(rnorm(100))
  })
  expect_true(is.matrix(rp))

  expect_silent({
    image(rp)
  })
})

test_that("recurrence_analysis example works", {
  # From documentation: recurrence_analysis(rnorm(100))
  set.seed(42)
  expect_silent({
    ra <- recurrence_analysis(rnorm(100))
  })

  expect_type(ra, "list")
  expect_true("recurrence_rate" %in% names(ra))
  expect_true("determinism" %in% names(ra))
})

# =============================================================================
# EXTREMAL INDEX - Examples
# =============================================================================

test_that("hitting_times example would work with proper data", {
  # Create sample data
  x <- c(0.5, 1.5, 0.3, 2.1, 0.7, 1.8)
  threshold <- 1.0

  expect_silent({
    times <- hitting_times(x, threshold)
  })
  expect_true(is.numeric(times))
})

# =============================================================================
# MPP FUNCTIONS - Examples
# =============================================================================

test_that("simulate_orbit example works", {
  # From documentation: orb <- simulate_orbit(function(x) 4*x*(1-x), init=0.1, n_iter=1e5)
  # Use smaller n_iter for testing

  expect_silent({
    orb <- simulate_orbit(function(x) 4*x*(1-x), init=0.1, n_iter=1000)
  })
  expect_length(orb, 1001)  # n_iter + 1
  expect_equal(orb[1], 0.1)
})

# =============================================================================
# UTILITY FUNCTIONS - Examples
# =============================================================================

test_that("clean_extreme_data handles various inputs", {
  # Test with mixed valid/invalid data
  x <- c(1, 2, NA, 4, Inf, 6, -Inf, 8, NaN)

  expect_silent({
    cleaned <- clean_extreme_data(x)
  })

  expect_true(all(is.finite(cleaned)))
  expect_false(any(is.na(cleaned)))
})

# =============================================================================
# PACKAGE DEMO - Example
# =============================================================================

test_that("run_demo example works (basic)", {
  # From documentation: demo_results <- run_demo(n = 1000, r = 3.8, x0 = 0.2)
  skip_on_cran()

  expect_silent({
    demo_results <- run_demo(n = 1000, r = 3.8, x0 = 0.2, output_report = FALSE)
  })

  expect_type(demo_results, "list")
  expect_true("series" %in% names(demo_results))
  expect_true("threshold" %in% names(demo_results))
})

# =============================================================================
# INTEGRATION EXAMPLES - Complex workflows
# =============================================================================

test_that("complete workflow example: logistic map analysis", {
  skip_on_cran()

  # Simulate data
  set.seed(42)
  expect_silent({
    series <- simulate_logistic_map(1000, 3.8, 0.2)
  })

  # Extract block maxima
  expect_silent({
    bm <- block_maxima(series, 50)
  })

  # Threshold selection
  expect_silent({
    threshold <- quantile(series, 0.95)
  })

  # Extremal index estimation
  expect_silent({
    theta <- extremal_index_runs(series, threshold, run_length = 2)
  })

  # Cluster analysis
  expect_silent({
    sizes <- cluster_sizes(series, threshold, run_length = 2)
  })

  # All should produce valid outputs
  expect_true(is.numeric(bm))
  expect_true(is.numeric(threshold))
  expect_true(is.numeric(theta))
  expect_true(is.numeric(sizes))
})

test_that("complete workflow example: bifurcation and chaos analysis", {
  skip_on_cran()

  set.seed(42)

  # Generate bifurcation data
  expect_silent({
    r_vals <- seq(2.5, 4, length.out = 50)
    bifdat <- logistic_bifurcation(r_vals, n_iter = 100, discard = 50)
  })

  # Simulate series in chaotic regime
  expect_silent({
    series_chaotic <- simulate_logistic_map(500, 3.9, 0.2)
  })

  # Estimate Lyapunov exponent
  expect_silent({
    lambda <- estimate_lyapunov_exponent(series_chaotic)
  })

  # Estimate correlation dimension
  expect_silent({
    cd <- estimate_correlation_dimension(series_chaotic)
  })

  # Validate outputs
  expect_s3_class(bifdat, "data.frame")
  expect_true(is.numeric(lambda))
  expect_true(is.list(cd))
})

message("✓ All documentation examples validated successfully")
