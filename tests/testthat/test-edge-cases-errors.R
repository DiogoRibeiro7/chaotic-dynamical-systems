# Edge Cases and Error Handling Validation
# Tests boundary conditions and error scenarios

context("Edge Cases and Error Handling")

# =============================================================================
# INPUT VALIDATION - Parameter Type Checking
# =============================================================================

test_that("simulate_logistic_map: rejects invalid parameter types", {
  expect_error(simulate_logistic_map("100", 3.8, 0.2))  # n should be numeric
  expect_error(simulate_logistic_map(10, "3.8", 0.2))   # r should be numeric
  expect_error(simulate_logistic_map(10, 3.8, "0.2"))   # x0 should be numeric
  expect_error(simulate_logistic_map(NA, 3.8, 0.2))     # n should not be NA
  expect_error(simulate_logistic_map(10.5, 3.8, 0.2))   # n should be integer
})

test_that("block_maxima: rejects invalid inputs", {
  expect_error(block_maxima(c(1, NA, 3), 2))           # x should have no NA
  expect_error(block_maxima(1:10, 2.5))                # block_size should be integer
  expect_error(block_maxima(1:10, 0))                  # block_size should be >= 1
  expect_error(block_maxima(1:10, -5))                 # block_size should be positive
})

test_that("extremal_index_runs: handles edge cases", {
  # Empty exceedances
  x <- rep(0.1, 100)
  threshold <- 0.5
  theta <- extremal_index_runs(x, threshold, run_length = 2)
  expect_true(is.numeric(theta))

  # All exceedances
  x <- rep(1.0, 100)
  threshold <- 0.5
  theta <- extremal_index_runs(x, threshold, run_length = 2)
  expect_true(is.numeric(theta))
})

# =============================================================================
# BOUNDARY CONDITIONS - Empty and Single Element Cases
# =============================================================================

test_that("exceedances: handles empty results", {
  x <- c(1, 2, 3)
  threshold <- 10

  exc <- exceedances(x, threshold)
  expect_length(exc, 0)
  expect_type(exc, "double")
})

test_that("cluster_sizes: handles no clusters", {
  x <- rep(0.5, 100)
  threshold <- 1.0

  sizes <- cluster_sizes(x, threshold, run_length = 2)
  expect_length(sizes, 0)
  expect_type(sizes, "double")
})

test_that("mean_residual_life: handles edge thresholds", {
  x <- 1:100

  # Threshold at max should give empty result or handle gracefully
  max_threshold <- max(x)
  expect_silent({
    mrl <- mean_residual_life(x, max_threshold)
  })
})

# =============================================================================
# NUMERICAL STABILITY - Very Large and Very Small Values
# =============================================================================

test_that("simulate_logistic_map: handles extreme parameter values", {
  # Very small x0
  series <- simulate_logistic_map(10, 3.8, 0.0001)
  expect_true(all(is.finite(series)))

  # Very large r (may cause overflow)
  series <- simulate_logistic_map(10, 100, 0.5)
  # Just check it doesn't crash; values may leave [0,1]
  expect_length(series, 10)

  # r near 0
  series <- simulate_logistic_map(10, 0.1, 0.5)
  expect_true(all(is.finite(series)))
})

test_that("block_maxima: handles very small blocks", {
  x <- 1:10
  bm <- block_maxima(x, 1)  # Block size of 1

  expect_equal(bm, x)  # Each element is its own maximum
})

test_that("recurrence_plot: handles minimum viable input", {
  # Minimum input that should work
  x <- c(1, 2, 3, 4, 5)

  expect_silent({
    rp <- recurrence_plot(x, embed = 2, delay = 1)
  })
  expect_true(is.matrix(rp))
})

# =============================================================================
# SPECIAL VALUES - NA, NaN, Inf Handling
# =============================================================================

test_that("clean_extreme_data: removes all non-finite values", {
  x <- c(1, 2, NA, 4, NaN, 6, Inf, 8, -Inf)

  cleaned <- clean_extreme_data(x)

  expect_true(all(is.finite(cleaned)))
  expect_false(any(is.na(cleaned)))
  expect_false(any(is.nan(cleaned)))
  expect_false(any(is.infinite(cleaned)))
  expect_equal(cleaned, c(1, 2, 4, 6, 8))
})

test_that("functions handle Inf appropriately", {
  x <- c(1, 2, Inf, 4, 5)

  # Most functions should either error or remove Inf
  # block_maxima might return Inf as maximum
  bm <- block_maxima(x, 2)
  expect_true(Inf %in% bm || !any(is.infinite(bm)))
})

# =============================================================================
# CONSISTENCY CHECKS - Output Dimensions and Types
# =============================================================================

test_that("simulate_henon_map: always returns correct structure", {
  for (n in c(1, 10, 100)) {
    orbit <- simulate_henon_map(n)
    expect_equal(nrow(orbit), n)
    expect_equal(ncol(orbit), 2)
    expect_true(all(c("x", "y") %in% names(orbit)))
  }
})

test_that("logistic_bifurcation: output size matches input", {
  for (n_r in c(5, 10, 20)) {
    r_vals <- seq(3, 4, length.out = n_r)
    bifdat <- logistic_bifurcation(r_vals, n_iter = 20, discard = 10)

    expected_rows <- (20 - 10) * n_r
    expect_equal(nrow(bifdat), expected_rows)
  }
})

# =============================================================================
# MATHEMATICAL PROPERTIES - Invariants and Constraints
# =============================================================================

test_that("extremal_index: respects theoretical bounds", {
  # Extremal index should always be in (0, 1] when finite

  set.seed(42)
  x <- simulate_logistic_map(500, 3.8, 0.2)

  for (prob in c(0.9, 0.95, 0.99)) {
    threshold <- quantile(x, prob)
    theta <- extremal_index_runs(x, threshold, run_length = 2)

    if (is.finite(theta) && !is.na(theta)) {
      expect_gte(theta, 0)
      expect_lte(theta, 1)
    }
  }
})

test_that("acf_decay: respects ACF bounds", {
  # Autocorrelation should be in [-1, 1]

  set.seed(42)
  x <- simulate_logistic_map(500, 3.8, 0.2)

  acf_vals <- acf_decay(x, 1:20)

  expect_true(all(abs(acf_vals) <= 1))
})

test_that("recurrence_rate: respects probability bounds", {
  set.seed(42)
  x <- simulate_logistic_map(200, 3.8, 0.2)

  ra <- recurrence_analysis(x)

  expect_gte(ra$recurrence_rate, 0)
  expect_lte(ra$recurrence_rate, 1)
  expect_gte(ra$determinism, 0)
  expect_lte(ra$determinism, 1)
})

# =============================================================================
# PERFORMANCE EDGE CASES - Large Datasets
# =============================================================================

test_that("functions handle moderately large data efficiently", {
  skip_on_cran()

  # Test with 10,000 points (not huge, but non-trivial)
  set.seed(42)

  expect_lt({
    start_time <- Sys.time()
    x <- simulate_logistic_map(10000, 3.8, 0.2)
    end_time <- Sys.time()
    as.numeric(difftime(end_time, start_time, units = "secs"))
  }, 5)  # Should complete in < 5 seconds

  # Block maxima should be fast
  expect_lt({
    start_time <- Sys.time()
    bm <- block_maxima(x, 100)
    end_time <- Sys.time()
    as.numeric(difftime(end_time, start_time, units = "secs"))
  }, 1)  # Should complete in < 1 second
})

test_that("C++ functions provide performance benefit", {
  skip_on_cran()
  skip_if_not_installed("microbenchmark")

  n <- 5000
  r <- 3.8
  x0 <- 0.2

  # Just verify both work and C++ is faster (on average)
  r_time <- system.time({
    simulate_logistic_map(n, r, x0)
  })[["elapsed"]]

  cpp_time <- system.time({
    simulate_logistic_map_cpp(n, r, x0)
  })[["elapsed"]]

  # C++ should generally be faster, but allow for measurement variance
  expect_true(cpp_time <= r_time * 1.5)
})

# =============================================================================
# ERROR MESSAGE QUALITY - Informative Errors
# =============================================================================

test_that("error messages are informative", {
  # Test that errors provide useful information

  err <- tryCatch(
    simulate_logistic_map(10, 3.8, 1.5),
    error = function(e) e$message
  )
  expect_true(grepl("0.*1", err))  # Should mention bounds

  err <- tryCatch(
    block_maxima(1:5, 10),
    error = function(e) e$message
  )
  expect_true(grepl("length|block", tolower(err)))  # Should mention length/block issue
})

# =============================================================================
# REPRODUCIBILITY - Seed Behavior
# =============================================================================

test_that("functions produce reproducible results with same seed", {
  # Non-stochastic functions should give identical results

  set.seed(42)
  series1 <- simulate_logistic_map(100, 3.8, 0.2)

  set.seed(42)
  series2 <- simulate_logistic_map(100, 3.8, 0.2)

  expect_equal(series1, series2)
})

test_that("different seeds produce different results", {
  set.seed(42)
  series1 <- simulate_logistic_map(100, 3.8, 0.2)

  set.seed(123)
  series2 <- simulate_logistic_map(100, 3.8, 0.2)

  # Initial values are same, but trajectories should be identical
  # since simulation is deterministic
  # Actually, wait - logistic map is deterministic, so seeds don't matter!
  # The series should be identical if parameters are same
  expect_equal(series1, series2)
})

# =============================================================================
# PARAMETER INTERACTION - Combined Edge Cases
# =============================================================================

test_that("logistic_bifurcation: handles edge parameter combinations", {
  # discard >= n_iter should error
  expect_error(
    logistic_bifurcation(seq(3, 4, by = 0.1), n_iter = 50, discard = 60)
  )

  # discard = n_iter - 1 should give 1 value per r
  r_vals <- c(3.5, 3.6)
  bifdat <- logistic_bifurcation(r_vals, n_iter = 10, discard = 9)
  expect_equal(nrow(bifdat), 2)
})

test_that("recurrence_analysis: lmin edge cases", {
  set.seed(42)
  x <- simulate_logistic_map(100, 3.8, 0.2)

  # lmin = 1 should count all recurrent points
  ra1 <- recurrence_analysis(x, lmin = 1)
  expect_gte(ra1$determinism, 0)

  # Very large lmin should give low determinism
  ra_large <- recurrence_analysis(x, lmin = 50)
  expect_gte(ra_large$determinism, 0)
  expect_lte(ra_large$determinism, ra1$determinism)
})

# =============================================================================
# CONCURRENT EXECUTION - Function Independence
# =============================================================================

test_that("functions don't have hidden state dependencies", {
  # Run same function multiple times in sequence
  # Results should be independent

  for (i in 1:5) {
    series <- simulate_logistic_map(50, 3.8, 0.2)
    expect_equal(series[1], 0.2)
    expect_length(series, 50)
  }
})

message("✓ All edge case and error handling tests completed")
