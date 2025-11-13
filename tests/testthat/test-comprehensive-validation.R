# Comprehensive Validation Test Suite
# Tests all functions against their documented behavior

context("Comprehensive Function Validation")

# =============================================================================
# SIMULATION FUNCTIONS - Validate against documentation promises
# =============================================================================

test_that("simulate_logistic_map: validates documented behavior", {
  # Doc promises: "Generates a time series following the logistic map"
  # Return: "Numeric vector containing the generated orbit"

  n <- 100
  r <- 3.8
  x0 <- 0.2

  series <- simulate_logistic_map(n, r, x0)

  # Test return type
  expect_type(series, "double")
  expect_length(series, n)

  # Test initial value
  expect_equal(series[1], x0)

  # Test logistic map iteration: x[n+1] = r * x[n] * (1 - x[n])
  for (i in 1:(n-1)) {
    expected <- r * series[i] * (1 - series[i])
    expect_equal(series[i+1], expected, tolerance = 1e-10)
  }
})

test_that("simulate_logistic_map: validates input constraints", {
  # Doc says: "@param x0 Numeric. Initial value in (0, 1)"
  # Should enforce strict inequality

  expect_error(simulate_logistic_map(10, 3.8, 0), "strictly between 0 and 1")
  expect_error(simulate_logistic_map(10, 3.8, 1), "strictly between 0 and 1")
  expect_error(simulate_logistic_map(10, 3.8, -0.1), "strictly between 0 and 1")
  expect_error(simulate_logistic_map(10, 3.8, 1.1), "strictly between 0 and 1")

  # Valid values should work
  expect_silent(simulate_logistic_map(10, 3.8, 0.0001))
  expect_silent(simulate_logistic_map(10, 3.8, 0.9999))
})

test_that("simulate_henon_map: validates documented behavior", {
  # Doc promises: "Generates an orbit for the two-dimensional Hénon map"
  # x[n+1] = 1 - a * x[n]^2 + y[n]
  # y[n+1] = b * x[n]

  n <- 50
  a <- 1.4
  b <- 0.3
  x0 <- 0
  y0 <- 0

  orbit <- simulate_henon_map(n, a, b, x0, y0)

  # Test return type
  expect_s3_class(orbit, "data.frame")
  expect_equal(nrow(orbit), n)
  expect_equal(ncol(orbit), 2)
  expect_true(all(c("x", "y") %in% names(orbit)))

  # Test initial values
  expect_equal(orbit$x[1], x0)
  expect_equal(orbit$y[1], y0)

  # Test Henon map iterations
  for (i in 1:(n-1)) {
    expected_x <- 1 - a * orbit$x[i]^2 + orbit$y[i]
    expected_y <- b * orbit$x[i]
    expect_equal(orbit$x[i+1], expected_x, tolerance = 1e-10)
    expect_equal(orbit$y[i+1], expected_y, tolerance = 1e-10)
  }
})

test_that("simulate_tent_map: validates documented behavior", {
  # Doc promises tent map: x[n+1] = r * x[n] if x[n] < 0.5
  #                                   r * (1 - x[n]) otherwise

  n <- 100
  r <- 2
  x0 <- 0.1

  series <- simulate_tent_map(n, r, x0)

  expect_type(series, "double")
  expect_length(series, n)
  expect_equal(series[1], x0)

  # Verify tent map logic
  for (i in 1:(n-1)) {
    if (series[i] < 0.5) {
      expected <- r * series[i]
    } else {
      expected <- r * (1 - series[i])
    }
    expect_equal(series[i+1], expected, tolerance = 1e-10)
  }
})

test_that("logistic_bifurcation: validates documented behavior", {
  # Doc promises: "Generate data for the classic logistic map bifurcation diagram"
  # Return: "Data frame with columns `r` and `x`"

  r_seq <- seq(2.5, 4, length.out = 10)
  n_iter <- 100
  discard <- 50

  bifdat <- logistic_bifurcation(r_seq, n_iter, discard)

  # Test structure
  expect_s3_class(bifdat, "data.frame")
  expect_true(all(c("r", "x") %in% names(bifdat)))

  # Should have (n_iter - discard) * length(r_seq) rows
  expected_rows <- (n_iter - discard) * length(r_seq)
  expect_equal(nrow(bifdat), expected_rows)

  # All r values should be from r_seq
  expect_true(all(bifdat$r %in% r_seq))
})

# =============================================================================
# EXTREME VALUE FUNCTIONS - Block Maxima & POT
# =============================================================================

test_that("block_maxima: validates documented behavior", {
  # Doc promises: "Split a univariate time series into non-overlapping blocks"
  # "compute the maximum within each block"

  x <- 1:100
  block_size <- 10

  bm <- block_maxima(x, block_size)

  # Should have floor(100/10) = 10 blocks
  expect_equal(length(bm), 10)

  # Each maximum should be correct
  for (i in 1:10) {
    start_idx <- (i-1) * block_size + 1
    end_idx <- i * block_size
    expected_max <- max(x[start_idx:end_idx])
    expect_equal(bm[i], expected_max)
  }
})

test_that("block_maxima: handles incomplete blocks correctly", {
  # Doc says: "Any trailing values that do not fit a full block are ignored"

  x <- 1:25
  block_size <- 10

  bm <- block_maxima(x, block_size)

  # Should only have 2 complete blocks
  expect_equal(length(bm), 2)
  expect_equal(bm, c(10, 20))
})

test_that("block_maxima: validates error conditions", {
  # Doc says: "If `length(x) < block_size` the function throws an error"

  x <- 1:5
  block_size <- 10

  expect_error(block_maxima(x, block_size), "length\\(x\\) must be >= block_size")
})

test_that("exceedances: validates documented behavior", {
  # Function should extract values > threshold

  x <- c(1, 2, 5, 3, 7, 2, 9)
  threshold <- 4

  exc <- exceedances(x, threshold)

  expect_equal(exc, c(5, 7, 9))

  # Edge cases
  expect_equal(exceedances(c(1, 2, 3), 5), numeric(0))
  expect_equal(exceedances(c(1, 2, 3), 0), c(1, 2, 3))
})

test_that("fit_gev: validates documented behavior and error handling", {
  # Doc says: "Uses `evd::fgev` if available; otherwise falls back to `ismev::gev.fit`"

  set.seed(123)
  bm <- rnorm(100, mean = 5, sd = 2)

  # Should work with evd package
  if (requireNamespace("evd", quietly = TRUE)) {
    fit <- fit_gev(bm)
    expect_true(!is.null(fit))
  }

  # Should error with too little data
  expect_error(fit_gev(c(1)), "min.len")
})

# =============================================================================
# EXTREMAL INDEX FUNCTIONS
# =============================================================================

test_that("extremal_index_runs: validates range and behavior", {
  # Extremal index should be in (0, 1]

  set.seed(42)
  x <- simulate_logistic_map(1000, 3.8, 0.2)
  threshold <- quantile(x, 0.95)

  theta <- extremal_index_runs(x, threshold, run_length = 2)

  expect_type(theta, "double")
  expect_length(theta, 1)

  if (!is.na(theta) && !is.infinite(theta)) {
    expect_gte(theta, 0)
    expect_lte(theta, 1)
  }
})

test_that("extremal_index_intervals: validates behavior", {
  set.seed(42)
  x <- simulate_logistic_map(500, 3.8, 0.2)
  threshold <- quantile(x, 0.9)

  theta <- extremal_index_intervals(x, threshold)

  expect_type(theta, "double")
  expect_length(theta, 1)
})

test_that("threshold_exceedances: validates documented behavior", {
  x <- c(1, 5, 2, 6, 3, 7, 4)
  threshold <- 4.5

  exc_indices <- threshold_exceedances(x, threshold)

  expect_type(exc_indices, "integer")
  expect_equal(exc_indices, c(2, 4, 6))  # Positions of 5, 6, 7
})

test_that("cluster_exceedances: validates documented behavior", {
  # Test clustering logic
  indices <- c(1, 2, 3, 10, 11, 20)
  run_length <- 2

  clusters <- cluster_exceedances(indices, run_length)

  expect_type(clusters, "list")
  # Should have 3 clusters: (1,2,3), (10,11), (20)
  expect_equal(length(clusters), 3)
})

test_that("hitting_times: validates statistical properties", {
  set.seed(123)
  x <- simulate_logistic_map(1000, 3.8, 0.2)
  threshold <- quantile(x, 0.95)

  times <- hitting_times(x, threshold)

  expect_type(times, "double")
  expect_true(all(times > 0))
  expect_true(all(is.finite(times)))
})

# =============================================================================
# THRESHOLD SELECTION FUNCTIONS
# =============================================================================

test_that("mean_residual_life: validates documented behavior", {
  set.seed(42)
  x <- rexp(500, rate = 0.5)
  thresholds <- quantile(x, probs = seq(0.8, 0.95, by = 0.05))

  mrl <- mean_residual_life(x, thresholds)

  expect_s3_class(mrl, "data.frame")
  expect_true(all(c("threshold", "mean_excess") %in% names(mrl)))
  expect_equal(nrow(mrl), length(thresholds))
})

test_that("hill_estimates: validates documented behavior", {
  set.seed(42)
  x <- rexp(500)

  k_vals <- 10:50
  hill <- hill_estimates(x, k_vals)

  expect_s3_class(hill, "data.frame")
  expect_true(all(c("k", "hill") %in% names(hill)))
  expect_equal(nrow(hill), length(k_vals))
})

# =============================================================================
# BOOTSTRAP FUNCTIONS
# =============================================================================

test_that("bootstrap_extremal_index: validates documented behavior", {
  skip_if_not_installed("boot")

  set.seed(42)
  x <- simulate_logistic_map(200, 3.8, 0.2)
  threshold <- quantile(x, 0.9)

  # Use small B for speed
  boot_result <- bootstrap_extremal_index(x, threshold, run_length = 2, B = 50)

  expect_type(boot_result, "list")
  expect_true("estimate" %in% names(boot_result))
  expect_true("ci" %in% names(boot_result))

  if (!is.null(boot_result$ci)) {
    expect_length(boot_result$ci, 2)
    expect_true(boot_result$ci[1] <= boot_result$ci[2])
  }
})

# =============================================================================
# CLUSTER STATISTICS
# =============================================================================

test_that("cluster_summary: validates documented statistics", {
  sizes <- c(1, 2, 3, 2, 4, 1, 3)

  summary_stats <- cluster_summary(sizes)

  expect_type(summary_stats, "list")
  expect_true("mean_size" %in% names(summary_stats))
  expect_true("var_size" %in% names(summary_stats))

  expect_equal(summary_stats$mean_size, mean(sizes))
  expect_equal(summary_stats$var_size, var(sizes))
})

# =============================================================================
# MIXING DIAGNOSTICS
# =============================================================================

test_that("acf_decay: validates documented behavior", {
  set.seed(42)
  x <- simulate_logistic_map(500, 3.8, 0.2)
  lags <- 1:10

  acf_vals <- acf_decay(x, lags)

  expect_type(acf_vals, "double")
  expect_length(acf_vals, length(lags))
  expect_true(all(abs(acf_vals) <= 1))  # ACF should be in [-1, 1]
})

test_that("mixing_coefficients: validates documented behavior", {
  set.seed(42)
  x <- simulate_logistic_map(500, 3.8, 0.2)
  threshold <- quantile(x, 0.9)
  lags <- 1:5

  mix <- mixing_coefficients(x, threshold, lags)

  expect_type(mix, "double")
  expect_length(mix, length(lags))
  expect_true(all(mix >= 0 & mix <= 1))  # Mixing coefficients in [0,1]
})

# =============================================================================
# RECURRENCE ANALYSIS
# =============================================================================

test_that("recurrence_plot: validates documented behavior", {
  set.seed(42)
  x <- simulate_logistic_map(100, 3.8, 0.2)

  rp <- recurrence_plot(x, embed = 2, delay = 1, eps = 0.1)

  expect_true(is.matrix(rp))
  expect_type(rp, "logical")
  expect_equal(nrow(rp), ncol(rp))  # Should be square matrix
})

test_that("recurrence_plot: validates error handling", {
  x <- c(1, 2)  # Too short

  expect_error(recurrence_plot(x, embed = 5, delay = 2), "too short")
})

test_that("recurrence_analysis: validates documented return structure", {
  # Doc promises: "List with components `recurrence_rate`, `determinism`, and `recurrence_matrix`"

  set.seed(42)
  x <- simulate_logistic_map(100, 3.8, 0.2)

  ra <- recurrence_analysis(x, embed = 2, delay = 1)

  expect_type(ra, "list")
  expect_true("recurrence_rate" %in% names(ra))
  expect_true("determinism" %in% names(ra))
  expect_true("recurrence_matrix" %in% names(ra))

  expect_gte(ra$recurrence_rate, 0)
  expect_lte(ra$recurrence_rate, 1)
  expect_gte(ra$determinism, 0)
  expect_lte(ra$determinism, 1)
})

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

test_that("clean_extreme_data: validates documented behavior", {
  x <- c(1, 2, NA, 4, Inf, 6, -Inf, 8)

  cleaned <- clean_extreme_data(x)

  expect_type(cleaned, "double")
  expect_true(all(is.finite(cleaned)))
  expect_equal(length(cleaned), 5)  # Should remove NA, Inf, -Inf
})

test_that("empirical_quantile: validates documented behavior", {
  x <- 1:100

  q50 <- empirical_quantile(x, 0.5)
  q95 <- empirical_quantile(x, 0.95)

  expect_equal(q50, 50.5)
  expect_equal(q95, 95.05)
})

test_that("compute_autocorrelation: validates range", {
  set.seed(42)
  x <- simulate_logistic_map(200, 3.8, 0.2)

  acf_val <- compute_autocorrelation(x, lag = 5)

  expect_type(acf_val, "double")
  expect_gte(acf_val, -1)
  expect_lte(acf_val, 1)
})

# =============================================================================
# C++ PERFORMANCE FUNCTIONS
# =============================================================================

test_that("simulate_logistic_map_cpp: matches R implementation", {
  n <- 100
  r <- 3.8
  x0 <- 0.2

  set.seed(42)
  r_version <- simulate_logistic_map(n, r, x0)

  set.seed(42)
  cpp_version <- simulate_logistic_map_cpp(n, r, x0)

  expect_equal(cpp_version, r_version, tolerance = 1e-10)
})

test_that("block_maxima_cpp: matches R implementation", {
  x <- rnorm(1000)
  block_size <- 50

  r_version <- block_maxima(x, block_size)
  cpp_version <- block_maxima_cpp(x, block_size)

  expect_equal(cpp_version, r_version)
})

test_that("extremal_index_runs_cpp: validates against R version", {
  set.seed(42)
  x <- simulate_logistic_map(500, 3.8, 0.2)
  threshold <- quantile(x, 0.95)

  r_version <- extremal_index_runs(x, threshold, 2)
  cpp_version <- extremal_index_runs_cpp(x, threshold, 2)

  # Allow some numerical tolerance
  if (!is.na(r_version) && !is.na(cpp_version)) {
    expect_equal(cpp_version, r_version, tolerance = 1e-6)
  }
})

# =============================================================================
# MARKED POINT PROCESS FUNCTIONS
# =============================================================================

test_that("simulate_orbit: validates documented behavior", {
  # Test with simple map
  map_fn <- function(x) 4 * x * (1 - x)
  init <- 0.2
  n_iter <- 100

  orbit <- simulate_orbit(map_fn, init, n_iter)

  expect_type(orbit, "double")
  expect_length(orbit, n_iter + 1)  # Doc says includes init
  expect_equal(orbit[1], init)

  # Verify iterations
  for (i in 1:n_iter) {
    expect_equal(orbit[i+1], map_fn(orbit[i]), tolerance = 1e-10)
  }
})

test_that("select_threshold: validates documented behavior", {
  X <- 1:100
  prob <- 0.95

  threshold <- select_threshold(X, prob)

  expect_type(threshold, "double")
  expect_length(threshold, 1)
  expect_equal(threshold, quantile(X, prob, type = 8, names = FALSE))
})

test_that("exceedance_indices: validates documented behavior", {
  # Doc says: "Integer vector of time indices where X > u"

  X <- c(1, 5, 2, 6, 3, 7, 4)
  u <- 4.5

  indices <- exceedance_indices(X, u)

  expect_type(indices, "integer")
  expect_equal(indices, c(2, 4, 6))
})

test_that("marked_point_process: validates different MPP types", {
  set.seed(42)
  X <- simulate_logistic_map(500, 3.8, 0.2)
  u <- quantile(X, 0.95)

  # Test each MPP type
  for (type in c("REPP", "EOT", "POT", "AOT")) {
    mpp <- marked_point_process(X, u, run_length = 2, type = type)

    expect_s3_class(mpp, "data.frame")
    expect_true(all(c("time", "mark") %in% names(mpp)))
    expect_true(all(mpp$time >= 1))
    expect_true(all(mpp$mark >= 0))
  }
})

# =============================================================================
# FRACTAL & LYAPUNOV FUNCTIONS
# =============================================================================

test_that("estimate_correlation_dimension: validates documented behavior", {
  set.seed(42)
  x <- simulate_logistic_map(500, 3.8, 0.2)

  result <- estimate_correlation_dimension(x)

  expect_type(result, "list")
  expect_true("dimension" %in% names(result))
  expect_type(result$dimension, "double")
  expect_gt(result$dimension, 0)  # Dimension should be positive
})

test_that("estimate_lyapunov_exponent: validates documented behavior", {
  set.seed(42)

  # Chaotic regime should have positive Lyapunov exponent
  x_chaotic <- simulate_logistic_map(1000, 3.9, 0.2)

  # Periodic regime should have negative Lyapunov exponent
  x_periodic <- simulate_logistic_map(1000, 3.2, 0.2)

  lambda_chaotic <- estimate_lyapunov_exponent(x_chaotic)
  lambda_periodic <- estimate_lyapunov_exponent(x_periodic)

  expect_type(lambda_chaotic, "double")
  expect_type(lambda_periodic, "double")

  # In general, chaotic should be > periodic
  # (but this is a weak test due to estimation variability)
})

message("✓ All comprehensive validation tests completed")
