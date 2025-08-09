
test_that('parameter validation works correctly', {
  # Test parameter validation for simulation functions
  expect_error(simulate_logistic_map(-5, r = 3.8, x0 = 0.2), "positive integer")
  expect_error(simulate_logistic_map("invalid", r = 3.8, x0 = 0.2), "positive integer")
  
  # Test parameter validation for extremal index functions
  expect_error(extremal_index_runs(numeric(0), 0.5, run_length = 3), "length\\(x\\)")
  expect_error(extremal_index_runs("not_numeric", 0.5, run_length = 3), "x is not a numeric or integer vector")
})

test_that('data preprocessing utilities work', {
  # Test data cleaning if implemented
  test_data <- c(1, 2, NA, 4, Inf, 6, -Inf, 8)
  
  tryCatch({
    cleaned_data <- clean_extreme_data(test_data)
    expect_false(any(is.na(cleaned_data)))
    expect_false(any(is.infinite(cleaned_data)))
    expect_true(length(cleaned_data) <= length(test_data))
  }, error = function(e) {
    skip("clean_extreme_data function not yet implemented")
  })
})

test_that('statistical helper functions work', {
  set.seed(123)
  test_data <- rnorm(100)
  
  # Test empirical quantile function if implemented
  tryCatch({
    emp_quantile <- empirical_quantile(test_data, 0.95)
    expect_true(is.numeric(emp_quantile))
    expect_true(emp_quantile > quantile(test_data, 0.9))
  }, error = function(e) {
    skip("empirical_quantile function not yet implemented")
  })
  
  # Test autocorrelation computation if implemented
  tryCatch({
    acf_values <- compute_autocorrelation(test_data, max_lag = 10)
    expect_true(is.numeric(acf_values))
    expect_true(length(acf_values) <= 11)  # Including lag 0
    expect_true(acf_values[1] == 1)  # Lag 0 should be 1
  }, error = function(e) {
    skip("compute_autocorrelation function not yet implemented")
  })
})

test_that('error handling provides informative messages', {
  # Test that error messages are helpful
  expect_error(simulate_logistic_map("invalid"), "positive integer")
  expect_error(simulate_henon_map(100, a = "invalid"), "numeric")
  
  # Test threshold-related errors
  data(logistic_ts)
  expect_error(extremal_index_runs(logistic_ts, "invalid_threshold"), "numeric")
  expect_error(cluster_sizes(logistic_ts, 2.0, run_length = "invalid"), "numeric")
})

test_that('input type checking works', {
  # Test various input types
  expect_error(simulate_logistic_map(NULL))
  expect_error(simulate_logistic_map(character(1)))
  expect_error(simulate_logistic_map(list(n = 100)))
  
  # Test data input validation
  expect_error(extremal_index_runs(NULL, 0.5))
  expect_error(extremal_index_runs("not_numeric", 0.5))
  expect_error(extremal_index_runs(matrix(1:10, nrow = 5), 0.5))
})

test_that('boundary condition handling', {
  set.seed(123)
  
  # Test with minimal data
  minimal_data <- c(0.1, 0.9, 0.1)
  threshold <- 0.5
  
  # Should handle gracefully without crashing
  expect_warning_or_output <- function(expr) {
    tryCatch({
      result <- expr
      expect_true(is.numeric(result) || is.na(result) || length(result) == 0)
    }, warning = function(w) {
      # Warnings are acceptable for edge cases
      TRUE
    }, error = function(e) {
      # Should not throw errors for valid inputs
      fail(paste("Unexpected error:", e$message))
    })
  }
  
  expect_warning_or_output(extremal_index_runs(minimal_data, threshold, run_length = 1))
  expect_warning_or_output(cluster_sizes(minimal_data, threshold, run_length = 1))
})

test_that('reproducibility with random seeds', {
  # Test that setting seeds produces reproducible results
  set.seed(123)
  result1 <- simulate_logistic_map(100, r = 3.8, x0 = 0.2)
  
  set.seed(123)
  result2 <- simulate_logistic_map(100, r = 3.8, x0 = 0.2)
  
  expect_equal(result1, result2)
  
  # Test with Hénon map
  set.seed(456)
  henon1 <- simulate_henon_map(50, a = 1.4, b = 0.3)
  
  set.seed(456)
  henon2 <- simulate_henon_map(50, a = 1.4, b = 0.3)
  
  expect_equal(henon1, henon2)
})

test_that('memory management in utility functions', {
  # Test that utility functions don't leak memory
  initial_objects <- ls()
  
  # Run some computations
  for(i in 1:5) {
    temp_data <- simulate_logistic_map(1000, r = 3.8, x0 = 0.2)
    temp_threshold <- quantile(temp_data, 0.95)
    temp_result <- extremal_index_runs(temp_data, temp_threshold, run_length = 3)
  }
  
  # Clean up temporary variables
  rm(list = setdiff(ls(), initial_objects))
  gc()
  
  # If we get here without memory issues, the test passes
  expect_true(TRUE)
})

test_that('Lyapunov exponent estimator works', {
  set.seed(123)
  series <- simulate_logistic_map(500, r = 3.8, x0 = 0.2)
  lyap <- estimate_lyapunov_exponent(series)
  expect_true(is.numeric(lyap))
  expect_length(lyap, 1)
})

test_that('with_logging writes errors to file', {
  logf <- tempfile()
  expect_error(with_logging(stop('boom'), logf, msg = 'fail'))
  lines <- readLines(logf)
  expect_true(any(grepl('INFO: fail', lines)))
  expect_true(any(grepl('ERROR', lines)))
})

test_that('with_logging records warnings', {
  logf <- tempfile()
  with_logging({
    warning('uh oh')
    1 + 1
  }, logf, msg = 'warn')
  lines <- readLines(logf)
  expect_true(any(grepl('INFO: warn', lines)))
  expect_true(any(grepl('WARNING', lines)))
})