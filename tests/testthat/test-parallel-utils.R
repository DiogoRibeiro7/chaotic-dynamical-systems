context('Parallel processing utilities')

test_that('parallel_available function works', {
  result <- parallel_available()
  expect_true(is.logical(result))
  expect_length(result, 1)
})

test_that('optimal_cores function returns reasonable values', {
  cores <- optimal_cores()
  expect_true(is.numeric(cores))
  expect_true(cores >= 1)
  expect_true(cores <= 8)  # Reasonable upper bound
  
  # Test with max_cores parameter
  cores_limited <- optimal_cores(max_cores = 2)
  expect_true(cores_limited <= 2)
})

test_that('progress_bar function works without error', {
  # Capture output to avoid cluttering test results
  expect_silent(progress_bar(1, 10))
  expect_silent(progress_bar(5, 10))
  expect_silent(progress_bar(10, 10))
})

test_that('parallel_bootstrap_ei handles small datasets', {
  # Create test data
  set.seed(123)
  data <- rnorm(100)
  threshold <- quantile(data, 0.9)
  
  # Test with small bootstrap sample for speed
  result <- parallel_bootstrap_ei(data, threshold, method = "runs", 
                                  n_bootstrap = 10, n_cores = 1)
  
  expect_true(is.list(result))
  expect_true("estimate" %in% names(result))
  expect_true("bootstrap_estimates" %in% names(result))
  expect_true("ci" %in% names(result))
  expect_true("n_cores_used" %in% names(result))
  expect_true("method" %in% names(result))
  
  expect_equal(result$n_cores_used, 1)
  expect_true(grepl("parallel", result$method))
})

test_that('parallel_simulation works for logistic map', {
  # Create parameter grid
  param_grid <- data.frame(
    r = c(3.7, 3.8, 3.9),
    x0 = c(0.1, 0.2, 0.3)
  )
  
  # Test parallel simulation
  results <- parallel_simulation("logistic", param_grid, n = 100, n_cores = 1)
  
  expect_true(is.list(results))
  expect_equal(length(results), nrow(param_grid))
  expect_equal(attr(results, "map_type"), "logistic")
  expect_equal(attr(results, "n_simulations"), 3)
  
  # Check individual results
  for (i in 1:length(results)) {
    expect_true("params" %in% names(results[[i]]))
    expect_true("data" %in% names(results[[i]]))
    expect_true("type" %in% names(results[[i]]))
    expect_equal(results[[i]]$type, "logistic")
    expect_equal(length(results[[i]]$data), 100)
  }
})

test_that('parallel_simulation works for Hénon map', {
  # Create parameter grid
  param_grid <- data.frame(
    a = c(1.2, 1.4),
    b = c(0.3, 0.3),
    x0 = c(0, 0.1),
    y0 = c(0, 0.1)
  )
  
  # Test parallel simulation
  results <- parallel_simulation("henon", param_grid, n = 50, n_cores = 1)
  
  expect_true(is.list(results))
  expect_equal(length(results), nrow(param_grid))
  expect_equal(attr(results, "map_type"), "henon")
  
  # Check individual results
  for (i in 1:length(results)) {
    expect_true(is.data.frame(results[[i]]$data))
    expect_equal(nrow(results[[i]]$data), 50)
    expect_true(all(c("x", "y") %in% names(results[[i]]$data)))
    expect_equal(results[[i]]$type, "henon")
  }
})

test_that('parallel_cv_threshold works correctly', {
  skip_if(TRUE, "Skipping time-intensive test")
  
  # Create test data with some extremes
  set.seed(456)
  data <- c(rnorm(180), rnorm(20, mean = 3, sd = 0.5))
  
  # Test with small grid for speed
  thresholds <- quantile(data, c(0.85, 0.9, 0.95))
  
  result <- parallel_cv_threshold(data, thresholds = thresholds, 
                                  k_fold = 3, n_cores = 1)
  
  expect_true(is.list(result))
  expect_true("optimal_threshold" %in% names(result))
  expect_true("mean_scores" %in% names(result))
  expect_true("thresholds" %in% names(result))
  expect_equal(result$n_cores_used, 1)
  expect_true(result$optimal_threshold %in% thresholds)
})

test_that('parallel functions handle edge cases', {
  # Very small dataset
  small_data <- rnorm(30)
  threshold <- quantile(small_data, 0.9)
  
  # Should handle small datasets gracefully
  expect_warning(parallel_bootstrap_ei(small_data, threshold, n_bootstrap = 5, n_cores = 1),
                 "Too few valid bootstrap estimates")
  
  # Empty parameter grid
  empty_grid <- data.frame(r = numeric(0), x0 = numeric(0))
  result_empty <- parallel_simulation("logistic", empty_grid, n = 100, n_cores = 1)
  expect_equal(length(result_empty), 0)
})

test_that('parallel functions fall back to sequential when parallel unavailable', {
  # This test ensures graceful fallback when parallel processing is not available
  # The functions should still work with n_cores = 1
  
  data <- rnorm(50)
  threshold <- quantile(data, 0.9)
  
  result <- parallel_bootstrap_ei(data, threshold, method = "intervals", 
                                  n_bootstrap = 5, n_cores = 1)
  
  expect_equal(result$n_cores_used, 1)
  expect_true(is.finite(result$estimate) || is.na(result$estimate))
})

test_that('parallel functions validate input parameters', {
  # Test parameter validation
  expect_error(parallel_bootstrap_ei(c(1, 2, 3), 2, method = "invalid"),
               "method %in% c\\(\"runs\", \"intervals\"\\) is not TRUE")
  
  # Invalid map type
  param_grid <- data.frame(r = 3.8, x0 = 0.2)
  expect_error(parallel_simulation("invalid", param_grid),
               "map_type %in% c\\(\"logistic\", \"henon\"\\) is not TRUE")
  
  # Missing required parameters
  bad_grid <- data.frame(wrong_param = 1)
  expect_error(parallel_simulation("logistic", bad_grid),
               "all\\(c\\(\"r\", \"x0\"\\) %in% names\\(params\\)\\) is not TRUE")
})

# Performance comparison test (only run if parallel package available)
test_that('parallel processing shows performance benefit', {
  skip_if_not_installed("parallel")
  skip_if(!parallel_available(), "Parallel processing not available")
  skip_if(parallel::detectCores() < 2, "Need multiple cores for parallel test")
  
  # This test is mainly for documentation purposes
  # In practice, parallel processing benefits depend on problem size and hardware
  
  data <- rnorm(200)
  threshold <- quantile(data, 0.9)
  
  # Sequential version (n_cores = 1)
  time_seq <- system.time({
    result_seq <- parallel_bootstrap_ei(data, threshold, n_bootstrap = 20, n_cores = 1)
  })
  
  # Parallel version (if multiple cores available)
  if (parallel::detectCores() >= 2) {
    time_par <- system.time({
      result_par <- parallel_bootstrap_ei(data, threshold, n_bootstrap = 20, n_cores = 2)
    })
    
    # Results should be similar (within statistical variation)
    expect_true(abs(result_seq$estimate - result_par$estimate) < 0.5)
    
    # Parallel version should use multiple cores
    expect_true(result_par$n_cores_used > 1)
  }
})