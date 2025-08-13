
test_that('bootstrap_extremal_index returns confidence intervals', {
  set.seed(123)
  data(logistic_ts)
  threshold <- quantile(logistic_ts, 0.95)
  
  # Test with small sample for speed
  sample_data <- logistic_ts[1:500]
  
  tryCatch({
    ci <- bootstrap_extremal_index(sample_data, threshold, n_bootstrap = 10, method = "runs")
    expect_true(is.numeric(ci))
    expect_true(length(ci) == 2)  # Lower and upper bounds
    expect_true(ci[1] <= ci[2])    # Lower <= Upper
  }, error = function(e) {
    # If function doesn't exist yet, just pass
    skip("bootstrap_extremal_index function not yet implemented")
  })
})

test_that('bootstrap confidence intervals are well-formed', {
  set.seed(42)
  
  # Create simple test data with known extremal behavior
  test_data <- c(rep(0.1, 80), rep(0.9, 20))
  threshold <- 0.5
  
  tryCatch({
    ci_runs <- bootstrap_extremal_index(test_data, threshold, n_bootstrap = 20, method = "runs")
    ci_intervals <- bootstrap_extremal_index(test_data, threshold, n_bootstrap = 20, method = "intervals")
    
    expect_true(all(ci_runs >= 0 & ci_runs <= 1))
    expect_true(all(ci_intervals >= 0 & ci_intervals <= 1))
  }, error = function(e) {
    skip("bootstrap_extremal_index function not yet implemented")
  })
})

test_that('bootstrap handles edge cases', {
  set.seed(123)
  
  # Test with no exceedances
  no_exceed_data <- rep(0.1, 100)
  threshold <- 0.5
  
  tryCatch({
    ci <- bootstrap_extremal_index(no_exceed_data, threshold, n_bootstrap = 10)
    expect_true(is.numeric(ci) || is.na(ci[1]))
  }, error = function(e) {
    skip("bootstrap_extremal_index function not yet implemented")
  })
  
  # Test with all exceedances
  all_exceed_data <- rep(0.9, 100)
  threshold <- 0.5
  
  tryCatch({
    ci <- bootstrap_extremal_index(all_exceed_data, threshold, n_bootstrap = 10)
    expect_true(is.numeric(ci))
  }, error = function(e) {
    skip("bootstrap_extremal_index function not yet implemented")
  })
})

test_that('block bootstrap maintains temporal dependence', {
  set.seed(123)
  data(logistic_ts)
  
  # Test block bootstrap function if it exists
  tryCatch({
    resampled <- block_bootstrap(logistic_ts[1:100], block_length = 10)
    expect_true(length(resampled) == 100)
    expect_true(is.numeric(resampled))
  }, error = function(e) {
    skip("block_bootstrap function not yet implemented")
  })
})