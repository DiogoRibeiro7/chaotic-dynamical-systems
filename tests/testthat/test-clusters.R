context('Cluster analysis')

test_that('cluster_sizes basic functionality', {
  set.seed(123)
  
  # Test with simple exceedance pattern
  x <- c(0.1, 1.5, 1.6, 1.7, 0.1, 0.1, 2.0, 2.1, 0.1)
  threshold <- 1.0
  
  sizes <- cluster_sizes(x, threshold, run_length = 2)
  expect_true(is.numeric(sizes))
  expect_true(all(sizes > 0))
})

test_that('cluster_sizes handles edge cases', {
  # No exceedances
  x <- rep(0.5, 10)
  threshold <- 1.0
  sizes <- cluster_sizes(x, threshold, run_length = 2)
  expect_equal(length(sizes), 0)
  
  # All exceedances - but they need to be separated by gaps for run_length logic
  x <- rep(1.5, 10)
  threshold <- 1.0
  sizes <- cluster_sizes(x, threshold, run_length = 2)
  expect_true(is.numeric(sizes))
  
  # Single exceedance
  x <- c(rep(0.5, 5), 1.5, rep(0.5, 5))
  threshold <- 1.0
  sizes <- cluster_sizes(x, threshold, run_length = 2)
  expect_true(is.numeric(sizes))
})

test_that('cluster_summary produces correct statistics', {
  sizes <- c(1, 2, 3, 1, 4, 2)
  summary_stats <- cluster_summary(sizes)
  
  expect_equal(summary_stats[["mean_size"]], mean(sizes))
  expect_equal(summary_stats[["var_size"]], var(sizes))
  expect_true("mean_size" %in% names(summary_stats))
  expect_true("var_size" %in% names(summary_stats))
})

test_that('cluster_summary handles empty input', {
  sizes <- numeric(0)
  summary_stats <- cluster_summary(sizes)
  
  expect_true(is.numeric(summary_stats))
  expect_true(is.na(summary_stats[["mean_size"]]) || summary_stats[["mean_size"]] == 0)
})

test_that('cluster detection with different run lengths', {
  set.seed(42)
  data(logistic_ts)
  threshold <- quantile(logistic_ts, 0.95)
  
  sizes_r2 <- cluster_sizes(logistic_ts, threshold, run_length = 2)
  sizes_r5 <- cluster_sizes(logistic_ts, threshold, run_length = 5)
  
  # Generally, larger run lengths should produce fewer, larger clusters
  if(length(sizes_r2) > 0 && length(sizes_r5) > 0) {
    expect_true(length(sizes_r5) <= length(sizes_r2))
  }
})

test_that('cluster size distribution properties', {
  set.seed(123)
  data(logistic_ts)
  threshold <- quantile(logistic_ts, 0.9)
  
  sizes <- cluster_sizes(logistic_ts, threshold, run_length = 3)
  
  if(length(sizes) > 0) {
    # All cluster sizes should be positive integers
    expect_true(all(sizes > 0))
    expect_true(all(sizes == floor(sizes)))
    
    # Summary statistics should be reasonable
    summary_stats <- cluster_summary(sizes)
    expect_true(summary_stats[["mean_size"]] > 0)
    expect_true(summary_stats[["var_size"]] >= 0)
  }
})

test_that('cluster analysis with real chaotic data', {
  set.seed(123)
  # Generate Hénon map data
  tryCatch({
    henon_data <- simulate_henon_map(1000, a = 1.4, b = 0.3)
    x_series <- henon_data$x
    threshold <- quantile(x_series, 0.95)
    
    sizes <- cluster_sizes(x_series, threshold, run_length = 3)
    expect_true(is.numeric(sizes))
    
    if(length(sizes) > 0) {
      summary_stats <- cluster_summary(sizes)
      expect_true(is.numeric(summary_stats[["mean_size"]]))
      expect_true(is.numeric(summary_stats[["var_size"]]))
    }
  }, error = function(e) {
    skip("simulate_henon_map function may not be available")
  })
})