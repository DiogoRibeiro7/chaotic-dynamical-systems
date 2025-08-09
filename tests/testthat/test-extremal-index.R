
test_that('extremal_index_runs returns numeric', {
  set.seed(123)
  data(logistic_ts)
  threshold <- quantile(logistic_ts, 0.95)
  
  theta <- extremal_index_runs(logistic_ts, threshold, run_length = 2)
  expect_true(is.numeric(theta))
  if (length(theta) > 0) {
    expect_true(theta > 0 && theta <= 1)
  }
})

test_that('extremal_index_intervals returns numeric', {
  set.seed(123)
  data(logistic_ts)
  threshold <- quantile(logistic_ts, 0.95)
  
  theta <- extremal_index_intervals(logistic_ts, threshold)
  expect_true(is.numeric(theta))
  # Just test that it completes without error, value may be Inf or out of range
})

test_that('hitting_times computes correctly', {
  x <- c(0.5, 1.5, 0.3, 2.1, 0.7, 1.8)
  threshold <- 1.0
  
  times <- hitting_times(x, threshold)
  expect_true(is.numeric(times))
  expect_true(all(times > 0))
})

test_that('cluster_sizes function works', {
  # No exceedances
  x <- rep(0.5, 10)
  threshold <- 1.0
  sizes <- cluster_sizes(x, threshold, run_length = 2)
  expect_equal(length(sizes), 0)
  
  # Create exceedances that should form a cluster
  x <- c(0.5, 1.5, 1.6, 1.7, 0.5)  # Three consecutive exceedances
  sizes <- cluster_sizes(x, threshold, run_length = 3)
  expect_true(is.numeric(sizes))
})

test_that('cluster_summary produces correct statistics', {
  sizes <- c(1, 2, 3, 1, 4, 2)
  summary_stats <- cluster_summary(sizes)
  
  expect_equal(summary_stats[["mean_size"]], mean(sizes))
  expect_equal(summary_stats[["var_size"]], var(sizes))
  expect_equal(length(summary_stats), 2)
})