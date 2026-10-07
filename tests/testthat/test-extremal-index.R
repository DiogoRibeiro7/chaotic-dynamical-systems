
test_that('extremal_index_runs returns a scalar in (0, 1]', {
  set.seed(123)
  data(logistic_ts)
  threshold <- quantile(logistic_ts, 0.95)

  theta <- extremal_index_runs(logistic_ts, threshold, run_length = 2)
  expect_length(theta, 1L)
  expect_true(is.numeric(theta))
  expect_gt(theta, 0)
  expect_lte(theta, 1)
})

test_that("extremal_index_intervals matches fixed Ferro-Segers references", {
  # Exceedance indices 1, 2, 3, 9 give gaps (1, 1, 6).
  # Since max(T_i) > 2:
  # theta = 2 * (0 + 0 + 5)^2 / (3 * (0 + 0 + 5 * 4)) = 5/6.
  x_clustered <- rep(0, 9)
  x_clustered[c(1, 2, 3, 9)] <- 1
  expect_equal(
    extremal_index_intervals(x_clustered, threshold = 0.5),
    5 / 6,
    tolerance = 1e-12
  )

  # Exceedance indices 1, 2, 4, 5 give gaps (1, 2, 1).
  # The short-gap branch gives a value above one, so the estimator is capped.
  x_short <- rep(0, 5)
  x_short[c(1, 2, 4, 5)] <- 1
  expect_equal(
    extremal_index_intervals(x_short, threshold = 0.5),
    1,
    tolerance = 1e-12
  )
})

test_that("R and C++ intervals estimators agree and stay in range", {
  x <- rep(0, 20)
  x[c(1, 2, 3, 9, 10, 20)] <- 1

  theta_r <- extremal_index_intervals(x, threshold = 0.5)
  theta_cpp <- extremal_index_intervals_cpp(x, threshold = 0.5)

  expect_equal(theta_cpp, theta_r, tolerance = 1e-12)
  expect_gte(theta_r, 0)
  expect_lte(theta_r, 1)
})

test_that("extremal_index_intervals handles insufficient exceedances", {
  expect_true(is.na(extremal_index_intervals(rep(0, 10), threshold = 0.5)))

  x <- rep(0, 10)
  x[4] <- 1
  expect_true(is.na(extremal_index_intervals(x, threshold = 0.5)))
  expect_true(is.na(extremal_index_intervals_cpp(x, threshold = 0.5)))
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