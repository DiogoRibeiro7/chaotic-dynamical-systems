context('Threshold selection and diagnostics')

test_that('mean_residual_life computes correctly', {
  x <- c(1, 2, 3, 4, 5)
  threshold <- 2.5
  
  mrl_df <- mean_residual_life(x, threshold)
  expected <- mean(x[x > threshold] - threshold)
  expect_equal(mrl_df$mean_excess, expected)
  expect_equal(mrl_df$threshold, threshold)
})

test_that('mean_residual_life handles no exceedances', {
  x <- c(1, 2, 3)
  threshold <- 5
  
  mrl_df <- mean_residual_life(x, threshold)
  expect_true(is.na(mrl_df$mean_excess))
  expect_equal(mrl_df$threshold, threshold)
})

test_that('threshold_diagnostics returns proper structure', {
  set.seed(42)
  x <- rnorm(100)
  thresholds <- quantile(x, c(0.8, 0.85, 0.9, 0.95))
  k_values <- c(5, 10, 15)
  
  diag <- threshold_diagnostics(x, thresholds, k_values)
  
  expect_true(is.list(diag))
  expect_true('mrl' %in% names(diag))
  expect_true('hill' %in% names(diag))
  expect_equal(length(diag$mrl$threshold), length(thresholds))
})

test_that('hill_estimates computes for valid k values', {
  set.seed(123)
  x <- rexp(100)  # exponential data
  k_values <- c(5, 10, 15)
  
  hill_est <- hill_estimates(x, k_values)
  expect_true(is.data.frame(hill_est))
  expect_true('k' %in% names(hill_est))
  expect_true('hill' %in% names(hill_est))
  expect_true(all(is.finite(hill_est$hill)))
})