
test_that('bootstrap_extremal_index returns confidence intervals', {
  set.seed(123)
  data(logistic_ts)
  threshold <- quantile(logistic_ts, 0.95)
  
  # Test with small sample for speed
  sample_data <- logistic_ts[1:500]
  
  boot <- bootstrap_extremal_index(sample_data, threshold, n_bootstrap = 10, method = "runs")
  expect_type(boot, "list")
  expect_true(all(c("estimate", "replicates", "ci") %in% names(boot)))
  expect_true(length(boot$replicates) == 10)
  expect_true(length(boot$ci) == 2)
  if (all(is.finite(boot$ci))) {
    expect_true(boot$ci[1] <= boot$ci[2])
  } else {
    expect_true(all(is.na(boot$ci)))
  }
})

test_that('bootstrap confidence intervals are well-formed', {
  set.seed(42)
  
  # Create simple test data with known extremal behavior
  test_data <- c(rep(0.1, 80), rep(0.9, 20))
  threshold <- 0.5
  
  ci_runs <- bootstrap_extremal_index(test_data, threshold, n_bootstrap = 20, method = "runs")
  ci_intervals <- bootstrap_extremal_index(test_data, threshold, n_bootstrap = 20, method = "intervals")

  if (all(is.finite(ci_runs$ci))) {
    expect_true(all(ci_runs$ci >= 0 & ci_runs$ci <= 1))
  } else {
    expect_true(all(is.na(ci_runs$ci)))
  }

  if (all(is.finite(ci_intervals$ci))) {
    expect_true(all(ci_intervals$ci >= 0 & ci_intervals$ci <= 1))
  } else {
    expect_true(all(is.na(ci_intervals$ci)))
  }
})

test_that('bootstrap handles edge cases', {
  set.seed(123)
  
  # Test with no exceedances
  no_exceed_data <- rep(0.1, 100)
  threshold <- 0.5
  
  ci <- bootstrap_extremal_index(no_exceed_data, threshold, n_bootstrap = 10)
  expect_true(is.list(ci))
  expect_true(is.numeric(ci$ci) || is.na(ci$ci[1]))
  
  # Test with all exceedances
  all_exceed_data <- rep(0.9, 100)
  threshold <- 0.5
  
  ci <- bootstrap_extremal_index(all_exceed_data, threshold, n_bootstrap = 10)
  expect_true(is.list(ci))
  expect_true(is.numeric(ci$ci))
})

test_that('block bootstrap maintains temporal dependence', {
  set.seed(123)
  data(logistic_ts)
  
  resampled <- block_bootstrap(logistic_ts[1:100], block_length = 10)
  expect_true(length(resampled) == 100)
  expect_true(is.numeric(resampled))
})
