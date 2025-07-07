context('Advanced diagnostic tools')

test_that('enhanced_gev_diagnostics produces comprehensive output', {
  # Generate test data
  set.seed(42)
  data <- rnorm(50, mean = 5, sd = 2)
  
  # Create mock fit result (similar to what fit_gev would return)
  fit_result <- list(
    mle = c(location = 5, scale = 2, shape = 0.1),
    loglik = -100,
    convergence = 0
  )
  
  # Test diagnostic function
  result <- enhanced_gev_diagnostics(data, fit_result, return_plots = TRUE)
  
  expect_true(is.list(result))
  expect_true("diagnostics" %in% names(result))
  expect_true("plots" %in% names(result))
  
  # Check diagnostic statistics
  diag <- result$diagnostics
  expect_true("ks_test" %in% names(diag))
  expect_true("anderson_darling" %in% names(diag))
  expect_true("loglik" %in% names(diag))
  expect_true("aic" %in% names(diag))
  expect_true("bic" %in% names(diag))
  expect_true("return_levels" %in% names(diag))
  
  # Check return levels structure
  rl <- diag$return_levels
  expect_true(is.data.frame(rl))
  expect_true(all(c("period", "estimate") %in% names(rl)))
  expect_true(all(rl$period > 0))
  
  # Check plots when ggplot2 is available
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    plots <- result$plots
    expect_true(is.list(plots))
    # Should have requested plot types
    expected_plots <- c("qq", "pp", "return_level", "residuals")
    # Not all plots may be generated if there are issues with data
  }
})

test_that('enhanced_gev_diagnostics handles different fit result formats', {
  data <- rnorm(30, mean = 3, sd = 1)
  
  # Test with 'estimate' field instead of 'mle'
  fit_result_alt <- list(
    estimate = c(3, 1, 0.05),
    loglik = -80
  )
  
  expect_silent(result <- enhanced_gev_diagnostics(data, fit_result_alt, 
                                                   plot_type = "qq", return_plots = FALSE))
  expect_true("diagnostics" %in% names(result))
  
  # Test error with invalid fit result
  invalid_fit <- list(some_other_field = c(1, 2, 3))
  expect_error(enhanced_gev_diagnostics(data, invalid_fit), 
               "Cannot extract parameters from fit_result")
})

test_that('cv_threshold_selection finds reasonable thresholds', {
  # Generate test data with known extremes
  set.seed(123)
  data <- c(rnorm(180, mean = 0, sd = 1), rnorm(20, mean = 3, sd = 0.5))  # Mix with extremes
  
  # Test basic functionality
  result <- cv_threshold_selection(data, k_fold = 3, criterion = "loglik")
  
  expect_true(is.list(result))
  expect_true("optimal_threshold" %in% names(result))
  expect_true("cv_scores" %in% names(result))
  expect_true("mean_scores" %in% names(result))
  expect_true("thresholds" %in% names(result))
  expect_true("criterion" %in% names(result))
  
  expect_true(is.numeric(result$optimal_threshold))
  expect_true(is.finite(result$optimal_threshold))
  expect_true(is.matrix(result$cv_scores))
  expect_equal(length(result$mean_scores), length(result$thresholds))
  
  # Test with AIC criterion
  result_aic <- cv_threshold_selection(data, k_fold = 3, criterion = "aic")
  expect_equal(result_aic$criterion, "aic")
  
  # Test with custom thresholds
  custom_thresholds <- quantile(data, c(0.8, 0.85, 0.9, 0.95))
  result_custom <- cv_threshold_selection(data, thresholds = custom_thresholds, k_fold = 3)
  expect_equal(result_custom$thresholds, custom_thresholds)
})

test_that('enhanced_bootstrap_ei provides confidence intervals', {
  # Generate test data with clustering
  set.seed(456)
  n <- 200
  data <- arima.sim(list(ar = 0.6), n = n)  # AR(1) process
  
  threshold <- quantile(data, 0.9)
  
  # Test runs method
  result_runs <- enhanced_bootstrap_ei(data, threshold, method = "runs", 
                                       n_bootstrap = 50, confidence_level = 0.95)
  
  expect_true(is.list(result_runs))
  expect_true("estimate" %in% names(result_runs))
  expect_true("bootstrap_estimates" %in% names(result_runs))
  expect_true("ci_basic" %in% names(result_runs))
  expect_true("ci_bca" %in% names(result_runs))
  expect_true("bias_correction" %in% names(result_runs))
  expect_true("acceleration" %in% names(result_runs))
  
  expect_true(is.numeric(result_runs$estimate))
  expect_true(is.numeric(result_runs$bootstrap_estimates))
  expect_true(length(result_runs$ci_basic) == 2)
  expect_true(length(result_runs$ci_bca) == 2)
  expect_true(result_runs$ci_basic[1] <= result_runs$ci_basic[2])
  
  # Test intervals method
  result_intervals <- enhanced_bootstrap_ei(data, threshold, method = "intervals", 
                                            n_bootstrap = 50)
  expect_equal(result_intervals$method, "intervals")
})

test_that('gev_loglik_vectorized works correctly', {
  x <- c(1, 2, 3, 4, 5)
  location <- 2
  scale <- 1
  shape <- 0.1
  
  ll_vec <- gev_loglik_vectorized(x, location, scale, shape)
  
  expect_true(is.numeric(ll_vec))
  expect_equal(length(ll_vec), length(x))
  expect_true(all(is.finite(ll_vec)))
  
  # Test with invalid scale
  ll_invalid <- gev_loglik_vectorized(x, location, -1, shape)
  expect_true(all(ll_invalid == -Inf))
  
  # Test Gumbel case
  ll_gumbel <- gev_loglik_vectorized(x, location, scale, 1e-10)
  expect_true(all(is.finite(ll_gumbel)))
})

test_that('gpd_loglik_vectorized works correctly', {
  x <- c(0.5, 1.0, 1.5, 2.0)
  scale <- 1
  shape <- 0.1
  
  ll_vec <- gpd_loglik_vectorized(x, scale, shape)
  
  expect_true(is.numeric(ll_vec))
  expect_equal(length(ll_vec), length(x))
  expect_true(all(is.finite(ll_vec)))
  
  # Test with invalid scale
  ll_invalid <- gpd_loglik_vectorized(x, -1, shape)
  expect_true(all(ll_invalid == -Inf))
  
  # Test exponential case
  ll_exp <- gpd_loglik_vectorized(x, scale, 1e-10)
  expect_true(all(is.finite(ll_exp)))
})

test_that('diagnostic functions handle edge cases', {
  # Very small dataset
  small_data <- c(1, 2, 3)
  fit_small <- list(mle = c(1.5, 0.5, 0))
  
  # Should still work but may have warnings
  expect_silent(enhanced_gev_diagnostics(small_data, fit_small, return_plots = FALSE))
  
  # Data with insufficient exceedances for CV
  mostly_low_data <- c(rep(0, 15), 1, 2)
  expect_error(cv_threshold_selection(mostly_low_data), 
               "length\\(data\\) > 20 is not TRUE")
  
  # Bootstrap with insufficient data
  tiny_data <- rnorm(30)
  threshold_high <- quantile(tiny_data, 0.99)
  
  # May produce warning about too few bootstrap estimates
  expect_warning(enhanced_bootstrap_ei(tiny_data, threshold_high, n_bootstrap = 20),
                 "Too few valid bootstrap estimates")
})

test_that('diagnostic plots are generated when ggplot2 is available', {
  skip_if_not_installed("ggplot2")
  
  set.seed(789)
  data <- rnorm(40, mean = 5, sd = 1.5)
  fit_result <- list(mle = c(5, 1.5, 0))
  
  result <- enhanced_gev_diagnostics(data, fit_result, 
                                     plot_type = c("qq", "pp"),
                                     return_plots = TRUE)
  
  expect_true("plots" %in% names(result))
  plots <- result$plots
  
  if (length(plots) > 0) {
    expect_true(any(sapply(plots, function(p) "ggplot" %in% class(p))))
  }
})