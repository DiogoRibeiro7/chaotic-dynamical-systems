test_that('multivariate extreme value analysis', {
  set.seed(123)
  henon_data <- simulate_henon_map(500, a = 1.4, b = 0.3)
  theta_bivariate <- extremal_index_bivariate(henon_data, c(0.95, 0.95))
  expect_true(is.numeric(theta_bivariate))
  expect_true(theta_bivariate > 0 && theta_bivariate <= 1)

  x_threshold <- quantile(henon_data$x, 0.95)
  y_threshold <- quantile(henon_data$y, 0.95)
  theta_x <- extremal_index_runs(henon_data$x, x_threshold, run_length = 3)
  theta_y <- extremal_index_runs(henon_data$y, y_threshold, run_length = 3)
  expect_true(is.numeric(theta_x))
  expect_true(is.numeric(theta_y))
})

test_that('time-varying threshold analysis', {
  set.seed(123)
  data(logistic_ts)
  adaptive_thresholds <- adaptive_threshold_selection(logistic_ts, window_size = 100)
  expect_true(is.numeric(adaptive_thresholds))
  expect_true(length(adaptive_thresholds) > 0)

  nonstat_fit <- fit_nonstationary_gev(logistic_ts, trend = TRUE)
  expect_true(is.list(nonstat_fit))
})

test_that('tail dependence analysis', {
  set.seed(123)
  n <- 500
  x <- rnorm(n)
  y <- 0.7 * x + sqrt(1 - 0.7^2) * rnorm(n)
  tail_dep <- tail_dependence_coefficient(x, y, quantile_level = 0.9)
  expect_true(is.numeric(tail_dep))
  expect_true(tail_dep >= 0 && tail_dep <= 1)
})

test_that('advanced diagnostic methods', {
  set.seed(123)
  data(logistic_ts)
  spectral_props <- spectral_analysis_extremes(logistic_ts, threshold = quantile(logistic_ts, 0.95))
  expect_true(is.list(spectral_props))

  recurrence_props <- recurrence_analysis(logistic_ts)
  expect_true(is.list(recurrence_props))

  lyapunov <- estimate_lyapunov_exponent(logistic_ts)
  expect_true(is.numeric(lyapunov))
})

test_that('return level calculations', {
  set.seed(123)
  data(logistic_ts)
  return_levels <- calculate_return_levels(logistic_ts,
                                          threshold = quantile(logistic_ts, 0.95),
                                          return_periods = c(10, 100, 1000))
  expect_true(is.numeric(return_levels))
  expect_true(length(return_levels) == 3)
  expect_true(all(diff(return_levels) > 0))  # Should be increasing
})

test_that('model validation methods', {
  set.seed(123)
  data(logistic_ts)
  threshold <- quantile(logistic_ts, 0.95)
  validation_results <- validate_extreme_model(logistic_ts, threshold, method = "qq")
  expect_true(is.list(validation_results))

  gof_test <- goodness_of_fit_test(logistic_ts, threshold)
  expect_true(is.list(gof_test))
  expect_true("p_value" %in% names(gof_test))
})
