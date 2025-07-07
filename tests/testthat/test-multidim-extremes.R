context('Multi-dimensional extremes')

test_that('multivariate_exceedances works correctly', {
  # Create test data
  X <- matrix(c(1, 2, 0.5, 1.5, 
                2, 0.8, 1.2, 2.1), 
              nrow = 4, ncol = 2)
  thresholds <- c(1.0, 1.0)
  
  # Test componentwise method
  result_comp <- multivariate_exceedances(X, thresholds, method = "componentwise")
  expect_true(is.list(result_comp))
  expect_true("indices" %in% names(result_comp))
  expect_true("n_exceedances" %in% names(result_comp))
  expect_true("method" %in% names(result_comp))
  
  # Should find rows where at least one component exceeds threshold
  expected_indices <- c(1, 2, 4)  # Rows with values > (1.0, 1.0)
  expect_equal(sort(result_comp$indices), expected_indices)
  
  # Test radial method
  result_radial <- multivariate_exceedances(X, thresholds, method = "radial")
  expect_equal(result_radial$method, "radial")
  expect_true(is.numeric(result_radial$indices))
})

test_that('extremal_dependence handles bivariate data', {
  # Generate correlated bivariate data
  n <- 100
  set.seed(123)
  X1 <- rnorm(n)
  X2 <- 0.5 * X1 + sqrt(0.75) * rnorm(n)
  X <- cbind(X1, X2)
  
  # Test chi statistic
  result_chi <- extremal_dependence(X, method = "chi", quantile_level = 0.9)
  expect_true(is.list(result_chi))
  expect_true("chi" %in% names(result_chi))
  expect_true(is.finite(result_chi$chi))
  expect_true(result_chi$chi >= -1 && result_chi$chi <= 1)
  
  # Test chi-bar statistic
  result_chibar <- extremal_dependence(X, method = "chibar", quantile_level = 0.9)
  expect_true("chi_bar" %in% names(result_chibar))
})

test_that('adaptive_thresholds selects reasonable thresholds', {
  set.seed(42)
  X <- matrix(rnorm(200), nrow = 100, ncol = 2)
  
  # Test basic functionality
  result <- adaptive_thresholds(X, target_exceedances = 20)
  expect_true(is.list(result))
  expect_true("thresholds" %in% names(result))
  expect_equal(length(result$thresholds), 2)
  expect_true(all(is.finite(result$thresholds)))
  
  # Test without stability check
  result_no_check <- adaptive_thresholds(X, target_exceedances = 20, 
                                         stability_check = FALSE)
  expect_equal(result_no_check$stability_check, FALSE)
})

test_that('time_varying_thresholds produces correct output', {
  set.seed(123)
  x <- cumsum(rnorm(200))  # Random walk
  
  # Test moving quantile method
  result_moving <- time_varying_thresholds(x, window_size = 50, 
                                           quantile_level = 0.9,
                                           method = "moving")
  expect_true(is.numeric(result_moving))
  expect_equal(length(result_moving), length(x))
  expect_true(all(is.finite(result_moving)))
  
  # Test local GP method
  result_gp <- time_varying_thresholds(x, window_size = 50,
                                       quantile_level = 0.9,
                                       method = "local_gp")
  expect_true(is.numeric(result_gp))
  expect_equal(length(result_gp), length(x))
  
  # Test with matrix input
  X_mat <- matrix(rnorm(400), nrow = 200, ncol = 2)
  result_matrix <- time_varying_thresholds(X_mat, window_size = 50)
  expect_true(is.matrix(result_matrix))
  expect_equal(dim(result_matrix), dim(X_mat))
})

test_that('multivariate functions handle edge cases', {
  # Single observation
  X_single <- matrix(c(1, 2), nrow = 1)
  thresholds_single <- c(0.5, 0.5)
  
  expect_silent(multivariate_exceedances(X_single, thresholds_single))
  
  # All observations below threshold
  X_low <- matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2)
  thresholds_high <- c(1.0, 1.0)
  
  result_low <- multivariate_exceedances(X_low, thresholds_high)
  expect_equal(result_low$n_exceedances, 0)
  expect_equal(length(result_low$indices), 0)
})

test_that('extremal_dependence requires bivariate data', {
  # Test error with non-bivariate data
  X_univariate <- matrix(rnorm(100), nrow = 100, ncol = 1)
  expect_error(extremal_dependence(X_univariate), "Only bivariate data supported")
  
  X_trivariate <- matrix(rnorm(300), nrow = 100, ncol = 3)
  expect_error(extremal_dependence(X_trivariate), "Only bivariate data supported")
})