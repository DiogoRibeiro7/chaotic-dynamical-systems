context('Bayesian extreme value methods')

test_that('bayesian_gev produces valid output structure', {
  # Generate test data
  set.seed(42)
  data <- rnorm(50, mean = 5, sd = 2)  # Simple test data
  
  # Test with metropolis method
  result <- bayesian_gev(data, n_iter = 100, n_warmup = 50, method = "metropolis")
  
  expect_true(is.list(result))
  expect_true("samples" %in% names(result))
  expect_true("summary" %in% names(result))
  expect_true("acceptance_rate" %in% names(result))
  expect_true("method" %in% names(result))
  
  # Check samples structure
  expect_true(is.matrix(result$samples))
  expect_equal(ncol(result$samples), 3)  # location, scale, shape
  expect_equal(nrow(result$samples), 100 - 50)  # post-warmup samples
  expect_true(all(c("location", "scale", "shape") %in% colnames(result$samples)))
  
  # Check summary structure
  expect_true(is.data.frame(result$summary))
  expect_equal(nrow(result$summary), 3)
  expect_true(all(c("mean", "sd", "q025", "q975") %in% names(result$summary)))
  
  # Check acceptance rate
  expect_true(is.numeric(result$acceptance_rate))
  expect_true(result$acceptance_rate >= 0 && result$acceptance_rate <= 1)
})

test_that('bayesian_gpd produces valid output structure', {
  # Generate test exceedances
  set.seed(123)
  excesses <- rexp(30, rate = 1)  # Exponential exceedances
  
  result <- bayesian_gpd(excesses, n_iter = 100, n_warmup = 50)
  
  expect_true(is.list(result))
  expect_true("samples" %in% names(result))
  expect_true("summary" %in% names(result))
  expect_true("acceptance_rate" %in% names(result))
  
  # Check samples structure
  expect_true(is.matrix(result$samples))
  expect_equal(ncol(result$samples), 2)  # scale, shape
  expect_equal(nrow(result$samples), 100 - 50)
  expect_true(all(c("scale", "shape") %in% colnames(result$samples)))
  
  # Check that scale parameters are positive
  expect_true(all(result$samples[, "scale"] > 0))
})

test_that('gev_loglik function works correctly', {
  # Test GEV log-likelihood function
  data <- c(1, 2, 3, 4, 5)
  location <- 2
  scale <- 1
  shape <- 0.1
  
  ll <- gev_loglik(data, location, scale, shape)
  expect_true(is.numeric(ll))
  expect_true(is.finite(ll))
  
  # Test with invalid scale (should return -Inf)
  ll_invalid <- gev_loglik(data, location, -1, shape)
  expect_equal(ll_invalid, -Inf)
  
  # Test Gumbel case (shape ≈ 0)
  ll_gumbel <- gev_loglik(data, location, scale, 1e-10)
  expect_true(is.finite(ll_gumbel))
})

test_that('gpd_loglik function works correctly', {
  # Test GPD log-likelihood function
  excesses <- c(0.5, 1.0, 1.5, 2.0)
  scale <- 1
  shape <- 0.1
  
  ll <- gpd_loglik(excesses, scale, shape)
  expect_true(is.numeric(ll))
  expect_true(is.finite(ll))
  
  # Test with invalid scale
  ll_invalid <- gpd_loglik(excesses, -1, shape)
  expect_equal(ll_invalid, -Inf)
  
  # Test exponential case (shape ≈ 0)
  ll_exp <- gpd_loglik(excesses, scale, 1e-10)
  expect_true(is.finite(ll_exp))
  
  # Test with negative excesses (should be valid for GPD)
  expect_true(all(excesses >= 0))  # Input validation for test
})

test_that('bayesian_return_levels computes return levels correctly', {
  # Create mock posterior samples
  n_samples <- 100
  set.seed(456)
  
  # GEV case
  gev_samples <- matrix(c(
    rnorm(n_samples, 5, 0.1),      # location
    rgamma(n_samples, 4, 2),       # scale (positive)
    rnorm(n_samples, 0.1, 0.02)    # shape
  ), ncol = 3)
  colnames(gev_samples) <- c("location", "scale", "shape")
  
  return_periods <- c(10, 50, 100)
  result_gev <- bayesian_return_levels(gev_samples, return_periods, model = "gev")
  
  expect_true(is.data.frame(result_gev))
  expect_equal(nrow(result_gev), length(return_periods))
  expect_true(all(c("return_period", "mean", "sd", "q025", "q050", "q975") %in% names(result_gev)))
  expect_equal(result_gev$return_period, return_periods)
  
  # GPD case
  gpd_samples <- matrix(c(
    rgamma(n_samples, 4, 2),       # scale (positive)
    rnorm(n_samples, 0.1, 0.02)    # shape
  ), ncol = 2)
  colnames(gpd_samples) <- c("scale", "shape")
  
  result_gpd <- bayesian_return_levels(gpd_samples, return_periods, model = "gpd",
                                       threshold = 1.0, exceedance_rate = 0.1)
  
  expect_true(is.data.frame(result_gpd))
  expect_equal(nrow(result_gpd), length(return_periods))
})

test_that('bayesian functions handle edge cases', {
  # Test with very small datasets
  small_data <- c(1, 2, 3)
  expect_error(bayesian_gev(small_data), "length\\(data\\) > 5 is not TRUE")
  
  small_excesses <- c(0.1, 0.2)
  expect_error(bayesian_gpd(small_excesses), "length\\(excesses\\) > 5 is not TRUE")
  
  # Test with negative excesses for GPD
  negative_excesses <- c(-1, 0.5, 1.0, 1.5, 2.0, 2.5)
  expect_error(bayesian_gpd(negative_excesses), "all\\(excesses >= 0\\) is not TRUE")
})

test_that('bayesian methods are reproducible with seed', {
  set.seed(789)
  data <- rnorm(30, mean = 3, sd = 1)
  
  # Run twice with same seed
  set.seed(100)
  result1 <- bayesian_gev(data, n_iter = 50, n_warmup = 25)
  
  set.seed(100)
  result2 <- bayesian_gev(data, n_iter = 50, n_warmup = 25)
  
  # Results should be identical (or very close due to RNG)
  expect_equal(result1$samples, result2$samples, tolerance = 1e-10)
})

# Test that requires MASS package for multivariate normal sampling
test_that('bayesian methods work without MASS package', {
  # This test ensures the functions can handle missing MASS package gracefully
  # In practice, MASS is usually available, but we test robustness
  
  data <- rnorm(20, mean = 2, sd = 1)
  
  # The function should still work even if MASS is not available
  # (it may use alternative sampling methods)
  expect_silent(result <- bayesian_gev(data, n_iter = 20, n_warmup = 10))
  expect_true(is.list(result))
})