context('C++ implementation tests')

test_that('C++ logistic map simulation works correctly', {
  skip_if_not_installed("Rcpp")
  
  n <- 100
  r <- 3.8
  x0 <- 0.2
  
  # Test basic functionality
  expect_silent(result_cpp <- simulate_logistic_map_cpp(n, r, x0))
  expect_true(is.numeric(result_cpp))
  expect_equal(length(result_cpp), n)
  expect_equal(result_cpp[1], x0)
  
  # Test consistency with R implementation
  result_r <- simulate_logistic_map(n, r, x0)
  expect_equal(result_cpp, result_r, tolerance = 1e-10)
})

test_that('C++ Hénon map simulation works correctly', {
  skip_if_not_installed("Rcpp")
  
  n <- 100
  a <- 1.4
  b <- 0.3
  x0 <- 0.1
  y0 <- 0.1
  
  # Test basic functionality  
  expect_silent(result_cpp <- simulate_henon_map_cpp(n, a, b, x0, y0))
  expect_true(is.data.frame(result_cpp))
  expect_equal(nrow(result_cpp), n)
  expect_equal(ncol(result_cpp), 2)
  expect_true(all(c("x", "y") %in% names(result_cpp)))
  
  # Test initial conditions
  expect_equal(result_cpp$x[1], x0)
  expect_equal(result_cpp$y[1], y0)
  
  # Test consistency with R implementation
  result_r <- simulate_henon_map(n, a, b, x0, y0)
  expect_equal(result_cpp$x, result_r$x, tolerance = 1e-10)
  expect_equal(result_cpp$y, result_r$y, tolerance = 1e-10)
})

test_that('C++ threshold exceedances works correctly', {
  skip_if_not_installed("Rcpp")
  
  x <- c(0.5, 1.5, 0.3, 2.1, 0.7, 1.8)
  threshold <- 1.0
  
  # Test basic functionality
  expect_silent(result_cpp <- threshold_exceedances_cpp(x, threshold))
  expect_true(is.integer(result_cpp))
  
  # Test correctness
  expected_indices <- c(2, 4, 6)  # 1-based indexing
  expect_equal(as.integer(result_cpp), expected_indices)
  
  # Test consistency with R implementation
  result_r <- threshold_exceedances(x, threshold)
  expect_equal(as.integer(result_cpp), result_r)
})

test_that('C++ cluster exceedances works correctly', {
  skip_if_not_installed("Rcpp")
  
  indices <- c(1L, 2L, 3L, 10L, 11L, 20L)
  run_length <- 2L
  
  # Test basic functionality
  expect_silent(result_cpp <- cluster_exceedances_cpp(indices, run_length))
  expect_true(is.list(result_cpp))
  expect_true("clusters" %in% names(result_cpp))
  expect_true("n_clusters" %in% names(result_cpp))
  
  # Test with empty input
  expect_silent(empty_result <- cluster_exceedances_cpp(integer(0), run_length))
  expect_equal(empty_result$n_clusters, 0)
  expect_equal(length(empty_result$clusters), 0)
})

test_that('C++ functions handle edge cases', {
  skip_if_not_installed("Rcpp")
  
  # Test with small inputs
  expect_silent(simulate_logistic_map_cpp(1, 3.8, 0.2))
  expect_silent(simulate_henon_map_cpp(1, 1.4, 0.3))
  
  # Test with no exceedances
  x <- c(0.1, 0.2, 0.3)
  threshold <- 1.0
  result <- threshold_exceedances_cpp(x, threshold)
  expect_equal(length(result), 0)
})