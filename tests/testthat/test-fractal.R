context('Fractal dimension estimation')

test_that('estimate_correlation_dimension returns numeric', {
  set.seed(123)
  x <- simulate_logistic_map(200, 3.8, 0.2)
  res <- estimate_correlation_dimension(x, m = 2, tau = 1)
  expect_true(is.list(res))
  expect_true('dimension' %in% names(res))
  expect_true(is.numeric(res$dimension))
  expect_length(res$dimension, 1)
})
