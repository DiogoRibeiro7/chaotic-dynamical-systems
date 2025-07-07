context('Example datasets')

test_that('logistic_ts dataset is properly formatted', {
  data(logistic_ts)
  
  expect_true(is.numeric(logistic_ts))
  expect_equal(length(logistic_ts), 5000)
  expect_true(all(logistic_ts >= 0 & logistic_ts <= 1))  # logistic map range
  expect_equal(logistic_ts[1], 0.2)  # initial condition
})

test_that('henon_ts dataset is properly formatted', {
  data(henon_ts)
  
  expect_true(is.data.frame(henon_ts))
  expect_equal(nrow(henon_ts), 3000)
  expect_true(all(c('x', 'y') %in% names(henon_ts)))
  expect_true(all(is.numeric(henon_ts$x)))
  expect_true(all(is.numeric(henon_ts$y)))
})

test_that('ar1_ts dataset is properly formatted', {
  data(ar1_ts)
  
  expect_true(is.numeric(ar1_ts))
  expect_equal(length(ar1_ts), 4000)
  expect_true(all(is.finite(ar1_ts)))
})

test_that('datasets can be used for basic analysis', {
  data(logistic_ts)
  data(henon_ts)
  data(ar1_ts)
  
  # Test basic extreme value analysis
  threshold <- quantile(logistic_ts, 0.9)
  exc_logistic <- exceedances(logistic_ts, threshold)
  expect_true(length(exc_logistic) > 0)
  
  # Test with Henon x-component
  threshold_henon <- quantile(henon_ts$x, 0.9)
  exc_henon <- exceedances(henon_ts$x, threshold_henon)
  expect_true(length(exc_henon) > 0)
  
  # Test with AR(1) data
  threshold_ar1 <- quantile(ar1_ts, 0.9)
  exc_ar1 <- exceedances(ar1_ts, threshold_ar1)
  expect_true(length(exc_ar1) > 0)
})