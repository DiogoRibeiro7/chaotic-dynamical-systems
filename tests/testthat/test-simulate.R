context('Simulation utilities')

test_that('simulate_logistic_map returns correct length and start value', {
  set.seed(123)
  res <- simulate_logistic_map(10, r = 3.8, x0 = 0.2)
  expect_equal(length(res), 10)
  expect_equal(res[1], 0.2)
})

test_that('simulate_henon_map returns data frame with n rows', {
  set.seed(42)
  df <- simulate_henon_map(15)
  expect_true(is.data.frame(df))
  expect_equal(nrow(df), 15)
  expect_true(all(c('x', 'y') %in% names(df)))
})
