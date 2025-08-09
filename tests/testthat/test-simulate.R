
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

test_that('simulate_tent_map returns values in [0,1]', {
  set.seed(99)
  series <- simulate_tent_map(20, r = 2, x0 = 0.1)
  expect_equal(length(series), 20)
  expect_true(all(series >= 0 & series <= 1))
})

test_that('simulate_lozi_map returns data frame with n rows', {
  set.seed(2024)
  df <- simulate_lozi_map(25)
  expect_true(is.data.frame(df))
  expect_equal(nrow(df), 25)
  expect_true(all(c('x', 'y') %in% names(df)))
})

test_that('simulate_cat_map returns data frame with n rows', {
  set.seed(2025)
  df <- simulate_cat_map(30)
  expect_true(is.data.frame(df))
  expect_equal(nrow(df), 30)
  expect_true(all(c('x', 'y') %in% names(df)))
  expect_true(all(df$x >= 0 & df$x < 1))
  expect_true(all(df$y >= 0 & df$y < 1))
})

test_that('logistic_bifurcation returns data frame', {
  r_vals <- seq(3, 3.5, length.out = 10)
  bif <- logistic_bifurcation(r_vals, n_iter = 50, discard = 25)
  expect_s3_class(bif, 'data.frame')
  expect_true(all(c('r', 'x') %in% names(bif)))
  expect_true(nrow(bif) > 0)
})

test_that('logistic map variance increases in chaotic regime', {
  set.seed(42)
  series_periodic <- simulate_logistic_map(1000, r = 3.2, x0 = 0.2)
  set.seed(42)
  series_chaotic <- simulate_logistic_map(1000, r = 3.8, x0 = 0.2)
  expect_lt(var(series_periodic), var(series_chaotic))
})
