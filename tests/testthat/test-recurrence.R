
test_that('recurrence_plot works', {
  set.seed(1)
  rp <- recurrence_plot(rnorm(50))
  expect_true(is.matrix(rp))
  expect_true(is.logical(rp))
})

test_that('recurrence_analysis computes measures', {
  set.seed(1)
  props <- recurrence_analysis(rnorm(50))
  expect_true(is.list(props))
  expect_true(all(c('recurrence_rate','determinism','recurrence_matrix') %in% names(props)))
})

test_that('recurrence_plot has correct class and errors on short series', {
  rp <- recurrence_plot(rnorm(10))
  expect_s3_class(rp, 'recurrence_plot')
  expect_error(recurrence_plot(rnorm(2), embed = 3),
               'time series too short')
})

test_that('recurrence_plot validates integer embed and delay', {
  expect_error(recurrence_plot(rnorm(10), embed = 2.5), 'integer')
  expect_error(recurrence_plot(rnorm(10), delay = 1.2), 'integer')
})

test_that('recurrence_analysis determinism drops to zero when lmin is large', {
  x <- rnorm(20)
  props <- recurrence_analysis(x, lmin = 50)
  expect_equal(props$determinism, 0)
})

