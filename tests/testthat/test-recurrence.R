context('Recurrence analysis')

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

