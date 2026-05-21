test_that("decluster returns the documented data-frame shape", {
  x <- c(0.1, 1.5, 1.6, 0.1, 0.1, 2.0, 2.1, 0.1, 3.0)
  u <- 1.0
  dec <- decluster(x, threshold = u, run_length = 1L)

  expect_s3_class(dec, "data.frame")
  expect_named(dec, c("cluster", "start_index", "end_index", "n", "value"))
  expect_type(dec$cluster,     "integer")
  expect_type(dec$start_index, "integer")
  expect_type(dec$end_index,   "integer")
  expect_type(dec$n,           "integer")
  expect_type(dec$value,       "double")
})

test_that("decluster identifies the expected clusters with run_length = 1", {
  # Exceedances at indices 2,3 | 6,7 | 9 with run_length = 1.
  x <- c(0.1, 1.5, 1.6, 0.1, 0.1, 2.0, 2.1, 0.1, 3.0)
  dec <- decluster(x, threshold = 1.0, run_length = 1L)

  expect_equal(nrow(dec), 3L)
  expect_equal(dec$cluster,     1:3)
  expect_equal(dec$start_index, c(2L, 6L, 9L))
  expect_equal(dec$end_index,   c(3L, 7L, 9L))
  expect_equal(dec$n,           c(2L, 2L, 1L))
  expect_equal(dec$value,       c(1.6, 2.1, 3.0))
})

test_that("run_length controls cluster merging", {
  # With a gap of 2 indices between consecutive exceedance runs, run_length = 2
  # merges them into one cluster but run_length = 1 keeps them separate.
  x <- c(0.1, 1.5, 1.6, 0.1, 1.7, 0.1, 0.1, 0.1)
  expect_equal(nrow(decluster(x, 1.0, run_length = 1L)), 2L)
  expect_equal(nrow(decluster(x, 1.0, run_length = 2L)), 1L)
})

test_that("stat options each pick the documented summary", {
  x <- c(0.1, 1.5, 1.6, 2.0, 0.1, 0.1)  # one cluster, exceedances 1.5, 1.6, 2.0
  args <- list(x = x, threshold = 1.0, run_length = 1L)
  expect_equal(do.call(decluster, c(args, list(stat = "max")))$value,   2.0)
  expect_equal(do.call(decluster, c(args, list(stat = "first")))$value, 1.5)
  expect_equal(do.call(decluster, c(args, list(stat = "last")))$value,  2.0)
  expect_equal(do.call(decluster, c(args, list(stat = "sum")))$value,   1.5 + 1.6 + 2.0)
  expect_equal(do.call(decluster, c(args, list(stat = "mean")))$value,  mean(c(1.5, 1.6, 2.0)))
})

test_that("decluster handles edge cases", {
  # No exceedances above the threshold.
  empty <- decluster(rep(0.1, 10), threshold = 1.0)
  expect_equal(nrow(empty), 0L)
  expect_named(empty, c("cluster", "start_index", "end_index", "n", "value"))

  # Single exceedance.
  one <- decluster(c(0.1, 0.1, 1.5, 0.1), threshold = 1.0)
  expect_equal(nrow(one), 1L)
  expect_equal(one$n,           1L)
  expect_equal(one$start_index, 3L)
  expect_equal(one$end_index,   3L)
  expect_equal(one$value,       1.5)
})

test_that("decluster rejects invalid arguments", {
  expect_error(decluster(numeric(0), threshold = 1))
  expect_error(decluster(1:10, threshold = NA))
  expect_error(decluster(1:10, threshold = 1, run_length = 0))
  expect_error(decluster(1:10, threshold = 1, stat = "median"))
})

test_that("decluster reduces the exceedance count when clustering is present", {
  # Two unambiguous clusters of three consecutive exceedances plus one
  # isolated exceedance: 7 raw values collapse to 3 cluster representatives.
  x <- c(rep(0.1, 5), 1.5, 1.6, 1.7, rep(0.1, 5),
         2.0, 2.1, 2.2, rep(0.1, 10), 3.0, rep(0.1, 5))
  raw <- exceedances(x, 1.0)
  dec <- decluster(x, threshold = 1.0, run_length = 1L)

  expect_length(raw, 7L)
  expect_equal(nrow(dec), 3L)
  expect_lt(nrow(dec), length(raw))
})

test_that("decluster output integrates with fit_gpd", {
  # Compose decluster -> fit_gpd on Gaussian data: a benign, well-conditioned
  # dataset that exercises the pipeline without the singular-MLE flakiness
  # that bites unseeded chaotic exceedances.
  skip_if_not_installed("evd")
  set.seed(1L)
  x <- rnorm(2000, mean = 0, sd = 1)
  u <- quantile(x, 0.9)
  dec <- decluster(x, threshold = u, run_length = 1L)

  expect_gt(nrow(dec), 10L)
  expect_true(all(dec$value > u))

  fit <- fit_gpd(dec$value, threshold = u)
  expect_s3_class(fit, "chaotic_model")
})
