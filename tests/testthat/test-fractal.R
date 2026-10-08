test_that("estimate_correlation_dimension returns auditable diagnostics", {
  x <- seq_len(20)

  res <- estimate_correlation_dimension(
    x,
    m = 1L,
    tau = 1L,
    r_vals = c(1.5, 2.5, 3.5, 4.5, 5.5),
    scaling_range = c(1.5, 5.5),
    min_scaling_points = 5L
  )

  expect_true(is.list(res))
  expect_named(
    res,
    c(
      "r", "C", "dimension", "scaling_indices", "scaling_range",
      "r_squared", "slope_se", "local_slope_sd", "n_pairs", "theiler"
    )
  )
  expect_equal(res$C, c(
    19 / 190,
    37 / 190,
    54 / 190,
    70 / 190,
    85 / 190
  ))
  expect_equal(res$scaling_indices, 1:5)
  expect_equal(res$scaling_range, c(1.5, 5.5))
  expect_equal(res$n_pairs, choose(20, 2))
  expect_equal(res$theiler, 0L)
  expect_true(is.finite(res$dimension))
  expect_true(res$r_squared > 0.95)
})

test_that("Theiler window excludes temporally adjacent pairs", {
  x <- seq_len(20)

  res0 <- estimate_correlation_dimension(
    x,
    m = 1L,
    r_vals = c(2.5, 3.5, 4.5, 5.5, 6.5),
    scaling_range = c(2.5, 6.5),
    min_scaling_points = 5L,
    theiler = 0L
  )
  res2 <- estimate_correlation_dimension(
    x,
    m = 1L,
    r_vals = c(2.5, 3.5, 4.5, 5.5, 6.5),
    scaling_range = c(2.5, 6.5),
    min_scaling_points = 5L,
    theiler = 2L
  )

  expect_equal(res0$n_pairs, choose(20, 2))
  expect_equal(res2$n_pairs, choose(18, 2))
  expect_lt(res2$n_pairs, res0$n_pairs)
  expect_equal(res2$theiler, 2L)
})

test_that("scaling-region validation rejects unusable choices", {
  x <- seq_len(20)

  expect_error(
    estimate_correlation_dimension(
      x,
      m = 1L,
      r_vals = c(1.5, 2.5, 3.5),
      scaling_range = c(1.5, 3.5),
      min_scaling_points = 4L
    ),
    "min_scaling_points"
  )

  expect_error(
    estimate_correlation_dimension(
      x,
      m = 1L,
      r_vals = c(1.5, 2.5, 3.5, 4.5, 5.5),
      scaling_range = c(9, 10),
      min_scaling_points = 3L
    ),
    "Scaling region"
  )

  expect_error(
    estimate_correlation_dimension(
      x,
      m = 1L,
      r_vals = c(1.5, 2.5, 3.5, 4.5, 5.5),
      theiler = 100L,
      min_scaling_points = 3L
    ),
    "excludes every"
  )
})
