test_that("ensemble_simulate returns long format for 1D simulators", {
  ens <- ensemble_simulate(
    simulate_logistic_map(50, r = 3.8, x0 = 0.2),
    n_replicates = 3
  )
  expect_s3_class(ens, "data.frame")
  expect_named(ens, c("replicate", "iter", "x"))
  expect_equal(nrow(ens), 150L)
  expect_setequal(ens$replicate, 1:3)
  expect_equal(range(ens$iter), c(1L, 50L))
})

test_that("ensemble_simulate returns long format for 2D simulators", {
  ens <- ensemble_simulate(
    simulate_henon_map(40, x0 = 0, y0 = 0),
    n_replicates = 4
  )
  expect_named(ens, c("replicate", "iter", "x", "y"))
  expect_equal(nrow(ens), 160L)
  expect_setequal(ens$replicate, 1:4)
})

test_that("ensemble_simulate preserves the t column from continuous simulators", {
  ens <- ensemble_simulate(
    simulate_lorenz(t_max = 1, dt = 0.05),
    n_replicates = 2
  )
  expect_true(all(c("replicate", "iter", "t", "x", "y", "z") %in% names(ens)))
  # Each replicate of simulate_lorenz(t_max = 1, dt = 0.05) has 21 rows.
  expect_equal(nrow(ens), 2L * 21L)
})

test_that("random expressions inside the captured call are re-drawn each replicate", {
  set.seed(1L)
  ens <- ensemble_simulate(
    simulate_logistic_map(20, r = 3.8, x0 = runif(1, 0.1, 0.9)),
    n_replicates = 5
  )
  # If the random expression were not re-drawn, every replicate would have
  # the same initial value at iter == 1.
  inits <- ens$x[ens$iter == 1L]
  expect_length(inits, 5L)
  expect_gt(length(unique(inits)), 1L)
})

test_that("seed yields a reproducible ensemble", {
  a <- ensemble_simulate(
    simulate_logistic_map(20, 3.8, runif(1, 0.1, 0.9)),
    n_replicates = 3, seed = 42L
  )
  b <- ensemble_simulate(
    simulate_logistic_map(20, 3.8, runif(1, 0.1, 0.9)),
    n_replicates = 3, seed = 42L
  )
  expect_identical(a, b)
})

test_that("ensemble_simulate rejects invalid arguments", {
  expect_error(ensemble_simulate(simulate_logistic_map(10, 3.8, 0.2),
                                  n_replicates = 0))
  expect_error(ensemble_simulate(simulate_logistic_map(10, 3.8, 0.2),
                                  n_replicates = -1))
  expect_error(ensemble_simulate(simulate_logistic_map(10, 3.8, 0.2),
                                  n_replicates = 3, seed = "abc"))
})

test_that("ensemble_simulate errors when replicates return mixed shapes", {
  # Force one replicate to be a vector and another a data frame by
  # constructing a non-simulator call that varies output per draw.
  fn <- function() if (runif(1) < 0.5) 1:5 else data.frame(x = 1:5, y = 1:5)
  set.seed(1L)
  expect_error(
    ensemble_simulate(fn(), n_replicates = 10),
    "same shape"
  )
})
