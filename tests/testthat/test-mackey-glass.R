test_that("simulate_mackey_glass returns the documented shape", {
  traj <- simulate_mackey_glass(t_max = 20, dt = 0.1)
  expect_s3_class(traj, "data.frame")
  expect_named(traj, c("t", "x"))
  expect_equal(nrow(traj), 201L)
  expect_equal(traj$t[1], 0)
  expect_equal(traj$t[nrow(traj)], 20, tolerance = 1e-9)
  expect_true(all(is.finite(traj$x)))
})

test_that("simulate_mackey_glass settles onto a bounded attractor", {
  # Default chaotic parameters: orbit stays in roughly [0.1, 1.5].
  traj <- simulate_mackey_glass(t_max = 200, dt = 0.1, transient = 100)
  expect_true(min(traj$x) > 0)
  expect_lt(max(traj$x), 1.5)
  expect_gt(stats::sd(traj$x), 0.1)   # non-trivial variability
})

test_that("simulate_mackey_glass is deterministic for given parameters", {
  a <- simulate_mackey_glass(t_max = 10, dt = 0.1)
  b <- simulate_mackey_glass(t_max = 10, dt = 0.1)
  expect_identical(a, b)
})

test_that("simulate_mackey_glass transient is removed from the output", {
  a <- simulate_mackey_glass(t_max = 10, dt = 0.1, transient = 5)
  expect_equal(a$t[1], 0)
  expect_equal(nrow(a), 101L)
  # The starting value after transient differs from the constant history x0.
  expect_false(isTRUE(all.equal(a$x[1], 1.2)))
})

test_that("simulate_mackey_glass_cpp matches the R reference", {
  skip_if_not_installed("Rcpp")
  r_   <- simulate_mackey_glass(t_max = 30, dt = 0.1, transient = 10)
  cpp_ <- simulate_mackey_glass_cpp(t_max = 30, dt = 0.1, transient = 10)
  expect_equal(cpp_$t, r_$t, tolerance = 1e-12)
  expect_equal(cpp_$x, r_$x, tolerance = 1e-10)
})

test_that("simulate_mackey_glass rejects invalid arguments", {
  expect_error(simulate_mackey_glass(t_max = -1))
  expect_error(simulate_mackey_glass(dt = 0))
  expect_error(simulate_mackey_glass(tau = 0))
  expect_error(simulate_mackey_glass(transient = -1))
})
