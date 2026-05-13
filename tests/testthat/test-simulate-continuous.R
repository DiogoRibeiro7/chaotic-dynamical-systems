test_that("simulate_lorenz returns expected shape and stays on the attractor", {
  traj <- simulate_lorenz(t_max = 5, dt = 0.01)
  expect_s3_class(traj, "data.frame")
  expect_named(traj, c("t", "x", "y", "z"))
  expect_equal(nrow(traj), 501)
  expect_equal(traj$t[1], 0)
  expect_equal(traj$t[nrow(traj)], 5, tolerance = 1e-8)
  expect_true(all(is.finite(unlist(traj))))
  expect_true(max(abs(traj$x)) < 60)
  expect_true(max(abs(traj$z)) < 60)
})

test_that("simulate_lorenz transient is removed from the output", {
  traj <- simulate_lorenz(t_max = 5, dt = 0.01, transient = 3)
  expect_equal(traj$t[1], 0)
  expect_equal(nrow(traj), 501)
})

test_that("simulate_lorenz is deterministic for given parameters", {
  a <- simulate_lorenz(t_max = 2, dt = 0.01)
  b <- simulate_lorenz(t_max = 2, dt = 0.01)
  expect_identical(a, b)
})

test_that("simulate_lorenz shows sensitive dependence on initial conditions", {
  # A 1e-8 perturbation on x0 saturates at attractor diameter (O(10)) by t=40
  # on this trajectory under our fixed-step RK4 (effective amplification rate
  # ~0.5 along the orbit; textbook averaged exponent is ~0.9).
  a <- simulate_lorenz(t_max = 40, dt = 0.01, x0 = 1)
  b <- simulate_lorenz(t_max = 40, dt = 0.01, x0 = 1 + 1e-8)
  expect_lt(abs(a$x[1] - b$x[1]), 1e-7)
  expect_gt(max(abs(a$x - b$x)), 1)
})

test_that("simulate_rossler returns expected shape and is bounded", {
  traj <- simulate_rossler(t_max = 20, dt = 0.05, transient = 10)
  expect_s3_class(traj, "data.frame")
  expect_named(traj, c("t", "x", "y", "z"))
  expect_equal(nrow(traj), 401)
  expect_true(all(is.finite(unlist(traj))))
  expect_true(max(abs(traj$x)) < 30)
  expect_true(max(abs(traj$y)) < 30)
})

test_that("simulate_duffing returns expected shape and is bounded", {
  traj <- simulate_duffing(t_max = 30, dt = 0.05, transient = 20)
  expect_s3_class(traj, "data.frame")
  expect_named(traj, c("t", "x", "v"))
  expect_equal(nrow(traj), 601)
  expect_true(all(is.finite(unlist(traj))))
  expect_true(max(abs(traj$x)) < 5)
})

test_that("simulate_duffing forcing changes the orbit", {
  forced   <- simulate_duffing(t_max = 30, dt = 0.05, gamma = 0.3)
  unforced <- simulate_duffing(t_max = 30, dt = 0.05, gamma = 0)
  expect_gt(max(abs(forced$x - unforced$x)), 0.1)
})

test_that("continuous simulators reject invalid arguments", {
  expect_error(simulate_lorenz(t_max = -1))
  expect_error(simulate_lorenz(dt = 0))
  expect_error(simulate_rossler(transient = -1))
  expect_error(simulate_duffing(t_max = "x"))
})
