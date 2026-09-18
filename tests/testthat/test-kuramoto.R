test_that("Kuramoto simulator returns the documented structure", {
  sim <- simulate_kuramoto(
    t_max = 2,
    dt = 0.05,
    n = 8L,
    coupling = 1.2
  )

  expect_s3_class(sim, "kuramoto_simulation")
  expect_named(
    sim,
    c("t", "theta", "order_parameter", "omega", "coupling", "dt")
  )
  expect_equal(dim(sim$theta), c(41L, 8L))
  expect_equal(length(sim$t), 41L)
  expect_equal(length(sim$order_parameter), 41L)
  expect_true(all(sim$order_parameter >= 0))
  expect_true(all(sim$order_parameter <= 1 + 1e-14))
})

test_that("zero Kuramoto coupling reproduces linear phase drift", {
  omega <- c(-0.3, 0.2, 0.7)
  theta0 <- c(0.1, 0.5, 1.2)
  sim <- simulate_kuramoto(
    t_max = 1,
    dt = 0.05,
    n = 3L,
    coupling = 0,
    omega = omega,
    theta0 = theta0
  )

  expected <- outer(sim$t, omega, "*")
  expected <- sweep(expected, 2L, theta0, "+")

  expect_equal(sim$theta, expected, tolerance = 1e-12)
})

test_that("Kuramoto order parameter has the expected limiting values", {
  expect_equal(
    kuramoto_order_parameter(rep(0, 8L)),
    1,
    tolerance = 1e-14
  )
  expect_equal(
    kuramoto_order_parameter(c(0, pi)),
    0,
    tolerance = 1e-14
  )
})

test_that("attractive Kuramoto coupling increases synchronization", {
  omega <- seq(-0.25, 0.25, length.out = 12L)
  theta0 <- 2 * pi * (seq_len(12L) - 1L) / 12L

  uncoupled <- simulate_kuramoto(
    t_max = 12,
    dt = 0.05,
    n = 12L,
    coupling = 0,
    omega = omega,
    theta0 = theta0
  )
  coupled <- simulate_kuramoto(
    t_max = 12,
    dt = 0.05,
    n = 12L,
    coupling = 2,
    omega = omega,
    theta0 = theta0
  )

  expect_gt(
    tail(coupled$order_parameter, 1L),
    tail(uncoupled$order_parameter, 1L)
  )
})

test_that("C++ Kuramoto simulator matches the R reference", {
  omega <- c(-0.4, -0.1, 0.2, 0.5)
  theta0 <- c(0.1, 1.0, 2.0, 2.8)

  r_sim <- simulate_kuramoto(
    t_max = 3,
    dt = 0.02,
    n = 4L,
    coupling = 1.3,
    omega = omega,
    theta0 = theta0,
    transient = 0.4
  )
  cpp_sim <- simulate_kuramoto_cpp(
    t_max = 3,
    dt = 0.02,
    n = 4L,
    coupling = 1.3,
    omega = omega,
    theta0 = theta0,
    transient = 0.4
  )

  expect_equal(cpp_sim$t, r_sim$t)
  expect_equal(cpp_sim$theta, r_sim$theta, tolerance = 1e-10)
  expect_equal(
    cpp_sim$order_parameter,
    r_sim$order_parameter,
    tolerance = 1e-10
  )
})

test_that("Kuramoto simulator validates dimensions and time settings", {
  expect_error(simulate_kuramoto(t_max = 0))
  expect_error(simulate_kuramoto(dt = 0))
  expect_error(simulate_kuramoto(n = 1L))
  expect_error(simulate_kuramoto(n = 4L, omega = c(0.1, 0.2)))
  expect_error(simulate_kuramoto(n = 4L, theta0 = c(0.1, 0.2)))
  expect_error(simulate_kuramoto(t_max = 0.01, dt = 1))
})
