test_that("coupled map lattice returns the documented matrix shape", {
  x0 <- c(0.1, 0.2, 0.3, 0.4, 0.5)
  orbit <- simulate_coupled_map_lattice(
    n = 40L,
    lattice_size = 5L,
    r = 4,
    coupling = 0.2,
    x0 = x0
  )

  expect_type(orbit, "double")
  expect_equal(dim(orbit), c(40L, 5L))
  expect_equal(orbit[1L, ], x0)
  expect_true(all(is.finite(orbit)))
})

test_that("zero coupling reproduces independent logistic maps", {
  x0 <- c(0.11, 0.23, 0.37, 0.49)
  orbit <- simulate_coupled_map_lattice(
    n = 25L,
    lattice_size = 4L,
    r = 3.8,
    coupling = 0,
    x0 = x0
  )

  expected <- vapply(
    x0,
    function(initial) {
      simulate_logistic_map(25L, r = 3.8, x0 = initial)
    },
    numeric(25L)
  )

  expect_equal(orbit, expected, tolerance = 1e-14)
})

test_that("periodic coupling uses both boundary neighbours", {
  x0 <- c(0.1, 0.2, 0.3, 0.4)
  r <- 4
  coupling <- 0.3
  mapped <- r * x0 * (1 - x0)

  orbit <- simulate_coupled_map_lattice(
    n = 2L,
    lattice_size = 4L,
    r = r,
    coupling = coupling,
    x0 = x0
  )

  expected_first <- (1 - coupling) * mapped[1L] +
    0.5 * coupling * (mapped[4L] + mapped[2L])

  expect_equal(orbit[2L, 1L], expected_first, tolerance = 1e-14)
})

test_that("C++ coupled map lattice matches the R reference", {
  x0 <- c(0.11, 0.23, 0.37, 0.49, 0.61)

  set.seed(42L)
  r_orbit <- simulate_coupled_map_lattice(
    n = 30L,
    lattice_size = 5L,
    r = 3.9,
    coupling = 0.15,
    x0 = x0,
    noise_sd = 1e-4
  )

  set.seed(42L)
  cpp_orbit <- simulate_coupled_map_lattice_cpp(
    n = 30L,
    lattice_size = 5L,
    r = 3.9,
    coupling = 0.15,
    x0 = x0,
    noise_sd = 1e-4
  )

  expect_equal(cpp_orbit, r_orbit, tolerance = 1e-14)
})

test_that("coupled map lattice validates arguments", {
  expect_error(simulate_coupled_map_lattice(0L))
  expect_error(simulate_coupled_map_lattice(10L, lattice_size = 2L))
  expect_error(simulate_coupled_map_lattice(10L, r = 4.1))
  expect_error(simulate_coupled_map_lattice(10L, coupling = -0.1))
  expect_error(simulate_coupled_map_lattice(
    10L,
    lattice_size = 4L,
    x0 = c(0.1, 0.2)
  ))
  expect_error(simulate_coupled_map_lattice(
    10L,
    lattice_size = 4L,
    x0 = c(0.1, 0.2, 0.3, 1.2)
  ))
})
