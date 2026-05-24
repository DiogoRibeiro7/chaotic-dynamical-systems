test_that("lyapunov_spectrum_logistic recovers log(2) at r = 4", {
  # Textbook value: for the fully chaotic logistic map at r = 4 the
  # Lyapunov exponent equals log(2).
  lam <- lyapunov_spectrum_logistic(n_iter = 8000L, transient = 1000L,
                                     r = 4, x0 = 0.2)
  expect_length(lam, 1L)
  expect_equal(lam, log(2), tolerance = 0.02)
})

test_that("lyapunov_spectrum_henon recovers the canonical Henon spectrum", {
  lam <- lyapunov_spectrum_henon(n_iter = 8000L, transient = 1000L)
  expect_length(lam, 2L)
  expect_equal(lam[1L],  0.418, tolerance = 0.03)
  expect_equal(lam[2L], -1.622, tolerance = 0.03)
  # Sum of the spectrum equals log|det J| = log(b) for the Henon map.
  expect_equal(sum(lam), log(0.3), tolerance = 0.01)
})

test_that("lyapunov_spectrum_lozi recovers the canonical Lozi spectrum", {
  lam <- lyapunov_spectrum_lozi(n_iter = 8000L, transient = 1000L)
  expect_length(lam, 2L)
  expect_equal(lam[1L],  0.47, tolerance = 0.05)
  expect_equal(lam[2L], -1.16, tolerance = 0.05)
  expect_equal(sum(lam), log(0.5), tolerance = 0.01)
})

test_that("lyapunov_spectrum is deterministic for given initial conditions", {
  a <- lyapunov_spectrum_henon(n_iter = 1000L, transient = 200L, x0 = 0.1, y0 = 0.1)
  b <- lyapunov_spectrum_henon(n_iter = 1000L, transient = 200L, x0 = 0.1, y0 = 0.1)
  expect_identical(a, b)
})

test_that("lyapunov_spectrum rejects mis-shaped Jacobians", {
  bad_jac <- function(s) matrix(0, 3, 3)
  bad_map <- function(s) c(s[1L] + 1, s[2L] - 1)
  expect_error(lyapunov_spectrum(bad_map, bad_jac, c(0, 0), n_iter = 5L),
               "2 x 2 matrix")
})

test_that("lyapunov_spectrum_lorenz recovers the canonical Lorenz spectrum", {
  # Standard textbook values for (sigma = 10, rho = 28, beta = 8/3):
  #   (~0.906, 0, ~-14.572). Sum equals -sigma - 1 - beta = -13.6667 by
  # the trace formula; that sum invariant is much tighter than the
  # individual exponents.
  sp <- lyapunov_spectrum_lorenz(t_max = 200, dt = 0.01, transient = 30)
  expect_length(sp, 3L)
  expect_equal(sp[1L],  0.906,    tolerance = 0.05)
  expect_equal(sp[2L],  0,        tolerance = 0.05)
  expect_equal(sp[3L], -14.572,   tolerance = 0.10)
  expect_equal(sum(sp), -10 - 1 - 8 / 3, tolerance = 1e-3)
})

test_that("lyapunov_spectrum_rossler recovers the canonical Rossler spectrum", {
  sp <- lyapunov_spectrum_rossler(t_max = 500, dt = 0.05, transient = 100)
  expect_length(sp, 3L)
  expect_equal(sp[1L],  0.0714,  tolerance = 0.05)
  expect_equal(sp[2L],  0,       tolerance = 0.05)
  expect_equal(sp[3L], -5.392,   tolerance = 0.20)
})

test_that("lyapunov_spectrum_continuous is deterministic for fixed inputs", {
  a <- lyapunov_spectrum_lorenz(t_max = 50, dt = 0.01, transient = 10)
  b <- lyapunov_spectrum_lorenz(t_max = 50, dt = 0.01, transient = 10)
  expect_identical(a, b)
})

test_that("lyapunov_spectrum_continuous rejects bad arguments", {
  ok_d <- function(t, x) c(-x[1L])
  ok_j <- function(t, x) matrix(-1, 1, 1)
  expect_error(lyapunov_spectrum_continuous("nope", ok_j, 1))
  expect_error(lyapunov_spectrum_continuous(ok_d, "nope", 1))
  expect_error(lyapunov_spectrum_continuous(ok_d, ok_j, 1, t_max = -1))
  expect_error(lyapunov_spectrum_continuous(ok_d, ok_j, 1, dt = 0))
  expect_error(lyapunov_spectrum_continuous(ok_d, ok_j, 1, qr_interval = 0))
})

test_that("lyapunov_spectrum rejects invalid arguments", {
  expect_error(lyapunov_spectrum("not a fn",  function(s) matrix(0, 1, 1), 0))
  expect_error(lyapunov_spectrum(function(s) s, "not a fn", 0))
  expect_error(lyapunov_spectrum_henon(n_iter = 0L))
  expect_error(lyapunov_spectrum_henon(transient = -1L))
})
