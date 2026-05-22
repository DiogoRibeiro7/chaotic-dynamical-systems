test_that("simulate_standard_map returns the documented shape", {
  set.seed(1L)
  orbit <- simulate_standard_map(500, K = 1.2)
  expect_s3_class(orbit, "data.frame")
  expect_named(orbit, c("p", "theta"))
  expect_equal(nrow(orbit), 500L)
  expect_true(all(orbit$p     >= 0 & orbit$p     < 2 * pi))
  expect_true(all(orbit$theta >= 0 & orbit$theta < 2 * pi))
})

test_that("simulate_standard_map is determined by initial conditions", {
  a <- simulate_standard_map(50, K = 1.2, p0 = 1, theta0 = 1)
  b <- simulate_standard_map(50, K = 1.2, p0 = 1, theta0 = 1)
  expect_identical(a, b)
})

test_that("simulate_ikeda_map returns the documented shape and stays bounded", {
  orbit <- simulate_ikeda_map(2000, u = 0.9)
  expect_s3_class(orbit, "data.frame")
  expect_named(orbit, c("x", "y"))
  expect_equal(nrow(orbit), 2000L)
  expect_true(all(is.finite(orbit$x)))
  expect_true(all(is.finite(orbit$y)))
  # The Ikeda attractor for u = 0.9 has compact support.
  expect_lt(max(abs(orbit$x)), 5)
  expect_lt(max(abs(orbit$y)), 5)
})

test_that("new maps reject invalid arguments", {
  expect_error(simulate_standard_map(0L))
  expect_error(simulate_standard_map(10, K = "abc"))
  expect_error(simulate_ikeda_map(0L))
  expect_error(simulate_ikeda_map(10, u = "abc"))
})

test_that("simulate_standard_map_cpp matches the R reference", {
  skip_if_not_installed("Rcpp")
  r_   <- simulate_standard_map(300, K = 1.2, p0 = 0.5, theta0 = 0.5)
  cpp_ <- simulate_standard_map_cpp(300, K = 1.2, p0 = 0.5, theta0 = 0.5)
  expect_equal(cpp_, r_, tolerance = 1e-12)
})

test_that("simulate_ikeda_map_cpp matches the R reference", {
  skip_if_not_installed("Rcpp")
  r_   <- simulate_ikeda_map(300, u = 0.9, x0 = 0.1, y0 = 0.1)
  cpp_ <- simulate_ikeda_map_cpp(300, u = 0.9, x0 = 0.1, y0 = 0.1)
  expect_equal(cpp_, r_, tolerance = 1e-12)
})
