test_that("noise_sd = 0 leaves all discrete simulators deterministic", {
  args1 <- list(simulate_logistic_map, list(n = 50, r = 3.8, x0 = 0.2))
  args2 <- list(simulate_henon_map,    list(n = 50))
  args3 <- list(simulate_tent_map,     list(n = 50))
  args4 <- list(simulate_lozi_map,     list(n = 50))
  args5 <- list(simulate_cat_map,      list(n = 50))
  args6 <- list(simulate_standard_map, list(n = 50))
  args7 <- list(simulate_ikeda_map,    list(n = 50))

  for (a in list(args1, args2, args3, args4, args5, args6, args7)) {
    fn   <- a[[1L]]
    args <- a[[2L]]
    a1 <- do.call(fn, c(args, list(noise_sd = 0)))
    a2 <- do.call(fn, c(args, list(noise_sd = 0)))
    expect_identical(a1, a2)
  }
})

test_that("noise_sd > 0 produces different orbits under different RNG seeds", {
  set.seed(1L); a <- simulate_logistic_map(100, r = 3.8, x0 = 0.2, noise_sd = 0.01)
  set.seed(2L); b <- simulate_logistic_map(100, r = 3.8, x0 = 0.2, noise_sd = 0.01)
  expect_false(isTRUE(all.equal(a, b)))
})

test_that("noise_sd > 0 produces identical orbits under the same RNG seed", {
  set.seed(1L); a <- simulate_logistic_map(100, r = 3.8, x0 = 0.2, noise_sd = 0.01)
  set.seed(1L); b <- simulate_logistic_map(100, r = 3.8, x0 = 0.2, noise_sd = 0.01)
  expect_identical(a, b)
})

test_that("noisy R and C++ simulators agree under the same RNG seed", {
  skip_if_not_installed("Rcpp")
  for (sd in c(0, 0.005, 0.02)) {
    set.seed(42L); r_  <- simulate_logistic_map    (200, r = 3.8, x0 = 0.2, noise_sd = sd)
    set.seed(42L); cpp_ <- simulate_logistic_map_cpp(200, r = 3.8, x0 = 0.2, noise_sd = sd)
    expect_equal(cpp_, r_, tolerance = 1e-12)

    set.seed(42L); r_  <- simulate_henon_map    (200, noise_sd = sd)
    set.seed(42L); cpp_ <- simulate_henon_map_cpp(200, noise_sd = sd)
    expect_equal(cpp_, r_, tolerance = 1e-12)

    set.seed(42L); r_  <- simulate_standard_map    (200, noise_sd = sd)
    set.seed(42L); cpp_ <- simulate_standard_map_cpp(200, noise_sd = sd)
    expect_equal(cpp_, r_, tolerance = 1e-12)
  }
})

test_that("noise_sd is rejected when negative or non-finite", {
  expect_error(simulate_logistic_map(10, 3.8, 0.2, noise_sd = -0.1))
  expect_error(simulate_logistic_map(10, 3.8, 0.2, noise_sd = NA_real_))
  expect_error(simulate_henon_map(10, noise_sd = -1))
})

test_that("noisy cat map stays on the unit torus", {
  set.seed(1L)
  o <- simulate_cat_map(200, noise_sd = 0.3)
  expect_true(all(o$x >= 0 & o$x < 1))
  expect_true(all(o$y >= 0 & o$y < 1))
})
