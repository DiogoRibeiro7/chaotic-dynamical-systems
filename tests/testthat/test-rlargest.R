test_that("block_r_largest returns a matrix sorted decreasingly per row", {
  set.seed(1L)
  x <- runif(1000)
  rl <- block_r_largest(x, block_size = 50, r = 3)

  expect_true(is.matrix(rl))
  expect_equal(dim(rl), c(20L, 3L))
  # Each row should be in decreasing order.
  expect_true(all(rl[, 1] >= rl[, 2]))
  expect_true(all(rl[, 2] >= rl[, 3]))
  # The leftmost column should equal vanilla block maxima.
  expect_equal(rl[, 1], block_maxima(x, 50), tolerance = 1e-12)
})

test_that("block_r_largest rejects invalid arguments", {
  x <- runif(100)
  expect_error(block_r_largest(x, block_size = 10, r = 11))   # r > block_size
  expect_error(block_r_largest(x, block_size = 200, r = 1))  # no full block
  expect_error(block_r_largest(x, block_size = 0, r = 1))
})

test_that("fit_gev_rlargest returns a chaotic_model with the expected structure", {
  skip_if_not_installed("evd")
  set.seed(2L)
  x  <- evd::rgev(2000, loc = 0, scale = 1, shape = 0.1)
  rl <- block_r_largest(x, block_size = 50, r = 3)
  fit <- fit_gev_rlargest(rl)

  expect_s3_class(fit, "chaotic_model")
  expect_equal(attr(fit, "chaotic_model"), "gev_rlargest")
  expect_equal(fit$r, 3L)
  expect_named(fit$estimate, c("loc", "scale", "shape"))
  expect_true(all(is.finite(fit$estimate)))
  expect_true(is.finite(fit$loglik))
})

test_that("fit_gev_rlargest agrees with fit_gev on block maxima within sampling error", {
  skip_if_not_installed("evd")
  set.seed(3L)
  # Long series so the MLE is in the asymptotic regime. We do NOT compare
  # against the underlying GEV(0, 1, 0.1) parameters because the r-largest
  # method estimates the *block-maximum-normalised* parameters (mu_n,
  # sigma_n, xi_n) rather than the parent's, exactly like a plain GEV fit
  # on block maxima does. The two fits should therefore agree.
  x  <- evd::rgev(20000, loc = 0, scale = 1, shape = 0.1)
  rl <- block_r_largest(x, block_size = 100, r = 5)
  fit <- fit_gev_rlargest(rl)

  ref <- fit_gev(rl[, 1L])
  ref_par <- as.numeric(.extract_param_vector(ref))
  rl_par  <- as.numeric(fit$estimate)

  # The r-largest fit pulls strength from r columns and should be within a
  # few standard errors of the column-1 GEV fit.
  for (i in seq_along(rl_par)) {
    expect_lt(abs(rl_par[i] - ref_par[i]),
              3 * fit$std.err[i] + 1e-6)
  }
})

test_that("fit_gev_rlargest with r = 1 agrees with fit_gev on block maxima", {
  skip_if_not_installed("evd")
  set.seed(4L)
  x  <- evd::rgev(1000, loc = 0, scale = 1, shape = 0.1)
  bm <- block_maxima(x, block_size = 50)
  rl <- block_r_largest(x, block_size = 50, r = 1)

  fit_rl <- fit_gev_rlargest(rl)
  fit_bm <- fit_gev(bm)
  rl_par <- as.numeric(fit_rl$estimate)
  bm_par <- as.numeric(.extract_param_vector(fit_bm))

  # Parameter point estimates from two different optimisers; agreement to 1e-3
  # is comfortably enough to confirm the likelihoods coincide for r = 1.
  expect_equal(rl_par, bm_par, tolerance = 1e-3)
})

test_that("tidy() and glance() work on r-largest fits", {
  skip_if_not_installed("evd")
  set.seed(5L)
  x  <- evd::rgev(1000, 0, 1, 0.1)
  rl <- block_r_largest(x, block_size = 50, r = 2)
  fit <- fit_gev_rlargest(rl)

  td <- generics::tidy(fit)
  expect_equal(nrow(td), 3L)
  expect_setequal(td$term, c("location", "scale", "shape"))

  gl <- generics::glance(fit)
  expect_equal(nrow(gl), 1L)
  expect_equal(gl$model, "gev_rlargest")
  expect_equal(gl$nobs, 40L)   # 20 blocks * r = 2
})
