test_that("fit_ppp returns a chaotic_model with PPL-shaped parameters", {
  skip_if_not_installed("evd")
  set.seed(1L)
  x <- evd::rgev(2000, loc = 0, scale = 1, shape = 0.1)
  u <- quantile(x, 0.9)
  fit <- fit_ppp(x, threshold = u, n_per_block = 50)

  expect_s3_class(fit, "chaotic_model")
  expect_equal(attr(fit, "chaotic_model"), "ppp")
  expect_equal(unname(attr(fit, "chaotic_threshold")),
               unname(as.numeric(u)),
               tolerance = 1e-12)
  expect_true(all(c("loc", "scale", "shape") %in% names(fit$estimate)))
  expect_true(all(is.finite(fit$estimate)))
})

test_that("tidy() and glance() work on PPL fits", {
  skip_if_not_installed("evd")
  set.seed(2L)
  x <- evd::rgev(2000, 0, 1, 0.1)
  u <- quantile(x, 0.9)
  fit <- fit_ppp(x, threshold = u, n_per_block = 50)

  td <- generics::tidy(fit)
  expect_equal(nrow(td), 3L)
  expect_setequal(td$term, c("location", "scale", "shape"))
  expect_true(all(is.finite(td$estimate)))

  gl <- generics::glance(fit)
  expect_equal(nrow(gl), 1L)
  expect_equal(gl$model, "ppp")
})

test_that("fit_ppp validates its arguments", {
  skip_if_not_installed("evd")
  expect_error(fit_ppp(1:5, threshold = 0))             # too few obs
  expect_error(fit_ppp(rnorm(100), threshold = "abc"))
  expect_error(fit_ppp(rnorm(100), threshold = 0, n_per_block = 0))
})
