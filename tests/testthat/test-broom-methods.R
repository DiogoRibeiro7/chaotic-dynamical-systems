test_that("tidy.chaotic_model returns one row per GEV parameter", {
  skip_if_not_installed("evd")
  set.seed(1L)
  fit <- fit_gev(evd::rgev(400, loc = 0, scale = 1, shape = 0.1))

  td <- generics::tidy(fit)
  expect_s3_class(td, "data.frame")
  expect_named(td, c("term", "estimate", "std.error"))
  expect_equal(nrow(td), 3L)
  expect_setequal(td$term, c("location", "scale", "shape"))
  expect_true(all(is.finite(td$estimate)))
  expect_true(all(td$std.error >= 0))
})

test_that("tidy.chaotic_model returns one row per GPD parameter", {
  skip_if_not_installed("evd")
  set.seed(2L)
  y <- (1 / 0.1) * ((1 - runif(500))^(-0.1) - 1)
  fit <- fit_gpd(y, threshold = 0)

  td <- generics::tidy(fit)
  expect_equal(nrow(td), 2L)
  expect_setequal(td$term, c("scale", "shape"))
})

test_that("tidy.chaotic_model adds Wald CIs when conf.int = TRUE", {
  skip_if_not_installed("evd")
  set.seed(3L)
  fit <- fit_gev(evd::rgev(400, 0, 1, 0.1))

  td <- generics::tidy(fit, conf.int = TRUE, conf.level = 0.9)
  expect_named(td, c("term", "estimate", "std.error", "conf.low", "conf.high"))
  expect_true(all(td$conf.low <= td$estimate))
  expect_true(all(td$conf.high >= td$estimate))
})

test_that("glance.chaotic_model returns a one-row summary with the documented columns", {
  skip_if_not_installed("evd")
  set.seed(4L)
  fit <- fit_gev(evd::rgev(400, 0, 1, 0.1))

  gl <- generics::glance(fit)
  expect_s3_class(gl, "data.frame")
  expect_equal(nrow(gl), 1L)
  expect_named(gl, c("model", "method", "threshold", "nobs", "logLik", "AIC", "BIC"))
  expect_equal(gl$model, "gev")
  expect_equal(gl$nobs, 400L)
  expect_true(is.finite(gl$logLik))
  expect_true(is.finite(gl$AIC))
  expect_true(is.finite(gl$BIC))
})

test_that("augment.chaotic_model returns CDF and survival columns for GEV", {
  skip_if_not_installed("evd")
  set.seed(5L)
  fit <- fit_gev(evd::rgev(300, 0, 1, 0.1))

  au <- generics::augment(fit)
  expect_s3_class(au, "data.frame")
  expect_named(au, c("index", "value", "cdf", "survival"))
  expect_equal(nrow(au), 300L)
  expect_true(all(au$cdf >= 0 & au$cdf <= 1))
  expect_equal(au$cdf + au$survival, rep(1, nrow(au)), tolerance = 1e-12)
})

test_that("augment.chaotic_model returns CDF and survival columns for GPD", {
  skip_if_not_installed("evd")
  set.seed(6L)
  y <- (1 / 0.1) * ((1 - runif(400))^(-0.1) - 1)
  fit <- fit_gpd(y, threshold = 0)

  au <- generics::augment(fit)
  expect_named(au, c("index", "value", "cdf", "survival"))
  expect_true(all(au$cdf >= 0 & au$cdf <= 1))
})
