
test_that('exceedances extracts values above threshold', {
  x <- c(-1, 0.5, 1.2, 2, 0.3)
  exc <- exceedances(x, 1)
  expect_equal(exc, c(1.2, 2))
})

test_that('block_maxima splits series correctly', {
  x <- 1:10
  bm <- block_maxima(x, 2)
  expect_equal(bm, c(2,4,6,8,10))
})

test_that("fit_gev returns unified chaotic_model object", {
  skip_if_not_installed("evd")
  x <- rnorm(500)
  bm <- block_maxima(x, 25)
  fit <- fit_gev(bm)

  expect_s3_class(fit, "chaotic_model")
  expect_equal(attr(fit, "chaotic_model"), "gev")
  expect_true(!is.null(attr(fit, "chaotic_method")))
  expect_s3_class(summary(fit), "summary.chaotic_model")
})

test_that("fit_gpd returns unified chaotic_model object", {
  skip_if_not_installed("evd")
  x <- rnorm(500)
  u <- quantile(x, 0.95)
  fit <- fit_gpd(x, u)

  expect_s3_class(fit, "chaotic_model")
  expect_equal(attr(fit, "chaotic_model"), "gpd")
  expect_equal(attr(fit, "chaotic_threshold"), as.numeric(u))
  expect_s3_class(summary(fit), "summary.chaotic_model")
})
