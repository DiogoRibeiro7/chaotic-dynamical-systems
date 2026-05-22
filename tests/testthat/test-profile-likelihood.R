test_that("profile_likelihood returns the documented shape for GEV", {
  skip_if_not_installed("evd")
  set.seed(1L)
  x   <- evd::rgev(400, loc = 0, scale = 1, shape = 0.1)
  fit <- fit_gev(x)

  pl <- profile_likelihood(fit, "shape", level = 0.95, n_points = 31L)
  expect_s3_class(pl, "profile_likelihood")
  expect_named(pl, c("parameter", "parameter_key", "model", "grid",
                     "log_lik", "threshold_ll", "mle", "max_log_lik",
                     "ci", "level"))
  expect_equal(pl$model, "gev")
  expect_equal(pl$parameter_key, "xi")
  expect_equal(length(pl$grid), 31L)
  expect_equal(length(pl$log_lik), 31L)
  expect_named(pl$ci, c("lower", "upper"))
})

test_that("GEV profile CIs bracket the MLE and are roughly consistent with Wald", {
  skip_if_not_installed("evd")
  set.seed(2L)
  x   <- evd::rgev(800, loc = 0, scale = 1, shape = 0.1)
  fit <- fit_gev(x)

  for (p in c("location", "scale", "shape")) {
    pl <- profile_likelihood(fit, p, n_points = 51L, span = 5)
    expect_false(is.na(pl$ci[["lower"]]))
    expect_false(is.na(pl$ci[["upper"]]))
    expect_lt(pl$ci[["lower"]], pl$mle)
    expect_gt(pl$ci[["upper"]], pl$mle)

    # Profile CIs should be in the same ballpark as Wald (within ~3x)
    # for a well-behaved fit on 800 observations.
    wald_se <- as.numeric(fit$std.err)[match(pl$parameter_key,
                                             c("mu", "sigma", "xi"))]
    wald_ci_width <- 2 * 1.96 * wald_se
    profile_ci_width <- pl$ci[["upper"]] - pl$ci[["lower"]]
    expect_gt(profile_ci_width, wald_ci_width / 3)
    expect_lt(profile_ci_width, wald_ci_width * 3)
  }
})

test_that("GPD profile CIs bracket the MLE", {
  skip_if_not_installed("evd")
  set.seed(3L)
  # Sample from a GPD via inverse-CDF transform: U ~ Unif(0,1) -> y = sigma/xi*((1-U)^(-xi)-1)
  u  <- runif(800)
  y  <- (1 / 0.1) * ((1 - u)^(-0.1) - 1)        # sigma = 1, xi = 0.1
  thr <- 0
  fit <- fit_gpd(y, threshold = thr)

  for (p in c("scale", "shape")) {
    pl <- profile_likelihood(fit, p, n_points = 51L, span = 5)
    expect_false(is.na(pl$ci[["lower"]]))
    expect_false(is.na(pl$ci[["upper"]]))
    expect_lt(pl$ci[["lower"]], pl$mle)
    expect_gt(pl$ci[["upper"]], pl$mle)
  }
})

test_that("profile_ci returns the documented tidy data frame", {
  skip_if_not_installed("evd")
  set.seed(4L)
  x   <- evd::rgev(300, loc = 0, scale = 1, shape = 0)
  fit <- fit_gev(x)

  ci_df <- profile_ci(fit, n_points = 31L)
  expect_s3_class(ci_df, "data.frame")
  expect_named(ci_df, c("parameter", "estimate", "lower", "upper"))
  expect_equal(nrow(ci_df), 3L)
  expect_setequal(ci_df$parameter, c("location", "scale", "shape"))
  expect_true(all(ci_df$lower < ci_df$estimate))
  expect_true(all(ci_df$upper > ci_df$estimate))
})

test_that("profile_likelihood rejects bad arguments", {
  skip_if_not_installed("evd")
  set.seed(5L)
  fit <- fit_gev(evd::rgev(200, 0, 1, 0.1))

  expect_error(profile_likelihood(fit, "median"))            # unknown param
  expect_error(profile_likelihood(fit, "shape", level = 2))  # out of range
  expect_error(profile_likelihood("not a fit", "shape"))     # wrong class
  expect_error(profile_likelihood(fit, "shape", n_points = 2L))
})

test_that("plot.profile_likelihood returns a ggplot", {
  skip_if_not_installed("evd")
  skip_if_not_installed("ggplot2")
  set.seed(6L)
  fit <- fit_gev(evd::rgev(300, 0, 1, 0.1))
  pl  <- profile_likelihood(fit, "shape", n_points = 21L)

  p <- plot(pl)
  expect_s3_class(p, "ggplot")
})
