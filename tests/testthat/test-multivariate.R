test_that("extremal_index_multivariate computes mean index", {
  set.seed(123)
  df <- data.frame(a = rnorm(500), b = rnorm(500), c = rnorm(500))
  theta <- extremal_index_multivariate(df, 0.9)
  expect_true(is.numeric(theta))
  expect_true(theta > 0 && theta <= 1)
})

test_that("extremal_index_multivariate handles missing estimates", {
  df <- data.frame(a = rnorm(100, -5), b = rnorm(100, -5))
  expect_true(is.na(extremal_index_multivariate(df, 10)))
})

test_that("extremal_index_multivariate validates arguments", {
  df <- data.frame(a = rnorm(10), b = rnorm(10))
  expect_silent(extremal_index_multivariate(df, c(0.5)))
  expect_error(extremal_index_multivariate(df, c(0.5, 0.6, 0.7)), "length")
})

test_that("tail_dependence_asymmetric removes NAs and uses thresholds", {
  set.seed(1)
  x <- c(rnorm(100), NA)
  y <- c(rnorm(100), NA)
  tx <- 1
  ty <- 1
  res <- tail_dependence_asymmetric(x, y, tx, ty)
  manual <- {
    cc <- complete.cases(x, y)
    mean(x[cc] > tx & y[cc] > ty) /
      min(mean(x[cc] > tx), mean(y[cc] > ty))
  }
  expect_equal(res, manual)
})

test_that("tail_dependence_heatmap requires at least two columns", {
  expect_error(tail_dependence_heatmap(data.frame(a = rnorm(10))),
               "at least")
})

test_that("extremal_index_bivariate matches two-column multivariate", {
  set.seed(2)
  df <- data.frame(x = rnorm(200), y = rnorm(200), z = rnorm(200))
  thr <- rep(0.8, 3)
  expect_equal(
    extremal_index_bivariate(df, thr),
    extremal_index_multivariate(df[, 1:2], thr[1:2])
  )
})

test_that("plot_exceedance_clusters returns ggplot and checks input", {
  set.seed(1)
  df <- data.frame(a = rnorm(50), b = rnorm(50))
  p <- plot_exceedance_clusters(df, c(0.9, 0.9))
  expect_s3_class(p, "ggplot")
  expect_error(plot_exceedance_clusters(df[, 1], 0.9), "at least")
})

test_that("tail_dependence_heatmap returns ggplot", {
  set.seed(1)
  df <- data.frame(a = rnorm(50), b = rnorm(50), c = rnorm(50))
  p <- tail_dependence_heatmap(df, 0.8)
  expect_s3_class(p, "ggplot")
})

test_that("multivariate_extreme_workflow returns structured output", {
  set.seed(42)
  x <- rnorm(400)
  y <- 0.5 * x + rnorm(400, sd = 0.8)
  z <- -0.2 * x + rnorm(400, sd = 1.1)
  df <- data.frame(x = x, y = y, z = z)

  wf <- multivariate_extreme_workflow(df, quantile_level = 0.9, run_length = 2)

  expect_true(is.list(wf))
  expect_true(all(c(
    "thresholds", "pairwise_dependence", "joint_exceedance_rate",
    "all_exceedance_rate", "multivariate_extremal_index", "settings"
  ) %in% names(wf)))

  expect_true(is.numeric(wf$thresholds))
  expect_equal(length(wf$thresholds), 3)
  expect_s3_class(wf$pairwise_dependence, "data.frame")
  expect_true(all(c("var1", "var2", "upper_tail", "lower_tail") %in% names(wf$pairwise_dependence)))
  expect_true(is.numeric(wf$joint_exceedance_rate))
  expect_true(is.numeric(wf$all_exceedance_rate))
  expect_true(wf$joint_exceedance_rate >= 0 && wf$joint_exceedance_rate <= 1)
  expect_true(wf$all_exceedance_rate >= 0 && wf$all_exceedance_rate <= 1)
})
