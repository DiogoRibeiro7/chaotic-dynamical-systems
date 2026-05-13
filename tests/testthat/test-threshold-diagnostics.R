
test_that('mean_residual_life computes correctly', {
  x <- c(1, 2, 3, 4, 5)
  threshold <- 2.5
  
  mrl_df <- mean_residual_life(x, threshold)
  expected <- mean(x[x > threshold] - threshold)
  expect_equal(mrl_df$mean_excess, expected)
  expect_equal(mrl_df$threshold, threshold)
})

test_that('mean_residual_life handles no exceedances', {
  x <- c(1, 2, 3)
  threshold <- 5
  
  mrl_df <- mean_residual_life(x, threshold)
  expect_true(is.na(mrl_df$mean_excess))
  expect_equal(mrl_df$threshold, threshold)
})

test_that('threshold_diagnostics returns proper structure', {
  set.seed(42)
  x <- rnorm(100)
  thresholds <- quantile(x, c(0.8, 0.85, 0.9, 0.95))
  k_values <- c(5, 10, 15)
  
  diag <- threshold_diagnostics(x, thresholds, k_values)
  
  expect_true(is.list(diag))
  expect_true('mrl' %in% names(diag))
  expect_true('hill' %in% names(diag))
  expect_equal(length(diag$mrl$threshold), length(thresholds))
})

test_that('hill_estimates computes for valid k values', {
  set.seed(123)
  x <- rexp(100)  # exponential data
  k_values <- c(5, 10, 15)
  
  hill_est <- hill_estimates(x, k_values)
  expect_true(is.data.frame(hill_est))
  expect_true('k' %in% names(hill_est))
  expect_true('hill' %in% names(hill_est))
  expect_true(all(is.finite(hill_est$hill)))
})

test_that("select_threshold_auto returns ranked candidates", {
  set.seed(123)
  x <- simulate_logistic_map(1000, r = 3.8, x0 = 0.2)

  res <- select_threshold_auto(
    x,
    candidate_probs = c(0.9, 0.92, 0.94, 0.96, 0.98),
    min_exceedances = 15
  )

  expect_true(is.list(res))
  expect_true(all(c("recommended_threshold", "recommended_probability", "score", "ranking") %in% names(res)))
  expect_true(is.numeric(res$recommended_threshold))
  expect_true(is.numeric(res$recommended_probability))
  expect_true(is.numeric(res$score))
  expect_s3_class(res$ranking, "data.frame")

  required_cols <- c(
    "probability", "threshold", "n_exceedances",
    "mrl_score", "stability_score", "exceedance_score",
    "score", "rationale"
  )
  expect_true(all(required_cols %in% names(res$ranking)))
})

test_that("select_threshold_auto score is sorted decreasing", {
  set.seed(42)
  x <- rnorm(1200)
  res <- select_threshold_auto(x, candidate_probs = seq(0.9, 0.98, by = 0.02))
  scores <- res$ranking$score
  finite_scores <- scores[is.finite(scores)]
  expect_true(all(diff(finite_scores) <= 1e-12))
})

test_that("select_threshold_auto respects score bounds", {
  set.seed(7)
  x <- rexp(1500)
  res <- select_threshold_auto(x, candidate_probs = seq(0.9, 0.99, by = 0.01))
  finite_scores <- res$ranking$score[is.finite(res$ranking$score)]
  expect_true(all(finite_scores >= 0 & finite_scores <= 1))
})
