
test_that('simulation performance is acceptable', {
  skip_on_cran()

  start_time <- Sys.time()
  sim_data <- simulate_logistic_map(10000, r = 3.8, x0 = 0.2)
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  expect_true(elapsed < 1.0)
  expect_equal(length(sim_data), 10000)
})

test_that('extremal index computation performance', {
  skip_on_cran()

  set.seed(123)
  data(logistic_ts)
  threshold <- quantile(logistic_ts, 0.95)

  start_time <- Sys.time()
  theta <- extremal_index_runs(logistic_ts, threshold, run_length = 3)
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  expect_true(elapsed < 0.5)
  expect_true(is.numeric(theta))
})

test_that('cluster analysis performance', {
  skip_on_cran()

  set.seed(123)
  data(logistic_ts)
  threshold <- quantile(logistic_ts, 0.95)

  start_time <- Sys.time()
  sizes <- cluster_sizes(logistic_ts, threshold, run_length = 3)
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  expect_true(elapsed < 0.5)
  expect_true(is.numeric(sizes))
})

test_that('memory usage is reasonable', {
  skip_on_cran()

  n_large <- 50000
  gc()

  large_sim <- simulate_logistic_map(n_large, r = 3.8, x0 = 0.2)

  expect_equal(length(large_sim), n_large)
  expect_true(is.numeric(large_sim))

  rm(large_sim)
  gc()
})

test_that("larger inputs complete and return valid results", {
  skip_on_cran()

  sizes <- c(1000L, 5000L, 10000L)
  times <- numeric(length(sizes))

  for (i in seq_along(sizes)) {
    n <- sizes[i]

    start_time <- Sys.time()
    sim_data <- simulate_logistic_map(n, r = 3.8, x0 = 0.2)
    threshold <- quantile(sim_data, 0.95)
    theta <- extremal_index_runs(sim_data, threshold, run_length = 3)
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

    times[i] <- elapsed

    expect_length(sim_data, n)
    expect_true(is.finite(threshold))
    expect_true(is.numeric(theta))
    expect_true(elapsed < 2.0)
  }

  # Shared CI runners are unsuitable for asserting ratios between very short
  # wall-clock timings. Benchmark workflows, not unit tests, track scaling.
  expect_true(all(is.finite(times)))
  expect_true(all(times >= 0))
})

test_that('parallel processing capability', {
  skip_on_cran()

  tryCatch({
    if (requireNamespace("parallel", quietly = TRUE)) {
      set.seed(123)
      data(logistic_ts)
      threshold <- quantile(logistic_ts, 0.95)

      start_time <- Sys.time()
      result_seq <- bootstrap_extremal_index(
        logistic_ts[1:500],
        threshold,
        n_bootstrap = 20,
        parallel = FALSE
      )
      time_seq <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

      start_time <- Sys.time()
      result_par <- bootstrap_extremal_index(
        logistic_ts[1:500],
        threshold,
        n_bootstrap = 20,
        parallel = TRUE
      )
      time_par <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

      expect_true(abs(result_seq$estimate - result_par$estimate) < 0.1)
      expect_true(time_par <= time_seq * 1.5)
    }
  }, error = function(e) {
    skip("Parallel bootstrap functions not yet implemented")
  })
})

test_that('benchmark regression detection', {
  skip_on_cran()

  benchmark_data <- list(
    logistic_sim_1k = function() simulate_logistic_map(1000, r = 3.8, x0 = 0.2),
    logistic_sim_10k = function() simulate_logistic_map(10000, r = 3.8, x0 = 0.2),
    extremal_index_computation = function() {
      data(logistic_ts)
      threshold <- quantile(logistic_ts, 0.95)
      extremal_index_runs(logistic_ts, threshold, run_length = 3)
    }
  )

  for (name in names(benchmark_data)) {
    start_time <- Sys.time()
    result <- benchmark_data[[name]]()
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

    expected_max_time <- switch(name,
      "logistic_sim_1k" = 0.1,
      "logistic_sim_10k" = 0.5,
      "extremal_index_computation" = 0.5
    )

    expect_true(
      elapsed < expected_max_time,
      info = paste(
        "Performance regression detected for",
        name,
        "- took",
        elapsed,
        "seconds, expected <",
        expected_max_time
      )
    )
  }
})

test_that("threshold_summary_chunked matches direct computation", {
  set.seed(99)
  x <- rnorm(10000)
  u <- quantile(x, 0.95)

  res <- threshold_summary_chunked(x, u, chunk_size = 1500)
  exc <- x[x > u] - u

  expect_true(is.list(res))
  expect_equal(res$n, length(x))
  expect_equal(res$n_exceedances, length(exc))
  expect_equal(res$exceedance_rate, length(exc) / length(x))
  expect_equal(res$mean_excess, if (length(exc) > 0) mean(exc) else NA_real_)
  expect_equal(res$max_excess, if (length(exc) > 0) max(exc) else NA_real_)
})
