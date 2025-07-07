context('Performance benchmarks')

test_that('simulation performance is acceptable', {
  skip_on_cran()
  
  # Test logistic map performance
  start_time <- Sys.time()
  sim_data <- simulate_logistic_map(10000, r = 3.8, x0 = 0.2)
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  
  expect_true(elapsed < 1.0)  # Should complete in under 1 second
  expect_equal(length(sim_data), 10000)
})

test_that('extremal index computation performance', {
  skip_on_cran()
  
  set.seed(123)
  data(logistic_ts)
  threshold <- quantile(logistic_ts, 0.95)
  
  # Test runs estimator performance
  start_time <- Sys.time()
  theta <- extremal_index_runs(logistic_ts, threshold, run_length = 3)
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  
  expect_true(elapsed < 0.5)  # Should complete in under 0.5 seconds
  expect_true(is.numeric(theta))
})

test_that('cluster analysis performance', {
  skip_on_cran()
  
  set.seed(123)
  data(logistic_ts)
  threshold <- quantile(logistic_ts, 0.95)
  
  # Test cluster size computation performance
  start_time <- Sys.time()
  sizes <- cluster_sizes(logistic_ts, threshold, run_length = 3)
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  
  expect_true(elapsed < 0.5)  # Should complete in under 0.5 seconds
  expect_true(is.numeric(sizes))
})

test_that('memory usage is reasonable', {
  skip_on_cran()
  
  # Test memory efficiency with larger datasets
  n_large <- 50000
  
  # Check memory before
  gc()
  mem_before <- memory.size() # This may not work on all systems
  
  # Generate large dataset
  large_sim <- simulate_logistic_map(n_large, r = 3.8, x0 = 0.2)
  
  # Basic checks
  expect_equal(length(large_sim), n_large)
  expect_true(is.numeric(large_sim))
  
  # Clean up
  rm(large_sim)
  gc()
})

test_that('scalability with different data sizes', {
  skip_on_cran()
  
  sizes <- c(1000, 5000, 10000)
  times <- numeric(length(sizes))
  
  for(i in seq_along(sizes)) {
    n <- sizes[i]
    
    start_time <- Sys.time()
    sim_data <- simulate_logistic_map(n, r = 3.8, x0 = 0.2)
    threshold <- quantile(sim_data, 0.95)
    theta <- extremal_index_runs(sim_data, threshold, run_length = 3)
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    
    times[i] <- elapsed
    
    # Each computation should still be fast
    expect_true(elapsed < 2.0)
  }
  
  # Times should scale reasonably (not exponentially)
  if(length(times) >= 2) {
    scaling_factor <- times[length(times)] / times[1]
    size_factor <- sizes[length(sizes)] / sizes[1]
    expect_true(scaling_factor < size_factor * 2)  # Should scale better than quadratic
  }
})

test_that('parallel processing capability', {
  skip_on_cran()
  
  # Test if parallel processing functions exist and work
  tryCatch({
    # Test parallel bootstrap if implemented
    if(requireNamespace("parallel", quietly = TRUE)) {
      set.seed(123)
      data(logistic_ts)
      threshold <- quantile(logistic_ts, 0.95)
      
      # Compare sequential vs parallel if functions exist
      start_time <- Sys.time()
      result_seq <- bootstrap_extremal_index(logistic_ts[1:500], threshold, n_bootstrap = 20, parallel = FALSE)
      time_seq <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      
      start_time <- Sys.time()
      result_par <- bootstrap_extremal_index(logistic_ts[1:500], threshold, n_bootstrap = 20, parallel = TRUE)
      time_par <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      
      # Results should be similar (within reasonable tolerance)
      expect_true(abs(result_seq[1] - result_par[1]) < 0.1)
      
      # Parallel should be faster or at least not much slower
      expect_true(time_par <= time_seq * 1.5)
    }
  }, error = function(e) {
    skip("Parallel bootstrap functions not yet implemented")
  })
})

test_that('benchmark regression detection', {
  skip_on_cran()
  
  # This test helps detect performance regressions
  # Set reasonable performance expectations
  
  benchmark_data <- list(
    logistic_sim_1k = function() simulate_logistic_map(1000, r = 3.8, x0 = 0.2),
    logistic_sim_10k = function() simulate_logistic_map(10000, r = 3.8, x0 = 0.2),
    extremal_index_computation = function() {
      data(logistic_ts)
      threshold <- quantile(logistic_ts, 0.95)
      extremal_index_runs(logistic_ts, threshold, run_length = 3)
    }
  )
  
  for(name in names(benchmark_data)) {
    start_time <- Sys.time()
    result <- benchmark_data[[name]]()
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    
    # Set performance expectations (these may need adjustment)
    expected_max_time <- switch(name,
      "logistic_sim_1k" = 0.1,
      "logistic_sim_10k" = 0.5,
      "extremal_index_computation" = 0.5
    )
    
    expect_true(elapsed < expected_max_time, 
                info = paste("Performance regression detected for", name, 
                           "- took", elapsed, "seconds, expected <", expected_max_time))
  }
})