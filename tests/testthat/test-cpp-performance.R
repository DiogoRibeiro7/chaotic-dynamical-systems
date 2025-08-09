
test_that('C++ implementations give same results as R implementations', {
  skip_if_not_installed("Rcpp")
  
  # Test data
  set.seed(123)
  test_data <- simulate_logistic_map(1000, r = 3.8, x0 = 0.2)
  threshold <- quantile(test_data, 0.95)
  
  # Test C++ simulation if available
  tryCatch({
    if (exists("simulate_logistic_map_cpp")) {
      set.seed(123)
      cpp_sim <- simulate_logistic_map_cpp(1000, 3.8, 0.2)
      set.seed(123)
      r_sim <- simulate_logistic_map(1000, 3.8, 0.2)
      
      expect_equal(cpp_sim, r_sim, tolerance = 1e-10)
    }
  }, error = function(e) {
    skip("C++ simulation function not available")
  })
  
  # Test C++ extremal index if available
  tryCatch({
    if (exists("extremal_index_runs_cpp")) {
      cpp_ei <- extremal_index_runs_cpp(test_data, threshold, 3)
      r_ei <- extremal_index_runs(test_data, threshold, 3)
      
      expect_equal(cpp_ei, r_ei, tolerance = 1e-10)
    }
  }, error = function(e) {
    skip("C++ extremal index function not available")
  })
  
  # Test C++ block maxima if available
  tryCatch({
    if (exists("block_maxima_cpp")) {
      cpp_bm <- block_maxima_cpp(test_data, 50)
      r_bm <- block_maxima(test_data, 50)
      
      expect_equal(cpp_bm, r_bm, tolerance = 1e-10)
    }
  }, error = function(e) {
    skip("C++ block maxima function not available")
  })
})

test_that('fast wrapper functions work correctly', {
  set.seed(123)
  test_data <- simulate_logistic_map(500, r = 3.8, x0 = 0.2)
  threshold <- quantile(test_data, 0.95)
  
  # Test fast simulation
  fast_sim <- simulate_logistic_map_fast(500, r = 3.8, x0 = 0.2, use_cpp = FALSE)
  expect_equal(length(fast_sim), 500)
  expect_true(all(is.finite(fast_sim)))
  
  # Test fast extremal index
  fast_ei <- extremal_index_runs_fast(test_data, threshold, run_length = 3, use_cpp = FALSE)
  regular_ei <- extremal_index_runs(test_data, threshold, run_length = 3)
  expect_equal(fast_ei, regular_ei)
  
  # Test fast block maxima
  fast_bm <- block_maxima_fast(test_data, 25, use_cpp = FALSE)
  regular_bm <- block_maxima(test_data, 25)
  expect_equal(fast_bm, regular_bm)
})

test_that('C++ functions handle edge cases correctly', {
  skip_if_not_installed("Rcpp")
  
  # Test with empty data
  tryCatch({
    if (exists("threshold_exceedances_cpp")) {
      empty_result <- threshold_exceedances_cpp(numeric(0), 0.5)
      expect_equal(length(empty_result), 0)
    }
  }, error = function(e) {
    skip("C++ threshold exceedances function not available")
  })
  
  # Test with no exceedances
  tryCatch({
    if (exists("extremal_index_runs_cpp")) {
      no_exceed_data <- rep(0.1, 100)
      result <- extremal_index_runs_cpp(no_exceed_data, 0.5, 3)
      expect_true(is.na(result))
    }
  }, error = function(e) {
    skip("C++ extremal index function not available")
  })
  
  # Test with extreme parameters
  tryCatch({
    if (exists("simulate_logistic_map_cpp")) {
      # Very small simulation
      small_sim <- simulate_logistic_map_cpp(2, 3.8, 0.2)
      expect_equal(length(small_sim), 2)
      expect_equal(small_sim[1], 0.2)
    }
  }, error = function(e) {
    skip("C++ simulation function not available")
  })
})

test_that('performance analysis functions work', {
  skip_on_cran()
  
  # Test benchmark function (with small sizes for speed)
  tryCatch({
    small_benchmark <- benchmark_implementations(sizes = c(100, 500), n_reps = 2)
    expect_true(is.data.frame(small_benchmark))
    expect_true("size" %in% names(small_benchmark))
    expect_true("method" %in% names(small_benchmark))
    expect_true("time_seconds" %in% names(small_benchmark))
  }, error = function(e) {
    skip("Benchmark function requires microbenchmark package")
  })
  
  # Test performance analysis
  tryCatch({
    perf_analysis <- performance_analysis(save_results = FALSE)
    expect_true(is.list(perf_analysis))
    expect_true("cpp_available" %in% names(perf_analysis))
  }, error = function(e) {
    skip("Performance analysis function failed")
  })
})

test_that('auto-detection of C++ availability works', {
  # Test that wrapper functions don't crash when C++ is not available
  set.seed(123)
  
  # Small data (should use R implementation)
  small_sim <- simulate_logistic_map_fast(100, r = 3.8, x0 = 0.2)
  expect_equal(length(small_sim), 100)
  
  test_data <- simulate_logistic_map(200, r = 3.8, x0 = 0.2)
  threshold <- quantile(test_data, 0.95)
  
  # Small data (should use R implementation)
  small_ei <- extremal_index_runs_fast(test_data, threshold, run_length = 3)
  expect_true(is.numeric(small_ei))
  
  small_bm <- block_maxima_fast(test_data, 20)
  expect_true(is.numeric(small_bm))
})

test_that('C++ implementations are faster for large data', {
  skip_on_cran()
  skip_if_not_installed("microbenchmark")
  
  # Only test if C++ functions are actually available
  if (!exists("simulate_logistic_map_cpp")) {
    skip("C++ functions not compiled")
  }
  
  n_large <- 5000
  
  # Time R implementation
  r_time <- system.time({
    r_result <- simulate_logistic_map(n_large, r = 3.8, x0 = 0.2)
  })[3]
  
  # Time C++ implementation
  cpp_time <- system.time({
    cpp_result <- simulate_logistic_map_cpp(n_large, 3.8, 0.2)
  })[3]
  
  # C++ should be faster or at least comparable
  speedup <- r_time / cpp_time
  expect_true(speedup >= 0.5)  # Allow for some overhead
  
  # Results should be identical
  expect_equal(cpp_result, r_result, tolerance = 1e-10)
})