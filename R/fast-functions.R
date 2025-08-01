#' Fast simulation and analysis functions
#'
#' Wrapper functions that automatically choose between R and C++ implementations
#' based on data size and availability of compiled code.
#'
#' @name fast-functions
NULL

#' Fast logistic map simulation with automatic method selection
#'
#' Automatically chooses between R and C++ implementations based on data size
#' and availability of compiled C++ code.
#'
#' @param n Number of observations to generate
#' @param r Parameter r of the logistic map
#' @param x0 Initial value
#' @param use_cpp Logical, whether to force C++ implementation (if available)
#' @return Numeric vector of simulated values
#' @export
simulate_logistic_map_fast <- function(n, r, x0, use_cpp = NULL) {
  # Auto-detect whether to use C++ based on data size and availability
  if (is.null(use_cpp)) {
    use_cpp <- n > 5000 && exists("simulate_logistic_map_cpp")
  }
  
  if (use_cpp && exists("simulate_logistic_map_cpp")) {
    tryCatch({
      return(simulate_logistic_map_cpp(n, r, x0))
    }, error = function(e) {
      warning("C++ implementation failed, falling back to R implementation: ", e$message)
      return(simulate_logistic_map(n, r, x0))
    })
  } else {
    return(simulate_logistic_map(n, r, x0))
  }
}

#' Fast extremal index estimation with automatic method selection
#'
#' @param x Numeric vector of data
#' @param threshold Threshold value
#' @param run_length Run length parameter
#' @param use_cpp Logical, whether to force C++ implementation (if available)
#' @return Extremal index estimate
#' @export
extremal_index_runs_fast <- function(x, threshold, run_length = 3, use_cpp = NULL) {
  # Auto-detect whether to use C++ based on data size and availability
  if (is.null(use_cpp)) {
    use_cpp <- length(x) > 10000 && exists("extremal_index_runs_cpp")
  }
  
  if (use_cpp && exists("extremal_index_runs_cpp")) {
    tryCatch({
      return(extremal_index_runs_cpp(x, threshold, run_length))
    }, error = function(e) {
      warning("C++ implementation failed, falling back to R implementation: ", e$message)
      return(extremal_index_runs(x, threshold, run_length))
    })
  } else {
    return(extremal_index_runs(x, threshold, run_length))
  }
}

#' Fast block maxima computation with automatic method selection
#'
#' @param x Numeric vector of data
#' @param block_size Size of each block
#' @param use_cpp Logical, whether to force C++ implementation (if available)
#' @return Block maxima
#' @export
block_maxima_fast <- function(x, block_size, use_cpp = NULL) {
  # Auto-detect whether to use C++ based on data size and availability
  if (is.null(use_cpp)) {
    use_cpp <- length(x) > 50000 && exists("block_maxima_cpp")
  }
  
  if (use_cpp && exists("block_maxima_cpp")) {
    tryCatch({
      return(block_maxima_cpp(x, block_size))
    }, error = function(e) {
      warning("C++ implementation failed, falling back to R implementation: ", e$message)
      return(block_maxima(x, block_size))
    })
  } else {
    return(block_maxima(x, block_size))
  }
}

#' Benchmark R vs C++ implementations
#'
#' Compare performance of R and C++ implementations for different data sizes.
#'
#' @param sizes Vector of data sizes to test
#' @param n_reps Number of repetitions for timing
#' @return Data frame with benchmark results
#' @export
benchmark_implementations <- function(sizes = c(1000, 5000, 10000, 50000), n_reps = 10) {
  if (!requireNamespace("microbenchmark", quietly = TRUE)) {
    stop("microbenchmark package required for benchmarking")
  }
  
  results <- data.frame(
    size = integer(0),
    method = character(0),
    time_seconds = numeric(0),
    speedup = numeric(0)
  )
  
  for (size in sizes) {
    cat("Benchmarking size:", size, "\n")
    
    # Generate test data
    test_data <- simulate_logistic_map(size, r = 3.8, x0 = 0.2)
    threshold <- quantile(test_data, 0.95)
    
    # R implementation timing
    r_time <- system.time({
      for (i in 1:n_reps) {
        extremal_index_runs(test_data, threshold, run_length = 3)
      }
    })[3] / n_reps
    
    # C++ implementation timing (if available)
    cpp_time <- NA
    speedup <- NA
    
    if (exists("extremal_index_runs_cpp")) {
      tryCatch({
        cpp_time <- system.time({
          for (i in 1:n_reps) {
            extremal_index_runs_cpp(test_data, threshold, 3)
          }
        })[3] / n_reps
        
        speedup <- r_time / cpp_time
      }, error = function(e) {
        warning("C++ benchmark failed: ", e$message)
      })
    }
    
    # Add results
    results <- rbind(results, data.frame(
      size = c(size, size),
      method = c("R", "C++"),
      time_seconds = c(r_time, cpp_time),
      speedup = c(1.0, speedup)
    ))
  }
  
  return(results)
}

#' Performance analysis report
#'
#' Generate a comprehensive performance analysis comparing R and C++ implementations.
#'
#' @param save_results Logical, whether to save results to file
#' @return List with performance analysis results
#' @importFrom utils object.size write.csv
#' @export
performance_analysis <- function(save_results = FALSE) {
  cat("=== Performance Analysis Report ===\n\n")
  
  # Check if C++ functions are available
  cpp_available <- exists("simulate_logistic_map_cpp") && 
                   exists("extremal_index_runs_cpp") && 
                   exists("block_maxima_cpp")
  
  cat("C++ implementations available:", cpp_available, "\n\n")
  
  if (!cpp_available) {
    cat("C++ implementations not compiled. Install Rcpp and recompile package for performance benefits.\n")
    return(list(cpp_available = FALSE))
  }
  
  # Run benchmarks
  cat("Running benchmarks...\n")
  benchmark_results <- benchmark_implementations()
  
  # Performance summary
  cat("\n=== Performance Summary ===\n")
  cpp_results <- benchmark_results[benchmark_results$method == "C++", ]
  if (nrow(cpp_results) > 0) {
    mean_speedup <- mean(cpp_results$speedup, na.rm = TRUE)
    max_speedup <- max(cpp_results$speedup, na.rm = TRUE)
    
    cat("Average C++ speedup:", round(mean_speedup, 2), "x\n")
    cat("Maximum C++ speedup:", round(max_speedup, 2), "x\n")
    
    # Memory efficiency test
    cat("\n=== Memory Efficiency ===\n")
    large_size <- 100000
    cat("Testing memory efficiency with", large_size, "observations...\n")
    
    mem_before <- as.numeric(object.size(ls()))
    large_data <- simulate_logistic_map_fast(large_size, r = 3.8, x0 = 0.2, use_cpp = TRUE)
    mem_after <- as.numeric(object.size(large_data))
    
    cat("Memory used:", round(mem_after / 1024^2, 2), "MB\n")
    cat("Memory per observation:", round(mem_after / large_size), "bytes\n")
    
    rm(large_data)
    gc()
  }
  
  # Save results if requested
  if (save_results) {
    write.csv(benchmark_results, "performance_benchmark_results.csv", row.names = FALSE)
    cat("\nResults saved to performance_benchmark_results.csv\n")
  }
  
  return(list(
    cpp_available = cpp_available,
    benchmark_results = benchmark_results,
    summary = list(
      mean_speedup = if(nrow(cpp_results) > 0) mean(cpp_results$speedup, na.rm = TRUE) else NA,
      max_speedup = if(nrow(cpp_results) > 0) max(cpp_results$speedup, na.rm = TRUE) else NA
    )
  ))
}