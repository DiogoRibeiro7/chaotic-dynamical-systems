## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  warning = FALSE,
  message = FALSE
)

## ----load-package-------------------------------------------------------------
library(chaoticds)

# Check if C++ implementations are available
cpp_available <- exists("simulate_logistic_map_cpp")
cat("C++ implementations available:", cpp_available, "\n")

## ----performance-comparison---------------------------------------------------
# Small dataset (uses R implementation)
small_n <- 1000
system.time({
  small_sim <- simulate_logistic_map_fast(small_n, r = 3.8, x0 = 0.2)
})

# Large dataset (automatically uses C++ if available)
large_n <- 10000
system.time({
  large_sim <- simulate_logistic_map_fast(large_n, r = 3.8, x0 = 0.2)
})

cat("Small dataset length:", length(small_sim), "\n")
cat("Large dataset length:", length(large_sim), "\n")

## ----memory-efficiency--------------------------------------------------------
# Check memory usage for different data sizes
sizes <- c(1000, 5000, 10000, 25000)
memory_usage <- sapply(sizes, function(n) {
  sim_data <- simulate_logistic_map_fast(n, r = 3.8, x0 = 0.2)
  mem_size <- as.numeric(object.size(sim_data))
  rm(sim_data)
  gc()
  return(mem_size)
})

mem_df <- data.frame(
  size = sizes,
  memory_bytes = memory_usage,
  memory_kb = round(memory_usage / 1024, 2),
  bytes_per_obs = round(memory_usage / sizes, 2)
)

print(mem_df)

## ----benchmarking, eval=FALSE-------------------------------------------------
# # Run performance analysis (commented out for vignette speed)
# # perf_results <- performance_analysis(save_results = TRUE)
# # print(perf_results$summary)

## ----manual-benchmark---------------------------------------------------------
# Compare R vs optimized implementations
set.seed(123)
test_data <- simulate_logistic_map(5000, r = 3.8, x0 = 0.2)
threshold <- quantile(test_data, 0.95)

# Time different implementations
times <- list()

# Standard R implementation
times$r_extremal <- system.time({
  ei_r <- extremal_index_runs(test_data, threshold, run_length = 3)
})[3]

# Fast wrapper (auto-selects implementation)
times$fast_extremal <- system.time({
  ei_fast <- extremal_index_runs_fast(test_data, threshold, run_length = 3)
})[3]

# Block maxima comparison
times$r_block <- system.time({
  bm_r <- block_maxima(test_data, 50)
})[3]

times$fast_block <- system.time({
  bm_fast <- block_maxima_fast(test_data, 50)
})[3]

# Print timing results
cat("Timing Results (seconds):\n")
for (name in names(times)) {
  cat(sprintf("%-15s: %.4f\n", name, times[[name]]))
}

# Verify results are identical
cat("\nResults verification:\n")
cat("Extremal index identical:", identical(ei_r, ei_fast), "\n")
cat("Block maxima identical:", identical(bm_r, bm_fast), "\n")

## ----scaling-analysis---------------------------------------------------------
# Test performance scaling
test_sizes <- c(1000, 2500, 5000, 7500, 10000)
scaling_results <- data.frame(
  size = test_sizes,
  time_sim = numeric(length(test_sizes)),
  time_analysis = numeric(length(test_sizes))
)

for (i in seq_along(test_sizes)) {
  n <- test_sizes[i]
  
  # Time simulation
  sim_time <- system.time({
    test_sim <- simulate_logistic_map_fast(n, r = 3.8, x0 = 0.2)
  })[3]
  
  # Time analysis
  analysis_time <- system.time({
    thresh <- quantile(test_sim, 0.95)
    ei <- extremal_index_runs_fast(test_sim, thresh, run_length = 3)
    bm <- block_maxima_fast(test_sim, 50)
  })[3]
  
  scaling_results$time_sim[i] <- sim_time
  scaling_results$time_analysis[i] <- analysis_time
}

print(scaling_results)

# Plot scaling
plot(scaling_results$size, scaling_results$time_sim, type = "b", 
     main = "Performance Scaling", xlab = "Data Size", ylab = "Time (seconds)",
     col = "blue", pch = 16, ylim = range(c(scaling_results$time_sim, scaling_results$time_analysis)))
lines(scaling_results$size, scaling_results$time_analysis, type = "b", 
      col = "red", pch = 17)
legend("topleft", legend = c("Simulation", "Analysis"), 
       col = c("blue", "red"), pch = c(16, 17), lty = 1)
grid()

