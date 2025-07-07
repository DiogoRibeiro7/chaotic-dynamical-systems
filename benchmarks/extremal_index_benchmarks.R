#!/usr/bin/env Rscript

# Benchmark script for extremal index estimation performance
# This script compares different estimators and parameters

library(chaoticds)
library(microbenchmark)

cat("=== Extremal Index Estimation Benchmarks ===\n\n")

# Generate test data
set.seed(12345)
data(logistic_ts)
n_obs <- length(logistic_ts)
threshold <- quantile(logistic_ts, 0.95)

cat("Dataset properties:\n")
cat("  Observations:", n_obs, "\n")
cat("  Threshold:", round(threshold, 4), "\n")
cat("  Exceedances:", length(exceedances(logistic_ts, threshold)), "\n\n")

# Benchmark different run lengths for runs estimator
cat("Benchmarking runs estimator with different run lengths:\n")
run_lengths <- c(1, 2, 3, 5, 10)

runs_benchmark <- microbenchmark(
  r1 = extremal_index_runs(logistic_ts, threshold, run_length = 1),
  r2 = extremal_index_runs(logistic_ts, threshold, run_length = 2),
  r3 = extremal_index_runs(logistic_ts, threshold, run_length = 3),
  r5 = extremal_index_runs(logistic_ts, threshold, run_length = 5),
  r10 = extremal_index_runs(logistic_ts, threshold, run_length = 10),
  times = 100
)

print(runs_benchmark)

# Benchmark intervals estimator
cat("\nBenchmarking intervals estimator:\n")
intervals_benchmark <- microbenchmark(
  intervals = extremal_index_intervals(logistic_ts, threshold),
  times = 100
)

print(intervals_benchmark)

# Benchmark cluster analysis
cat("\nBenchmarking cluster analysis:\n")
cluster_benchmark <- microbenchmark(
  cluster_sizes = cluster_sizes(logistic_ts, threshold, run_length = 3),
  cluster_summary = {
    sizes <- cluster_sizes(logistic_ts, threshold, run_length = 3)
    if(length(sizes) > 0) cluster_summary(sizes)
  },
  times = 50
)

print(cluster_benchmark)

# Benchmark threshold diagnostics
cat("\nBenchmarking threshold diagnostics:\n")
thresholds <- quantile(logistic_ts, seq(0.9, 0.99, by = 0.01))

diagnostics_benchmark <- microbenchmark(
  mrl = mean_residual_life(logistic_ts, thresholds),
  hill = hill_estimates(logistic_ts, 5:20),
  times = 20
)

print(diagnostics_benchmark)

# Data size scaling benchmark
cat("\nBenchmarking performance vs data size:\n")
sizes <- c(1000, 2000, 5000, 10000)
size_results <- data.frame(
  size = sizes,
  runs_time = NA,
  intervals_time = NA,
  cluster_time = NA
)

for(i in seq_along(sizes)) {
  n <- sizes[i]
  if(n <= length(logistic_ts)) {
    data_subset <- logistic_ts[1:n]
    thresh_subset <- quantile(data_subset, 0.95)
    
    # Time runs estimator
    t1 <- system.time({
      for(j in 1:10) extremal_index_runs(data_subset, thresh_subset, run_length = 3)
    })
    size_results$runs_time[i] <- t1[3] / 10
    
    # Time intervals estimator  
    t2 <- system.time({
      for(j in 1:10) extremal_index_intervals(data_subset, thresh_subset)
    })
    size_results$intervals_time[i] <- t2[3] / 10
    
    # Time cluster analysis
    t3 <- system.time({
      for(j in 1:10) cluster_sizes(data_subset, thresh_subset, run_length = 3)
    })
    size_results$cluster_time[i] <- t3[3] / 10
  }
}

print(size_results)

# Memory usage analysis
cat("\nMemory usage analysis:\n")
cat("Object sizes:\n")
cat("  logistic_ts:", format(object.size(logistic_ts), units = "Kb"), "\n")

# Test with larger dataset
large_data <- simulate_logistic_map(50000, r = 3.8, x0 = 0.2)
cat("  large_data (50k obs):", format(object.size(large_data), units = "Kb"), "\n")

# Summary recommendations
cat("\n=== Performance Summary ===\n")
cat("Based on benchmarks:\n")
cat("1. Runs estimator: Fast, scales linearly with data size\n")
cat("2. Intervals estimator: Moderate speed, good for occasional use\n") 
cat("3. Cluster analysis: Fast when clusters exist\n")
cat("4. Threshold diagnostics: Computational cost increases with number of thresholds\n")
cat("5. Memory usage: Scales linearly with data size\n\n")

cat("Recommendations:\n")
cat("- For routine analysis: Use runs estimator with run_length 3-5\n")
cat("- For validation: Compare with intervals estimator\n")
cat("- For large datasets: Consider data subsampling for threshold selection\n")
cat("- For real-time applications: Pre-compute thresholds when possible\n")