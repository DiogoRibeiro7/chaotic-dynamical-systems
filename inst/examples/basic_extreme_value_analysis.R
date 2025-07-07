#!/usr/bin/env Rscript

# Example: Basic Extreme Value Analysis for Chaotic Time Series
# This script demonstrates the basic workflow for extreme value analysis

library(chaoticds)

# Load example data
data(logistic_ts)

cat("=== Basic Extreme Value Analysis Example ===\n\n")

# 1. Explore the time series
cat("1. Time Series Overview:\n")
cat("   Length:", length(logistic_ts), "\n")
cat("   Range: [", round(min(logistic_ts), 3), ",", round(max(logistic_ts), 3), "]\n")
cat("   Mean:", round(mean(logistic_ts), 3), "\n")
cat("   Std Dev:", round(sd(logistic_ts), 3), "\n\n")

# 2. Block maxima analysis
cat("2. Block Maxima Analysis:\n")
block_size <- 50
bm <- block_maxima(logistic_ts, block_size)
cat("   Block size:", block_size, "\n")
cat("   Number of blocks:", length(bm), "\n")
cat("   Range of maxima: [", round(min(bm), 3), ",", round(max(bm), 3), "]\n")

# Try to fit GEV (may fail without full extreme value packages)
tryCatch({
  gev_fit <- fit_gev(bm)
  cat("   GEV fit successful\n")
}, error = function(e) {
  cat("   GEV fit not available (missing dependencies)\n")
})

# 3. Peaks-over-threshold analysis
cat("\n3. Peaks-over-Threshold Analysis:\n")
threshold <- quantile(logistic_ts, 0.95)
exc <- exceedances(logistic_ts, threshold)
cat("   Threshold (95th percentile):", round(threshold, 3), "\n")
cat("   Number of exceedances:", length(exc), "\n")
cat("   Exceedance rate:", round(length(exc)/length(logistic_ts), 3), "\n")

# Try to fit GPD (may fail without full extreme value packages)
tryCatch({
  gpd_fit <- fit_gpd(logistic_ts, threshold)
  cat("   GPD fit successful\n")
}, error = function(e) {
  cat("   GPD fit not available (missing dependencies)\n")
})

# 4. Extremal index estimation
cat("\n4. Extremal Index Estimation:\n")
theta_runs <- extremal_index_runs(logistic_ts, threshold, run_length = 5)
theta_int <- extremal_index_intervals(logistic_ts, threshold)
cat("   Runs estimator (r=5):", round(theta_runs, 3), "\n")
cat("   Intervals estimator:", round(theta_int, 3), "\n")

# 5. Cluster analysis
cat("\n5. Cluster Analysis:\n")
sizes <- cluster_sizes(logistic_ts, threshold, run_length = 5)
if(length(sizes) > 0) {
  summary_stats <- cluster_summary(sizes)
  cat("   Number of clusters:", length(sizes), "\n")
  cat("   Mean cluster size:", round(summary_stats[["mean_size"]], 2), "\n")
  cat("   Cluster size variance:", round(summary_stats[["var_size"]], 2), "\n")
} else {
  cat("   Number of clusters: 0\n")
  cat("   No clusters found with the given run length\n")
}

cat("\n=== Analysis Complete ===\n")
cat("This example demonstrates the basic workflow for extreme value analysis\n")
cat("of chaotic time series using the chaoticds package.\n")