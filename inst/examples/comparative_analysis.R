#!/usr/bin/env Rscript

# Example: Comparing Different Maps for Extreme Value Properties
# This script compares extreme value characteristics across different systems

library(chaoticds)

cat("=== Comparative Analysis of Chaotic vs Non-Chaotic Systems ===\n\n")

# Load data
data(logistic_ts)
data(henon_ts) 
data(ar1_ts)

# Function to analyze extreme value properties
analyze_extremes <- function(x, name, threshold_q = 0.95) {
  cat(sprintf("--- %s ---\n", name))
  
  threshold <- quantile(x, threshold_q)
  exc <- exceedances(x, threshold)
  
  # Basic statistics
  cat(sprintf("Length: %d\n", length(x)))
  cat(sprintf("Threshold (%.1f%%): %.3f\n", threshold_q * 100, threshold))
  cat(sprintf("Exceedances: %d (%.2f%%)\n", 
              length(exc), 100 * length(exc) / length(x)))
  
  # Extremal index estimates
  theta_runs <- extremal_index_runs(x, threshold)
  theta_int <- extremal_index_intervals(x, threshold)
  cat(sprintf("Extremal Index (runs): %.3f\n", theta_runs))
  cat(sprintf("Extremal Index (intervals): %.3f\n", theta_int))
  
  # Cluster analysis
  sizes <- cluster_sizes(x, threshold)
  summary_stats <- cluster_summary(sizes)
  cat(sprintf("Mean cluster size: %.2f\n", summary_stats$mean))
  cat(sprintf("Max cluster size: %d\n", summary_stats$max))
  
  cat("\n")
  
  return(list(
    name = name,
    threshold = threshold,
    n_exc = length(exc),
    theta_runs = theta_runs,
    theta_int = theta_int,
    mean_cluster_size = summary_stats$mean,
    max_cluster_size = summary_stats$max
  ))
}

# Analyze each system
results <- list()
results$logistic <- analyze_extremes(logistic_ts, "Logistic Map (r=3.8)")
results$henon_x <- analyze_extremes(henon_ts$x, "Hénon Map (x-component)")
results$ar1 <- analyze_extremes(ar1_ts, "AR(1) Model")

# Create comparison table
cat("=== Summary Comparison ===\n")
cat(sprintf("%-20s %8s %8s %8s %12s\n", 
            "System", "θ_runs", "θ_int", "Mean_Clust", "Max_Clust"))
cat(paste(rep("-", 60), collapse = ""), "\n")

for (r in results) {
  cat(sprintf("%-20s %8.3f %8.3f %8.2f %12d\n",
              r$name, r$theta_runs, r$theta_int, 
              r$mean_cluster_size, r$max_cluster_size))
}

cat("\n=== Interpretation ===\n")
cat("- Extremal index θ measures clustering of extremes (θ=1: no clustering)\n")
cat("- Lower θ indicates stronger clustering of extreme events\n")
cat("- Chaotic systems often show different clustering patterns than linear models\n")
cat("- Mean cluster size reflects typical duration of extreme periods\n")