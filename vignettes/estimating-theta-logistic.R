## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  warning = FALSE,
  message = FALSE
)

## ----simulate-----------------------------------------------------------------
library(chaoticds)

# Load the pre-generated dataset
data(logistic_ts)

# Basic properties
cat("Time series length:", length(logistic_ts), "\n")
cat("Range: [", round(min(logistic_ts), 3), ",", round(max(logistic_ts), 3), "]\n")
cat("Mean:", round(mean(logistic_ts), 3), "\n")

## ----plot-series, fig.cap="Logistic map time series (first 1000 observations)"----
plot(logistic_ts[1:1000], type = "l", 
     main = "Logistic Map Time Series (r = 3.8)",
     xlab = "Time", ylab = "Value")

## ----threshold-selection------------------------------------------------------
# Define candidate thresholds
thresholds <- quantile(logistic_ts, probs = seq(0.85, 0.98, by = 0.01))

# Mean Residual Life plot for threshold selection
mrl_data <- mean_residual_life(logistic_ts, thresholds)

## ----mrl-plot, fig.cap="Mean Residual Life plot for threshold selection"------
mrl_plot(mrl_data)

## ----threshold----------------------------------------------------------------
threshold <- quantile(logistic_ts, 0.95)
cat("Selected threshold:", round(threshold, 4), "\n")

# Number of exceedances
exc <- exceedances(logistic_ts, threshold)
cat("Number of exceedances:", length(exc), "\n")
cat("Exceedance rate:", round(length(exc)/length(logistic_ts), 3), "\n")

## ----runs-estimator-----------------------------------------------------------
# Try different run lengths
run_lengths <- 1:8
theta_runs_vec <- sapply(run_lengths, function(r) {
  result <- extremal_index_runs(logistic_ts, threshold, run_length = r)
  if(length(result) == 0) NA else result
})

# Display results
results_df <- data.frame(
  run_length = run_lengths,
  theta_estimate = theta_runs_vec
)
print(results_df)

## ----intervals-estimator------------------------------------------------------
theta_intervals <- extremal_index_intervals(logistic_ts, threshold)
cat("Intervals estimator result:", theta_intervals, "\n")

## ----cluster-analysis---------------------------------------------------------
# Analyze cluster sizes with smaller run length to find clusters
sizes <- cluster_sizes(logistic_ts, threshold, run_length = 1)

if(length(sizes) > 0) {
  cat("Cluster statistics:\n")
  summary_stats <- cluster_summary(sizes)
  cat("  Number of clusters:", length(sizes), "\n")
  cat("  Mean cluster size:", round(summary_stats[["mean_size"]], 2), "\n")
  cat("  Cluster size variance:", round(summary_stats[["var_size"]], 2), "\n")
  cat("  Max cluster size:", max(sizes), "\n")
} else {
  cat("No clusters found\n")
}

## ----hitting-times------------------------------------------------------------
# Calculate hitting times
hts <- hitting_times(logistic_ts, threshold)

if(length(hts) > 0) {
  cat("Hitting time statistics:\n")
  cat("  Number of hitting times:", length(hts), "\n")
  cat("  Mean hitting time:", round(mean(hts), 2), "\n")
  cat("  Median hitting time:", median(hts), "\n")
  
  # Plot hitting times if we have a valid theta estimate
  valid_theta <- theta_runs_vec[!is.na(theta_runs_vec)]
  if(length(valid_theta) > 0) {
    plot_hts(hts, valid_theta[1])
  }
}

