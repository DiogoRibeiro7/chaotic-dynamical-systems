## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  warning = FALSE,
  message = FALSE
)

## ----load-data----------------------------------------------------------------
library(chaoticds)

# Load pre-generated Hénon map data
data(henon_ts)

# Extract x-component for analysis
x_series <- henon_ts$x
y_series <- henon_ts$y

cat("Dataset properties:\n")
cat("  Number of observations:", nrow(henon_ts), "\n")
cat("  x-component range: [", round(min(x_series), 3), ",", round(max(x_series), 3), "]\n")
cat("  y-component range: [", round(min(y_series), 3), ",", round(max(y_series), 3), "]\n")

## ----plot-attractor, fig.cap="Hénon attractor"--------------------------------
plot(henon_ts$x, henon_ts$y, pch = ".", cex = 0.5,
     main = "Hénon Attractor (a=1.4, b=0.3)",
     xlab = "x", ylab = "y")

## ----plot-timeseries, fig.cap="Hénon map x-component time series"-------------
plot(x_series[1:1000], type = "l",
     main = "Hénon Map x-component Time Series",
     xlab = "Time", ylab = "x")

## ----block-sizes--------------------------------------------------------------
# Test different block sizes
block_sizes <- c(25, 50, 100, 200)
n_obs <- length(x_series)

cat("Block size analysis:\n")
for(bs in block_sizes) {
  n_blocks <- floor(n_obs / bs)
  cat(sprintf("  Block size %d: %d blocks, %d observations used\n", 
              bs, n_blocks, n_blocks * bs))
}

## ----block-maxima-------------------------------------------------------------
block_size <- 50
maxima <- block_maxima(x_series, block_size)

cat("Block maxima statistics:\n")
cat("  Number of maxima:", length(maxima), "\n")
cat("  Range: [", round(min(maxima), 3), ",", round(max(maxima), 3), "]\n")
cat("  Mean:", round(mean(maxima), 3), "\n")
cat("  Standard deviation:", round(sd(maxima), 3), "\n")

## ----plot-maxima, fig.cap="Distribution of block maxima"----------------------
hist(maxima, breaks = 20, freq = FALSE,
     main = "Distribution of Block Maxima",
     xlab = "Block Maximum", ylab = "Density")
lines(density(maxima), col = "red", lwd = 2)

## ----gev-fit------------------------------------------------------------------
# Attempt to fit GEV distribution
gev_result <- tryCatch({
  fit_gev(maxima)
}, error = function(e) {
  cat("GEV fitting failed:", e$message, "\n")
  list(
    estimate = c(location = NA, scale = NA, shape = NA),
    method = "GEV fitting not available"
  )
})

if(!is.null(gev_result) && !any(is.na(gev_result$estimate))) {
  cat("GEV parameter estimates:\n")
  print(gev_result$estimate)
} else {
  cat("GEV distribution fitting not available with current dependencies\n")
}

## ----threshold-selection------------------------------------------------------
# Define candidate thresholds
quantiles <- seq(0.85, 0.99, by = 0.01)
thresholds <- quantile(x_series, quantiles)

# Calculate MRL for each threshold
mrl_data <- mean_residual_life(x_series, thresholds)

## ----mrl-plot, fig.cap="Mean Residual Life plot for threshold selection"------
mrl_plot(mrl_data)

## ----pot-threshold------------------------------------------------------------
threshold <- quantile(x_series, 0.95)
cat("Selected threshold:", round(threshold, 4), "\n")

# Extract exceedances
exc <- exceedances(x_series, threshold)
cat("Number of exceedances:", length(exc), "\n")
cat("Exceedance rate:", round(length(exc)/length(x_series), 3), "\n")

## ----gpd-fit------------------------------------------------------------------
# Attempt to fit GPD
gpd_result <- tryCatch({
  fit_gpd(x_series, threshold)
}, error = function(e) {
  cat("GPD fitting failed:", e$message, "\n")
  list(
    estimate = c(scale = NA, shape = NA),
    threshold = threshold,
    method = "GPD fitting not available"
  )
})

if(!is.null(gpd_result) && !any(is.na(gpd_result$estimate))) {
  cat("GPD parameter estimates:\n")
  print(gpd_result$estimate)
} else {
  cat("GPD distribution fitting not available with current dependencies\n")
}

## ----plot-exceedances, fig.cap="Time series with threshold and exceedances"----
plot(x_series[1:1000], type = "l", 
     main = "Hénon x-series with Threshold",
     xlab = "Time", ylab = "x")
abline(h = threshold, col = "red", lwd = 2, lty = 2)
legend("topright", legend = "95th percentile threshold", 
       col = "red", lty = 2, lwd = 2)

## ----extremal-index-----------------------------------------------------------
# Estimate extremal index using different methods
theta_runs <- extremal_index_runs(x_series, threshold, run_length = 3)
theta_intervals <- extremal_index_intervals(x_series, threshold)

cat("Extremal index estimates:\n")
cat("  Runs estimator:", if(length(theta_runs) > 0) theta_runs else "No result", "\n")
cat("  Intervals estimator:", theta_intervals, "\n")

# Cluster analysis
sizes <- cluster_sizes(x_series, threshold, run_length = 3)
if(length(sizes) > 0) {
  cat("\nCluster statistics:\n")
  summary_stats <- cluster_summary(sizes)
  cat("  Number of clusters:", length(sizes), "\n")
  cat("  Mean cluster size:", round(summary_stats[["mean_size"]], 2), "\n")
}

## ----efficiency-comparison----------------------------------------------------
# Compare data usage
n_total <- length(x_series)
n_maxima <- length(maxima)
n_exceedances <- length(exc)

cat("Data usage comparison:\n")
cat("  Total observations:", n_total, "\n")
cat("  Block maxima approach:", n_maxima, "observations (",
    round(100 * n_maxima / n_total, 1), "%)\n")
cat("  POT approach:", n_exceedances, "observations (",
    round(100 * n_exceedances / n_total, 1), "%)\n")

## ----sensitivity-analysis-----------------------------------------------------
# Test sensitivity to block size
block_results <- data.frame(
  block_size = c(25, 50, 100, 200),
  n_maxima = NA,
  max_value = NA,
  mean_maxima = NA
)

for(i in 1:nrow(block_results)) {
  bs <- block_results$block_size[i]
  bm <- block_maxima(x_series, bs)
  block_results$n_maxima[i] <- length(bm)
  block_results$max_value[i] <- max(bm)
  block_results$mean_maxima[i] <- mean(bm)
}

print(block_results)

# Test sensitivity to threshold
threshold_results <- data.frame(
  quantile = c(0.90, 0.95, 0.98, 0.99),
  threshold_value = NA,
  n_exceedances = NA,
  mean_excess = NA
)

for(i in 1:nrow(threshold_results)) {
  q <- threshold_results$quantile[i]
  thr <- quantile(x_series, q)
  exc <- exceedances(x_series, thr)
  threshold_results$threshold_value[i] <- thr
  threshold_results$n_exceedances[i] <- length(exc)
  threshold_results$mean_excess[i] <- if(length(exc) > 0) mean(exc - thr) else NA
}

print(threshold_results)

