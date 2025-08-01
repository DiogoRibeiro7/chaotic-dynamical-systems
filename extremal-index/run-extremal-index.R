#!/usr/bin/env Rscript

# Example workflow for estimating the extremal index using the chaoticds
# package. The required packages should be installed beforehand using the
# provided setup script or your favourite package manager.

if (!requireNamespace("chaoticds", quietly = TRUE)) {
  stop("Please install the 'chaoticds' package before running this script.")
}

suppressPackageStartupMessages(library(chaoticds))

log_file <- "run-extremal-index.log"

with_logging({

# 4.1 Generate synthetic AR(1) data
set.seed(42)
x <- arima.sim(model = list(ar = 0.7), n = 10000)

# 4.2 Choose a high threshold (e.g. 95th percentile)
thr <- quantile(x, 0.95)

# 4.3 Estimate extremal index via runs (runs length r = 5)
theta_runs <- extremal_index_runs(x, threshold = thr, run_length = 5)
cat("Runs estimator:", theta_runs, "\n")

# 4.4 Estimate via intervals
theta_int <- extremal_index_intervals(x, threshold = thr)
cat("Intervals estimator:", theta_int, "\n")

# 4.5 Compute hitting times & plot survival
times <- hitting_times(x, threshold = thr)
  plot_obj <- plot_hts(times, theta_runs)
  print(plot_obj)
}, log_file, msg = 'extremal_index_example')

