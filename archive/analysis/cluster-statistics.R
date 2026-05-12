#!/usr/bin/env Rscript

# Example script for computing cluster size statistics of exceedances

if (!requireNamespace("chaoticds", quietly = TRUE)) {
  stop("Please install the 'chaoticds' package before running this script.")
}

suppressPackageStartupMessages(library(chaoticds))

set.seed(123)
x <- arima.sim(model = list(ar = 0.5), n = 2000)
thr <- quantile(x, 0.95)
sizes <- cluster_sizes(x, thr, 3)
print(head(sizes))
print(cluster_summary(sizes))
