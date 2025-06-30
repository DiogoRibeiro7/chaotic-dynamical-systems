#!/usr/bin/env Rscript

# Example script demonstrating bootstrap confidence intervals for the extremal index

if (!requireNamespace("chaoticds", quietly = TRUE)) {
  stop("Please install the 'chaoticds' package before running this script.")
}

suppressPackageStartupMessages(library(chaoticds))

set.seed(123)
x <- arima.sim(model = list(ar = 0.7), n = 2000)
thr <- quantile(x, 0.95)
res <- bootstrap_extremal_index(x, thr, estimator = "runs", run_length = 5,
                                 block_size = 50, B = 200)
print(res$ci)
