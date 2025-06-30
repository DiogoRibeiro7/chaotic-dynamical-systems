#!/usr/bin/env Rscript

# Example script for computing simple mixing diagnostics

if (!requireNamespace("chaoticds", quietly = TRUE)) {
  stop("Please install the 'chaoticds' package before running this script.")
}

suppressPackageStartupMessages(library(chaoticds))

set.seed(123)
x <- arima.sim(model = list(ar = 0.6), n = 2000)
lags <- 1:10
print(acf_decay(x, lags))
thr <- quantile(x, 0.95)
print(mixing_coefficients(x, thr, lags))
print(d_check(x, thr, 3))
