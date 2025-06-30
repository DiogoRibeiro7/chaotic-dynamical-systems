#!/usr/bin/env Rscript

# Example script for simple peaks-over-threshold analysis

if (!requireNamespace("chaoticds", quietly = TRUE)) {
  stop("Please install the 'chaoticds' package before running this script.")
}

suppressPackageStartupMessages(library(chaoticds))

set.seed(123)
x <- rnorm(1000)
thr <- 1.5
exc <- exceedances(x, thr)
print(head(exc))

if (requireNamespace("evd", quietly = TRUE) || requireNamespace("evir", quietly = TRUE) ||
    requireNamespace("ismev", quietly = TRUE)) {
  print(fit_gpd(x, thr))
} else {
  message("GPD packages not installed; skipping fit")
}

mrl_df <- mean_residual_life(x, seq(0, 2, by = 0.2))
print(head(mrl_df))
