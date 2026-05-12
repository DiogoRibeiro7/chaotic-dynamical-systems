#!/usr/bin/env Rscript

# Example usage of block maxima utilities from the chaoticds package

if (!requireNamespace("chaoticds", quietly = TRUE)) {
  stop("Please install the 'chaoticds' package before running this script.")
}

suppressPackageStartupMessages(library(chaoticds))

set.seed(123)
series <- rnorm(1000)
maxima <- block_maxima(series, 50)
print(head(maxima))

# Fit a GEV distribution if fitting packages are available
if (requireNamespace("evd", quietly = TRUE) || requireNamespace("ismev", quietly = TRUE)) {
  print(fit_gev(maxima))
} else {
  message("GEV packages not installed; skipping fit")
}
