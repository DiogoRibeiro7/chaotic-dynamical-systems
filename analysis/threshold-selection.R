#!/usr/bin/env Rscript

# Example script demonstrating MRL and Hill diagnostics for threshold selection

if (!requireNamespace("chaoticds", quietly = TRUE)) {
  stop("Please install the 'chaoticds' package before running this script.")
}

suppressPackageStartupMessages(library(chaoticds))

set.seed(123)
x <- rexp(1000)
diag <- threshold_diagnostics(x, seq(0, 2, by = 0.2), 1:30)
print(head(diag$mrl))
print(head(diag$hill))
