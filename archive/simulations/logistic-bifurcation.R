#!/usr/bin/env Rscript

# Simple wrapper script demonstrating the logistic_bifurcation function

if (!requireNamespace("chaoticds", quietly = TRUE)) {
  stop("Please install the 'chaoticds' package before running this script.")
}

suppressPackageStartupMessages(library(chaoticds))

log_file <- "logistic-bifurcation.log"

with_logging({
  vals <- seq(2.5, 4, length.out = 200)
  bif <- logistic_bifurcation(vals)
  print(head(bif))
}, log_file, msg = 'logistic_bifurcation')
