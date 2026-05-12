#!/usr/bin/env Rscript

# Simple wrapper demonstrating simulate_logistic_map from the package

if (!requireNamespace("chaoticds", quietly = TRUE)) {
  stop("Please install the 'chaoticds' package before running this script.")
}

suppressPackageStartupMessages(library(chaoticds))

log_file <- "simulate-logistic-map.log"

args <- commandArgs(trailingOnly = TRUE)
n <- if (length(args) >= 1) as.integer(args[1]) else 100
r <- if (length(args) >= 2) as.numeric(args[2]) else 3.8
x0 <- if (length(args) >= 3) as.numeric(args[3]) else 0.2

with_logging({
  set.seed(123)
  series <- simulate_logistic_map(n, r, x0)
  print(head(series))
}, log_file, msg = 'simulate_logistic_map')
