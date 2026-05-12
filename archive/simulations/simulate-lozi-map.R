#!/usr/bin/env Rscript

# Simple wrapper demonstrating simulate_lozi_map from the package

if (!requireNamespace("chaoticds", quietly = TRUE)) {
  stop("Please install the 'chaoticds' package before running this script.")
}

suppressPackageStartupMessages(library(chaoticds))

log_file <- "simulate-lozi-map.log"

args <- commandArgs(trailingOnly = TRUE)
n <- if (length(args) >= 1) as.integer(args[1]) else 100
a <- if (length(args) >= 2) as.numeric(args[2]) else 1.7
b <- if (length(args) >= 3) as.numeric(args[3]) else 0.5
x0 <- if (length(args) >= 4) as.numeric(args[4]) else 0
y0 <- if (length(args) >= 5) as.numeric(args[5]) else 0

with_logging({
  set.seed(123)
  orbit <- simulate_lozi_map(n, a, b, x0, y0)
  print(head(orbit))
}, log_file, msg = 'simulate_lozi_map')
