#!/usr/bin/env Rscript
# Simulate the Arnold cat map and save the orbit.

library(chaoticds)

log_file <- "simulate-cat-map.log"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  n <- 100
} else {
  n <- as.integer(args[1])
}

with_logging({
  orbit <- simulate_cat_map(n)
  print(head(orbit))
}, log_file, msg = 'simulate_cat_map')
