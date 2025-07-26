#!/usr/bin/env Rscript

# Demonstration of recurrence plots and basic RQA using chaoticds

if (!requireNamespace("chaoticds", quietly = TRUE)) {
  stop("Please install the 'chaoticds' package before running this script.")
}

suppressPackageStartupMessages(library(chaoticds))

data(logistic_ts)

rp <- recurrence_plot(logistic_ts, embed = 2, delay = 1)
props <- recurrence_analysis(logistic_ts)

cat(sprintf("Recurrence rate: %.3f\n", props$recurrence_rate))
cat(sprintf("Determinism: %.3f\n", props$determinism))

if (interactive()) {
  image(rp, main = "Recurrence Plot")
}
