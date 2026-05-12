#!/usr/bin/env Rscript

# Demonstration of recurrence plots and basic RQA using chaoticds

if (!requireNamespace("chaoticds", quietly = TRUE)) {
  stop("Please install the 'chaoticds' package before running this script.")
}

suppressPackageStartupMessages(library(chaoticds))

data(logistic_ts)

args <- commandArgs(trailingOnly = TRUE)
outfile <- if (length(args) >= 1) args[1] else "recurrence-plot.png"

rp <- recurrence_plot(logistic_ts, embed = 2, delay = 1)
props <- recurrence_analysis(logistic_ts)

cat(sprintf("Recurrence rate: %.3f\n", props$recurrence_rate))
cat(sprintf("Determinism: %.3f\n", props$determinism))

dir.create(dirname(outfile), showWarnings = FALSE, recursive = TRUE)
png(outfile, width = 600, height = 600)
image(rp, main = "Recurrence Plot")
dev.off()
cat(sprintf("Plot saved to %s\n", outfile))
