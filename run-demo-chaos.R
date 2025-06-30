#!/usr/bin/env Rscript

# Wrapper script to execute the demo from the chaoticds package
if (!requireNamespace("chaoticds", quietly = TRUE)) {
  stop("Please install the 'chaoticds' package before running this script.")
}

suppressPackageStartupMessages(library(chaoticds))

set.seed(123)
res <- run_demo()
print(res$extremal_index)
