#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(chaoticds)
})

if (!dir.exists("benchmark-results")) {
  dir.create("benchmark-results", recursive = TRUE)
}

run_time <- function(expr) {
  as.numeric(system.time(force(expr))[3])
}

set.seed(123)

sim_time <- run_time({
  sim_data <- simulate_logistic_map(10000, r = 3.8, x0 = 0.2)
})

threshold <- as.numeric(quantile(sim_data, 0.95))

ei_time <- run_time({
  theta <- extremal_index_runs(sim_data, threshold, run_length = 3)
})

cluster_time <- run_time({
  sizes <- cluster_sizes(sim_data, threshold, run_length = 3)
})

large_n <- 1000000L
large_series <- simulate_logistic_map(large_n, r = 3.8, x0 = 0.2)
large_threshold <- as.numeric(quantile(large_series, 0.95))

chunked_time <- run_time({
  chunked_summary <- threshold_summary_chunked(
    large_series,
    threshold = large_threshold,
    chunk_size = 100000L
  )
})

timing_results <- data.frame(
  test = c(
    "logistic_sim_10k",
    "extremal_index",
    "cluster_analysis",
    "chunked_exceedance_1m"
  ),
  time_seconds = c(sim_time, ei_time, cluster_time, chunked_time),
  timestamp = as.character(Sys.time()),
  stringsAsFactors = FALSE
)

write.csv(timing_results, "benchmark-results/timing_results.csv", row.names = FALSE)
message("Saved benchmark-results/timing_results.csv")

