#!/usr/bin/env Rscript

# Benchmark multivariate extremal index estimators

library(chaoticds)
library(microbenchmark)

set.seed(123)
# combine logistic map with two additional simulated series
n <- 2000
x <- simulate_logistic_map(n, r = 3.8, x0 = 0.2)
y <- simulate_logistic_map(n, r = 3.8, x0 = 0.4)
z <- simulate_logistic_map(n, r = 3.8, x0 = 0.6)

multi_df <- data.frame(x = x, y = y, z = z)
thresholds <- apply(multi_df, 2, quantile, probs = 0.95)

bench <- microbenchmark(
  univariate = extremal_index_runs(x, thresholds[1], run_length = 3),
  multivariate = extremal_index_multivariate(multi_df, thresholds, run_length = 3),
  times = 50
)
print(bench)
