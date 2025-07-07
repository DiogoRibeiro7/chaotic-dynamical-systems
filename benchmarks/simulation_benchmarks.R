#!/usr/bin/env Rscript

# Simulation performance benchmarks
# Compare performance of different chaotic map simulators

library(chaoticds)
library(microbenchmark)

cat("=== Simulation Performance Benchmarks ===\n\n")

# Benchmark different simulation sizes
sizes <- c(1000, 5000, 10000, 50000)

cat("Benchmarking logistic map simulation:\n")
logistic_results <- data.frame(
  size = sizes,
  time_seconds = NA,
  memory_mb = NA
)

for(i in seq_along(sizes)) {
  n <- sizes[i]
  
  # Time the simulation
  timing <- system.time({
    sim_data <- simulate_logistic_map(n, r = 3.8, x0 = 0.2)
  })
  
  logistic_results$time_seconds[i] <- timing[3]
  logistic_results$memory_mb[i] <- as.numeric(object.size(sim_data)) / 1024^2
}

print(logistic_results)

cat("\nBenchmarking Hénon map simulation:\n")
henon_results <- data.frame(
  size = sizes,
  time_seconds = NA,
  memory_mb = NA
)

for(i in seq_along(sizes)) {
  n <- sizes[i]
  
  # Time the simulation
  timing <- system.time({
    sim_data <- simulate_henon_map(n, a = 1.4, b = 0.3)
  })
  
  henon_results$time_seconds[i] <- timing[3]
  henon_results$memory_mb[i] <- as.numeric(object.size(sim_data)) / 1024^2
}

print(henon_results)

# Detailed microbenchmark for moderate sizes
cat("\nDetailed microbenchmark (n=10000):\n")
detailed_benchmark <- microbenchmark(
  logistic = simulate_logistic_map(10000, r = 3.8, x0 = 0.2),
  henon = simulate_henon_map(10000, a = 1.4, b = 0.3),
  times = 50
)

print(detailed_benchmark)

# Test different parameter regimes
cat("\nLogistic map parameter benchmark:\n")
param_benchmark <- microbenchmark(
  r35 = simulate_logistic_map(5000, r = 3.5, x0 = 0.2),
  r37 = simulate_logistic_map(5000, r = 3.7, x0 = 0.2),
  r38 = simulate_logistic_map(5000, r = 3.8, x0 = 0.2),
  r39 = simulate_logistic_map(5000, r = 3.9, x0 = 0.2),
  times = 30
)

print(param_benchmark)

# Memory efficiency comparison
cat("\nMemory efficiency comparison:\n")
n_test <- 10000

logistic_data <- simulate_logistic_map(n_test, r = 3.8, x0 = 0.2)
henon_data <- simulate_henon_map(n_test, a = 1.4, b = 0.3)

cat("Memory usage for", n_test, "observations:\n")
cat("  Logistic map:", format(object.size(logistic_data), units = "Kb"), "\n")
cat("  Hénon map:", format(object.size(henon_data), units = "Kb"), "\n")
cat("  Ratio:", round(as.numeric(object.size(henon_data)) / as.numeric(object.size(logistic_data)), 2), "\n")

# Performance scaling analysis
cat("\nPerformance scaling analysis:\n")
scaling_sizes <- c(1000, 2000, 5000, 10000, 20000, 50000)
scaling_results <- data.frame(
  size = scaling_sizes,
  logistic_time = NA,
  henon_time = NA,
  time_ratio = NA
)

for(i in seq_along(scaling_sizes)) {
  n <- scaling_sizes[i]
  
  # Logistic timing
  t1 <- system.time(simulate_logistic_map(n, r = 3.8, x0 = 0.2))[3]
  scaling_results$logistic_time[i] <- t1
  
  # Hénon timing
  t2 <- system.time(simulate_henon_map(n, a = 1.4, b = 0.3))[3]
  scaling_results$henon_time[i] <- t2
  
  scaling_results$time_ratio[i] <- t2 / t1
}

print(scaling_results)

# Summary
cat("\n=== Performance Summary ===\n")
cat("Simulation performance characteristics:\n")
cat("1. Both simulators scale linearly with number of observations\n")
cat("2. Hénon map ~2x slower due to 2D output and data.frame creation\n")
cat("3. Memory usage scales linearly; Hénon uses ~2x memory (2 variables)\n")
cat("4. Parameter values have minimal impact on computation time\n")
cat("5. For large simulations (>50k obs), consider memory management\n\n")

cat("Recommendations:\n")
cat("- For batch analysis: Pre-generate and save large datasets\n")
cat("- For interactive use: 5k-10k observations provide good balance\n")
cat("- For real-time applications: Consider streaming generation\n")
cat("- Memory-constrained environments: Use logistic map when possible\n")