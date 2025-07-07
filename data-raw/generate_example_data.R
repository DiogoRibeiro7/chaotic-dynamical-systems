#!/usr/bin/env Rscript

# Generate comprehensive example datasets for the chaoticds package

library(chaoticds)

# Set seed for reproducibility
set.seed(12345)

# 1. Core datasets (maintaining backward compatibility)
cat("Generating core datasets...\n")

# Original datasets (for backward compatibility)
logistic_ts <- simulate_logistic_map(n = 5000, r = 3.8, x0 = 0.2)
henon_ts <- simulate_henon_map(n = 3000, a = 1.4, b = 0.3, x0 = 0.1, y0 = 0.1)
ar1_ts <- arima.sim(model = list(ar = 0.7), n = 4000)

# 2. Extended chaotic systems datasets
cat("Generating extended chaotic datasets...\n")

# Multiple logistic map parameter regimes
logistic_chaotic <- simulate_logistic_map(5000, r = 3.8, x0 = 0.2)
logistic_period_doubling <- simulate_logistic_map(5000, r = 3.56995, x0 = 0.2)
logistic_supercritical <- simulate_logistic_map(5000, r = 3.9, x0 = 0.2)

# Multiple Hénon map configurations
henon_classic <- simulate_henon_map(5000, a = 1.4, b = 0.3, x0 = 0, y0 = 0)
henon_alt <- simulate_henon_map(5000, a = 1.2, b = 0.3, x0 = 0.1, y0 = 0.1)

# 3. Extreme value datasets
cat("Generating extreme value datasets...\n")

# GEV distributed data (block maxima)
if (requireNamespace("evd", quietly = TRUE)) {
  gev_data <- evd::rgev(100, loc = 10, scale = 2, shape = 0.1)
  gpd_data <- evd::rgpd(200, scale = 1.5, shape = 0.2)
} else {
  # Fallback generation
  gev_data <- 10 + 2 * (-log(runif(100)))  # Gumbel approximation
  gpd_data <- rexp(200, rate = 1/1.5)      # Exponential approximation
}

# 4. Multivariate datasets
cat("Generating multivariate datasets...\n")

# Coupled logistic maps
n_multi <- 3000
x1 <- numeric(n_multi)
x2 <- numeric(n_multi)
x1[1] <- 0.2
x2[1] <- 0.7
coupling <- 0.1

for(i in 2:n_multi) {
  x1[i] <- 3.8 * x1[i-1] * (1 - x1[i-1]) + coupling * (x2[i-1] - x1[i-1])
  x2[i] <- 3.8 * x2[i-1] * (1 - x2[i-1]) + coupling * (x1[i-1] - x2[i-1])
}
coupled_logistic <- data.frame(x1 = x1, x2 = x2)

# Multivariate normal with correlation
if (requireNamespace("MASS", quietly = TRUE)) {
  rho <- 0.6
  Sigma <- matrix(c(1, rho, rho, 1), 2, 2)
  mv_normal <- MASS::mvrnorm(2000, mu = c(0, 0), Sigma = Sigma)
  colnames(mv_normal) <- c("X1", "X2")
} else {
  # Simple correlated normal
  n_mv <- 2000
  X1 <- rnorm(n_mv)
  X2 <- 0.6 * X1 + sqrt(1 - 0.6^2) * rnorm(n_mv)
  mv_normal <- cbind(X1, X2)
}

# 5. Time series with temporal dependence
cat("Generating time series with dependence...\n")

# AR(1) with heavy tails
n_ar <- 2000
phi <- 0.7
ar_innovations <- rt(n_ar, df = 3)
ar_series <- numeric(n_ar)
ar_series[1] <- ar_innovations[1]

for(i in 2:n_ar) {
  ar_series[i] <- phi * ar_series[i-1] + sqrt(1 - phi^2) * ar_innovations[i]
}

# Financial-like returns (GARCH-inspired)
n_fin <- 1000
returns <- numeric(n_fin)
volatility <- numeric(n_fin)
alpha0 <- 0.01
alpha1 <- 0.1
beta1 <- 0.85
volatility[1] <- sqrt(alpha0)

for(i in 2:n_fin) {
  volatility[i] <- sqrt(alpha0 + alpha1 * returns[i-1]^2 + beta1 * volatility[i-1]^2)
  returns[i] <- volatility[i] * rnorm(1)
}
financial_returns <- returns

# Environmental-like data
n_env <- 365 * 50  # 50 years daily
seasonal_component <- sin(2 * pi * (1:n_env) / 365)
trend_component <- 0.02 * (1:n_env) / 365
noise_component <- arima.sim(list(ar = c(0.8, -0.2)), n = n_env, sd = 0.5)
temperature_anomalies <- 2 * seasonal_component + trend_component + noise_component

# 6. Save all datasets using internal data format
cat("Saving datasets...\n")

# Core datasets (backward compatibility)
if (requireNamespace("usethis", quietly = TRUE)) {
  usethis::use_data(logistic_ts, overwrite = TRUE, internal = FALSE)
  usethis::use_data(henon_ts, overwrite = TRUE, internal = FALSE)
  usethis::use_data(ar1_ts, overwrite = TRUE, internal = FALSE)
  
  # Extended datasets
  usethis::use_data(logistic_chaotic, overwrite = TRUE, internal = FALSE)
  usethis::use_data(logistic_period_doubling, overwrite = TRUE, internal = FALSE)
  usethis::use_data(logistic_supercritical, overwrite = TRUE, internal = FALSE)
  usethis::use_data(henon_classic, overwrite = TRUE, internal = FALSE)
  usethis::use_data(henon_alt, overwrite = TRUE, internal = FALSE)
  usethis::use_data(gev_data, overwrite = TRUE, internal = FALSE)
  usethis::use_data(gpd_data, overwrite = TRUE, internal = FALSE)
  usethis::use_data(coupled_logistic, overwrite = TRUE, internal = FALSE)
  usethis::use_data(mv_normal, overwrite = TRUE, internal = FALSE)
  usethis::use_data(ar_series, overwrite = TRUE, internal = FALSE)
  usethis::use_data(financial_returns, overwrite = TRUE, internal = FALSE)
  usethis::use_data(temperature_anomalies, overwrite = TRUE, internal = FALSE)
} else {
  # Manual save if usethis not available
  if (!dir.exists("data")) dir.create("data")
  save(logistic_ts, file = "data/logistic_ts.rda")
  save(henon_ts, file = "data/henon_ts.rda")
  save(ar1_ts, file = "data/ar1_ts.rda")
  save(logistic_chaotic, file = "data/logistic_chaotic.rda")
  save(logistic_period_doubling, file = "data/logistic_period_doubling.rda")
  save(logistic_supercritical, file = "data/logistic_supercritical.rda")
  save(henon_classic, file = "data/henon_classic.rda")
  save(henon_alt, file = "data/henon_alt.rda")
  save(gev_data, file = "data/gev_data.rda")
  save(gpd_data, file = "data/gpd_data.rda")
  save(coupled_logistic, file = "data/coupled_logistic.rda")
  save(mv_normal, file = "data/mv_normal.rda")
  save(ar_series, file = "data/ar_series.rda")
  save(financial_returns, file = "data/financial_returns.rda")
  save(temperature_anomalies, file = "data/temperature_anomalies.rda")
}

# 7. Generate external data files for examples
cat("Generating external data files...\n")

# Create directories
if (!dir.exists("inst")) dir.create("inst")
if (!dir.exists("inst/extdata")) dir.create("inst/extdata")

# Extreme events from logistic map
extreme_threshold <- quantile(logistic_ts, 0.95)
extreme_events <- which(logistic_ts > extreme_threshold)

write.csv(
  data.frame(
    time = extreme_events,
    value = logistic_ts[extreme_events]
  ),
  file = "inst/extdata/logistic_extremes.csv",
  row.names = FALSE
)

# Block maxima example
block_size <- 50
blocks <- seq(1, length(logistic_ts), by = block_size)
block_maxima_data <- sapply(1:(length(blocks)-1), function(i) {
  max(logistic_ts[blocks[i]:(blocks[i+1]-1)])
})

write.csv(
  data.frame(
    block = 1:length(block_maxima_data),
    maxima = block_maxima_data
  ),
  file = "inst/extdata/logistic_block_maxima.csv",
  row.names = FALSE
)

# Multivariate extremes example
if (is.matrix(mv_normal) || is.data.frame(mv_normal)) {
  write.csv(
    as.data.frame(mv_normal),
    file = "inst/extdata/bivariate_normal.csv",
    row.names = FALSE
  )
}

# Time series summary
dataset_summary <- data.frame(
  dataset = c("logistic_ts", "henon_ts", "ar1_ts", "logistic_chaotic", 
              "logistic_period_doubling", "logistic_supercritical",
              "henon_classic", "henon_alt", "gev_data", "gpd_data",
              "coupled_logistic", "mv_normal", "ar_series", 
              "financial_returns", "temperature_anomalies"),
  type = c("univariate", "bivariate", "univariate", "univariate",
           "univariate", "univariate", "bivariate", "bivariate",
           "univariate", "univariate", "bivariate", "bivariate",
           "univariate", "univariate", "univariate"),
  n_obs = c(length(logistic_ts), nrow(henon_ts), length(ar1_ts),
            length(logistic_chaotic), length(logistic_period_doubling),
            length(logistic_supercritical), nrow(henon_classic), nrow(henon_alt),
            length(gev_data), length(gpd_data), nrow(coupled_logistic),
            nrow(mv_normal), length(ar_series), length(financial_returns),
            length(temperature_anomalies)),
  description = c("Original logistic map (r=3.8)", "Original Hénon map",
                  "AR(1) process", "Chaotic logistic map", 
                  "Period-doubling logistic", "Supercritical logistic",
                  "Classic Hénon map", "Alternative Hénon parameters",
                  "GEV distributed data", "GPD distributed data",
                  "Coupled logistic maps", "Correlated bivariate normal",
                  "AR(1) with heavy tails", "GARCH-like returns",
                  "Environmental time series")
)

write.csv(dataset_summary, file = "inst/extdata/dataset_summary.csv", row.names = FALSE)

# 8. Report completion
cat("Dataset generation complete!\n")
cat("Generated", nrow(dataset_summary), "datasets:\n")
cat("- Core datasets:", length(logistic_ts), "+", nrow(henon_ts), "+", length(ar1_ts), "observations\n")
cat("- Extended datasets:", sum(dataset_summary$n_obs[4:15]), "additional observations\n")
cat("- External data files saved to inst/extdata/\n")
print(dataset_summary[, c("dataset", "type", "n_obs")])