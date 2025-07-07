#!/usr/bin/env Rscript

# Generate example datasets for the chaoticds package

library(chaoticds)

# Set seed for reproducibility
set.seed(12345)

# Generate logistic map time series
logistic_ts <- simulate_logistic_map(n = 5000, r = 3.8, x0 = 0.2)

# Generate Henon map trajectory
henon_ts <- simulate_henon_map(n = 3000, a = 1.4, b = 0.3, x0 = 0.1, y0 = 0.1)

# Generate AR(1) data for comparison
ar1_ts <- arima.sim(model = list(ar = 0.7), n = 4000)

# Save the datasets
usethis::use_data(logistic_ts, overwrite = TRUE)
usethis::use_data(henon_ts, overwrite = TRUE)
usethis::use_data(ar1_ts, overwrite = TRUE)

# Generate additional external data files for examples
# Extreme events from logistic map
extreme_threshold <- quantile(logistic_ts, 0.95)
extreme_events <- which(logistic_ts > extreme_threshold)

# Save external data
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

cat("Example datasets generated successfully!\n")
cat("- logistic_ts: ", length(logistic_ts), " observations from logistic map\n")
cat("- henon_ts: ", nrow(henon_ts), " observations from Henon map\n") 
cat("- ar1_ts: ", length(ar1_ts), " observations from AR(1) model\n")
cat("- External data files saved to inst/extdata/\n")