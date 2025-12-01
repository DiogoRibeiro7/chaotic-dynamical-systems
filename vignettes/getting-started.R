## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  fig.align = "center",
  warning = FALSE,
  message = FALSE
)

## ----installation, eval = FALSE-----------------------------------------------
# # From GitHub (development version)
# devtools::install_github("DiogoRibeiro7/chaotic-dynamical-systems")
# 
# # From CRAN (when available)
# install.packages("chaoticds")

## ----load-package-------------------------------------------------------------
library(chaoticds)

## ----simulate-logistic--------------------------------------------------------
# Generate 1000 iterations with r = 3.8 (chaotic regime)
series <- simulate_logistic_map(n = 1000, r = 3.8, x0 = 0.2)

# Visualize
plot(series, type = "l", main = "Logistic Map Time Series (r = 3.8)",
     xlab = "Iteration", ylab = "x", col = "steelblue")

## ----parameter-exploration, fig.height = 6------------------------------------
par(mfrow = c(2, 2))

# r = 2.5: Convergence to fixed point
series1 <- simulate_logistic_map(500, r = 2.5, x0 = 0.2)
plot(series1, type = "l", main = "r = 2.5 (Fixed Point)", ylab = "x", col = "darkgreen")

# r = 3.2: Period-2 oscillation
series2 <- simulate_logistic_map(500, r = 3.2, x0 = 0.2)
plot(series2, type = "l", main = "r = 3.2 (Periodic)", ylab = "x", col = "orange")

# r = 3.8: Chaos
series3 <- simulate_logistic_map(500, r = 3.8, x0 = 0.2)
plot(series3, type = "l", main = "r = 3.8 (Chaotic)", ylab = "x", col = "red")

# r = 4.0: Fully chaotic
series4 <- simulate_logistic_map(500, r = 4.0, x0 = 0.2)
plot(series4, type = "l", main = "r = 4.0 (Fully Chaotic)", ylab = "x", col = "purple")

par(mfrow = c(1, 1))

## ----bifurcation--------------------------------------------------------------
# Generate bifurcation data
r_values <- seq(2.5, 4, length.out = 500)
bif_data <- logistic_bifurcation(r_values, n_iter = 300, discard = 250)

# Plot
plot(bif_data$r, bif_data$x, pch = ".", cex = 0.3,
     main = "Logistic Map Bifurcation Diagram",
     xlab = "Parameter r", ylab = "x",
     col = rgb(0, 0, 1, 0.3))

## ----block-maxima-------------------------------------------------------------
# Use chaotic series from r = 3.8
block_size <- 50
bm <- block_maxima(series, block_size)

cat("Original series length:", length(series), "\n")
cat("Number of block maxima:", length(bm), "\n")
cat("Block maxima range: [", round(min(bm), 3), ",", round(max(bm), 3), "]\n")

# Visualize distribution
hist(bm, breaks = 15, col = "lightblue", border = "white",
     main = "Distribution of Block Maxima",
     xlab = "Block Maximum Value", prob = TRUE)
lines(density(bm), col = "darkblue", lwd = 2)

