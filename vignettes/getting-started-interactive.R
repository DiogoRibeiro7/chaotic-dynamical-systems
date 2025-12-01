## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  warning = FALSE,
  message = FALSE
)

## ----load-package-------------------------------------------------------------
library(chaoticds)

# Generate a logistic map time series
set.seed(123)
n <- 2000
logistic_data <- simulate_logistic_map(n, r = 3.8, x0 = 0.2)

# Quick look at the data
head(logistic_data)
summary(logistic_data)

## ----time-series-plot---------------------------------------------------------
# Basic time series plot
plot(1:length(logistic_data), logistic_data, type = "l", 
     main = "Logistic Map Time Series (r = 3.8)",
     xlab = "Time", ylab = "Value", col = "steelblue")

## ----histogram----------------------------------------------------------------
# Distribution of values
hist(logistic_data, breaks = 30, main = "Distribution of Logistic Map Values",
     xlab = "Value", col = "lightblue", border = "black")

