# Mixing diagnostics utilities

Provides functions to compute auto-correlation decay, estimate simple
mixing coefficients, and check Leadbetter's D(un) condition for extreme
value theory applications.

## Usage

``` r
acf_decay(x, lags)
```

## Arguments

- x:

  Numeric vector containing the time series.

- lags:

  Integer vector of lags for correlation or mixing calculations.

## Value

Depends on the function. For \`acf_decay\` this is a numeric vector of
autocorrelation values. Returns \`NA\` when \`lags\` contains values
larger than the length of the series.

## Examples

``` r
# Simulate logistic map
x <- simulate_logistic_map(1000, r = 3.8, x0 = 0.2)

# Compute ACF decay
acf_vals <- acf_decay(x, lags = 1:10)
plot(1:10, acf_vals, type = "b", xlab = "Lag", ylab = "ACF")
```
