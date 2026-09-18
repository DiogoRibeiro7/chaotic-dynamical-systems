# Estimate simple mixing coefficients

Computes empirical alpha-mixing coefficients using indicator functions
of threshold exceedances separated by specified lags.

## Usage

``` r
mixing_coefficients(x, threshold, lags)
```

## Arguments

- x:

  Numeric vector containing the time series.

- threshold:

  Numeric exceedance threshold.

- lags:

  Integer vector of lags at which to estimate coefficients.

## Value

Numeric vector of estimated coefficients corresponding to each lag. For
lags greater than the series length the function returns \`NA\`.

## Examples

``` r
x <- simulate_logistic_map(1000, r = 3.8, x0 = 0.2)
mix_coef <- mixing_coefficients(x, threshold = 0.9, lags = 1:10)
plot(1:10, mix_coef, type = "b", xlab = "Lag", ylab = "Mixing Coefficient")
```
