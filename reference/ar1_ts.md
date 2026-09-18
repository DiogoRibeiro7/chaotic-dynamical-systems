# AR(1) Time Series

A time series generated from an autoregressive model of order 1 with
coefficient 0.7, useful for comparison with chaotic time series.

## Usage

``` r
ar1_ts
```

## Format

A numeric vector with 4000 observations

## Source

Generated using `arima.sim(model = list(ar = 0.7), n = 4000)`

## Details

Generated using the AR(1) model x\[t\] = 0.7 \* x\[t-1\] + epsilon\[t\],
where epsilon\[t\] are independent normal random variables with mean 0
and variance 1. This provides a reference for non-chaotic behavior in
extreme value analysis.

## Examples

``` r
data(ar1_ts)
plot(ar1_ts[1:500], type = "l", main = "AR(1) Time Series")

acf(ar1_ts, main = "AR(1) Autocorrelation Function")
```
