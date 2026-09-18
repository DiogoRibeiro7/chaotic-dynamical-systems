# Compute autocorrelation function

Returns the sample autocorrelation values up to \`max_lag\` using
\`stats::acf\` with \`plot = FALSE\`.

## Usage

``` r
compute_autocorrelation(x, max_lag = 10)
```

## Arguments

- x:

  Numeric vector.

- max_lag:

  Integer. Maximum lag to compute.

## Value

Numeric vector of length \`max_lag + 1\` with autocorrelation values
starting at lag 0.

## Examples

``` r
compute_autocorrelation(rnorm(100), max_lag = 10)
#>  [1]  1.00000000 -0.07714739  0.09569169  0.03349290 -0.05600963  0.13667909
#>  [7] -0.10215524  0.09990824 -0.09576726  0.19130242  0.02102618
```
