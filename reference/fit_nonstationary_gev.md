# Fit a simple non-stationary GEV model

Estimates a GEV distribution allowing the location parameter to vary
linearly with time if \`trend\` is TRUE. This implementation uses
maximum likelihood from the `ismev` package when available and falls
back to optimisation via
[`stats::optim`](https://rdrr.io/r/stats/optim.html) otherwise.

## Usage

``` r
fit_nonstationary_gev(x, trend = TRUE)
```

## Arguments

- x:

  Numeric vector of block maxima.

- trend:

  Logical. If TRUE include a linear trend in the location parameter.

## Value

List containing estimated parameters.

## Examples

``` r
x <- block_maxima(rnorm(1000), 50)
fit_nonstationary_gev(x)
#> $location
#> [1] 2.065755
#> 
#> $location_trend
#> [1] 0.2221423
#> 
#> $scale
#> [1] 0.3852404
#> 
#> $shape
#> [1] -0.04518581
#> 
#> $convergence
#> [1] 0
#> 
```
