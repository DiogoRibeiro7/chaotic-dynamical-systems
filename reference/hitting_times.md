# Compute hitting/return times

Compute hitting/return times

## Usage

``` r
hitting_times(x, threshold)
```

## Arguments

- x:

  Numeric vector of observations.

- threshold:

  Numeric threshold value.

## Value

Numeric vector of gaps between exceedances. If fewer than two
exceedances are present the function returns an empty vector.

## Examples

``` r
x <- simulate_logistic_map(1000, r = 3.8, x0 = 0.2)
hts <- hitting_times(x, threshold = 0.9)
head(hts)
#> [1] 4 3 6 4 4 5
```
