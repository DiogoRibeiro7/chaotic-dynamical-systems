# Identify indices of threshold exceedances

Identify indices of threshold exceedances

## Usage

``` r
threshold_exceedances(x, threshold)
```

## Arguments

- x:

  Numeric vector of observations.

- threshold:

  Numeric threshold value.

## Value

Integer vector of indices where \`x \> threshold\`.

## Examples

``` r
x <- simulate_logistic_map(1000, r = 3.8, x0 = 0.2)
exc_idx <- threshold_exceedances(x, threshold = 0.9)
head(exc_idx)
#> [1]  3  7 10 16 20 24
```
