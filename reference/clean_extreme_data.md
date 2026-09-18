# Data cleaning for extreme value analysis

Removes missing and infinite values from a numeric vector.

## Usage

``` r
clean_extreme_data(x)
```

## Arguments

- x:

  Numeric vector possibly containing \`NA\`, \`Inf\`, or \`-Inf\`.

## Value

Numeric vector with non-finite entries removed. If the result is empty,
a numeric vector of length zero is returned.

## Examples

``` r
clean_extreme_data(c(1, NA, 2, Inf, 3))
#> [1] 1 2 3
```
