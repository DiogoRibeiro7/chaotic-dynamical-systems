# Tail dependence coefficient

Computes the upper tail dependence coefficient between two series.

## Usage

``` r
tail_dependence_coefficient(x, y, quantile_level = 0.9)
```

## Arguments

- x:

  Numeric vector.

- y:

  Numeric vector of the same length as \`x\`.

- quantile_level:

  Numeric in (0,1) indicating the high quantile.

## Value

Numeric value in \[0,1\].

## Examples

``` r
x <- rnorm(1000)
y <- x + rnorm(1000, sd = 0.5)
tail_dependence_coefficient(x, y)
#> [1] 0.68
```
