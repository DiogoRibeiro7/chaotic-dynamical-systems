# Empirical quantile

Computes the empirical quantile of a numeric vector using \`type = 8\`
which is recommended for statistical applications.

## Usage

``` r
empirical_quantile(x, prob)
```

## Arguments

- x:

  Numeric vector of observations.

- prob:

  Numeric probability in \`\[0, 1\]\` specifying the quantile.

## Value

Numeric value giving the empirical quantile.

## Examples

``` r
empirical_quantile(rnorm(100), 0.95)
#> [1] 1.856004
```
