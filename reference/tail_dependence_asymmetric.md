# Asymmetric tail dependence coefficient

Computes a tail dependence coefficient allowing separate thresholds for
the two variables and supporting upper or lower tail analysis.

## Usage

``` r
tail_dependence_asymmetric(x, y, ux, uy, lower = FALSE)
```

## Arguments

- x:

  \[numeric\] Vector of observations.

- y:

  \[numeric\] Vector of the same length as \`x\`.

- ux:

  \[numeric\] Threshold for \`x\`.

- uy:

  \[numeric\] Threshold for \`y\`.

- lower:

  \[logical\] If \`TRUE\` compute lower tail dependence.

## Value

\[numeric\] Tail dependence coefficient in \[0,1\] or \`NA\` if the
denominator is zero.

## Examples

``` r
x <- rnorm(1000)
y <- 0.5 * x + rnorm(1000, sd = 0.5)
tx <- quantile(x, 0.95)
ty <- quantile(y, 0.9)
tail_dependence_asymmetric(x, y, tx, ty)
#> [1] 0.52
```
