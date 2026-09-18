# Lower tail dependence

Convenience wrapper for \[tail_dependence_asymmetric()\] computing lower
tail dependence with possibly different quantile levels.

## Usage

``` r
lower_tail_dependence(x, y, ux, uy)
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

## Value

\[numeric\] Lower tail dependence coefficient.
