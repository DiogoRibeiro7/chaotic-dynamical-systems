# Upper tail dependence

Convenience wrapper for \[tail_dependence_asymmetric()\] computing upper
tail dependence with possibly different quantile levels.

## Usage

``` r
upper_tail_dependence(x, y, ux, uy)
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

\[numeric\] Upper tail dependence coefficient.
