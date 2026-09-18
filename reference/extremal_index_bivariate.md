# Bivariate wrapper for backward compatibility

Calls \[extremal_index_multivariate()\] for the first two columns of
\`df\`.

## Usage

``` r
extremal_index_bivariate(df, thresholds, run_length = 3L)
```

## Arguments

- df:

  \[data.frame\] or \[matrix\] with numeric columns.

- thresholds:

  \[numeric\] Vector of length equal to \`ncol(df)\` or a single
  threshold applied to all columns.

- run_length:

  \[integer\] Run parameter for the runs estimator.

## Value

\[numeric\] Estimated extremal index for the first two columns or
\`NA\`.

## See also

\[extremal_index_multivariate()\]

## Examples

``` r
set.seed(1)
df <- data.frame(a = rnorm(1000), b = rnorm(1000))
extremal_index_bivariate(df, 0.9)
#> [1] 0.4622549
```
