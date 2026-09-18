# Multivariate extremal index

Estimates a multivariate extremal index for a dataset with two or more
variables. The estimator uses a runs approach applied to joint
exceedances across any component and averages it with the component wise
runs estimators.

## Usage

``` r
extremal_index_multivariate(df, thresholds, run_length = 3L)
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

\[numeric\] Estimated extremal index between 0 and 1, or \`NA\` if no
valid estimates are available.

## Examples

``` r
set.seed(1)
df <- data.frame(a = rnorm(1000), b = rnorm(1000), c = rnorm(1000))
extremal_index_multivariate(df, 0.9)
#> [1] 0.4469696
```
