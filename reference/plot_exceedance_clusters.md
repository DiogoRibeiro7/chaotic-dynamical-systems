# Plot bivariate exceedance clusters

Visualizes exceedance clusters for two variables, colouring points by
cluster membership.

## Usage

``` r
plot_exceedance_clusters(df, thresholds, run_length = 3L)
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

\[ggplot2::ggplot\] Scatter plot with clusters coloured.

## Examples

``` r
set.seed(1)
df <- data.frame(a = rnorm(100), b = rnorm(100))
plot_exceedance_clusters(df, 0.9)
```
