# Tail dependence heatmap

Computes pairwise upper tail dependence coefficients and displays them
in a heatmap.

## Usage

``` r
tail_dependence_heatmap(df, quantile_level = 0.9)
```

## Arguments

- df:

  \[data.frame\] or \[matrix\] of numeric columns.

- quantile_level:

  \[numeric\] High quantile level for the tail dependence coefficient.

## Value

\[ggplot2::ggplot\] Heatmap of pairwise tail dependence coefficients.

## Examples

``` r
set.seed(1)
df <- data.frame(a = rnorm(100), b = rnorm(100), c = rnorm(100))
tail_dependence_heatmap(df)
```
