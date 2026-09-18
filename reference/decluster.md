# Decluster threshold exceedances

Identify clusters of exceedances over \`threshold\` using the runs
method and return one summary value per cluster. The returned \`value\`
column is an approximately IID series of cluster representatives
suitable as direct input to \[fit_gpd()\] without violating its
independence assumption.

## Usage

``` r
decluster(
  x,
  threshold,
  run_length = 1L,
  stat = c("max", "first", "last", "sum", "mean")
)
```

## Arguments

- x:

  Numeric vector. The time series to decluster.

- threshold:

  Numeric scalar. Exceedances above this value are clustered.

- run_length:

  Integer (\\\ge 1\\). Maximum time gap between two exceedances assigned
  to the same cluster. Defaults to 1 (only strictly consecutive
  exceedances cluster).

- stat:

  Character. How to reduce each cluster to a single value. One of
  \`"max"\`, \`"first"\`, \`"last"\`, \`"sum"\`, \`"mean"\`.

## Value

A data frame with one row per cluster and columns:

- cluster:

  Integer cluster id, starting at 1.

- start_index:

  Time index of the first exceedance in the cluster.

- end_index:

  Time index of the last exceedance in the cluster.

- n:

  Number of exceedances in the cluster.

- value:

  The cluster representative chosen by \`stat\`.

When no exceedance lies above \`threshold\`, a zero-row data frame with
these columns and the correct types is returned.

## Details

Two consecutive exceedances belong to the same cluster when their
time-index gap is at most \`run_length\`. Each cluster is then reduced
to a single number via \`stat\`:

\- \`"max"\` (default): block-maximum-style representative, the standard
choice for GPD declustering. - \`"first"\` / \`"last"\`: preserve the
timing of cluster onset / decay. - \`"sum"\`: cluster intensity (sum of
exceedances above zero). - \`"mean"\`: average exceedance within the
cluster.

For chaotic dynamical systems extremes typically cluster (extremal index
\\\theta \< 1\\), so applying \`fit_gpd\` directly to raw exceedances
underestimates the scale and biases the shape parameter. Declustering
first restores the IID assumption that GPD asymptotics rest on.

## References

Coles, S. (2001). \*An Introduction to Statistical Modeling of Extreme
Values\*. Springer, Chapter 5.3.

Smith, R. L., & Weissman, I. (1994). Estimating the extremal index.
\*Journal of the Royal Statistical Society: Series B\*, 56(3), 515-528.

## See also

\[fit_gpd()\] for the downstream GPD fit, \[cluster_sizes()\] for the
raw cluster size distribution, \[extremal_index_runs()\] for the
underlying clustering statistic.

## Examples

``` r
x <- simulate_logistic_map(2000, r = 3.8, x0 = 0.2)
u <- quantile(x, 0.95)

dec <- decluster(x, threshold = u, run_length = 2)
head(dec)
#>   cluster start_index end_index n     value
#> 1       1           7         7 1 0.9469523
#> 2       2          39        39 1 0.9452228
#> 3       3          52        52 1 0.9499997
#> 4       4          60        60 1 0.9486507
#> 5       5          82        82 1 0.9474536
#> 6       6         113       113 1 0.9495384

# Declustered cluster maxima feed straight into fit_gpd
if (requireNamespace("evd", quietly = TRUE) && nrow(dec) > 5) {
  fit_gpd(dec$value, threshold = u)
}
#> <chaotic_model>
#>   Model:  gpd
#>   Method: evd::fpot
#>   Threshold: 0.9434633
#>   Parameters:
#>    scale    shape 
#> 0.004147 0.000000 
```
