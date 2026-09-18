# Group exceedances into clusters via runs method

Group exceedances into clusters via runs method

## Usage

``` r
cluster_exceedances(indices, run_length)
```

## Arguments

- indices:

  Integer vector of sorted exceedance indices.

- run_length:

  Integer maximum gap to join exceedances into one cluster.

## Value

List with elements \`clusters\` and \`n_clusters\`.

## Examples

``` r
x <- simulate_logistic_map(1000, r = 3.8, x0 = 0.2)
exc_idx <- threshold_exceedances(x, threshold = 0.9)
clusters <- cluster_exceedances(exc_idx, run_length = 2)
clusters$n_clusters
#> [1] 198
```
