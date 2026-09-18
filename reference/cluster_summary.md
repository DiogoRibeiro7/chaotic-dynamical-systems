# Summary statistics for cluster sizes

Computes the mean and variance of cluster sizes.

## Usage

``` r
cluster_summary(sizes)
```

## Arguments

- sizes:

  Integer vector of cluster sizes.

## Value

Named numeric vector with elements \`mean_size\` and \`var_size\`.

## Examples

``` r
x <- simulate_logistic_map(1000, r = 3.8, x0 = 0.2)
sizes <- cluster_sizes(x, threshold = 0.9, run_length = 2)
cluster_summary(sizes)
#> mean_size  var_size 
#>         1         0 
```
