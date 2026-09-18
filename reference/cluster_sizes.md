# Cluster size utilities

Provides tools to compute the distribution of cluster sizes of threshold
exceedances, summarize them, and visualize the distribution.

## Usage

``` r
cluster_sizes(x, threshold, run_length)
```

## Arguments

- x:

  Numeric vector containing the time series.

- threshold:

  Numeric threshold defining exceedances.

- run_length:

  Integer specifying the maximum gap between exceedances that belongs to
  the same cluster.

## Value

Various depending on the function; see details below.

## Examples

``` r
# Simulate logistic map
x <- simulate_logistic_map(1000, r = 3.8, x0 = 0.2)

# Compute cluster sizes
sizes <- cluster_sizes(x, threshold = 0.9, run_length = 2)
head(sizes)
#> [1] 1 1 1 1 1 1
```
