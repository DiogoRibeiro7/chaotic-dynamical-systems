# Plot cluster size distribution

Creates a bar chart of cluster size frequencies.

## Usage

``` r
cluster_histogram(sizes)
```

## Arguments

- sizes:

  Integer vector of cluster sizes.

## Value

A ggplot object showing the distribution.

## Examples

``` r
x <- simulate_logistic_map(1000, r = 3.8, x0 = 0.2)
sizes <- cluster_sizes(x, threshold = 0.9, run_length = 2)
cluster_histogram(sizes)
```
