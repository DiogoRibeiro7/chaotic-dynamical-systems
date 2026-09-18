# Benchmark R vs C++ implementations

Compare performance of R and C++ implementations for different data
sizes.

## Usage

``` r
benchmark_implementations(sizes = c(1000, 5000, 10000, 50000), n_reps = 10)
```

## Arguments

- sizes:

  Vector of data sizes to test

- n_reps:

  Number of repetitions for timing

## Value

Data frame with benchmark results
