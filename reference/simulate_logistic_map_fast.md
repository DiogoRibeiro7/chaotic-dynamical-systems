# Fast logistic map simulation with automatic method selection

Automatically chooses between R and C++ implementations based on data
size and availability of compiled C++ code.

## Usage

``` r
simulate_logistic_map_fast(n, r, x0, use_cpp = NULL)
```

## Arguments

- n:

  Number of observations to generate

- r:

  Parameter r of the logistic map

- x0:

  Initial value

- use_cpp:

  Logical, whether to force C++ implementation (if available)

## Value

Numeric vector of simulated values
