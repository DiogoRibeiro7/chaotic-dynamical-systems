# Fast logistic bifurcation diagram data (C++ implementation)

Generate bifurcation diagram data efficiently.

## Usage

``` r
logistic_bifurcation_cpp(r_values, n_iter = 200L, discard = 100L, x0 = 0.2)
```

## Arguments

- r_values:

  Numeric vector of r parameters

- n_iter:

  Number of iterations per r value

- discard:

  Number of initial iterations to discard

- x0:

  Initial value

## Value

DataFrame with r and x columns
