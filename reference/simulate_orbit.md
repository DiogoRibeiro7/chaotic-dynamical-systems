# Simulate Orbit of a One-Dimensional Map

Simulate Orbit of a One-Dimensional Map

## Usage

``` r
simulate_orbit(map_fn, init, n_iter)
```

## Arguments

- map_fn:

  Function(x) returning numeric scalar. The dynamical map f:
  X-\>\[0,1\].

- init:

  Numeric scalar in domain of f. Initial condition.

- n_iter:

  Integer \> 0. Number of iterations.

## Value

Numeric vector of length n_iter+1 (includes init). Orbit values.

## Examples

``` r
orb <- simulate_orbit(function(x) 4*x*(1-x), init=0.1, n_iter=1e5)
```
