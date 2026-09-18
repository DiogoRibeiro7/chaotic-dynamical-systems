# Logistic map bifurcation diagram

Generate data for the classic logistic map bifurcation diagram by
iterating the map for a sequence of parameter values.

## Usage

``` r
logistic_bifurcation(r_seq, n_iter = 200, discard = 100, x0 = 0.2)
```

## Arguments

- r_seq:

  Numeric vector of \\r\\ parameters to evaluate.

- n_iter:

  Integer. Number of iterations for each parameter.

- discard:

  Integer. Number of initial iterations to discard as transient. Must be
  less than `n_iter`.

- x0:

  Numeric. Initial value in (0, 1) for the orbit.

## Value

Data frame with columns \`r\` and \`x\` containing orbit values after
the transient period for each parameter in \`r_seq\`.

## Examples

``` r
r_vals <- seq(2.5, 4, length.out = 200)
bifdat <- logistic_bifurcation(r_vals, n_iter = 200, discard = 100)
plot(bifdat$r, bifdat$x, pch = '.', cex = 0.5)
```
