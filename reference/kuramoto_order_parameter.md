# Kuramoto synchronization order parameter

Computes the magnitude of the complex Kuramoto order parameter for one
phase vector or for every row of a phase matrix.

## Usage

``` r
kuramoto_order_parameter(theta)
```

## Arguments

- theta:

  Numeric vector or matrix of oscillator phases in radians.

## Value

A numeric scalar for a vector input, or one value per matrix row. Values
lie in \[0, 1\], where 1 indicates complete phase synchronization.

## Examples

``` r
kuramoto_order_parameter(c(0, pi))
#> [1] 6.123234e-17
kuramoto_order_parameter(matrix(c(0, 0, 0, pi), nrow = 2, byrow = TRUE))
#> [1] 1.000000e+00 6.123234e-17
```
