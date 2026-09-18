# Lyapunov exponent of the logistic map

One-dimensional special case of \[lyapunov_spectrum()\] for the logistic
map. At \`r = 4\` the Lyapunov exponent is the textbook value \\\log 2
\approx 0.693\\.

## Usage

``` r
lyapunov_spectrum_logistic(
  n_iter = 5000L,
  transient = 1000L,
  r = 3.8,
  x0 = 0.2
)
```

## Arguments

- n_iter, transient:

  As in \[lyapunov_spectrum()\].

- r:

  Logistic parameter.

- x0:

  Initial condition in (0, 1).

## Value

Scalar Lyapunov exponent.

## See also

\[lyapunov_spectrum()\], \[simulate_logistic_map()\].

## Examples

``` r
lyapunov_spectrum_logistic(n_iter = 5000, r = 4)
#> [1] 0.6926562
```
