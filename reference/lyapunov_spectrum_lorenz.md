# Lyapunov spectrum of the Lorenz system

Convenience wrapper around \[lyapunov_spectrum_continuous()\]. At the
canonical parameters (\\\sigma = 10\\, \\\rho = 28\\, \\\beta = 8/3\\)
the spectrum is approximately \\(0.906, 0, -14.572)\\, summing to
\\-\sigma - 1 - \beta\\.

## Usage

``` r
lyapunov_spectrum_lorenz(
  t_max = 1000,
  dt = 0.01,
  qr_interval = 1,
  transient = 50,
  sigma = 10,
  rho = 28,
  beta = 8/3,
  x0 = 1,
  y0 = 1,
  z0 = 1.05
)
```

## Arguments

- t_max, dt, qr_interval, transient:

  As in \[lyapunov_spectrum_continuous()\].

- sigma, rho, beta:

  Lorenz parameters.

- x0, y0, z0:

  Initial condition.

## Value

Length-3 numeric vector of Lyapunov exponents.

## See also

\[lyapunov_spectrum_continuous()\], \[simulate_lorenz()\].

## Examples

``` r
# \donttest{
lyapunov_spectrum_lorenz(t_max = 200)
#> [1]   0.89562972  -0.01200114 -14.55019326
# }
```
