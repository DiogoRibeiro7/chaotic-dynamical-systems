# Lyapunov spectrum of the Rossler system

Convenience wrapper around \[lyapunov_spectrum_continuous()\]. At the
canonical parameters (\\a = 0.2\\, \\b = 0.2\\, \\c = 5.7\\) the
spectrum is approximately \\(0.0714, 0, -5.392)\\; the small positive
exponent makes Rossler a low-entropy chaotic flow relative to Lorenz.

## Usage

``` r
lyapunov_spectrum_rossler(
  t_max = 2000,
  dt = 0.05,
  qr_interval = 1,
  transient = 100,
  a = 0.2,
  b = 0.2,
  c = 5.7,
  x0 = 0,
  y0 = 1,
  z0 = 0
)
```

## Arguments

- t_max, dt, qr_interval, transient:

  As in \[lyapunov_spectrum_continuous()\].

- a, b, c:

  Rossler parameters.

- x0, y0, z0:

  Initial condition.

## Value

Length-3 numeric vector of Lyapunov exponents.

## See also

\[lyapunov_spectrum_continuous()\], \[simulate_rossler()\].

## Examples

``` r
# \donttest{
lyapunov_spectrum_rossler(t_max = 400)
#> [1]  0.071458857 -0.005298321 -5.370578766
# }
```
