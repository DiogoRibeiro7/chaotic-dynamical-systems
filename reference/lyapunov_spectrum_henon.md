# Lyapunov spectrum of the Henon map

Convenience wrapper around \[lyapunov_spectrum()\] that hard-codes the
Henon-map dynamics and Jacobian. At the canonical parameters (\`a =
1.4\`, \`b = 0.3\`) the spectrum is approximately \\(\lambda_1,
\lambda_2) \approx (0.418, -1.622)\\.

## Usage

``` r
lyapunov_spectrum_henon(
  n_iter = 5000L,
  transient = 1000L,
  a = 1.4,
  b = 0.3,
  x0 = 0,
  y0 = 0
)
```

## Arguments

- n_iter, transient:

  As in \[lyapunov_spectrum()\].

- a, b:

  Henon parameters.

- x0, y0:

  Initial condition.

## Value

Length-2 numeric vector of Lyapunov exponents.

## See also

\[lyapunov_spectrum()\], \[simulate_henon_map()\].

## Examples

``` r
lyapunov_spectrum_henon(n_iter = 2000)
#> [1]  0.4256528 -1.6296256
```
