# Lyapunov spectrum of the Lozi map

Convenience wrapper around \[lyapunov_spectrum()\] that hard-codes the
Lozi-map dynamics and Jacobian. At the canonical parameters (\`a =
1.7\`, \`b = 0.5\`) the spectrum is approximately \\(\lambda_1,
\lambda_2) \approx (0.47, -1.16)\\.

## Usage

``` r
lyapunov_spectrum_lozi(
  n_iter = 5000L,
  transient = 1000L,
  a = 1.7,
  b = 0.5,
  x0 = 0.1,
  y0 = 0.1
)
```

## Arguments

- n_iter, transient:

  As in \[lyapunov_spectrum()\].

- a, b:

  Lozi parameters.

- x0, y0:

  Initial condition.

## Value

Length-2 numeric vector of Lyapunov exponents.

## See also

\[lyapunov_spectrum()\], \[simulate_lozi_map()\].

## Examples

``` r
lyapunov_spectrum_lozi(n_iter = 2000)
#> [1]  0.4708993 -1.1640465
```
