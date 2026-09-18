# Lyapunov spectrum of a continuous-time flow

Estimates the full Lyapunov spectrum of an autonomous ODE \\\dot x =
f(x)\\ by integrating the orbit and the variational equation \\\dot V =
J(x) V\\ jointly with a fixed-step RK4 scheme, QR-decomposing the
tangent basis every \`qr_interval\` time units, and accumulating \\\log
\|\det R\_{ii}\|\\.

## Usage

``` r
lyapunov_spectrum_continuous(
  deriv_fn,
  jac_fn,
  x0,
  t_max = 1000,
  dt = 0.01,
  qr_interval = 1,
  transient = 100
)
```

## Arguments

- deriv_fn:

  Function with signature \`function(t, x)\` returning the length-\`d\`
  derivative \`dx/dt\`.

- jac_fn:

  Function with signature \`function(t, x)\` returning the \\d \times
  d\\ Jacobian.

- x0:

  Numeric vector of length \`d\`. Initial state.

- t_max:

  Numeric (\\\> 0\\). Integration time over which the spectrum is
  averaged, after the transient.

- dt:

  Numeric (\\\> 0\\). RK4 step size.

- qr_interval:

  Numeric (\\\> 0\\). Time between QR re-orthogonalisations.

- transient:

  Numeric (\\\ge 0\\). Orbit-only integration time discarded from the
  start.

## Value

Numeric vector of length \`d\` holding the Lyapunov exponents in
decreasing order.

## Details

Standard Benettin/Wolf approach for flows. The orbit and the \\d \times
d\\ tangent matrix \\V\\ are advanced with the same RK4 step, sharing
intermediate evaluations of the Jacobian at the half- and full-step
states. QR re-orthogonalisation prevents \\V\\ from collapsing onto the
leading expanding direction; the time-averaged diagonal logs of \\R\\
give the spectrum.

Sum check: for Lorenz with the canonical parameters the spectrum is
approximately \\(0.906, 0, -14.572)\\ with sum \\-\sigma - 1 - \beta =
-13.667\\; for Rossler at \\(0.2, 0.2, 5.7)\\ it is approximately
\\(0.0714, 0, -5.392)\\. The middle exponent is identically zero in the
direction of the flow.

## References

Wolf, A., Swift, J. B., Swinney, H. L., & Vastano, J. A. (1985).
Determining Lyapunov exponents from a time series. \*Physica D\*, 16(3),
285-317.
[doi:10.1016/0167-2789(85)90011-9](https://doi.org/10.1016/0167-2789%2885%2990011-9)

## See also

\[lyapunov_spectrum()\] for the discrete-map version,
\[lyapunov_spectrum_lorenz()\] and \[lyapunov_spectrum_rossler()\] for
hard-coded presets.

## Examples

``` r
# \donttest{
# Lorenz spectrum at canonical (10, 28, 8/3).
lyapunov_spectrum_lorenz(t_max = 200)
#> [1]   0.89562972  -0.01200114 -14.55019326
# }
```
