# Dynamical systems catalogue

The simulation layer is the starting point for most controlled experiments in **chaoticds**. It includes discrete maps, continuous flows, a delay system, a coupled lattice, and a globally coupled oscillator model.

The goal is not to collect every known chaotic system. The goal is to provide representative systems with clear mathematical definitions and predictable software contracts.

## Discrete maps

### Logistic map

The logistic map is

\[
x_{t+1}=r x_t(1-x_t).
\]

At suitable values of \(r\), it moves from fixed points to period doubling and then chaos.

~~~r
x <- simulate_logistic_map(
  n = 5000,
  r = 3.8,
  x0 = 0.2
)
~~~

Related functions include <code>simulate_logistic_map_cpp()</code>, <code>simulate_logistic_map_fast()</code>, <code>simulate_logistic_map_smart()</code>, and the bifurcation helpers.

### Hénon map

The Hénon system is

\[
x_{t+1}=1-a x_t^2+y_t,
\]

\[
y_{t+1}=b x_t.
\]

~~~r
henon <- simulate_henon_map(
  n = 5000,
  a = 1.4,
  b = 0.3
)

plot(
  henon$x,
  henon$y,
  pch = ".",
  xlab = "x",
  ylab = "y"
)
~~~

The two-dimensional state makes it a useful example for studying how the choice of observable changes the resulting extreme-value problem.

### Tent map

The tent map gives a piecewise-linear counterpart to the logistic map. It is useful when the geometry of the dynamics should remain simple while the orbit is chaotic.

Use <code>simulate_tent_map()</code> or <code>simulate_tent_map_cpp()</code>.

### Lozi map

The Lozi map is a piecewise-linear analogue of the Hénon map. It provides a two-dimensional strange attractor with simpler local algebra.

Use <code>simulate_lozi_map()</code> or <code>simulate_lozi_map_cpp()</code>.

### Arnold cat map

The cat map acts on the torus and is useful for experiments involving mixing and recurrence.

Use <code>simulate_cat_map()</code> or <code>simulate_cat_map_cpp()</code>.

### Standard and Ikeda maps

The package also includes the Chirikov standard map and Ikeda map:

- <code>simulate_standard_map()</code>
- <code>simulate_standard_map_cpp()</code>
- <code>simulate_ikeda_map()</code>
- <code>simulate_ikeda_map_cpp()</code>

## Continuous systems

### Lorenz system

The Lorenz equations are

\[
\dot x=\sigma(y-x),
\]

\[
\dot y=x(\rho-z)-y,
\]

\[
\dot z=xy-\beta z.
\]

~~~r
lorenz <- simulate_lorenz(
  t_max = 50,
  dt = 0.01
)
~~~

The package also exposes <code>simulate_lorenz_cpp()</code>.

### Rössler system

The Rössler flow provides another standard low-dimensional chaotic system with a different attractor geometry.

Use <code>simulate_rossler()</code> and <code>simulate_rossler_cpp()</code>.

### Duffing oscillator

The forced Duffing oscillator provides a nonlinear oscillatory system with parameter regimes containing periodic and chaotic responses.

Use <code>simulate_duffing()</code> and <code>simulate_duffing_cpp()</code>.

## Delay dynamics

### Mackey–Glass

Mackey–Glass dynamics provide a delay-differential example in which complex behaviour arises from delayed feedback.

Use <code>simulate_mackey_glass()</code> or <code>simulate_mackey_glass_cpp()</code>.

## Coupled systems

### Coupled logistic-map lattice

For site \(i\),

\[
x_i^{(t+1)}
=
(1-\varepsilon)f(x_i^{(t)})
+
\frac{\varepsilon}{2}
\left[
f(x_{i-1}^{(t)})
+
f(x_{i+1}^{(t)})
\right],
\]

with periodic boundaries.

~~~r
cml <- simulate_coupled_map_lattice(
  n = 2000,
  lattice_size = 32,
  r = 3.8,
  epsilon = 0.15
)
~~~

Use the C++ variant for larger experiments.

### Kuramoto oscillators

For \(N\) globally coupled phase oscillators,

\[
\frac{d\theta_i}{dt}
=
\omega_i
+
\frac{K}{N}
\sum_{j=1}^{N}
\sin(\theta_j-\theta_i).
\]

~~~r
kuramoto <- simulate_kuramoto(
  n = 3000,
  n_oscillators = 64,
  coupling = 2
)
~~~

Synchronization can be summarized with <code>kuramoto_order_parameter()</code>.

The implementation uses the complex order-parameter identity, reducing the coupling calculation from a naive \(O(N^2)\) sum to \(O(N)\) work per derivative evaluation.

## Stochastic perturbations

Several discrete simulators accept <code>noise_sd</code>. This makes it possible to study how deterministic dynamics change under controlled observational or dynamical perturbations.

Where R/C++ parity is promised, the random-number handling is designed so that both implementations can be compared under the same seed.

## Ensembles

<code>ensemble_simulate()</code> evaluates a simulator repeatedly under controlled seeds and returns replicate-labelled data.

This is useful for:

- finite-sample estimator studies;
- sensitivity to initial conditions;
- parameter sweeps;
- Monte Carlo comparisons of EVT procedures.

## Dynamical diagnostics

Simulation can be followed by:

- <code>estimate_lyapunov_exponent()</code>;
- <code>lyapunov_spectrum()</code>;
- <code>lyapunov_spectrum_continuous()</code>;
- system-specific Lyapunov-spectrum helpers;
- <code>recurrence_plot()</code>;
- <code>rqa()</code>;
- <code>estimate_correlation_dimension()</code>;
- symbolic-dynamics and entropy tools.

## Choosing a starting system

| Goal | Suggested model |
|---|---|
| Simple scalar chaos | Logistic map |
| Two-dimensional strange attractor | Hénon map |
| Piecewise-linear dynamics | Tent or Lozi map |
| Hyperbolic-style torus dynamics | Arnold cat map |
| Continuous chaotic flow | Lorenz or Rössler |
| Forced nonlinear oscillator | Duffing |
| Delayed feedback | Mackey–Glass |
| Spatial coupling | Coupled map lattice |
| Synchronization | Kuramoto model |

After generating a trajectory, continue with [Extreme value analysis](../extreme-value-analysis.md) or [Diagnostics](../diagnostics.md).
