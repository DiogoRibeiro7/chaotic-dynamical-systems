# Dynamical systems

The package treats the data-generating mechanism as part of the statistical model. Simulation is therefore the controlled setting in which clustering, dependence, and asymptotic extreme-value behaviour can be studied.

## Discrete maps

The logistic map is

\[
x_{t+1}=r x_t(1-x_t).
\]

Main functions include \`simulate_logistic_map()\`, \`simulate_logistic_map_cpp()\`, \`logistic_bifurcation()\`, and \`logistic_bifurcation_cpp()\`.

The package also includes Hénon, Lozi, Arnold cat, standard, and Ikeda maps.

## Continuous systems

Continuous-flow simulators include Lorenz, Rössler, and Duffing. Mackey–Glass is represented as a delay-differential system.

## Coupled systems

For the periodic coupled logistic-map lattice,

\[
x_i^{(t+1)}
=
(1-\varepsilon) f(x_i^{(t)})
+
\frac{\varepsilon}{2}
\left[
f(x_{i-1}^{(t)}) + f(x_{i+1}^{(t)})
\right].
\]

For \(N\) globally coupled Kuramoto oscillators,

\[
\frac{d\theta_i}{dt}
=
\omega_i
+
\frac{K}{N}
\sum_{j=1}^{N}
\sin(\theta_j-\theta_i).
\]

The Kuramoto implementation uses the complex order parameter to avoid an explicit \(O(N^2)\) pairwise sum.

## Stochastic perturbations

Discrete simulators can add controlled perturbations through \`noise_sd\`. Where documented, R and C++ implementations preserve parity under the same random seed.

## Ensemble simulation

\`ensemble_simulate()\` evaluates a simulator repeatedly under controlled seeds and returns long-format replicate data.

## Chaos summaries

The package exposes Lyapunov exponents and spectra, correlation dimension, symbolic dynamics, block entropy, and source entropy. These describe the underlying dynamics and complement rather than replace EVT diagnostics.
