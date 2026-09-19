# Performance and C++

Performance work in **chaoticds** follows two rules:

1. improve the algorithm before micro-optimizing syntax;
2. keep a readable R reference implementation.

## C++ fast paths

Several expensive routines have C++ counterparts, including simulation, block maxima, exceedance extraction, cluster calculations, and extremal-index operations.

The existence of a C++ implementation does not imply that it is always faster. For small inputs, call overhead can dominate.

## Fast and smart wrappers

Some operations expose workload-aware wrappers:

- <code>simulate_logistic_map_fast()</code>
- <code>simulate_logistic_map_smart()</code>
- <code>block_maxima_fast()</code>
- <code>block_maxima_smart()</code>
- <code>extremal_index_runs_fast()</code>
- <code>extremal_index_runs_smart()</code>

These choose an implementation while preserving the same statistical target.

## Example: Kuramoto coupling

A direct evaluation of

\[
\frac{K}{N}
\sum_{j=1}^{N}
\sin(\theta_j-\theta_i)
\]

for every \(i\) requires \(O(N^2)\) work per derivative evaluation.

Define

\[
Re^{i\Psi}
=
\frac{1}{N}
\sum_{j=1}^{N}
e^{i\theta_j}.
\]

Then

\[
\frac{1}{N}
\sum_{j=1}^{N}
\sin(\theta_j-\theta_i)
=
R\sin(\Psi-\theta_i),
\]

reducing the coupling evaluation to \(O(N)\).

This is the preferred kind of optimization: change the computational complexity first, then compile the hot path.

## Benchmarking

Use:

~~~r
benchmark_implementations()
~~~

and:

~~~r
performance_analysis()
~~~

A benchmark should compare implementations that produce the same object.

## What a useful benchmark reports

At minimum:

- input size;
- repeated timings;
- median or robust summary;
- variability;
- output equivalence;
- environment information.

One elapsed time is not a performance study.

## CI and timing noise

Shared CI runners are noisy. Performance tests should detect large regressions, not enforce brittle exact speed ratios.

Correctness should remain a hard requirement. Timing is usually a softer signal.

## Memory behaviour

For large trajectories, memory allocation may dominate runtime.

Useful strategies include:

- preallocation;
- avoiding copies;
- chunked computation;
- reducing temporary objects;
- using C++ only where it changes the actual bottleneck.

## Reproducibility

When stochastic perturbations are present, random-number generation must be controlled before comparing R and C++ paths.

Where parity is documented, both implementations should be testable under the same seed.
