# Performance

The engineering rule is simple: **R is the readable reference implementation, C++ is the fast path**.

## R and C++ parity

Many compute-heavy functions have a \`_cpp\` counterpart. Tests should verify numerical parity to an explicitly chosen tolerance.

## Smart and fast wrappers

Some operations expose \`_fast\` or \`_smart\` variants that select an implementation appropriate to the workload while retaining the same statistical target.

## Complexity matters

For the Kuramoto model, a naive globally coupled derivative requires \(O(N^2)\) work. Using the complex order parameter,

\[
R e^{i\Psi}
=
\frac{1}{N}
\sum_{j=1}^{N}
e^{i\theta_j},
\]

reduces the coupling calculation to \(O(N)\).

Algorithmic structure usually matters more than translating an inefficient loop from R to C++.

## Benchmarking

Use \`benchmark_implementations()\` and \`performance_analysis()\` for package-level comparisons. Benchmarks should compare equal outputs, use sufficiently large problem sizes, and report variability.
