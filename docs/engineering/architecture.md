# Package architecture

The package is structured around four scientific layers and one engineering rule.

## High-level design

~~~mermaid
flowchart TB
    API[Public R API] --> SIM[Simulation]
    API --> EVT[EVT inference]
    API --> DEP[Dependence and clustering]
    API --> DYN[Dynamical diagnostics]
    API --> APP[Reporting and explorer]

    SIM --> R[R reference implementations]
    EVT --> R
    DEP --> R
    DYN --> R

    R --> CPP[C++ fast paths]
    R --> TEST[Parity and statistical tests]

    EVT --> MODEL[chaotic_model objects]
    MODEL --> PROF[Profile likelihood]
    MODEL --> TIDY[tidy / glance / augment]
~~~

## Simulation layer

The main modules include:

- <code>R/simulate.R</code>
- <code>R/simulate-continuous.R</code>
- <code>R/simulate-dde.R</code>
- <code>R/coupled-map-lattice.R</code>
- <code>R/kuramoto.R</code>
- <code>R/ensemble.R</code>

The R implementation describes the intended numerical algorithm and API contract.

## Extreme-value layer

Core modules include:

- <code>R/block-maxima.R</code>
- <code>R/peaks-over-threshold.R</code>
- <code>R/rlargest.R</code>
- <code>R/mppexcesses.R</code>
- <code>R/profile-likelihood.R</code>
- <code>R/advanced-extremes.R</code>

Together they cover block maxima, POT, point-process, r-largest, non-stationary, return-level, and profile-likelihood functionality.

## Dependence and clustering layer

Key modules are:

- <code>R/extremal-index.R</code>
- <code>R/cluster-statistics.R</code>
- <code>R/bootstrap-ci.R</code>
- <code>R/mixing-diagnostics.R</code>
- <code>R/multivariate-extremes.R</code>

This layer handles the difference between raw tail observations and effective extreme episodes.

## Dynamical diagnostics layer

The dynamics-facing analysis lives in:

- <code>R/lyapunov.R</code>
- <code>R/lyapunov-spectrum.R</code>
- <code>R/fractal-dimension.R</code>
- <code>R/recurrence-analysis.R</code>
- <code>R/rqa.R</code>
- <code>R/symbolic-dynamics.R</code>

## Model API

Fitted extreme-value models use a common <code>chaotic_model</code> interface.

Methods registered through <code>generics</code> include:

- <code>tidy()</code>
- <code>glance()</code>
- <code>augment()</code>

This makes fitted objects easier to integrate into downstream analysis while preserving their model-specific likelihoods.

## The central engineering rule

> R defines the specification. C++ accelerates the specification.

This matters because an optimized implementation should not quietly change:

- update order;
- boundary checks;
- random-number handling;
- likelihood parameterization;
- support conditions.

## Documentation layers

| Layer | Purpose |
|---|---|
| MkDocs | concepts, methods, tutorials, architecture |
| R help | exact function arguments, return values, examples |
| Vignettes | executable long-form analyses |
| README | repository overview |
| Tests | executable behavioural contract |

The public documentation should therefore explain *why* and *how*. Roxygen remains the exact API reference.
