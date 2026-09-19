# Extreme values from deterministic dynamics

<div class="hero" markdown>

<div markdown>

**chaoticds** is an R package for a specific problem: studying rare and extreme events when the observations come from a chaotic dynamical system rather than an IID sample.

It joins simulation, extreme-value inference, clustering, dynamical diagnostics, and optimized C++ paths in one workflow.

[Get started](getting-started.md){ .md-button .md-button--primary }
[Browse the API](reference.md){ .md-button }
[View on GitHub](https://github.com/DiogoRibeiro7/chaotic-dynamical-systems){ .md-button }

</div>

<img src="https://raw.githubusercontent.com/DiogoRibeiro7/chaotic-dynamical-systems/main/man/figures/logo.png" alt="chaoticds logo">

</div>

<div class="metric-strip">
  <div><strong>Dynamics</strong> maps, flows, delay systems, coupled oscillators</div>
  <div><strong>EVT</strong> GEV, GPD, point process, r-largest</div>
  <div><strong>Dependence</strong> extremal index, clusters, mixing, tail dependence</div>
  <div><strong>Engineering</strong> R reference implementations with C++ fast paths</div>
</div>

## Why this package exists

Classical EVT is often introduced with independent observations. A chaotic orbit is deterministic, but successive states are not independent. That distinction matters most in the tail: extreme observations can arrive in clusters, and the effective frequency of extreme episodes can differ from the raw exceedance count.

For a stationary process with maxima

\[
M_n = \max(X_1,\ldots,X_n),
\]

the IID benchmark gives, under the usual normalization,

\[
\Pr(M_n \le u_n) \longrightarrow e^{-\tau}.
\]

Under suitable dependence conditions the limit becomes

\[
\Pr(M_n \le u_n) \longrightarrow e^{-\theta\tau},
\qquad 0 < \theta \le 1,
\]

where \(\theta\) is the **extremal index**. The package is built around this interaction between the generating dynamics and the extreme-value model.

[:octicons-arrow-right-24: Read the conceptual introduction](concepts/why-chaos-extremes.md)

## What is implemented

<div class="grid cards" markdown>

-   **Simulation**

    Logistic, Hénon, tent, Lozi, Arnold cat, standard and Ikeda maps; Lorenz, Rössler and Duffing flows; Mackey–Glass delay dynamics; coupled logistic-map lattices; Kuramoto oscillators.

    [:octicons-arrow-right-24: Dynamical systems](dynamical-systems.md)

-   **Extreme-value inference**

    Block maxima + GEV, peaks over threshold + GPD, Poisson point-process likelihood, r-largest order statistics, non-stationary GEV, return levels, and profile likelihood.

    [:octicons-arrow-right-24: EVT methods](extreme-value-analysis.md)

-   **Clustering and dependence**

    Runs and intervals extremal-index estimators, cluster extraction, declustering, block-bootstrap uncertainty, multivariate extremal indices, and upper/lower tail dependence.

    [:octicons-arrow-right-24: Extremal clustering](extremal-index.md)

-   **Dynamics diagnostics**

    Lyapunov exponents and spectra, correlation dimension, recurrence plots and RQA, symbolic dynamics, block entropy, source entropy, ACF decay, and mixing diagnostics.

    [:octicons-arrow-right-24: Diagnostics](diagnostics.md)

-   **Performance**

    The readable R implementation is the specification. Expensive operations have C++ counterparts where useful, with parity tests and workload-aware wrappers.

    [:octicons-arrow-right-24: Engineering](performance.md)

-   **End-to-end workflows**

    The package includes bundled example datasets, vignettes, a Shiny explorer, reporting helpers, and high-level workflows for reproducible analyses.

    [:octicons-arrow-right-24: Logistic-map tutorial](getting-started.md)

</div>

## The analysis pipeline

~~~mermaid
flowchart LR
    A[Generate or observe a trajectory] --> B{Define extreme events}
    B -->|Block maxima| C[GEV / r-largest]
    B -->|High threshold| D[GPD / point process]
    A --> E[Dependence diagnostics]
    E --> F[Extremal index and clusters]
    A --> G[Dynamical diagnostics]
    C --> H[Return levels and uncertainty]
    D --> H
    F --> H
    G --> I[Joint interpretation]
    H --> I
~~~

The package does not treat the EVT fit as an isolated last step. Threshold selection, cluster structure, mixing behaviour, and the dynamics of the orbit belong in the same analysis.

## First analysis in a few lines

~~~r
library(chaoticds)

x <- simulate_logistic_map(
  n = 5000,
  r = 3.8,
  x0 = 0.2
)

u <- quantile(x, 0.95)

theta <- extremal_index_runs(
  x,
  threshold = u,
  run_length = 2
)

gpd <- fit_gpd(x, threshold = u)

theta
summary(gpd)
~~~

That example is intentionally small. A defensible analysis should also examine threshold sensitivity, cluster stability, goodness of fit, and uncertainty.

[:octicons-arrow-right-24: Follow the full workflow](getting-started.md)

## Method map

| Question | Main functions |
|---|---|
| How do I simulate the system? | <code>simulate_*()</code>, <code>ensemble_simulate()</code> |
| How do maxima behave? | <code>block_maxima()</code>, <code>fit_gev()</code>, <code>block_r_largest()</code>, <code>fit_gev_rlargest()</code> |
| How do threshold exceedances behave? | <code>exceedances()</code>, <code>fit_gpd()</code>, <code>fit_ppp()</code> |
| Do extremes cluster? | <code>extremal_index_runs()</code>, <code>extremal_index_intervals()</code>, <code>cluster_sizes()</code> |
| Is my threshold defensible? | <code>mean_residual_life()</code>, <code>hill_estimates()</code>, <code>threshold_diagnostics()</code> |
| How uncertain are the tail quantities? | <code>bootstrap_extremal_index()</code>, <code>profile_likelihood()</code>, <code>profile_ci()</code> |
| Is the orbit actually chaotic? | <code>estimate_lyapunov_exponent()</code>, <code>lyapunov_spectrum_*()</code>, <code>rqa()</code> |
| Is there joint tail dependence? | <code>upper_tail_dependence()</code>, <code>lower_tail_dependence()</code>, <code>extremal_index_multivariate()</code> |

## Where to go next

If you are new to the package, start with [Getting started](getting-started.md). If you already know EVT, the most package-specific material is [Why extremes in chaotic systems?](concepts/why-chaos-extremes.md), [Extremal index and clustering](extremal-index.md), and the [engineering architecture](reference.md).
