# Roadmap

> Where **chaoticds** is heading. The Phase-1 milestones from
> [`docs/dev/roadmap.md`](docs/dev/roadmap.md) — simulation of canonical maps,
> block-maxima / POT, threshold diagnostics, bootstrap CIs for θ, cluster
> analysis, mixing checks — are done. This document sets the direction for
> v0.2 and beyond.

## Vision

`chaoticds` should be **the** R reference toolkit for extreme value analysis
on systems with deterministic dynamics. Where `evd`, `ismev`, and `extRemes`
treat the data as IID or weakly dependent draws, `chaoticds` reasons about
where the data *came from* — and exploits that knowledge to produce sharper,
more honest estimates of tail risk under clustering, non-stationarity, and
finite-sample bias.

Two design commitments stay constant:

1. **R reference + C++ fast path.** Every compute-heavy function ships with a
   `_cpp` variant ([CONTRIBUTING.md § C++ fast paths](CONTRIBUTING.md)).
   No black boxes — the R version is the spec.
2. **Methodology first, ergonomics second.** Adding a method we can defend
   in a paper trumps adding a wrapper we can demo on a slide.

---

## Where we are (v0.1)

- **Simulation** — 5 discrete maps (logistic, Hénon, tent, Lozi, cat) and 3
  continuous flows (Lorenz, Rössler, Duffing), all with `_cpp` fast paths.
- **EVT primitives** — block maxima → GEV, peaks-over-threshold → GPD,
  extremal index via runs + intervals + multivariate, block-bootstrap CIs.
- **Cluster workflow** — `cluster_exceedances`, `cluster_sizes`,
  `cluster_summary`, `decluster` (the IID-ish input pipeline for `fit_gpd`),
  `marked_point_process`.
- **Diagnostics** — mean residual life, Hill plot, mixing checks, recurrence
  plots, Lyapunov exponent, correlation dimension.
- **Modelling extras** — non-stationary GEV, tail dependence, return levels,
  goodness-of-fit, compound Poisson MPPs.
- **Surface** — Shiny explorer, 6 vignettes, one-call HTML report,
  `examples/walkthrough.Rmd`, pkgdown site, 20 test files green.

---

## Phase 2 — Statistical maturity (target v0.2)

Close the gap between `chaoticds` and a textbook EVT toolbox. These items
each correct a known weakness in the current fits.

- **Profile-likelihood intervals.** Add `profile_ci()` for GEV/GPD parameters
  and return levels. Wald intervals on the shape ξ are notoriously
  asymmetric and miscalibrated; profile intervals are the standard fix and
  the default in `ismev`. Pairs with the existing bootstrap CIs.
- **Point-process likelihood (PPL).** A unified fit that subsumes BM and POT
  as marginalisations of the same Poisson point process. Coles 2001 §7.4;
  removes the bias-variance tradeoff in threshold choice.
- **r-largest order statistics.** Fit GEV to the top `r` order statistics
  per block, not just the maximum. Useful when blocks are short and the
  maximum alone loses too much information.
- **Bayesian posteriors.** Stan back-end (via `rstan` or `cmdstanr`) for
  full posteriors on GEV/GPD parameters and return levels. Gives
  predictive intervals, not just point estimates and Wald CIs.
- **Penultimate / sub-asymptotic corrections.** Apply Smith's (1987)
  penultimate approximation so finite-block GEV fits are bias-corrected.
- **broom tidiers.** `tidy()`, `glance()`, `augment()` methods for
  `chaotic_model` so EVT fits drop straight into `dplyr` / `ggplot2`
  pipelines. Cheap and high signal-to-noise for adoption.

---

## Phase 3 — Dynamics expansion (target v0.3)

Broaden the chaotic-systems side from "canonical maps + 3 ODEs" to a
genuine simulation catalog.

- **More dynamical systems** — Standard map, Ikeda map, Mackey-Glass
  delay-differential equation, Belousov-Zhabotinsky, coupled map lattices,
  Kuramoto oscillators, Burgers / KdV PDE surrogates.
- **Stochastic perturbations.** Wrap each deterministic map with optional
  additive / multiplicative noise (`simulate_*(..., noise = function(x) ...)`).
  Captures the real-world case where chaotic structure is contaminated.
- **Ensemble simulators.** First-class support for Monte Carlo over initial
  conditions — `ensemble_simulate(map_fn, n_replicates, ...)` returning a
  long-format data frame keyed by replicate.
- **Lyapunov spectrum.** Estimate the full spectrum, not just the largest
  exponent. Enables Kaplan-Yorke dimension and finer chaos diagnostics.
- **Symbolic dynamics.** Partition-based encoders that turn an orbit into
  a symbol sequence (Markov partition where available, generating partition
  otherwise). Opens the door to entropy estimation and itinerary statistics.
- **Recurrence quantification analysis.** Extend `recurrence_analysis` from
  the current handful of statistics to the standard RQA suite (RR, DET,
  LAM, L, ENT, TT, etc.).

---

## Phase 4 — High-dim and spatial extremes (target v0.4)

Move from univariate / bivariate to genuine multivariate and spatial
extreme value theory. This is the hardest tier — these are open research
areas, not textbook material.

- **Max-stable processes.** Brown-Resnick and Schlather model fits over a
  spatial grid. Enables joint return-level estimation across multiple
  monitoring stations.
- **Generalised Pareto on R^d.** Multivariate POT with parametric and
  semi-parametric dependence structures (Pareto processes, Rootzén-Tajvidi).
- **Conditional extremes (Heffernan-Tawn).** Fit conditional extreme value
  models for asymmetric tail dependence. Standard tool when one variable's
  extreme behaviour drives another's.
- **Spatial threshold selection.** Replace per-station threshold choice
  with a spatially-coherent procedure (e.g. Northrop & Coleman 2014).
- **Functional extremes.** Apply functional-data perspectives to time
  series of extreme curves (heatwaves, hurricane intensity profiles).

---

## Phase 5 — Frontier methods (v1.0+)

Things that aren't yet standard but should be, and are within reach of an R
package built on this foundation.

- **Conformal prediction for extreme quantiles.** Assumption-free coverage
  for return-level intervals. The natural antidote to the parametric
  fragility of GEV/GPD.
- **Causal extremes.** Causal-inference tools for extreme events
  (Engelke-Hitz 2020 et al.) — when does X's extreme behaviour *cause*
  Y's, beyond mere tail dependence?
- **Neural surrogates for chaotic systems.** Neural ODE / PINN-style
  emulators trained on `simulate_*` output, exposed as drop-in fast paths
  for very long simulations. Honest about the surrogate-vs-true gap.
- **Diffusion / generative models for extremes.** Generate plausible
  synthetic extreme-event trajectories conditioned on regime parameters.
  Useful for downstream stress-testing.
- **Streaming / online EVT.** Update GEV/GPD fits and extremal-index
  estimates as new data arrives, without re-fitting from scratch. Targets
  real-time monitoring use cases.
- **Differentiable EVT.** Pose the fit as a differentiable program (`RTMB`
  or `torch`) so gradients propagate into upstream models. Enables joint
  training of a chaotic surrogate and its tail summary.

---

## Cross-cutting infrastructure

Not phase-bound — keep these healthy throughout.

- **Performance.** Parallel bootstrap via `future` / `clustermq`. GPU
  variants of the long-simulation paths via `gpuMagic` or similar. Honest
  benchmarks under `benchmarks/`, regression-gated in CI.
- **Numerics.** Move toward arbitrary-precision integration (`Rmpfr`) for
  the chaotic simulators when extreme-precision orbits matter for the EVT
  asymptotics. Optional, behind a flag.
- **Reproducibility.** Vignettes and the `examples/` notebook pinned via
  `renv.lock`; release tags carry full lockfile snapshots.
- **Data.** Real-world example datasets (climate from ERA5, finance from
  Yahoo / Quandl, structural-engineering loads) bundled or fetched lazily.
  Pre-baked example workflows for each domain.
- **C++ coverage.** Continue the convention from CONTRIBUTING.md. Long-term
  target: zero pure-R inner loops for any function called inside a bootstrap.

---

## Community and dissemination

- **CRAN submission.** First release once Phase 2 lands and `R CMD check
  --as-cran` is clean on all platforms.
- **JOSS paper.** Concise software paper once we have a non-trivial set of
  unique methods (decluster + profile CIs + non-stationary + multivariate).
- **Quarto Book.** A full open-access textbook ("EVT for Chaotic Systems")
  that uses the package end-to-end. Bigger ambition: become the de facto
  teaching reference for this corner of EVT.
- **webR / shinyapps.io.** Host the Shiny explorer publicly so users can
  try the package without installing R.
- **Conference presence.** UseR! / EVA / SBSS lightning talks; aim for
  citations as papers land that use the package.

---

## Explicit non-goals

To keep the scope honest, things this package will **not** try to be:

- A general nonlinear time-series library. We deliberately stay focused on
  *extremes* of dynamical systems, not their generic identification, model
  selection, or forecasting. `nonlinearTseries`, `tsDyn`, and `forecast`
  already cover those.
- A SDE / Itô-calculus toolkit. We borrow from stochastic perturbation
  ideas where they touch deterministic-system contamination, but the core
  is deterministic dynamics. Use `sde` / `yuima` for general SDEs.
- A python re-implementation. We may add `reticulate`-based interop for
  ML model loading (Phase 5 neural surrogates), but the API stays R.

---

## How to use this document

- **Contributors:** the phase your work belongs to determines the bar.
  Phase 2 needs to *just work* and be CRAN-ready. Phase 5 can be exploratory.
- **Users:** if a feature you want is in Phase 4 or 5 and you have a
  concrete use case, file an issue — concrete use cases pull items
  forward.
- **Reviewers:** "is this in the roadmap?" is a legitimate review question.
  If it isn't, the PR description should make the case for adding it.

This roadmap is a living document. Revise as evidence arrives.
