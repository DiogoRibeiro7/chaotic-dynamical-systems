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

- **Profile-likelihood intervals.** ✅ `profile_likelihood()`,
  `profile_ci()`, and `profile_return_level()` invert the LRT for GEV
  parameters, GPD parameters, and m-period return levels. Wald intervals
  on ξ and on return levels were the package's most-cited weakness;
  profile-likelihood is the standard fix.
- **Point-process likelihood (PPL).** ✅ `fit_ppp()` wraps the Poisson
  point-process MLE (Coles 2001 §7.4), parameterising directly in
  block-maximum GEV coordinates. PPL profile-likelihood support is a
  follow-up (different likelihood structure than GEV/GPD; needs its own
  log-lik + extractors).
- **r-largest order statistics.** ✅ `block_r_largest()` extracts the
  top r values per block and `fit_gev_rlargest()` fits the joint
  Poisson-process likelihood (Coles 2001 §3.5). Inherits from
  `chaotic_model` so the tidier and profile-likelihood machinery work
  unchanged.
- **broom tidiers.** ✅ `tidy.chaotic_model()`, `glance.chaotic_model()`,
  `augment.chaotic_model()` registered against the `generics` package's
  generics. EVT fits drop straight into dplyr / ggplot2 pipelines.
- **Bayesian posteriors.** Deferred to a focused session. Stan back-end
  (via `rstan` or `cmdstanr`) requires `.stan` model files under
  `inst/stan/`, a posterior-summary class, and likely its own vignette;
  too large a footprint to bundle with the rest of Phase 2.
- **Penultimate / sub-asymptotic corrections.** Deferred. No clean R
  implementation to mirror in the EVT ecosystem (the standard packages
  -- `evd`, `ismev`, `extRemes`, `evir` -- don't ship one); needs
  methodology decisions before code.

---

## Phase 3 — Dynamics expansion (target v0.3)

Broaden the chaotic-systems side from "canonical maps + 3 ODEs" to a
genuine simulation catalog.

- **More dynamical systems.** Partial. ✅ Chirikov standard map, Ikeda
  map, and Mackey-Glass delay-differential equation are in (R + C++
  fast paths). Coupled map lattices, Kuramoto oscillators, Belousov-
  Zhabotinsky, and Burgers / KdV PDE surrogates remain.
- **Stochastic perturbations.** ✅ Every discrete simulator now takes a
  `noise_sd` argument (R + C++); under the same RNG seed the two
  implementations are bit-identical, which made R-vs-C++ parity tests
  possible on noisy orbits.
- **Ensemble simulators.** ✅ `ensemble_simulate(expr, n_replicates,
  seed)` captures any simulator call unevaluated and re-evaluates it per
  replicate, so random sub-expressions like `runif(1)` give per-replicate
  draws automatically. Returns a long-format data frame keyed by
  `replicate`.
- **Lyapunov spectrum.** ✅ Discrete maps via Benettin's QR algorithm
  (`lyapunov_spectrum()` + presets for Henon, Lozi, logistic) and
  continuous flows via the variational equation
  (`lyapunov_spectrum_continuous()` + presets for Lorenz, Rossler). The
  Lorenz preset recovers the textbook (0.906, 0, -14.572) and the trace
  identity \eqn{\sum \lambda = -\sigma - 1 - \beta} holds to 1e-3.
- **Symbolic dynamics.** ✅ `symbolize()` partitions an orbit into a
  symbol sequence (equiprobable or user-supplied breaks); `block_entropy()`
  reports \eqn{H_k} of the resulting word distribution; `source_entropy()`
  estimates the per-symbol entropy via the empirical conditional entropy
  with an automatic undersampling guard. Recovers \eqn{\log 2} for the
  logistic map at \eqn{r = 4} with the generating partition.
- **Recurrence quantification analysis.** ✅ `rqa()` ships the standard
  measures (RR, DET, LAM, L, L_max, TT, V_max, ENT) with a Theiler
  window. The lighter `recurrence_analysis()` (RR + DET only) stays for
  backwards compatibility.

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
