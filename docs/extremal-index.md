# Extremal index and clustering

The extremal index is the main bridge between classical EVT and dependent extremes in chaotic dynamics.

## Limiting interpretation

For suitable thresholds \(u_n\), a stationary dependent process may satisfy

\[
\Pr(M_n\le u_n) \longrightarrow e^{-\theta\tau},
\qquad 0<\theta\le1.
\]

The scalar \(\theta\) is the **extremal index**.

- \(\theta\approx1\): little asymptotic clustering;
- \(\theta<1\): extremes occur in clusters;
- under standard cluster-process conditions, \(1/\theta\) is related to limiting mean cluster size.

That final relation is asymptotic. A finite-sample average cluster size does not have to equal \(1/\theta\).

## Runs estimator

The runs estimator declares exceedances to belong to the same cluster when fewer than a chosen number of non-exceedances separate them.

```r
u <- quantile(x, 0.95)

theta_runs <- extremal_index_runs(
  x,
  threshold = u,
  run_length = 2
)
```

`run_length` is a tuning parameter. Report it and study sensitivity to nearby values.

## Intervals estimator

`extremal_index_intervals()` uses inter-exceedance times rather than an explicit runs rule.

```r
theta_intervals <- extremal_index_intervals(
  x,
  threshold = u
)
```

Agreement between structurally different estimators is useful. Disagreement is also informative because it can expose threshold or cluster-definition sensitivity.

## Inspect the clusters themselves

```r
sizes <- cluster_sizes(
  x,
  threshold = u,
  run_length = 2
)

cluster_summary(sizes)
```

A single \(\theta\) estimate can hide very different cluster-size distributions. Inspect both.

## Bootstrap uncertainty

IID resampling destroys local dependence, so use block-based resampling.

```r
boot <- bootstrap_extremal_index(
  x,
  threshold = u,
  run_length = 2,
  B = 1000
)
```

For larger workloads, use `bootstrap_extremal_index_parallel()`.

## Sensitivity surface

A useful diagnostic is a grid over thresholds and run lengths rather than one selected pair.

```r
grid <- expand.grid(
  probability = seq(0.90, 0.99, by = 0.01),
  run_length = 1:5
)

grid$theta <- mapply(
  function(p, r) {
    u <- quantile(x, p)
    extremal_index_runs(x, u, r)
  },
  grid$probability,
  grid$run_length
)
```

The aim is to identify stable regions, not to search for the most convenient estimate.

## Declustering

`decluster()` constructs representative extremes from clusters. This can be useful before conventional POT fitting, but it changes the effective sample and introduces another modelling rule.

!!! caution
    Declustering should be reported as part of the model, not hidden as preprocessing.

## Minimum reporting standard

Report threshold, estimator, run length or interval rule, exceedance count, number of clusters, cluster-size summary, uncertainty interval, and sensitivity to nearby tuning choices.