# Extremal index

For a stationary dependent process, extremes can appear in clusters. The extremal index \(\theta\in(0,1]\) summarizes this clustering in the limiting distribution of maxima.

Roughly, \(\theta=1\) is consistent with no asymptotic clustering, while smaller values correspond to stronger clustering. Under standard conditions, \(1/\theta\) is related to the limiting mean cluster size.

## Runs estimator

\`extremal_index_runs()\` groups exceedances into clusters using a runs rule. A C++ implementation is available through \`extremal_index_runs_cpp()\`.

## Intervals estimator

\`extremal_index_intervals()\` uses inter-exceedance times instead of a direct runs declaration.

## Bootstrap uncertainty

\`bootstrap_extremal_index()\` estimates uncertainty with a block bootstrap so that local dependence is not destroyed by IID resampling. For larger workloads, use \`bootstrap_extremal_index_parallel()\`.

## Declustering

Relevant functions include \`cluster_exceedances()\`, \`cluster_sizes()\`, \`cluster_summary()\`, \`decluster()\`, and \`marked_point_process()\`.

Declustering changes the effective sample and introduces tuning choices, so the rule should be reported explicitly.

## Multivariate extremal dependence

\`extremal_index_bivariate()\`, \`extremal_index_multivariate()\`, and the tail-dependence utilities describe joint extreme behaviour across components and through time.
