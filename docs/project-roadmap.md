# Project roadmap

The canonical roadmap is maintained in [ROADMAP.md](https://github.com/DiogoRibeiro7/chaotic-dynamical-systems/blob/main/ROADMAP.md).

## Current foundation

The package already includes canonical discrete maps, several continuous flows, stochastic perturbations, R/C++ simulation parity, coupled systems, GEV/GPD/point-process/r-largest likelihoods, profile-likelihood inference, extremal-index workflows, Lyapunov spectra, symbolic dynamics, and recurrence quantification.

## Statistical maturity

Remaining v0.2 work includes larger methodology items such as Bayesian posterior inference and sub-asymptotic corrections.

## Dynamics expansion

The v0.3 direction broadens the simulation catalogue while keeping the R-reference/C++-fast-path convention.

## High-dimensional and spatial extremes

Longer-term work includes max-stable processes, multivariate generalized Pareto models, conditional extremes, spatial threshold selection, and functional extremes.

## Frontier directions

Research-oriented targets include conformal methods for extreme quantiles, causal extremes, streaming EVT, differentiable EVT, and carefully validated surrogate models.

## Design constraints

Two principles remain central:

1. the methodology must be defensible before it is convenient;
2. optimized implementations must preserve an inspectable reference specification.
