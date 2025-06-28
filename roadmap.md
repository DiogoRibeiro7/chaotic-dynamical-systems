# Roadmap for `chaotic-dynamical-systems` Toolbox

This document outlines planned modules and scripts to expand the repository into a comprehensive toolkit for extreme-value analysis in chaotic dynamical systems.

---

## 1. Simulation of Canonical Maps

- **simulate-logistic-map.R** ✅
  Generate orbits for the logistic map $x_{n+1} = r\,x_n(1 - x_n)$ with user-specified parameters.
- **simulate-henon-map.R** ✅
  Two-dimensional Hénon map simulation with tunable parameters (a, b).

## 2. Block-Maxima & POT Methods

- **block-maxima.R** ✅
  Split a time series into non-overlapping blocks, compute maxima, then fit a Generalized Extreme Value (GEV) distribution via packages like `ismev` or `evd`.
- **peaks-over-threshold.R** ✅
  Implement the Peaks-Over-Threshold approach: fit a Generalized Pareto Distribution (GPD) and provide diagnostic plots (mean residual life, parameter stability).

## 3. Threshold Diagnostics

- **threshold-selection.R** ✅
  Compute and plot:
  - Mean Residual Life (MRL) plot
  - Hill plot
  to guide choice of threshold for POT methods.

## 4. Bootstrap Uncertainty

- **bootstrap-ci.R** ✅
  Wrap `extremal_index_runs()` and `extremal_index_intervals()` in a block bootstrap or stationary bootstrap to obtain confidence intervals for the extremal index θ.

## 5. Cluster-Size & Extremal-Cluster Analysis

- **cluster-statistics.R** ✅
  Compute and visualize:
  - Empirical distribution of cluster sizes
  - Run-length histograms
  - Summary statistics (mean, variance) of cluster sizes

## 6. Mixing-Condition Checks

 - **mixing-diagnostics.R** ✅
  Compute and plot:
  - Auto-correlation decay curves  
  - Empirical mixing coefficients  
  - Tests for Leadbetter’s $D(u_n)$ conditions

## 7. End-to-End Demo Script

- **run-demo-chaos.R**  
  Source simulations, threshold diagnostics, block-maxima, POT, extremal-index estimators, and assemble all figures into a single PDF report (e.g., via `rmarkdown::render`).

## 8. Documentation & Vignettes

- **vignettes/**  
  R Markdown examples with narratives such as:
  - “Estimating θ for the Logistic Map”  
  - “Comparison of Block Maxima vs POT for the Hénon Map”

---

This roadmap is designed to guide the incremental development of a comprehensive, well-documented toolkit for extreme-value statistics in chaotic dynamical systems.

