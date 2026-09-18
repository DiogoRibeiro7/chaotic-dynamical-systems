# Package index

## Simulation Functions

Functions for generating chaotic time series

- [`simulate_logistic_map()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_logistic_map.md)
  : Simulate Logistic Map Dynamics
- [`simulate_henon_map()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_henon_map.md)
  : Simulate the Hénon map
- [`simulate_tent_map()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_tent_map.md)
  : Simulate the tent map
- [`simulate_lozi_map()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_lozi_map.md)
  : Simulate the Lozi map
- [`simulate_cat_map()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_cat_map.md)
  : Simulate the Arnold cat map
- [`simulate_coupled_map_lattice()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_coupled_map_lattice.md)
  [`simulate_coupled_map_lattice_cpp()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_coupled_map_lattice.md)
  : Simulate a coupled map lattice
- [`simulate_kuramoto()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_kuramoto.md)
  [`simulate_kuramoto_cpp()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_kuramoto.md)
  : Simulate globally coupled Kuramoto oscillators
- [`simulate_standard_map()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_standard_map.md)
  : Simulate the Chirikov standard map
- [`simulate_ikeda_map()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_ikeda_map.md)
  : Simulate the Ikeda map
- [`simulate_lorenz()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_lorenz.md)
  : Simulate the Lorenz system
- [`simulate_rossler()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_rossler.md)
  : Simulate the Rossler system
- [`simulate_duffing()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_duffing.md)
  : Simulate the forced Duffing oscillator
- [`simulate_mackey_glass()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_mackey_glass.md)
  : Simulate the Mackey-Glass delay-differential equation
- [`logistic_bifurcation()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/logistic_bifurcation.md)
  : Logistic map bifurcation diagram
- [`ensemble_simulate()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/ensemble_simulate.md)
  : Run a simulator across multiple replicates
- [`kuramoto_order_parameter()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/kuramoto_order_parameter.md)
  : Kuramoto synchronization order parameter

## Extreme Value Analysis

Core extreme value theory functions

- [`block_maxima()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/block_maxima.md)
  : Extract Block Maxima from Time Series
- [`block_r_largest()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/block_r_largest.md)
  : Extract the r largest order statistics in each block
- [`exceedances()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/exceedances.md)
  : Identify exceedances above a threshold
- [`fit_gev()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/fit_gev.md)
  : Fit Generalized Extreme Value Distribution to Block Maxima
- [`fit_gev_rlargest()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/fit_gev_rlargest.md)
  : Fit GEV via the r-largest order statistics method
- [`fit_gpd()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/fit_gpd.md)
  : Fit a Generalized Pareto Distribution (GPD)
- [`fit_ppp()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/fit_ppp.md)
  : Fit a Poisson point-process likelihood (PPL) model

## Threshold Diagnostics

Tools for threshold selection and validation

- [`mean_residual_life()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/mean_residual_life.md)
  : Mean Residual Life (MRL) values
- [`mrl_plot()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/mrl_plot.md)
  : Plot Mean Residual Life (MRL)
- [`hill_estimates()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/hill_estimates.md)
  : Hill estimator across k values
- [`hill_plot()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/hill_plot.md)
  : Plot Hill estimates
- [`threshold_diagnostics()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/threshold_diagnostics.md)
  : Threshold selection diagnostics

## Extremal Index Estimation

Functions for estimating the extremal index

- [`extremal_index_runs()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/extremal_index_runs.md)
  : Estimate Extremal Index Using Runs Method
- [`extremal_index_intervals()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/extremal_index_intervals.md)
  : Intervals estimator of extremal index (Ferro & Segers)
- [`threshold_exceedances()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/threshold_exceedances.md)
  : Identify indices of threshold exceedances
- [`cluster_exceedances()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/cluster_exceedances.md)
  : Group exceedances into clusters via runs method
- [`hitting_times()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/hitting_times.md)
  : Compute hitting/return times
- [`plot_hts()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/plot_hts.md)
  : Plot empirical hitting time survival vs exponential

## Cluster Analysis

Tools for analyzing extreme event clustering

- [`cluster_sizes()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/cluster_sizes.md)
  : Cluster size utilities
- [`cluster_summary()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/cluster_summary.md)
  : Summary statistics for cluster sizes
- [`cluster_histogram()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/cluster_histogram.md)
  : Plot cluster size distribution
- [`decluster()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/decluster.md)
  : Decluster threshold exceedances

## Statistical Diagnostics

Functions for assessing mixing and dependence

- [`acf_decay()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/acf_decay.md)
  : Mixing diagnostics utilities
- [`mixing_coefficients()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/mixing_coefficients.md)
  : Estimate simple mixing coefficients
- [`d_check()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/d_check.md)
  : Check D(un) condition
- [`estimate_lyapunov_exponent()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/estimate_lyapunov_exponent.md)
  : Estimate the Lyapunov exponent of a logistic map
- [`estimate_correlation_dimension()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/estimate_correlation_dimension.md)
  : Estimate correlation dimension
- [`lyapunov_spectrum()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/lyapunov_spectrum.md)
  : Lyapunov spectrum of a discrete map
- [`lyapunov_spectrum_henon()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/lyapunov_spectrum_henon.md)
  : Lyapunov spectrum of the Henon map
- [`lyapunov_spectrum_lozi()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/lyapunov_spectrum_lozi.md)
  : Lyapunov spectrum of the Lozi map
- [`lyapunov_spectrum_logistic()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/lyapunov_spectrum_logistic.md)
  : Lyapunov exponent of the logistic map
- [`lyapunov_spectrum_continuous()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/lyapunov_spectrum_continuous.md)
  : Lyapunov spectrum of a continuous-time flow
- [`lyapunov_spectrum_lorenz()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/lyapunov_spectrum_lorenz.md)
  : Lyapunov spectrum of the Lorenz system
- [`lyapunov_spectrum_rossler()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/lyapunov_spectrum_rossler.md)
  : Lyapunov spectrum of the Rossler system
- [`rqa()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/rqa.md)
  : Recurrence Quantification Analysis (full RQA suite)

## Symbolic Dynamics

Partition encoders and information-theoretic summaries

- [`symbolize()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/symbolize.md)
  : Encode a numeric orbit as a symbol sequence
- [`block_entropy()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/block_entropy.md)
  : Shannon entropy of length-k words in a symbol sequence
- [`source_entropy()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/source_entropy.md)
  : Estimate source entropy from a symbol sequence

## Bootstrap Methods

Uncertainty quantification for extremal index

- [`bootstrap_extremal_index()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/bootstrap_extremal_index.md)
  : Bootstrap confidence intervals for the extremal index
- [`block_bootstrap()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/block_bootstrap.md)
  : Moving block bootstrap resampler

## Profile-Likelihood Inference

Likelihood-ratio confidence intervals for GEV, r-largest, GPD, and PPL
fits

- [`profile_likelihood()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/profile_likelihood.md)
  : Profile-likelihood inference for an extreme-value fit
- [`profile_ci()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/profile_ci.md)
  : Profile-likelihood confidence intervals for an extreme-value fit
- [`profile_return_level()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/profile_return_level.md)
  : Profile-likelihood interval for an extreme-value return level

## broom Tidiers

tidy() / glance() / augment() methods for chaotic_model fits

- [`tidy(`*`<chaotic_model>`*`)`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/tidy.chaotic_model.md)
  : Tidy a chaoticds GEV or GPD fit
- [`glance(`*`<chaotic_model>`*`)`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/glance.chaotic_model.md)
  : One-row summary of a chaoticds GEV or GPD fit
- [`augment(`*`<chaotic_model>`*`)`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/augment.chaotic_model.md)
  : Augment a chaoticds GEV or GPD fit with fitted CDF / survival
  columns

## Advanced Analysis

Higher-level modelling utilities

- [`extremal_index_multivariate()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/extremal_index_multivariate.md)
  : Multivariate extremal index
- [`adaptive_threshold_selection()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/adaptive_threshold_selection.md)
  : Adaptive threshold selection
- [`fit_nonstationary_gev()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/fit_nonstationary_gev.md)
  : Fit a simple non-stationary GEV model
- [`tail_dependence_coefficient()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/tail_dependence_coefficient.md)
  : Tail dependence coefficient
- [`tail_dependence_asymmetric()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/tail_dependence_asymmetric.md)
  : Asymmetric tail dependence coefficient
- [`upper_tail_dependence()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/upper_tail_dependence.md)
  : Upper tail dependence
- [`lower_tail_dependence()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/lower_tail_dependence.md)
  : Lower tail dependence
- [`plot_exceedance_clusters()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/plot_exceedance_clusters.md)
  : Plot bivariate exceedance clusters
- [`tail_dependence_heatmap()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/tail_dependence_heatmap.md)
  : Tail dependence heatmap
- [`spectral_analysis_extremes()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/spectral_analysis_extremes.md)
  : Spectral analysis of extremes
- [`calculate_return_levels()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/calculate_return_levels.md)
  : Calculate return levels from GPD fit
- [`validate_extreme_model()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/validate_extreme_model.md)
  : Validate extreme value model
- [`goodness_of_fit_test()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/goodness_of_fit_test.md)
  : Goodness-of-fit test for GPD exceedances

## Utility Functions

General helpers for scripts and examples

- [`clean_extreme_data()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/clean_extreme_data.md)
  : Data cleaning for extreme value analysis
- [`empirical_quantile()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/empirical_quantile.md)
  : Empirical quantile
- [`compute_autocorrelation()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/compute_autocorrelation.md)
  : Compute autocorrelation function
- [`with_logging()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/with_logging.md)
  : Evaluate an expression with error logging

## Comprehensive Analysis

High-level analysis functions

- [`run_demo()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/run_demo.md)
  : Complete Extreme Value Analysis Workflow for Chaotic Systems

## Example Datasets

Built-in datasets for demonstrations

- [`logistic_ts`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/logistic_ts.md)
  : Logistic Map Time Series
- [`henon_ts`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/henon_ts.md)
  : Hénon Map Trajectory
- [`ar1_ts`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/ar1_ts.md)
  : AR(1) Time Series

## Other API

Additional exported helpers and low-level interfaces

- [`acf_cpp()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/acf_cpp.md)
  : Fast ACF computation (C++ implementation)
- [`advanced_extremes`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/advanced_extremes.md)
  : Advanced extreme value analysis utilities
- [`benchmark_implementations()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/benchmark_implementations.md)
  : Benchmark R vs C++ implementations
- [`block_maxima_cpp()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/block_maxima_cpp.md)
  : Fast block maxima computation (C++ implementation)
- [`block_maxima_fast()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/block_maxima_fast.md)
  : Fast block maxima computation with automatic method selection
- [`block_maxima_from_file()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/block_maxima_from_file.md)
  : Memory-efficient block maxima for very large datasets
- [`block_maxima_smart()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/block_maxima_smart.md)
  : Smart block maxima computation
- [`bootstrap_extremal_index_parallel()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/bootstrap_extremal_index_parallel.md)
  : Parallel bootstrap extremal index
- [`bootstrap_samples_cpp()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/bootstrap_samples_cpp.md)
  : Fast bootstrap sample generation (C++ implementation)
- [`cluster_sizes_cpp()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/cluster_sizes_cpp.md)
  : Fast cluster size computation (C++ implementation)
- [`ecdf_cpp()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/ecdf_cpp.md)
  : Fast empirical CDF (C++ implementation)
- [`exceedance_indices()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/exceedance_indices.md)
  : Identify Exceedance Indices
- [`exceedances_cpp()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/exceedances_cpp.md)
  : Fast exceedances extraction (C++ implementation)
- [`extremal_index`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/extremal_index.md)
  : Extremal index and hitting time utilities
- [`extremal_index_bivariate()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/extremal_index_bivariate.md)
  : Bivariate wrapper for backward compatibility
- [`extremal_index_chunked()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/extremal_index_chunked.md)
  : Chunked processing for large datasets
- [`extremal_index_intervals_cpp()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/extremal_index_intervals_cpp.md)
  : Fast extremal index intervals estimator (C++ implementation)
- [`extremal_index_runs_cpp()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/extremal_index_runs_cpp.md)
  : Fast extremal index estimation using runs method (C++
  implementation)
- [`extremal_index_runs_fast()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/extremal_index_runs_fast.md)
  : Fast extremal index estimation with automatic method selection
- [`extremal_index_runs_smart()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/extremal_index_runs_smart.md)
  : Smart extremal index estimation
- [`fast-functions`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/fast-functions.md)
  : Fast simulation and analysis functions
- [`fit_compound_poisson()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/fit_compound_poisson.md)
  : Fit Compound Poisson Process
- [`inter_exceedance_times_cpp()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/inter_exceedance_times_cpp.md)
  : Fast inter-exceedance times (C++ implementation)
- [`launch_explorer()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/launch_explorer.md)
  : Launch Interactive Extreme Value Explorer
- [`logistic_bifurcation_cpp()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/logistic_bifurcation_cpp.md)
  : Fast logistic bifurcation diagram data (C++ implementation)
- [`marked_point_process()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/marked_point_process.md)
  : Build Marked Point Process
- [`mean_excess_cpp()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/mean_excess_cpp.md)
  : Fast threshold stability diagnostic (C++ implementation)
- [`moving_average_cpp()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/moving_average_cpp.md)
  : Fast moving average (C++ implementation)
- [`multivariate_extreme_workflow()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/multivariate_extreme_workflow.md)
  : End-to-end multivariate extremes workflow
- [`multivariate_extremes`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/multivariate_extremes.md)
  : Multivariate Extreme Value Utilities
- [`optimized_wrappers`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/optimized_wrappers.md)
  : Optimized Wrapper Functions
- [`performance_analysis()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/performance_analysis.md)
  : Performance analysis report
- [`plot(`*`<chaotic_model>`*`)`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/plot.chaotic_model.md)
  : Plot chaoticds model fits
- [`plot(`*`<profile_likelihood>`*`)`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/plot.profile_likelihood.md)
  : Plot a profile-likelihood curve
- [`print(`*`<bootstrap_ei>`*`)`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/print.bootstrap_ei.md)
  : Print method for bootstrap_ei
- [`print(`*`<chaotic_model>`*`)`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/print.chaotic_model.md)
  : Print chaoticds model summary header
- [`print(`*`<profile_likelihood>`*`)`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/print.profile_likelihood.md)
  : Print method for profile-likelihood objects
- [`print(`*`<summary.chaotic_model>`*`)`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/print.summary.chaotic_model.md)
  : Print method for \[summary.chaotic_model()\]
- [`quantile_cpp()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/quantile_cpp.md)
  : Fast quantile computation (C++ implementation)
- [`recurrence_analysis()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/recurrence_analysis.md)
  : Recurrence quantification analysis
- [`recurrence_plot()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/recurrence_plot.md)
  : Recurrence plot for a time series
- [`report_extremes()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/report_extremes.md)
  : Generate an Extreme-Value Analysis HTML Report
- [`return_level_empirical_cpp()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/return_level_empirical_cpp.md)
  : Fast return level estimation (C++ implementation)
- [`select_threshold()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/select_threshold.md)
  : Select High Threshold via Quantile
- [`select_threshold_adaptive()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/select_threshold_adaptive.md)
  : Adaptive threshold selection
- [`select_threshold_auto()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/select_threshold_auto.md)
  : Automatic threshold selection for POT analysis
- [`simulate_cat_map_cpp()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_cat_map_cpp.md)
  : Fast Arnold cat map simulation (C++ implementation)
- [`simulate_duffing_cpp()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_duffing_cpp.md)
  : Fast forced Duffing oscillator simulation (C++ implementation)
- [`simulate_henon_map_cpp()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_henon_map_cpp.md)
  : Fast Hénon map simulation (C++ implementation)
- [`simulate_ikeda_map_cpp()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_ikeda_map_cpp.md)
  : Fast Ikeda map simulation (C++ implementation)
- [`simulate_logistic_map_cpp()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_logistic_map_cpp.md)
  : Fast logistic map simulation (C++ implementation)
- [`simulate_logistic_map_fast()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_logistic_map_fast.md)
  : Fast logistic map simulation with automatic method selection
- [`simulate_logistic_map_smart()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_logistic_map_smart.md)
  : Smart logistic map simulation
- [`simulate_lorenz_cpp()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_lorenz_cpp.md)
  : Fast Lorenz system simulation (C++ implementation)
- [`simulate_lozi_map_cpp()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_lozi_map_cpp.md)
  : Fast Lozi map simulation (C++ implementation)
- [`simulate_mackey_glass_cpp()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_mackey_glass_cpp.md)
  : Fast Mackey-Glass DDE simulation (C++ implementation)
- [`simulate_orbit()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_orbit.md)
  : Simulate Orbit of a One-Dimensional Map
- [`simulate_rossler_cpp()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_rossler_cpp.md)
  : Fast Rossler system simulation (C++ implementation)
- [`simulate_standard_map_cpp()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_standard_map_cpp.md)
  : Fast Chirikov standard map simulation (C++ implementation)
- [`simulate_tent_map_cpp()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/simulate_tent_map_cpp.md)
  : Fast tent map simulation (C++ implementation)
- [`summary(`*`<chaotic_model>`*`)`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/summary.chaotic_model.md)
  : Summarize chaoticds model fits
- [`threshold_exceedances_cpp()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/threshold_exceedances_cpp.md)
  : Fast threshold exceedance detection (C++ implementation)
- [`threshold_exceedances_smart()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/threshold_exceedances_smart.md)
  : Smart threshold exceedances
- [`threshold_summary_chunked()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/threshold_summary_chunked.md)
  : Chunked exceedance summary for very large vectors
- [`wrap_chaotic_model()`](https://diogoribeiro7.github.io/chaotic-dynamical-systems/reference/wrap_chaotic_model.md)
  : Wrap fitted model objects with chaoticds metadata
