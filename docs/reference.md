# Function reference

This page groups the main exported functions by purpose. In R, use \`?function_name\` for argument-level help.

## Simulation

Discrete and coupled systems include the logistic, Hénon, tent, Lozi, cat, standard, Ikeda, coupled-map-lattice, and Kuramoto simulators, with C++ counterparts where implemented.

Continuous and delayed systems include Lorenz, Rössler, Duffing, and Mackey–Glass.

## Extreme-value fitting

- \`block_maxima()\`
- \`block_r_largest()\`
- \`exceedances()\`
- \`fit_gev()\`
- \`fit_gpd()\`
- \`fit_ppp()\`
- \`fit_gev_rlargest()\`
- \`fit_nonstationary_gev()\`
- \`fit_compound_poisson()\`
- \`calculate_return_levels()\`

## Profile likelihood

- \`profile_likelihood()\`
- \`profile_ci()\`
- \`profile_return_level()\`

## Extremal clustering

- \`extremal_index_runs()\`
- \`extremal_index_intervals()\`
- \`extremal_index_bivariate()\`
- \`extremal_index_multivariate()\`
- \`bootstrap_extremal_index()\`
- \`cluster_exceedances()\`
- \`cluster_sizes()\`
- \`cluster_summary()\`
- \`decluster()\`

## Diagnostics

- \`mean_residual_life()\`
- \`hill_estimates()\`
- \`threshold_diagnostics()\`
- \`goodness_of_fit_test()\`
- \`validate_extreme_model()\`
- \`mixing_coefficients()\`
- \`recurrence_plot()\`
- \`rqa()\`
- \`estimate_lyapunov_exponent()\`
- \`lyapunov_spectrum()\`
- \`estimate_correlation_dimension()\`

## Multivariate tails

- \`upper_tail_dependence()\`
- \`lower_tail_dependence()\`
- \`tail_dependence_coefficient()\`
- \`tail_dependence_asymmetric()\`
- \`tail_dependence_heatmap()\`
- \`multivariate_extreme_workflow()\`

## Reporting and engineering

- \`run_demo()\`
- \`launch_explorer()\`
- \`report_extremes()\`
- \`benchmark_implementations()\`
- \`performance_analysis()\`
- \`with_logging()\`

The complete exported surface is defined by \`NAMESPACE\` and the generated R help files.
