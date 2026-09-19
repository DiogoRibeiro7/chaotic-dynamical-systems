# Diagnostics

Extreme-value inference is sensitive to threshold choice, dependence, finite samples, and misspecification. Diagnostics are therefore part of the model.

## Threshold diagnostics

Useful functions include \`mean_residual_life()\`, \`mrl_plot()\`, \`hill_estimates()\`, \`hill_plot()\`, \`threshold_diagnostics()\`, and the threshold-selection helpers.

A threshold should balance asymptotic bias against variance.

## Mixing and dependence

\`mixing_coefficients()\`, \`acf_decay()\`, and related utilities provide empirical checks on serial dependence.

For chaotic systems, strong structure can remain even when linear autocorrelation looks small.

## Recurrence analysis

\`recurrence_plot()\` constructs a recurrence matrix after phase-space embedding. \`rqa()\` exposes recurrence rate, determinism, laminarity, trapping time, entropy, and related quantities.

## Lyapunov diagnostics

A positive largest Lyapunov exponent is a standard signature of sensitive dependence on initial conditions. Use \`estimate_lyapunov_exponent()\` or the spectrum functions where appropriate.

## Correlation dimension

\`estimate_correlation_dimension()\` estimates an effective attractor dimension from scaling behaviour. Inspect the scaling region rather than treating the output as exact.

## Tail dependence

Use \`upper_tail_dependence()\`, \`lower_tail_dependence()\`, \`tail_dependence_coefficient()\`, \`tail_dependence_asymmetric()\`, and \`tail_dependence_heatmap()\`.

## Validation workflow

A defensible workflow combines dynamical diagnostics, dependence checks, threshold or block-size sensitivity, model goodness-of-fit, clustering analysis, and uncertainty quantification.
