# Logistic map: end-to-end EVT workflow

This tutorial follows a complete analysis of one chaotic trajectory. The focus is not only fitting a tail model, but checking the dynamical and dependence assumptions around it.

## 1. Simulate the orbit

For

\[
x_{t+1}=r x_t(1-x_t),
\]

choose \(r=3.8\).

~~~r
library(chaoticds)

x <- simulate_logistic_map(
  n = 10000,
  r = 3.8,
  x0 = 0.2
)
~~~

Inspect a short segment:

~~~r
plot(
  x[1:500],
  type = "l",
  xlab = "Iteration",
  ylab = "x",
  main = "Logistic map, r = 3.8"
)
~~~

## 2. Check that the trajectory has chaotic structure

A jagged line plot is not evidence by itself.

~~~r
lambda <- estimate_lyapunov_exponent(x)
lambda
~~~

You can also inspect recurrence structure:

~~~r
qa <- rqa(
  x,
  embed = 3,
  delay = 1,
  lmin = 2,
  vmin = 2
)

qa
~~~

These diagnostics describe the orbit. They do not replace EVT diagnostics.

## 3. Build a threshold grid

Start with candidate upper quantiles.

~~~r
probs <- seq(
  0.90,
  0.99,
  by = 0.01
)

thresholds <- quantile(
  x,
  probs
)
~~~

Inspect mean residual life:

~~~r
mrl <- mean_residual_life(
  x,
  thresholds
)

mrl_plot(mrl)
~~~

And, when appropriate, Hill estimates:

~~~r
hill <- hill_estimates(
  x,
  k_values = 20:200
)

hill_plot(hill)
~~~

The goal is to find a region where the tail representation is reasonably stable.

## 4. Check extremal-index stability

Threshold selection and clustering interact.

~~~r
grid <- expand.grid(
  probability = probs,
  run_length = 1:5
)

grid$theta <- mapply(
  function(p, r) {
    u <- quantile(x, p)

    extremal_index_runs(
      x,
      threshold = u,
      run_length = r
    )
  },
  grid$probability,
  grid$run_length
)
~~~

Look for a region where modest changes in threshold and run length do not produce radically different estimates.

## 5. Choose one working threshold

Assume the diagnostic region supports the empirical 95th percentile.

~~~r
u <- quantile(
  x,
  0.95
)
~~~

Inspect cluster sizes:

~~~r
sizes <- cluster_sizes(
  x,
  threshold = u,
  run_length = 2
)

cluster_summary(sizes)
~~~

Compare two extremal-index estimators:

~~~r
theta_runs <- extremal_index_runs(
  x,
  threshold = u,
  run_length = 2
)

theta_intervals <- extremal_index_intervals(
  x,
  threshold = u
)

c(
  runs = theta_runs,
  intervals = theta_intervals
)
~~~

## 6. Fit the POT model

~~~r
gpd <- fit_gpd(
  x,
  threshold = u
)

summary(gpd)
~~~

Check model diagnostics:

~~~r
validate_extreme_model(
  x,
  threshold = u,
  method = "qq"
)
~~~

A converged optimization is not the same thing as a validated tail model.

## 7. Quantify uncertainty in clustering

Use block-based resampling rather than IID resampling.

~~~r
boot_theta <- bootstrap_extremal_index(
  x,
  threshold = u,
  run_length = 2,
  B = 1000
)
~~~

## 8. Profile the tail parameter

~~~r
profile_shape <- profile_likelihood(
  gpd,
  parameter = "shape"
)

profile_ci(profile_shape)
~~~

Profile likelihood is useful when the likelihood is asymmetric or the shape parameter is near a support boundary.

## 9. Compare against block maxima

~~~r
bm <- block_maxima(
  x,
  block_size = 100
)

gev <- fit_gev(bm)

summary(gev)
~~~

The two approaches use the trajectory differently:

- block maxima retain one observation per block;
- POT retains all exceedances above \(u\).

Agreement is useful. Disagreement is a reason to diagnose the analysis rather than automatically choose one method.

## 10. Report the analysis

A useful report should state:

- system and parameter regime;
- initial condition;
- trajectory length;
- transient handling;
- observable used;
- threshold or block size;
- threshold/block sensitivity;
- extremal-index estimator;
- run length;
- number of exceedances and clusters;
- fitted tail parameters;
- uncertainty intervals;
- goodness-of-fit diagnostics;
- dynamical diagnostics.

## What not to say

Avoid claims such as:

> The 95th percentile was used because 95% is a standard EVT threshold.

A percentile is a starting point. Its adequacy has to be checked.

Also avoid:

> The model converged, therefore the fit is valid.

Convergence is numerical. Validity is statistical.
