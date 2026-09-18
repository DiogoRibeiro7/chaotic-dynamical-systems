# Automatic threshold selection for POT analysis

Ranks candidate thresholds using a composite score that combines: (1)
MRL smoothness, (2) extremal-index stability, and (3) exceedance
adequacy.

## Usage

``` r
select_threshold_auto(
  x,
  candidate_probs = seq(0.9, 0.99, by = 0.01),
  min_exceedances = 20L,
  estimator = c("runs", "intervals"),
  run_length = 3L,
  weights = c(mrl = 0.4, stability = 0.4, exceedance = 0.2)
)
```

## Arguments

- x:

  Numeric vector of observations.

- candidate_probs:

  Numeric vector of quantile probabilities in (0, 1).

- min_exceedances:

  Integer minimum desirable number of exceedances.

- estimator:

  Character string, either \`"runs"\` or \`"intervals"\`.

- run_length:

  Integer run parameter for the runs estimator.

- weights:

  Named numeric vector with elements \`mrl\`, \`stability\`, and
  \`exceedance\` controlling score aggregation.

## Value

A list with:

- recommended_threshold:

  Selected threshold value.

- recommended_probability:

  Selected quantile probability.

- score:

  Composite score of the selected threshold.

- ranking:

  Data frame of all candidates sorted by score.

## Examples

``` r
set.seed(42)
x <- simulate_logistic_map(1000, r = 3.8, x0 = 0.2)
auto <- select_threshold_auto(x)
auto$recommended_threshold
#> [1] 0.9492493
head(auto$ranking)
#>   probability threshold n_exceedances mrl_score stability_score
#> 1        0.98 0.9492493            20 1.0000000               1
#> 2        0.97 0.9486456            30 0.8378060               1
#> 3        0.96 0.9472664            40 0.7736604               1
#> 4        0.99 0.9497249            10 1.0000000               1
#> 5        0.95 0.9448111            50 0.7146667               1
#> 6        0.94 0.9431859            60 0.0000000               1
#>   exceedance_score     score                    rationale
#> 1              1.0 1.0000000 Stable extremal-index region
#> 2              1.0 0.9351224 Stable extremal-index region
#> 3              1.0 0.9094642 Stable extremal-index region
#> 4              0.5 0.9000000          Too few exceedances
#> 5              1.0 0.8858667 Stable extremal-index region
#> 6              1.0 0.6000000 Stable extremal-index region
```
