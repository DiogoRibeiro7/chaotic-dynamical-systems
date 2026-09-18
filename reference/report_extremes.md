# Generate an Extreme-Value Analysis HTML Report

One-command reporting utility that runs a complete univariate extremes
workflow and renders an HTML report with key diagnostics and model
outputs.

## Usage

``` r
report_extremes(
  x = NULL,
  output_file = "extremes-report.html",
  n = 5000L,
  r = 3.8,
  x0 = 0.2,
  block_size = 100L,
  threshold_q = 0.95,
  run_length = 3L
)
```

## Arguments

- x:

  Optional numeric vector. If \`NULL\`, a logistic-map series is
  simulated using \`n\`, \`r\`, and \`x0\`.

- output_file:

  Output HTML file path.

- n:

  Number of observations to simulate when \`x\` is \`NULL\`.

- r:

  Logistic-map parameter used when simulating.

- x0:

  Logistic-map initial condition used when simulating.

- block_size:

  Block size for block maxima.

- threshold_q:

  Quantile level for POT threshold.

- run_length:

  Run parameter for extremal index and clustering.

## Value

Invisibly returns the output report path.

## Examples

``` r
if (FALSE) { # \dontrun{
report_extremes(output_file = "extremes-report.html")
} # }
```
