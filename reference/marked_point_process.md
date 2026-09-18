# Build Marked Point Process

Build Marked Point Process

## Usage

``` r
marked_point_process(
  X,
  u,
  run_length = 1L,
  type = c("REPP", "EOT", "POT", "AOT")
)
```

## Arguments

- X:

  Numeric vector. Time series.

- u:

  Numeric scalar. Threshold.

- run_length:

  Integer. Cluster run-length p.

- type:

  Character: one of "REPP","EOT","POT","AOT".

## Value

Data.frame with columns time (start of cluster) and mark. Returns an
empty data.frame with the correct columns when there are no exceedances
above \`u\`.
