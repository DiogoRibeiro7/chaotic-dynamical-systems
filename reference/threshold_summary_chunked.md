# Chunked exceedance summary for very large vectors

Computes exceedance diagnostics in fixed-size chunks to keep memory
usage stable for very large vectors (1M+ observations).

## Usage

``` r
threshold_summary_chunked(x, threshold, chunk_size = 1000000L)
```

## Arguments

- x:

  Numeric vector.

- threshold:

  Numeric exceedance threshold.

- chunk_size:

  Integer chunk size used for streaming computation.

## Value

A list with \`n\`, \`n_exceedances\`, \`exceedance_rate\`,
\`mean_excess\`, and \`max_excess\`.
