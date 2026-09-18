# Encode a numeric orbit as a symbol sequence

Partitions \`x\` along \`breaks\` (user-supplied or equiprobable) and
returns the integer index of the bin each observation falls into. The
resulting symbol sequence is the input to \[block_entropy()\] and
\[source_entropy()\].

## Usage

``` r
symbolize(x, breaks = NULL, n_symbols = 2L)
```

## Arguments

- x:

  Numeric vector. The orbit to encode.

- breaks:

  Numeric vector of partition points, strictly increasing. If \`NULL\`
  (default), uses equiprobable quantiles of \`x\`.

- n_symbols:

  Integer (\\\ge 2\\). Number of symbols; ignored when \`breaks\` is
  supplied.

## Value

Integer vector of the same length as \`x\` with values in
\`0:(n_symbols - 1)\`.

## Details

If \`breaks = NULL\` the function builds an equiprobable partition by
calling \`quantile(x, probs)\` with \`n_symbols - 1\` interior
quantiles. This is the obvious "no-prior" choice for an unknown map; for
specific maps a generating / Markov partition (e.g. \\x = 0.5\\ for the
tent and logistic maps) gives a sharper entropy estimate.

The partition is applied via \[base::findInterval()\], which is
left-open and right-closed by default: a value exactly on a break-point
goes to the higher bin.

## See also

\[block_entropy()\], \[source_entropy()\].

## Examples

``` r
x <- simulate_logistic_map(500, r = 3.8, x0 = 0.2)
# Generating partition for the logistic map: break at the critical point.
s <- symbolize(x, breaks = 0.5)
head(s, 20)
#>  [1] 0 1 1 0 1 1 1 0 1 1 0 1 1 1 1 1 0 1 1 1
```
