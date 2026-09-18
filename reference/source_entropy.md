# Estimate source entropy from a symbol sequence

Estimates the per-symbol source entropy \\h\\ of a symbol process by
taking the empirical conditional entropy \\h \approx H\_{k+1} - H_k\\ at
the largest \`k\` for which there is enough data to compute \\H\_{k+1}\\
reliably.

## Usage

``` r
source_entropy(symbols, max_word_length = 8L, min_obs_per_word = 5)
```

## Arguments

- symbols:

  Integer-valued vector. Typically the output of \[symbolize()\].

- max_word_length:

  Integer (\\\ge 2\\). Upper bound on \`k\`. The function lowers this
  automatically if the input is too short.

- min_obs_per_word:

  Numeric (\\\ge 1\\). Average occupancy required per word at the chosen
  \`k\`. Defaults to 5; values below 2 risk substantial undersampling
  bias.

## Value

Numeric scalar – the conditional-entropy estimate of \`h\` in nats per
symbol. Returns \`NA\` when the sequence is too short.

## Details

Theoretically \\h = \lim\_{k\to\infty}(H\_{k+1} - H_k)\\ for a
stationary ergodic process. In practice the convergence is slow and
undersampling at large \`k\` biases \\H_k\\ downward; the function
therefore caps \`k\` so each \`k+1\`-length word is expected to appear
at least \`min_obs_per_word\` times on average.

Useful sanity checks:

\- Logistic map at \\r = 4\\ with the generating partition \\x = 0.5\\:
the symbol process is Bernoulli-1/2 in the limit, so \\h \to \log 2
\approx 0.693\\. - Tent map at \\r = 2\\: same limit.

## See also

\[symbolize()\], \[block_entropy()\].

## Examples

``` r
x <- simulate_logistic_map(20000, r = 4, x0 = 0.2)
s <- symbolize(x, breaks = 0.5)
source_entropy(s)   # should be close to log(2)
#> [1] 0.6862573
```
