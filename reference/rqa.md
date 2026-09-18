# Recurrence Quantification Analysis (full RQA suite)

Computes the standard RQA measures from a univariate time series via a
delay-embedded recurrence plot. Complements the lighter
\[recurrence_analysis()\], which reports only the recurrence rate and a
simple determinism.

## Usage

``` r
rqa(
  x,
  embed = 2L,
  delay = 1L,
  eps = NULL,
  l_min = 2L,
  v_min = 2L,
  theiler = 1L
)
```

## Arguments

- x:

  Numeric vector. The time series to analyse.

- embed:

  Integer (\\\ge 1\\). Embedding dimension. Defaults to 2.

- delay:

  Integer (\\\ge 1\\). Embedding delay. Defaults to 1.

- eps:

  Numeric (\\\> 0\\). Recurrence threshold. Defaults to \`0.1 \*
  sd(x)\`.

- l_min:

  Integer (\\\ge 2\\). Minimum diagonal-line length to count toward DET,
  L, ENT.

- v_min:

  Integer (\\\ge 2\\). Minimum vertical-line length to count toward LAM
  and TT.

- theiler:

  Integer (\\\ge 0\\). Half-width of the Theiler exclusion band around
  the main diagonal. Defaults to 1 (excludes the main diagonal only).

## Value

A named list with elements \`RR\`, \`DET\`, \`LAM\`, \`L\`, \`L_max\`,
\`TT\`, \`V_max\`, \`ENT\`. Each is a single number; entries are \`NA\`
when no segments of the required length exist.

## Details

The recurrence matrix \\R\_{ij} = \mathbf{1}\[\\X_i - X_j\\\_\infty \le
\epsilon\]\\ is built from the time-delay embedding of \`x\`. A Theiler
window of half-width \`theiler\` excludes the main diagonal (and
optionally adjacent diagonals); without this step the autocorrelation
band biases every measure upward.

Measures returned: - \*\*RR\*\* – recurrence rate, the fraction of
recurrent points outside the Theiler window. - \*\*DET\*\* –
determinism, fraction of recurrent points forming diagonal segments of
length \\\ge\\ \`l_min\`. - \*\*L\*\* – mean length of diagonal segments
\\\ge\\ \`l_min\`. - \*\*L_max\*\* – longest diagonal segment. -
\*\*ENT\*\* – Shannon entropy of the diagonal-length distribution
(lengths \\\ge\\ \`l_min\`). - \*\*LAM\*\* – laminarity, fraction in
vertical segments \\\ge\\ \`v_min\`. - \*\*TT\*\* – trapping time, mean
length of vertical segments \\\ge\\ \`v_min\`. - \*\*V_max\*\* – longest
vertical segment.

## References

Marwan, N., Romano, M. C., Thiel, M., & Kurths, J. (2007). Recurrence
plots for the analysis of complex systems. \*Physics Reports\*,
438(5-6), 237-329.
[doi:10.1016/j.physrep.2006.11.001](https://doi.org/10.1016/j.physrep.2006.11.001)

Webber, C. L., & Zbilut, J. P. (1994). Dynamical assessment of
physiological systems and states using recurrence plot strategies.
\*Journal of Applied Physiology\*, 76(2), 965-973.

## See also

\[recurrence_plot()\], \[recurrence_analysis()\] for the lightweight
RR + DET pair.

## Examples

``` r
x <- simulate_logistic_map(1000, r = 3.8, x0 = 0.2)
rqa(x, embed = 3, delay = 1)
#> $RR
#> [1] 0.02479583
#> 
#> $DET
#> [1] 0.9002108
#> 
#> $LAM
#> [1] 0.002067121
#> 
#> $L
#> [1] 4.295938
#> 
#> $L_max
#> [1] 27
#> 
#> $TT
#> [1] 2.684211
#> 
#> $V_max
#> [1] 5
#> 
#> $ENT
#> [1] 2.013334
#> 
```
