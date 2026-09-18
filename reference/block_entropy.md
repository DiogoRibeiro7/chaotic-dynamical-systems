# Shannon entropy of length-k words in a symbol sequence

Returns the Shannon entropy (in nats) of the word distribution \\H_k =
-\sum_w p_w \log p_w\\, where \\p_w\\ is the empirical frequency of word
\\w\\ of length \`word_length\` in \`symbols\`.

## Usage

``` r
block_entropy(symbols, word_length = 1L)
```

## Arguments

- symbols:

  Integer-valued vector. Typically the output of \[symbolize()\].

- word_length:

  Integer (\\\ge 1\\). Block size.

## Value

Numeric scalar – the Shannon entropy in nats. Returns \`NA\` when the
sequence is shorter than \`word_length\`.

## Details

Words are formed by a sliding window of length \`word_length\` over the
input sequence. For a finite alphabet of size \\m\\ the entropy is
bounded above by \\k \log m\\.

## See also

\[symbolize()\], \[source_entropy()\].

## Examples

``` r
x <- simulate_logistic_map(2000, r = 4, x0 = 0.2)
s <- symbolize(x, breaks = 0.5)
# For the fully chaotic logistic map at r = 4 the symbol process is a
# shift of full type, so H_k -> k * log(2).
block_entropy(s, word_length = 1)
#> [1] 0.6929052
block_entropy(s, word_length = 3)
#> [1] 2.078042
```
