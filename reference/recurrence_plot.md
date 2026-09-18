# Recurrence plot for a time series

Compute a simple recurrence plot using delay embedding.

## Usage

``` r
recurrence_plot(x, embed = 2L, delay = 1L, eps = NULL)
```

## Arguments

- x:

  Numeric vector representing the time series.

- embed:

  Integer embedding dimension. Defaults to 2.

- delay:

  Integer delay between coordinates in the embedding. Defaults to 1.

- eps:

  Positive radius for defining recurrences. If NULL, uses 10% of the
  standard deviation of \`x\`.

## Value

A logical matrix representing the recurrence plot. Points within \`eps\`
distance are marked as TRUE.

## Examples

``` r
rp <- recurrence_plot(rnorm(100))
image(rp)
```
