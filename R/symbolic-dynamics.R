# ---------------------------------------------------------------------------
# Symbolic dynamics: partition a continuous orbit into a symbol sequence
# and compute information-theoretic summaries of the resulting word
# statistics.
# ---------------------------------------------------------------------------

#' Encode a numeric orbit as a symbol sequence
#'
#' @description
#' Partitions `x` along `breaks` (user-supplied or equiprobable) and
#' returns the integer index of the bin each observation falls into. The
#' resulting symbol sequence is the input to [block_entropy()] and
#' [source_entropy()].
#'
#' @details
#' If `breaks = NULL` the function builds an equiprobable partition by
#' calling `quantile(x, probs)` with `n_symbols - 1` interior quantiles.
#' This is the obvious "no-prior" choice for an unknown map; for
#' specific maps a generating / Markov partition (e.g. \eqn{x = 0.5} for
#' the tent and logistic maps) gives a sharper entropy estimate.
#'
#' The partition is applied via [base::findInterval()], which is left-open
#' and right-closed by default: a value exactly on a break-point goes to
#' the higher bin.
#'
#' @param x Numeric vector. The orbit to encode.
#' @param breaks Numeric vector of partition points, strictly increasing.
#'   If `NULL` (default), uses equiprobable quantiles of `x`.
#' @param n_symbols Integer (\eqn{\ge 2}). Number of symbols; ignored when
#'   `breaks` is supplied.
#'
#' @return Integer vector of the same length as `x` with values in
#'   `0:(n_symbols - 1)`.
#'
#' @seealso [block_entropy()], [source_entropy()].
#'
#' @examples
#' x <- simulate_logistic_map(500, r = 3.8, x0 = 0.2)
#' # Generating partition for the logistic map: break at the critical point.
#' s <- symbolize(x, breaks = 0.5)
#' head(s, 20)
#'
#' @export
symbolize <- function(x, breaks = NULL, n_symbols = 2L) {
  checkmate::assert_numeric(x, any.missing = FALSE, min.len = 1L)
  if (is.null(breaks)) {
    checkmate::assert_int(n_symbols, lower = 2L)
    probs <- seq(0, 1, length.out = n_symbols + 1L)
    breaks <- as.numeric(stats::quantile(x, probs = probs[-c(1L, length(probs))],
                                          names = FALSE, type = 7L))
  } else {
    checkmate::assert_numeric(breaks, any.missing = FALSE, min.len = 1L,
                              sorted = TRUE)
  }
  as.integer(findInterval(x, breaks))
}

#' Shannon entropy of length-k words in a symbol sequence
#'
#' @description
#' Returns the Shannon entropy (in nats) of the word distribution
#' \eqn{H_k = -\sum_w p_w \log p_w}, where \eqn{p_w} is the empirical
#' frequency of word \eqn{w} of length `word_length` in `symbols`.
#'
#' @details
#' Words are formed by a sliding window of length `word_length` over the
#' input sequence. For a finite alphabet of size \eqn{m} the entropy is
#' bounded above by \eqn{k \log m}.
#'
#' @param symbols Integer-valued vector. Typically the output of
#'   [symbolize()].
#' @param word_length Integer (\eqn{\ge 1}). Block size.
#'
#' @return Numeric scalar -- the Shannon entropy in nats. Returns `NA`
#'   when the sequence is shorter than `word_length`.
#'
#' @examples
#' x <- simulate_logistic_map(2000, r = 4, x0 = 0.2)
#' s <- symbolize(x, breaks = 0.5)
#' # For the fully chaotic logistic map at r = 4 the symbol process is a
#' # shift of full type, so H_k -> k * log(2).
#' block_entropy(s, word_length = 1)
#' block_entropy(s, word_length = 3)
#'
#' @seealso [symbolize()], [source_entropy()].
#'
#' @export
block_entropy <- function(symbols, word_length = 1L) {
  checkmate::assert_integerish(symbols, any.missing = FALSE, min.len = 1L)
  checkmate::assert_int(word_length, lower = 1L)
  if (length(symbols) < word_length) {
    return(NA_real_)
  }
  emb <- stats::embed(as.integer(symbols), word_length)
  words <- apply(emb, 1L, paste, collapse = ",")
  counts <- table(words)
  p <- counts / sum(counts)
  -sum(p * log(p))
}

#' Estimate source entropy from a symbol sequence
#'
#' @description
#' Estimates the per-symbol source entropy \eqn{h} of a symbol process by
#' taking the empirical conditional entropy
#' \eqn{h \approx H_{k+1} - H_k} at the largest `k` for which there is
#' enough data to compute \eqn{H_{k+1}} reliably.
#'
#' @details
#' Theoretically \eqn{h = \lim_{k\to\infty}(H_{k+1} - H_k)} for a stationary
#' ergodic process. In practice the convergence is slow and undersampling
#' at large `k` biases \eqn{H_k} downward; the function therefore caps
#' `k` so each `k+1`-length word is expected to appear at least
#' `min_obs_per_word` times on average.
#'
#' Useful sanity checks:
#'
#' - Logistic map at \eqn{r = 4} with the generating partition \eqn{x = 0.5}:
#'   the symbol process is Bernoulli-1/2 in the limit, so
#'   \eqn{h \to \log 2 \approx 0.693}.
#' - Tent map at \eqn{r = 2}: same limit.
#'
#' @param symbols Integer-valued vector. Typically the output of
#'   [symbolize()].
#' @param max_word_length Integer (\eqn{\ge 2}). Upper bound on `k`. The
#'   function lowers this automatically if the input is too short.
#' @param min_obs_per_word Numeric (\eqn{\ge 1}). Average occupancy required
#'   per word at the chosen `k`. Defaults to 5; values below 2 risk
#'   substantial undersampling bias.
#'
#' @return Numeric scalar -- the conditional-entropy estimate of `h` in
#'   nats per symbol. Returns `NA` when the sequence is too short.
#'
#' @examples
#' x <- simulate_logistic_map(20000, r = 4, x0 = 0.2)
#' s <- symbolize(x, breaks = 0.5)
#' source_entropy(s)   # should be close to log(2)
#'
#' @seealso [symbolize()], [block_entropy()].
#'
#' @export
source_entropy <- function(symbols, max_word_length = 8L,
                            min_obs_per_word = 5) {
  checkmate::assert_integerish(symbols, any.missing = FALSE, min.len = 1L)
  checkmate::assert_int(max_word_length, lower = 2L)
  checkmate::assert_number(min_obs_per_word, lower = 1)

  n_alphabet <- length(unique(symbols))
  if (n_alphabet < 2L) return(0)

  # Trim k so each (k+1)-length word has min_obs_per_word average occupancy.
  k_max <- max_word_length
  while (k_max > 1L) {
    n_words <- length(symbols) - k_max
    if (n_words / (n_alphabet^(k_max + 1L)) >= min_obs_per_word) break
    k_max <- k_max - 1L
  }
  if (k_max < 1L) return(NA_real_)

  block_entropy(symbols, word_length = k_max + 1L) -
    block_entropy(symbols, word_length = k_max)
}
