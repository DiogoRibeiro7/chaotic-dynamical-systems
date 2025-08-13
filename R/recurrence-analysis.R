#' Recurrence plot for a time series
#'
#' Compute a simple recurrence plot using delay embedding.
#'
#' @param x Numeric vector representing the time series.
#' @param embed Integer embedding dimension. Defaults to 2.
#' @param delay Integer delay between coordinates in the embedding. Defaults to 1.
#' @param eps Positive radius for defining recurrences. If NULL, uses 10% of the standard deviation of `x`.
#'
#' @return A logical matrix representing the recurrence plot. Points within `eps` distance are marked as TRUE.
#' @examples
#' rp <- recurrence_plot(rnorm(100))
#' image(rp)
#' @importFrom stats dist sd
#' @export
recurrence_plot <- function(x, embed = 2L, delay = 1L, eps = NULL) {
  checkmate::assert_numeric(x, any.missing = FALSE)
  checkmate::assert_int(embed, lower = 1)
  checkmate::assert_int(delay, lower = 1)
  checkmate::assert_number(eps, null.ok = TRUE, lower = 0, left.open = TRUE)
  n <- length(x) - (embed - 1) * delay
  checkmate::assert_true(n > 0, message = "time series too short for chosen embedding")

  emb <- stats::embed(x, embed)[seq_len(n), , drop = FALSE]
  dmat <- as.matrix(dist(emb))
  if (is.null(eps)) eps <- 0.1 * stats::sd(x)
  rp <- dmat <= eps
  class(rp) <- c("recurrence_plot", class(rp))
  rp
}

#' Recurrence quantification analysis
#'
#' Compute basic statistics from a recurrence plot, including recurrence rate and determinism.
#'
#' @inheritParams recurrence_plot
#' @param lmin Minimum diagonal line length to count toward determinism.
#'
#' @return List with components `recurrence_rate`, `determinism`, and `recurrence_matrix`.
#' @examples
#' recurrence_analysis(rnorm(100))
#' @export
recurrence_analysis <- function(x, embed = 2L, delay = 1L, eps = NULL, lmin = 2L) {
  checkmate::assert_int(lmin, lower = 1)
  rp <- recurrence_plot(x, embed = embed, delay = delay, eps = eps)
  n <- nrow(rp)
  rr <- sum(rp) / (n * n)

  diag_lengths <- function(mat, lmin) {
    n <- nrow(mat)
    lens_list <- vector("list", 2 * n - 1)
    for (k in seq(-(n - 1), n - 1)) {
      v <- mat[row(mat) - col(mat) == k]
      r <- rle(v)
      lens_list[[k + n]] <- r$lengths[r$values]
    }
    lens <- unlist(lens_list, use.names = FALSE)
    lens[lens >= lmin]
  }

  dl <- diag_lengths(rp, lmin)
  det <- if (length(dl) == 0) 0 else sum(dl) / sum(rp)

  list(recurrence_rate = rr,
       determinism = det,
       recurrence_matrix = rp)
}
