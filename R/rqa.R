# ---------------------------------------------------------------------------
# Recurrence Quantification Analysis (RQA) - full standard suite.
#
# Builds on the existing recurrence_plot() but adds the Theiler window
# (excluding the main diagonal and a band of width `theiler` either side)
# and computes the full battery of standard measures:
#   RR, DET, LAM, L, L_max, TT, V_max, ENT.
# rqa() is the complement of the lighter recurrence_analysis(), which
# stays around for backwards compatibility and computes only RR and DET.
# ---------------------------------------------------------------------------

# Extract lengths of contiguous runs of TRUE values along all off-diagonals
# of `mat`. Diagonals within Theiler window of the main are skipped.
.diagonal_run_lengths <- function(mat, theiler) {
  n <- nrow(mat)
  out <- vector("list", 2L * n - 1L)
  for (k in seq(-(n - 1L), n - 1L)) {
    if (abs(k) < theiler) next
    v   <- mat[row(mat) - col(mat) == k]
    rl  <- rle(as.logical(v))
    out[[k + n]] <- rl$lengths[rl$values]
  }
  unlist(out, use.names = FALSE)
}

# Vertical runs: just iterate columns, but blank the Theiler band first.
.vertical_run_lengths <- function(mat, theiler) {
  n <- nrow(mat)
  if (theiler > 0L) {
    mask <- abs(row(mat) - col(mat)) < theiler
    mat[mask] <- FALSE
  }
  out <- vector("list", n)
  for (j in seq_len(n)) {
    rl <- rle(as.logical(mat[, j]))
    out[[j]] <- rl$lengths[rl$values]
  }
  unlist(out, use.names = FALSE)
}

#' Recurrence Quantification Analysis (full RQA suite)
#'
#' @description
#' Computes the standard RQA measures from a univariate time series via a
#' delay-embedded recurrence plot. Complements the lighter
#' [recurrence_analysis()], which reports only the recurrence rate and a
#' simple determinism.
#'
#' @details
#' The recurrence matrix \eqn{R_{ij} = \mathbf{1}[\|X_i - X_j\|_\infty \le
#' \epsilon]} is built from the time-delay embedding of `x`. A Theiler
#' window of half-width `theiler` excludes the main diagonal (and
#' optionally adjacent diagonals); without this step the autocorrelation
#' band biases every measure upward.
#'
#' Measures returned:
#' - **RR** -- recurrence rate, the fraction of recurrent points outside
#'   the Theiler window.
#' - **DET** -- determinism, fraction of recurrent points forming
#'   diagonal segments of length \eqn{\ge} `l_min`.
#' - **L** -- mean length of diagonal segments \eqn{\ge} `l_min`.
#' - **L_max** -- longest diagonal segment.
#' - **ENT** -- Shannon entropy of the diagonal-length distribution
#'   (lengths \eqn{\ge} `l_min`).
#' - **LAM** -- laminarity, fraction in vertical segments \eqn{\ge} `v_min`.
#' - **TT** -- trapping time, mean length of vertical segments \eqn{\ge}
#'   `v_min`.
#' - **V_max** -- longest vertical segment.
#'
#' @param x Numeric vector. The time series to analyse.
#' @param embed Integer (\eqn{\ge 1}). Embedding dimension. Defaults to 2.
#' @param delay Integer (\eqn{\ge 1}). Embedding delay. Defaults to 1.
#' @param eps Numeric (\eqn{> 0}). Recurrence threshold. Defaults to
#'   `0.1 * sd(x)`.
#' @param l_min Integer (\eqn{\ge 2}). Minimum diagonal-line length to
#'   count toward DET, L, ENT.
#' @param v_min Integer (\eqn{\ge 2}). Minimum vertical-line length to
#'   count toward LAM and TT.
#' @param theiler Integer (\eqn{\ge 0}). Half-width of the Theiler
#'   exclusion band around the main diagonal. Defaults to 1 (excludes
#'   the main diagonal only).
#'
#' @return A named list with elements `RR`, `DET`, `LAM`, `L`, `L_max`,
#'   `TT`, `V_max`, `ENT`. Each is a single number; entries are `NA` when
#'   no segments of the required length exist.
#'
#' @references
#' Marwan, N., Romano, M. C., Thiel, M., & Kurths, J. (2007). Recurrence
#' plots for the analysis of complex systems. *Physics Reports*, 438(5-6),
#' 237-329. \doi{10.1016/j.physrep.2006.11.001}
#'
#' Webber, C. L., & Zbilut, J. P. (1994). Dynamical assessment of
#' physiological systems and states using recurrence plot strategies.
#' *Journal of Applied Physiology*, 76(2), 965-973.
#'
#' @seealso [recurrence_plot()], [recurrence_analysis()] for the
#'   lightweight RR + DET pair.
#'
#' @examples
#' x <- simulate_logistic_map(1000, r = 3.8, x0 = 0.2)
#' rqa(x, embed = 3, delay = 1)
#'
#' @export
rqa <- function(x, embed = 2L, delay = 1L, eps = NULL,
                l_min = 2L, v_min = 2L, theiler = 1L) {
  checkmate::assert_numeric(x, any.missing = FALSE, min.len = 10L)
  checkmate::assert_int(embed, lower = 1L)
  checkmate::assert_int(delay, lower = 1L)
  checkmate::assert_int(l_min, lower = 2L)
  checkmate::assert_int(v_min, lower = 2L)
  checkmate::assert_int(theiler, lower = 0L)

  rp <- recurrence_plot(x, embed = embed, delay = delay, eps = eps)
  n  <- nrow(rp)

  if (theiler > 0L) {
    mask <- abs(row(rp) - col(rp)) < theiler
    rp[mask] <- FALSE
  }
  n_recurrent      <- sum(rp)
  n_cells_outside  <- n * n - sum(abs(row(rp) - col(rp)) < theiler)

  diag_lengths <- .diagonal_run_lengths(rp, theiler)
  vert_lengths <- .vertical_run_lengths(rp, theiler)
  diag_long    <- diag_lengths[diag_lengths >= l_min]
  vert_long    <- vert_lengths[vert_lengths >= v_min]

  RR    <- if (n_cells_outside > 0L) n_recurrent / n_cells_outside else NA_real_
  DET   <- if (n_recurrent > 0L) sum(diag_long) / n_recurrent else NA_real_
  LAM   <- if (n_recurrent > 0L) sum(vert_long) / n_recurrent else NA_real_
  L     <- if (length(diag_long) > 0L) mean(diag_long) else NA_real_
  L_max <- if (length(diag_lengths) > 0L) max(diag_lengths) else 0L
  TT    <- if (length(vert_long) > 0L) mean(vert_long) else NA_real_
  V_max <- if (length(vert_lengths) > 0L) max(vert_lengths) else 0L

  ENT <- if (length(diag_long) > 0L) {
    counts <- table(diag_long)
    p <- counts / sum(counts)
    -sum(p * log(p))
  } else NA_real_

  list(
    RR    = RR,
    DET   = DET,
    LAM   = LAM,
    L     = L,
    L_max = as.integer(L_max),
    TT    = TT,
    V_max = as.integer(V_max),
    ENT   = ENT
  )
}
