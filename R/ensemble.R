# ---------------------------------------------------------------------------
# ensemble_simulate(): Monte Carlo over initial conditions / parameters.
#
# Captures the simulator call as an unevaluated expression and re-evaluates
# it n_replicates times. Any random calls inside the expression (runif,
# rnorm, sample, etc.) get a fresh draw per replicate, so a single
# argument list controls both fixed and varying inputs. Long-format
# output is suitable for direct piping into dplyr / ggplot summaries.
# ---------------------------------------------------------------------------

#' Run a simulator across multiple replicates
#'
#' @description
#' Repeats a simulator call `n_replicates` times, re-evaluating any random
#' arguments on each pass, and returns a long-format data frame with a
#' `replicate` column for easy grouped summaries.
#'
#' @details
#' The first argument is captured as an unevaluated expression (the same
#' mechanism [base::replicate()] uses), so calls such as `runif(1)` inside
#' the simulator's argument list draw a fresh value per replicate.
#'
#' Results from the simulator are stacked into a single data frame:
#'
#' - If each replicate returns a numeric vector (e.g. [simulate_logistic_map()]
#'   without noise), the output has columns `replicate`, `iter`, `x`.
#' - If each replicate returns a data frame (e.g. [simulate_henon_map()],
#'   [simulate_lorenz()]), the output has `replicate`, `iter`, and the
#'   simulator's own columns appended.
#'
#' Set `seed` for reproducibility across the whole ensemble.
#'
#' @param expr A call to a simulator, captured unevaluated. Random
#'   sub-expressions in `expr` are re-drawn on every replicate.
#' @param n_replicates Integer. Number of replicates to run.
#' @param seed Optional integer seed for [set.seed()]. Set this when you
#'   need a reproducible ensemble across sessions.
#'
#' @return A data frame in long format with a leading `replicate` column.
#'
#' @examples
#' # Monte Carlo over initial conditions for the logistic map.
#' ens <- ensemble_simulate(
#'   simulate_logistic_map(200, r = 3.8, x0 = runif(1)),
#'   n_replicates = 5,
#'   seed = 1
#' )
#' head(ens)
#'
#' # Same idea for a 2D map; columns x, y are preserved.
#' ens2 <- ensemble_simulate(
#'   simulate_henon_map(100, x0 = runif(1, -0.1, 0.1)),
#'   n_replicates = 3,
#'   seed = 1
#' )
#' head(ens2)
#'
#' @seealso the `simulate_*` family.
#'
#' @export
ensemble_simulate <- function(expr, n_replicates, seed = NULL) {
  checkmate::assert_count(n_replicates, positive = TRUE)
  if (!is.null(seed)) {
    checkmate::assert_int(seed)
    set.seed(seed)
  }

  call_expr  <- substitute(expr)
  parent_env <- parent.frame()
  results    <- vector("list", n_replicates)
  for (i in seq_len(n_replicates)) {
    results[[i]] <- eval(call_expr, envir = parent_env)
  }

  .bind_replicates(results)
}

# Stack a list of simulator outputs into a long data frame with a leading
# `replicate` column. Accepts either all-numeric-vector or all-data-frame
# returns; refuses to silently coerce a mixed list.
.bind_replicates <- function(results) {
  if (length(results) == 0L) {
    return(data.frame(replicate = integer(0), iter = integer(0), x = numeric(0)))
  }

  is_df  <- vapply(results, is.data.frame,                       logical(1L))
  is_vec <- vapply(results, function(x) is.numeric(x) && is.null(dim(x)),
                                                                 logical(1L))

  if (all(is_df)) {
    out <- lapply(seq_along(results), function(i) {
      df <- results[[i]]
      cbind(replicate = i, iter = seq_len(nrow(df)), df)
    })
    return(do.call(rbind, out))
  }
  if (all(is_vec)) {
    out <- lapply(seq_along(results), function(i) {
      v <- results[[i]]
      data.frame(replicate = i, iter = seq_along(v), x = v)
    })
    return(do.call(rbind, out))
  }
  stop("ensemble_simulate(): every replicate must return the same shape ",
       "(either a numeric vector or a data frame); mixed returns are not ",
       "supported.")
}
