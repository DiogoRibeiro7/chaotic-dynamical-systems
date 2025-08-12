#' Data cleaning for extreme value analysis
#'
#' Removes missing and infinite values from a numeric vector.
#'
#' @param x Numeric vector possibly containing `NA`, `Inf`, or `-Inf`.
#'
#' @return Numeric vector with non-finite entries removed. If the result
#'   is empty, a numeric vector of length zero is returned.
#'
#' @examples
#' clean_extreme_data(c(1, NA, 2, Inf, 3))
#' @export
clean_extreme_data <- function(x) {
  checkmate::assert_numeric(x, any.missing = TRUE)
  x[is.finite(x)]
}

#' Empirical quantile
#'
#' Computes the empirical quantile of a numeric vector using `type = 8`
#' which is recommended for statistical applications.
#'
#' @param x Numeric vector of observations.
#' @param prob Numeric probability in `[0, 1]` specifying the quantile.
#'
#' @return Numeric value giving the empirical quantile.
#'
#' @examples
#' empirical_quantile(rnorm(100), 0.95)
#' @export
empirical_quantile <- function(x, prob) {
  checkmate::assert_numeric(x, any.missing = TRUE, min.len = 1)
  checkmate::assert_number(prob, lower = 0, upper = 1)
  as.numeric(stats::quantile(x, prob, type = 8, na.rm = TRUE))
}

#' Compute autocorrelation function
#'
#' Returns the sample autocorrelation values up to `max_lag` using
#' `stats::acf` with `plot = FALSE`.
#'
#' @param x Numeric vector.
#' @param max_lag Integer. Maximum lag to compute.
#'
#' @return Numeric vector of length `max_lag + 1` with autocorrelation
#'   values starting at lag 0.
#'
#' @examples
#' compute_autocorrelation(rnorm(100), max_lag = 10)
#' @export
compute_autocorrelation <- function(x, max_lag = 10) {
  checkmate::assert_numeric(x, any.missing = FALSE, min.len = 2)
  checkmate::assert_int(max_lag, lower = 1)
  stats::acf(x, lag.max = max_lag, plot = FALSE)$acf[1:(max_lag + 1)]
}

#' Evaluate an expression with error logging
#'
#' This helper evaluates `expr` and writes any warning or error message to
#' `log_file` before re-throwing the error. A custom informational `msg` can be
#' provided to annotate the log when evaluation begins. It is useful for scripts
#' that should record failures for later inspection while still stopping
#' execution.
#'
#' @param expr An expression to evaluate.
#' @param log_file Character string giving the path to a log file. Defaults to
#'   "chaoticds.log" in the current directory.
#' @param msg Optional message to record before evaluating `expr` for context.
#'
#' @return The result of evaluating `expr` if successful.
#' @examples
#' tmp <- tempfile()
#' try(with_logging(stop("oops"), tmp, msg = "example"))
#' readLines(tmp)
#' @export
with_logging <- function(expr, log_file = "chaoticds.log", msg = NULL) {
  safe_log <- function(text) {
    tryCatch(
      cat(text, file = log_file, append = TRUE),
      error = function(e) message("Logging failed: ", e$message)
    )
  }

  tryCatch(
    withCallingHandlers(
      {
        if (!is.null(msg)) {
          safe_log(sprintf("%s INFO: %s\n", Sys.time(), msg))
        }
        eval(substitute(expr), parent.frame())
      },
      warning = function(w) {
        safe_log(paste0(Sys.time(), " WARNING: ", conditionMessage(w), "\n"))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      safe_log(paste0(Sys.time(), " ERROR: ", conditionMessage(e), "\n"))
      stop(e)
    }
  )
}
