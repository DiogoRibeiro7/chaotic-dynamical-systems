#' Package startup tasks
#'
#' Registers global variables to appease R CMD check, and wires up the
#' Rcpp-compiled DLL so the package's `_cpp` functions resolve at `.Call`
#' time.
#'
#' @importFrom utils globalVariables
#' @importFrom Rcpp evalCpp
#' @useDynLib chaoticds, .registration = TRUE
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  utils::globalVariables(c(
    "hill", "k", "threshold", "mean_excess",
    "emp_surv", "theo_surv", "Freq"
  ))
}
