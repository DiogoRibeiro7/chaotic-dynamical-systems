#' Package startup tasks
#'
#' Registers global variables to appease R CMD check and prints a startup message.
#' @importFrom utils globalVariables
.onLoad <- function(libname, pkgname) {
  utils::globalVariables(c(
    "hill", "k", "threshold", "mean_excess",
    "emp_surv", "theo_surv", "Freq"
  ))
  packageStartupMessage("chaoticds package loaded")
}
