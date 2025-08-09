if (!requireNamespace('testthat', quietly = TRUE) ||
    !requireNamespace('checkmate', quietly = TRUE) ||
    !requireNamespace('assertthat', quietly = TRUE)) {
  message("Skipping tests: packages 'testthat', 'checkmate', and 'assertthat' must be installed.")
} else {
  library(testthat)
  library(chaoticds)
  test_check('chaoticds')
}
